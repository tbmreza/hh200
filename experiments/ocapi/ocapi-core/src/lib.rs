//! `ocapi-core` is the analysis half of ocapi: parse a traffic dump into events and classify a
//! workload as open- or closed-loop.
use std::collections::HashMap;
use std::fmt::Write as _;
use std::time::Duration;

use std::collections::BTreeSet;
use std::fmt;
use std::num::NonZeroU32;

// ───────────────────────── 1. provenance ─────────────────────────────────

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Provenance {
    Assumed, // declared order matters: Assumed < Known
    Known,
}

impl Provenance {
    /// Weakest link: a derived fact is Known only if every input was.
    pub fn join(self, other: Self) -> Self {
        self.min(other)
    }
}

// ───────────────────────── 2. meet (intersection of knowledge) ───────────

#[derive(Debug, PartialEq, Eq)]
pub struct Contradiction(pub String);

pub trait Meet: Sized {
    fn meet(self, other: Self) -> Result<Self, Contradiction>;
}

// ───────────────────────── 3. Count: "how many clients" as an interval ───

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Count {
    lo: NonZeroU32,
    hi: Option<NonZeroU32>, // None = unbounded above
}

fn nz(n: u32) -> NonZeroU32 {
    NonZeroU32::new(n).expect("client count must be non-zero")
}

impl Count {
    pub fn exactly(n: u32) -> Self {
        Self { lo: nz(n), hi: Some(nz(n)) }
    }
    pub fn at_least(n: u32) -> Self {
        Self { lo: nz(n), hi: None }
    }
    pub fn at_most(n: u32) -> Self {
        Self { lo: nz(1), hi: Some(nz(n)) }
    }
    pub fn between(lo: u32, hi: u32) -> Self {
        assert!(lo <= hi);
        Self { lo: nz(lo), hi: Some(nz(hi)) }
    }
    pub fn upper(&self) -> Option<u32> {
        self.hi.map(NonZeroU32::get)
    }
}

impl Meet for Count {
    fn meet(self, o: Self) -> Result<Self, Contradiction> {
        let lo = self.lo.max(o.lo);
        let hi = match (self.hi, o.hi) {
            (Some(a), Some(b)) => Some(a.min(b)),
            (a, b) => a.or(b),
        };
        match hi {
            Some(h) if h < lo => Err(Contradiction(format!("{self} ∩ {o} is empty"))),
            _ => Ok(Count { lo, hi }),
        }
    }
}

impl fmt::Display for Count {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.hi {
            Some(h) if h == self.lo => write!(f, "{}", self.lo),
            Some(h) => write!(f, "[{}, {}]", self.lo, h),
            None => write!(f, "≥{}", self.lo),
        }
    }
}

// ───────────────────────── 4. Fact<T> and Knowledge<T> ───────────────────

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fact<T> {
    pub value: T,
    pub provenance: Provenance,
}

impl<T> Fact<T> {
    pub fn known(value: T) -> Self {
        Self { value, provenance: Provenance::Known }
    }
    pub fn assumed(value: T) -> Self {
        Self { value, provenance: Provenance::Assumed }
    }
    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Fact<U> {
        Fact { value: f(self.value), provenance: self.provenance }
    }
    /// Combine two independent facts; provenance degrades to the weaker one.
    pub fn zip<U>(self, other: Fact<U>) -> Fact<(T, U)> {
        Fact {
            value: (self.value, other.value),
            provenance: self.provenance.join(other.provenance),
        }
    }
}

impl<T: Meet> Fact<T> {
    /// Two sources describing the *same* quantity: intersect the values.
    pub fn meet(self, other: Self) -> Result<Self, Contradiction> {
        Ok(Fact {
            value: self.value.meet(other.value)?,
            provenance: self.provenance.join(other.provenance),
        })
    }
}

/// Runtime-level knowledge: what config files / CLI flags produce.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Knowledge<T> {
    Unknown,
    Fact(Fact<T>),
}

impl<T> Knowledge<T> {
    /// Prefer the better-provenanced source; ties go to `self`.
    /// (`cli.or(config)` = "CLI wins unless config is strictly more certain".)
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Knowledge::Unknown, k) | (k, Knowledge::Unknown) => k,
            (Knowledge::Fact(a), Knowledge::Fact(b)) => {
                Knowledge::Fact(if b.provenance > a.provenance { b } else { a })
            }
        }
    }
}

impl<T: Meet> Knowledge<T> {
    /// Unknown is the identity; two facts intersect.
    pub fn meet(self, other: Self) -> Result<Self, Contradiction> {
        match (self, other) {
            (Knowledge::Unknown, k) | (k, Knowledge::Unknown) => Ok(k),
            (Knowledge::Fact(a), Knowledge::Fact(b)) => a.meet(b).map(Knowledge::Fact),
        }
    }
}

// ───────────────────────── 5. typed context (typestate) ──────────────────

pub struct NoInfo;

/// Defaulted type parameter: `Context` alone means "nothing known".
pub struct Context<C = NoInfo> {
    clients: C,
}

impl Context {
    pub fn empty() -> Self {
        Context { clients: NoInfo }
    }
}

impl<C> Context<C> {
    pub fn with_clients(self, c: Fact<Count>) -> Context<Fact<Count>> {
        Context { clients: c }
    }
}

/// Capability trait: strategies state what they need from the context.
pub trait HasClients {
    fn clients(&self) -> &Fact<Count>;
}
impl HasClients for Context<Fact<Count>> {
    fn clients(&self) -> &Fact<Count> {
        &self.clients
    }
}

// ───────────────────────── 6. verdicts that remember their basis ─────────

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Undetermined, // ⊤ : no information
    Open,
    Closed,
    Conflict, //     ⊥ : sources disagree
}

impl Kind {
    pub fn meet(self, o: Kind) -> Kind {
        match (self, o) {
            (Kind::Undetermined, k) | (k, Kind::Undetermined) => k,
            (a, b) if a == b => a,
            _ => Kind::Conflict,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Basis {
    Unconditional,
    Conditional(BTreeSet<&'static str>), // names of the assumptions relied on
}

impl Basis {
    pub fn join(self, other: Basis) -> Basis {
        match (self, other) {
            (Basis::Unconditional, b) | (b, Basis::Unconditional) => b,
            (Basis::Conditional(mut a), Basis::Conditional(b)) => {
                a.extend(b);
                Basis::Conditional(a)
            }
        }
    }
    /// Known facts leave no trace; assumed ones are recorded by name.
    pub fn of<T>(fact: &Fact<T>, name: &'static str) -> Basis {
        match fact.provenance {
            Provenance::Known => Basis::Unconditional,
            Provenance::Assumed => Basis::Conditional(BTreeSet::from([name])),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Verdict {
    pub kind: Kind,
    pub basis: Basis,
}

impl Verdict {
    pub fn undetermined() -> Self {
        Verdict { kind: Kind::Undetermined, basis: Basis::Unconditional }
    }
    /// Combine two verdicts about the same client. A side that said nothing
    /// contributes neither a kind nor an assumption.
    pub fn meet(self, o: Verdict) -> Verdict {
        let kind = self.kind.meet(o.kind);
        let basis = match (self.kind, o.kind) {
            (Kind::Undetermined, _) => o.basis,
            (_, Kind::Undetermined) => self.basis,
            _ => self.basis.join(o.basis),
        };
        Verdict { kind, basis }
    }
}

// ───────────────────────── 7. trace ──────────────────────────────────────

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Ev {
    Arrive,
    Complete,
}

/// (time in ms, event), sorted by time.
#[derive(Debug)]
pub struct Trace(pub Vec<(u64, Ev)>);

impl Trace {
    fn times(&self, which: Ev) -> Vec<u64> {
        self.0.iter().filter(|(_, e)| *e == which).map(|(t, _)| *t).collect()
    }
    pub fn peak_outstanding(&self) -> u32 {
        let (mut cur, mut peak) = (0i64, 0i64);
        for &(_, e) in &self.0 {
            cur += if e == Ev::Arrive { 1 } else { -1 };
            peak = peak.max(cur);
        }
        peak as u32
    }
}

// ───────────────────────── 8. strategies and their combinators ───────────

pub trait Strategy<Ctx> {
    fn classify(&self, trace: &Trace, ctx: &Ctx) -> Verdict;
}

/// Builders live in their own trait, with no `Ctx` parameter, so a pipeline
/// can be assembled before the context type is known. The `HasClients`
/// requirement is then checked where the pipeline meets a context, at
/// `.classify(..)`. (Blanket-implemented for all types; a real crate would
/// seal it or restrict it to strategy types.)
pub trait Combine: Sized {
    /// Fallback: first determined verdict wins.
    fn or_else<S>(self, other: S) -> OrElse<Self, S> {
        OrElse(self, other)
    }
    /// Cross-check: both run, verdicts are met (disagreement => Conflict).
    fn agree<S>(self, other: S) -> Agree<Self, S> {
        Agree(self, other)
    }
}
impl<T> Combine for T {}

pub struct OrElse<A, B>(A, B);
impl<Ctx, A: Strategy<Ctx>, B: Strategy<Ctx>> Strategy<Ctx> for OrElse<A, B> {
    fn classify(&self, t: &Trace, c: &Ctx) -> Verdict {
        println!("here0");
        let v = self.0.classify(t, c);
        if v.kind != Kind::Undetermined { v } else { self.1.classify(t, c) }
    }
}

pub struct Agree<A, B>(A, B);
impl<Ctx, A: Strategy<Ctx>, B: Strategy<Ctx>> Strategy<Ctx> for Agree<A, B> {
    fn classify(&self, t: &Trace, c: &Ctx) -> Verdict {
        println!("here1");
        self.0.classify(t, c).meet(self.1.classify(t, c))
    }
}

/// Needs nothing from the context, so works for *every* `Context<_>`.
/// Toy heuristic: closed-loop clients fire right after a completion.
pub struct InterArrival {
    pub window_ms: u64,
}
impl<Ctx> Strategy<Ctx> for InterArrival {
    fn classify(&self, t: &Trace, _: &Ctx) -> Verdict {
        let (arr, comp) = (t.times(Ev::Arrive), t.times(Ev::Complete));
        if arr.len() < 5 {
            return Verdict::undetermined();
        }
        let hits = arr[1..]
            .iter()
            .filter(|&&a| comp.iter().any(|&c| c <= a && a - c <= self.window_ms))
            .count();
        let ratio = hits as f64 / (arr.len() - 1) as f64;
        let kind = match ratio {
            r if r >= 0.8 => Kind::Closed,
            r if r <= 0.2 => Kind::Open,
            _ => Kind::Undetermined,
        };
        Verdict { kind, basis: Basis::Unconditional }
    }
}

/// Requires `HasClients`: a closed model with ≤ N clients never has more
/// than N requests outstanding, so exceeding the upper bound ⇒ Open.
pub struct ConcurrencyBound;
impl<Ctx: HasClients> Strategy<Ctx> for ConcurrencyBound {
    fn classify(&self, t: &Trace, c: &Ctx) -> Verdict {
        let f = c.clients();
        println!("here3: {:?}", f);
        println!("Trace: {:?}", t);
        let tpeak = t.peak_outstanding();
        let fupper = f.value.upper();
        println!("f.value.upper()={:?}, t.peak_outstanding()={:?}", fupper, tpeak);
        match f.value.upper() {
            Some(n) if t.peak_outstanding() > n => {
                Verdict { kind: Kind::Open, basis: Basis::of(f, "clients") }
            }
            Some(n) => {
                Verdict { kind: Kind::Closed, basis: Basis::of(f, "clients") }
            }
            _ => {
                println!("here3.1");
                Verdict::undetermined()
            },
        }
    }
}

// ───────────────────────── 9. the runtime → typed boundary ───────────────

/// One `match`, once: runtime Knowledge enters the typed world here.
pub fn diagnose(trace: &Trace, clients: Knowledge<Count>) -> Verdict {
    let ia = InterArrival { window_ms: 2 };
    match clients {
        Knowledge::Unknown => ia.classify(trace, &Context::empty()),
        Knowledge::Fact(f) => {
            let ctx = Context::empty().with_clients(f);
            ia.or_else(ConcurrencyBound).classify(trace, &ctx)
        }
    }
}

// ───────────────────────── demo ──────────────────────────────────────────

fn closed_trace() -> Trace {
    // 2 clients, 10 rounds each: service 10ms, think 1ms, client B offset 3ms.
    let mut ev = vec![];
    for off in [0u64, 3] {
        let mut t = off;
        for _ in 0..10 {
            ev.push((t, Ev::Arrive));
            ev.push((t + 10, Ev::Complete));
            t += 11;
        }
    }
    ev.sort_by_key(|&(t, e)| (t, e == Ev::Arrive)); // completions before arrivals on ties
    Trace(ev)
}

fn open_trace() -> Trace {
    // arrivals every 7ms regardless of completions; service 50ms.
    let mut ev = vec![];
    for k in 0..20u64 {
        ev.push((7 * k, Ev::Arrive));
        ev.push((7 * k + 50, Ev::Complete));
    }
    ev.sort_by_key(|&(t, e)| (t, e == Ev::Arrive));
    Trace(ev)
}

fn short_burst() -> Trace {
    // too few events for InterArrival to say anything.
    Trace(vec![
        (0, Ev::Arrive), (1, Ev::Arrive), (2, Ev::Arrive), (3, Ev::Arrive),
        (50, Ev::Complete), (51, Ev::Complete), (52, Ev::Complete), (53, Ev::Complete),
    ])
}

fn show(label: &str, v: Verdict) {
    println!("  {label:<46} {:?} / {:?}", v.kind, v.basis);
}

// Too little data
//
// Empty trace: no events at all.
// Lone arrive: A1 with no completion.
// Single pair: A1 C1. There's no inter-arrival time, so the verdict should be "can't tell", not a guess.
// [DGC-01] One and a half pairs: A1 C1 A2. There's one inter-arrival, and it's consistent with both models.
//
// The half pair has a mirror image that's worth its own case: A1 A2 C1. Here A2 arrives before C1 completes, so the client didn't wait. That's evidence for open, or for closed with at least 2 clients, so the classifier needs to know the client count or stay inconclusive.
//
// [DGC-02] Malformed input
//
// Orphan complete: C1 with no matching arrive.
// Complete before arrive: C1 timestamped earlier than A1 (clock skew or reordering).
// Duplicate IDs: two arrives with the same request ID.
// Out-of-order completion: A1 A2 C2 C1, which is legitimate with HTTP/2 or pipelining and must not be read as misbehavior.
//
// Zero-width timing
//
// Zero service time: C1 at the same timestamp as A1.
// Zero inter-arrival: A1 and A2 at the same timestamp (a burst).
// Zero think time: a single closed client with A(n+1) == C(n) exactly, repeated.
//
// Genuinely ambiguous (ground truth can't be recovered from timing alone)
//
// Constant period, constant service time: an open generator with a fixed period is indistinguishable from a closed client with a fixed think time. The verdict should be "can't tell", or at least low confidence.
// Lockstep clients: N closed clients all arriving at t0, all completing, all re-arriving at t1. This looks like periodic bursts, which an open generator could also produce.
//
// Structural extremes
//
// All arrives, then all completes: A1 A2 A3 C1 C2 C3. Overload where the client clearly didn't wait, so a strong open signal.
// Strictly serial: A1 C1 A2 C2 A3 C3 with irregular gaps. The cleanest closed signal, and a good positive control.
// Mid-trace idle gap: a long silence between two otherwise regular segments, so the classifier shouldn't treat the gap as think time.
pub fn classify_at_once(capped_logs: String) -> () {
}

pub type Timestamp = u64;

/// A black-box logger (routes.rs) has no notion of which
/// client/session a request belongs to; it only knows "this request, when it
/// arrived, and if/when it completed." therefore not a session ID.
pub type EventId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Event {
    pub arrival: Timestamp,
    pub completion: Option<Timestamp>,
}

// ---------------------------------------------------------------------------
// inter_arrivals projection (unchanged in spirit from before, session-free now)
// ---------------------------------------------------------------------------

pub fn events_to_inter_arrivals(events: &[Event]) -> (Option<Timestamp>, Vec<Duration>) {
    if events.is_empty() {
        return (None, Vec::new());
    }

    let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
    arrivals.sort_unstable();

    let first = arrivals[0];
    let gaps = arrivals
        .windows(2)
        .map(|w| Duration::from_nanos(w[1] - w[0]))
        .collect();

    (Some(first), gaps)
}

pub fn inter_arrivals_to_arrivals(first: Option<Timestamp>, gaps: &[Duration]) -> Vec<Timestamp> {
    let Some(first) = first else {
        debug_assert!(gaps.is_empty(), "gaps present with no first arrival");
        return Vec::new();
    };

    let mut arrivals = Vec::with_capacity(gaps.len() + 1);
    let mut current = first;
    arrivals.push(current);
    for gap in gaps {
        current += gap.as_nanos() as u64;
        arrivals.push(current);
    }
    arrivals
}

pub fn sorted_arrivals(events: &[Event]) -> Vec<Timestamp> {
    let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
    arrivals.sort_unstable();
    arrivals
}

// ---------------------------------------------------------------------------
// traffic.dump textual format
//
// Line-oriented, append-only, two-phase per event:
//
//   # ocapi traffic.dump v1
//   A <id> <arrival_ns>
//   C <id> <completion_ns>
//
// `A` is written the instant a request arrives -- routes.rs knows nothing
// else at that point. `C` is appended later, once (if) the response is sent.
// A dangling `A` with no matching `C` is not an error: it's a request that
// was still in flight (or crashed, or the dump was truncated) when the log
// ends, and becomes `Event { completion: None, .. }` on read -- the same
// semantics as `Option<Timestamp>` in the in-memory type. This is the
// meeting-point contract between routes.rs (writer) and
// ocapi-core (reader): neither side needs to buffer the whole trace or
// coordinate beyond "share this file/stream".
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    /// Line didn't match `A <id> <ts>` / `C <id> <ts>` / a comment.
    Malformed { line_no: usize, line: String },
    /// A `C` line referenced an id with no prior `A` line -- a genuine
    /// contract violation (routes.rs must always log arrival before
    /// completion), not a "still in flight" case.
    CompletionWithoutArrival { line_no: usize, id: EventId },
    /// Same id logged as `A` twice -- routes.rs's id assignment is broken.
    DuplicateArrival { line_no: usize, id: EventId },
}

pub fn write_dump(events: &[Event]) -> String {
    let mut out = String::from("# ocapi traffic.dump v1\n");
    for (id, event) in events.iter().enumerate() {
        let id = id as EventId;
        let _ = writeln!(out, "A {id} {}", event.arrival);
        if let Some(completion) = event.completion {
            let _ = writeln!(out, "C {id} {completion}");
        }
    }
    out
}

pub fn parse_dump(text: &str) -> Result<Vec<Event>, ParseError> {
    let mut arrivals: HashMap<EventId, Timestamp> = HashMap::new();
    let mut completions: HashMap<EventId, Timestamp> = HashMap::new();
    let mut order: Vec<EventId> = Vec::new();

    for (line_no, line) in text.lines().enumerate() {
        let line_no = line_no + 1; // 1-indexed for humans reading errors
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let mut parts = trimmed.split_whitespace();
        let (Some(tag), Some(id_str), Some(ts_str), None) =
            (parts.next(), parts.next(), parts.next(), parts.next())
        else {
            return Err(ParseError::Malformed {
                line_no,
                line: line.to_string(),
            });
        };

        let parse_u64 = |s: &str| {
            s.parse::<u64>().map_err(|_| ParseError::Malformed {
                line_no,
                line: line.to_string(),
            })
        };
        let id: EventId = parse_u64(id_str)?;
        let ts: Timestamp = parse_u64(ts_str)?;

        match tag {
            "A" => {
                if arrivals.insert(id, ts).is_some() {
                    return Err(ParseError::DuplicateArrival { line_no, id });
                }
                order.push(id);
            }
            "C" => {
                if !arrivals.contains_key(&id) {
                    return Err(ParseError::CompletionWithoutArrival { line_no, id });
                }
                // A later C for the same id overwrites -- treated as a
                // corrected/updated completion record rather than an error,
                // since "logger re-emits a line" is a plausible append-only
                // recovery strategy.
                completions.insert(id, ts);
            }
            _ => {
                return Err(ParseError::Malformed {
                    line_no,
                    line: line.to_string(),
                })
            }
        }
    }

    Ok(order
        .into_iter()
        .map(|id| Event {
            arrival: arrivals[&id],
            completion: completions.get(&id).copied(),
        })
        .collect())
}

/// Convert a `traffic.dump` string into a `Trace` for the classifiers.
pub fn validate_dump(text: &str) -> Result<Trace, ParseError> {
    let events = parse_dump(text)?;
    Ok(events_to_trace(&events))
}

fn events_to_trace(events: &[Event]) -> Trace {
    let mut ev = Vec::with_capacity(events.len() * 2);
    for e in events {
        ev.push((e.arrival, Ev::Arrive));
        if let Some(c) = e.completion {
            ev.push((c, Ev::Complete));
        }
    }
    ev.sort_by_key(|&(t, e)| (t, e == Ev::Arrive)); // completions before arrivals on ties
    Trace(ev)
}

// #[cfg(test)]
// mod proptests {
//     use super::*;
//     use proptest::prelude::*;
//
//     // #[test]
//     // fn debug_capped_logs() {
//     //     // let path = "/home/tbmreza/gh/hh200/experiments/ocapi/rotated.log";
//     //     let path = "/home/tbmreza/gh/hh200/experiments/ocapi/ocapi-core/fixtures/empty.dump";
//     //     let capped_logs = std::fs::read_to_string(path).expect("failed to read rotated.log");
//     //     let verdict = classify_at_once(capped_logs);
//     //     eprintln!("verdict: {verdict}");
//     // }
//
//     fn event_strategy() -> impl Strategy<Value = Event> {
//         (
//             0u64..1_000_000_000u64,
//             proptest::option::of(0u64..1_000_000_000u64),
//         )
//             .prop_map(|(arrival, completion)| Event {
//                 arrival,
//                 completion,
//             })
//     }
//
//     fn events_strategy() -> impl Strategy<Value = Vec<Event>> {
//         proptest::collection::vec(event_strategy(), 0..200)
//     }
//
//     proptest! {
//         // -- inter_arrivals projection properties (carried over) --------
//
//         /// Core roundtrip: projecting to (first, gaps) and reconstructing
//         /// must reproduce exactly the sorted arrival timestamps of the
//         /// original events. Holds regardless of `completion` content and
//         /// input order.
//         #[test]
//         fn inter_arrivals_roundtrip(events in events_strategy()) {
//             let (first, gaps) = events_to_inter_arrivals(&events);
//             let reconstructed = inter_arrivals_to_arrivals(first, &gaps);
//             let expected = sorted_arrivals(&events);
//             prop_assert_eq!(reconstructed, expected);
//         }
//
//         #[test]
//         fn gap_count_matches_event_count(events in events_strategy()) {
//             let (_, gaps) = events_to_inter_arrivals(&events);
//             prop_assert_eq!(gaps.len(), events.len().saturating_sub(1));
//         }
//
//         // -- traffic.dump textual format properties ----------------------
//
//         /// The real roundtrip this session was about: write_dump then
//         /// parse_dump must reproduce the original events exactly (arrival
//         /// AND completion, since the text format is not lossy the way the
//         /// inter_arrivals projection is -- id-order in equals id-order out).
//         #[test]
//         fn dump_roundtrip_is_exact(events in events_strategy()) {
//             let text = write_dump(&events);
//             let parsed = parse_dump(&text).expect("well-formed dump must parse");
//             prop_assert_eq!(parsed, events);
//         }
//
//         /// A dangling A with no C must parse as `completion: None`, not as
//         /// an error -- this is the "still in flight when the log ends" case
//         /// the two-phase format exists to support.
//         #[test]
//         fn dangling_arrival_becomes_none(arrival in 0u64..1_000_000_000u64) {
//             let text = format!("# ocapi traffic.dump v1\nA 0 {arrival}\n");
//             let parsed = parse_dump(&text).expect("dangling A is valid");
//             prop_assert_eq!(parsed, vec![Event { arrival, completion: None }]);
//         }
//
//         /// A C line with no prior A is a genuine contract violation (not a
//         /// "missing data" case) and must be rejected, not silently dropped
//         /// or silently accepted as a completion-only event.
//         #[test]
//         fn completion_without_arrival_is_rejected(
//             id in any::<EventId>(),
//             ts in 0u64..1_000_000_000u64,
//         ) {
//             let text = format!("# ocapi traffic.dump v1\nC {id} {ts}\n");
//             let result = parse_dump(&text);
//             prop_assert_eq!(
//                 result,
//                 Err(ParseError::CompletionWithoutArrival { line_no: 2, id })
//             );
//         }
//
//         /// Blank lines and comment lines anywhere in the file must be
//         /// transparently ignored -- a logger may want to emit periodic
//         /// human-readable markers (rotation, restart) without corrupting
//         /// the stream for ocapi-core.
//         #[test]
//         fn comments_and_blank_lines_are_ignored(events in events_strategy()) {
//             let mut text = write_dump(&events);
//             text.push_str("\n# a mid-stream comment\n\n");
//             let parsed = parse_dump(&text).expect("comments must not break parsing");
//             prop_assert_eq!(parsed, events);
//         }
//
//         /// Order preservation: parse_dump returns events in EventId order
//         /// (== write_dump's enumeration order), not sorted by arrival. This
//         /// is what lets `events_to_inter_arrivals` be the single place that
//         /// imposes sort order, rather than every producer/consumer having
//         /// its own opinion.
//         #[test]
//         fn parse_preserves_write_order(events in events_strategy()) {
//             let text = write_dump(&events);
//             let parsed = parse_dump(&text).unwrap();
//             let arrivals: Vec<Timestamp> = parsed.iter().map(|e| e.arrival).collect();
//             let original_arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
//             prop_assert_eq!(arrivals, original_arrivals);
//         }
//     }
//
//     // -- a few hand-written edge cases, since proptest strategies won't
//     // -- reliably generate specific malformed inputs on their own --------
//
//     #[test]
//     fn duplicate_arrival_is_rejected() {
//         let text = "# ocapi traffic.dump v1\nA 0 100\nA 0 200\n";
//         assert_eq!(
//             parse_dump(text),
//             Err(ParseError::DuplicateArrival { line_no: 3, id: 0 })
//         );
//     }
//
//     #[test]
//     fn malformed_line_is_rejected() {
//         let text = "# ocapi traffic.dump v1\nX 0 100\n";
//         assert_eq!(
//             parse_dump(text),
//             Err(ParseError::Malformed {
//                 line_no: 2,
//                 line: "X 0 100".to_string(),
//             })
//         );
//     }
//
//     #[test]
//     fn empty_dump_parses_to_empty_events() {
//         let text = "# ocapi traffic.dump v1\n";
//         assert_eq!(parse_dump(text), Ok(Vec::new()));
//     }
//
//     #[test]
//     fn later_completion_line_overwrites_earlier_one() {
//         // Simulates a logger re-emitting a corrected completion record.
//         let text = "# ocapi traffic.dump v1\nA 0 100\nC 0 150\nC 0 160\n";
//         let parsed = parse_dump(text).unwrap();
//         assert_eq!(
//             parsed,
//             vec![Event {
//                 arrival: 100,
//                 completion: Some(160)
//             }]
//         );
//     }
// }

#[cfg(test)]
mod fixture_tests {
    use super::*;

    /// One test per golden fixture in `fixtures/*.dump`. Each test embeds its
    /// file at compile time (`include_str!`), parses it into a `Trace`,
    /// classifies it, and asserts the verdict recorded in the fixture's own
    /// `#` header and `fixtures/README.md`.

    #[test]
    fn empty_dump_is_undetermined() {
        let trace = validate_dump(include_str!("../fixtures/empty.dump")).unwrap();
        assert_eq!(diagnose(&trace, Knowledge::Unknown), Verdict::undetermined());
    }

    #[test]
    fn lone_arrive_is_undetermined() {
        let trace = validate_dump(include_str!("../fixtures/lone_arrive.dump")).unwrap();
        assert_eq!(diagnose(&trace, Knowledge::Unknown), Verdict::undetermined());
    }

    #[test]
    fn single_pair_is_undetermined() {
        let trace = validate_dump(include_str!("../fixtures/single_pair.dump")).unwrap();
        assert_eq!(diagnose(&trace, Knowledge::Unknown), Verdict::undetermined());
    }

    #[test]
    fn one_and_a_half_pairs_is_undetermined() {
        let trace = validate_dump(include_str!("../fixtures/one_and_a_half_pairs.dump")).unwrap();
        assert_eq!(diagnose(&trace, Knowledge::Unknown), Verdict::undetermined());
    }

    #[test]
    fn one_and_a_half_pairs_mirror_depends_on_client_count() {
        let trace =
            validate_dump(include_str!("../fixtures/one_and_a_half_pairs_mirror.dump")).unwrap();

        // A2 arrives before C1 completes (peak outstanding = 2): a single
        // client can't be in flight twice, so Open; >=2 clients is Closed.
        assert_eq!(
            diagnose(&trace, Knowledge::Fact(Fact::known(Count::exactly(1)))).kind,
            Kind::Open,
        );
        assert_eq!(
            diagnose(&trace, Knowledge::Fact(Fact::known(Count::exactly(2)))).kind,
            Kind::Closed,
        );
    }
}
