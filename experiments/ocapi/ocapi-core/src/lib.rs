//! `ocapi-core` is the analysis half of ocapi: parse a traffic dump into events and classify a
//! workload as open- or closed-loop.

mod context;
mod dump;
mod lattice;
mod strategy;
mod trace;
mod verdict;

pub use context::{Context, HasClients, NoInfo};
pub use dump::{
    events_to_inter_arrivals, inter_arrivals_to_arrivals, parse_dump, sorted_arrivals,
    validate_dump, write_dump, Event, EventId, ParseError, Timestamp,
};
pub use lattice::{Contradiction, Count, Fact, Knowledge, Meet, Provenance};
pub use strategy::{diagnose, Agree, Combine, ConcurrencyBound, InterArrival, OrElse, Strategy};
pub use trace::{Ev, Trace};
pub use verdict::{Basis, Kind, Verdict};

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
