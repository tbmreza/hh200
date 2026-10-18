//! Strategies and their combinators.

use crate::context::{Context, HasClients};
use crate::lattice::{Count, Knowledge};
use crate::trace::{Ev, Trace};
use crate::verdict::{Basis, Kind, Verdict};

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
