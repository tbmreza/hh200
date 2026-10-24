//! Strategies and their combinators.

use crate::context::Context;
use crate::lattice::{Count, Knowledge};
use crate::trace::{Ev, Trace};
use crate::verdict::{Basis, Kind, Verdict};

// ───────────────────────── 8. strategies and their combinators ───────────

pub trait Strategy: Sized {
    fn classify(&self, trace: &Trace, ctx: &Context) -> Verdict;

    /// Fallback: first determined verdict wins.
    fn or_else<S: Strategy>(self, other: S) -> OrElse<Self, S> {
        OrElse(self, other)
    }
    /// Cross-check: both run, verdicts are met (disagreement => Conflict).
    fn agree<S: Strategy>(self, other: S) -> Agree<Self, S> {
        Agree(self, other)
    }
}

// The `A`/`B` parameters are the one remaining generic: composing two concrete
// strategies without heap allocation. (Could be `Box<dyn Strategy>` for type
// erasure if the pipeline ever becomes dynamic.)

pub struct OrElse<A, B>(A, B);
impl<A: Strategy, B: Strategy> Strategy for OrElse<A, B> {
    fn classify(&self, t: &Trace, c: &Context) -> Verdict {
        let v = self.0.classify(t, c);
        if v.kind != Kind::Undetermined { v } else { self.1.classify(t, c) }
    }
}

pub struct Agree<A, B>(A, B);
impl<A: Strategy, B: Strategy> Strategy for Agree<A, B> {
    fn classify(&self, t: &Trace, c: &Context) -> Verdict {
        self.0.classify(t, c).meet(self.1.classify(t, c))
    }
}

/// Needs nothing from the context, so ignores it.
/// Toy heuristic: closed-loop clients fire right after a completion.
pub struct InterArrival {
    pub window_ms: u64,
}
impl Strategy for InterArrival {
    fn classify(&self, t: &Trace, _: &Context) -> Verdict {
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

/// A closed model with ≤ N clients never has more than N requests
/// outstanding, so exceeding the upper bound ⇒ Open. Undetermined when the
/// client count is unknown.
pub struct ConcurrencyBound;
impl Strategy for ConcurrencyBound {
    fn classify(&self, t: &Trace, c: &Context) -> Verdict {
        let Some(f) = c.clients() else {
            return Verdict::undetermined();
        };
        let tpeak = t.peak_outstanding();
        match f.value.upper() {
            Some(n) if tpeak > n => {
                Verdict { kind: Kind::Open, basis: Basis::of(f, "clients") }
            }
            Some(_) => {
                Verdict { kind: Kind::Closed, basis: Basis::of(f, "clients") }
            }
            _ => Verdict::undetermined(),
        }
    }
}

// ───────────────────────── 9. the runtime → context boundary ──────────────

/// One `match`, once: runtime Knowledge enters the context here.
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
