//! Verdicts that remember their basis (which assumptions they rely on).

use std::collections::BTreeSet;

use crate::lattice::{Fact, Provenance};

// ───────────────────────── 6. verdicts that remember their basis ─────────

//        Conflict (⊤)
//        /        \
//     Open       Closed
//        \        /
//     Undetermined (⊥)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    Conflict,
    Open, Closed,
    Undetermined,
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
