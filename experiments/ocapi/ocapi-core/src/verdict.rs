//! Verdicts that remember their basis (which assumptions they rely on).

use std::collections::BTreeSet;

use crate::lattice::{Fact, Provenance};

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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Basis {
    Unconditional,
    Conditional(BTreeSet<&'static str>), // names of the assumptions relied on
}

impl Basis {
    /// Known facts leave no trace; assumed ones are recorded by name.
    pub(crate) fn of<T>(fact: &Fact<T>, name: &'static str) -> Basis {
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
}
