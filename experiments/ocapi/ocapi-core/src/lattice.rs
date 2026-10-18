//! Provenance-tagged facts and the meet/join lattice they live in.

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
