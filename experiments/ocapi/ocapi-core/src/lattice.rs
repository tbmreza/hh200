//! Provenance-tagged facts and the client-count knowledge they live in.
//!
//! The count-aware path (a `Knowledge::Fact` handed to `diagnose`) is currently
//! exercised only by tests; the `#[cfg(test)]` / `#[allow(dead_code)]` markers
//! below reflect that until a client count is wired into a public entry point.

use std::num::NonZeroU32;

// ───────────────────────── 1. provenance ─────────────────────────────────

/// Where a fact came from: assumed (a default) or known (observed).
///
/// `#[allow(dead_code)]`: the variants are *matched* by `Basis::of` in the
/// library, but only *constructed* in tests until the count-aware API is public.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum Provenance {
    Assumed,
    Known,
}

// ───────────────────────── 2. Count: "how many clients" as an interval ───

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Count {
    lo: NonZeroU32,
    hi: Option<NonZeroU32>, // None = unbounded above
}

#[cfg(test)]
fn nz(n: u32) -> NonZeroU32 {
    NonZeroU32::new(n).expect("client count must be non-zero")
}

impl Count {
    #[cfg(test)]
    pub fn exactly(n: u32) -> Self {
        Self { lo: nz(n), hi: Some(nz(n)) }
    }

    pub fn upper(&self) -> Option<u32> {
        self.hi.map(NonZeroU32::get)
    }
}

// ───────────────────────── 3. Fact<T> and Knowledge<T> ───────────────────

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fact<T> {
    pub value: T,
    pub provenance: Provenance,
}

impl<T> Fact<T> {
    #[cfg(test)]
    pub fn known(value: T) -> Self {
        Self { value, provenance: Provenance::Known }
    }
}

/// Runtime-level knowledge: what config files / CLI flags produce.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Knowledge<T> {
    Unknown,
    /// Constructed only in tests until the count-aware API is public.
    #[allow(dead_code)]
    Fact(Fact<T>),
}
