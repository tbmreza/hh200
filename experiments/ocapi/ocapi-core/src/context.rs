//! Context: what a strategy is allowed to observe.

use crate::lattice::{Count, Fact};

// ───────────────────────── 5. context ─────────────────────────────────────

/// What is known at classification time — currently just an optional client
/// count. `None` means "unknown", so a strategy that needs the count falls
/// back to an undetermined verdict.
///
/// (If strategies ever need compile-time capability checking again, this can
/// return to a typestate `Context<C>` with a `HasClients` trait. For now the
/// extra type parameter hurt readability more than it bought safety.)
pub struct Context {
    clients: Option<Fact<Count>>,
}

impl Context {
    pub fn empty() -> Self {
        Context { clients: None }
    }

    pub fn with_clients(self, c: Fact<Count>) -> Self {
        Context { clients: Some(c) }
    }

    /// The known client count, if any.
    pub fn clients(&self) -> Option<&Fact<Count>> {
        self.clients.as_ref()
    }
}
