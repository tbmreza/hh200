//! Typestate context: what a strategy is allowed to observe.

use crate::lattice::{Count, Fact};

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
