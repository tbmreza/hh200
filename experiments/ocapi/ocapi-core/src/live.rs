//! Live, incremental verdict engine.
//!
//! Unlike `dump::parse_dump` (which classifies a whole trace in one go), this
//! accumulates events as they happen and re-derives the verdict on demand.
//! Doesn't need UI dependencies.

use std::collections::HashMap;

use crate::dump::{Event, EventId, Timestamp};
use crate::lattice::Knowledge;
use crate::strategy::diagnose;
use crate::trace::{Ev, Trace};
use crate::verdict::Verdict;

/// Nanoseconds per millisecond, used to normalize real epoch-nanosecond
/// timestamps down to the milliseconds the `InterArrival` strategy reasons in.
const NS_PER_MS: u64 = 1_000_000;

/// Accumulates arrivals/completions and classifies the running workload.
///
/// The traffic model has no session/client identity, so ids are treated as
/// opaque event ids (the same contract as `dump::Event`). `record_completion`
/// for an id that has not been recorded as arrived is a no-op, mirroring the
/// "still in flight" semantics of the dump reader.
#[derive(Debug, Default)]
pub struct VerdictEngine {
    arrivals: HashMap<EventId, Timestamp>,
    completions: HashMap<EventId, Timestamp>,
    order: Vec<EventId>,
}

impl VerdictEngine {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn record_arrival(&mut self, id: EventId, ts: Timestamp) {
        // ??: Duplicate arrivals are ignored: a live monitor must not corrupt its
        // own state over a re-emitted line. Maybe interesting how often this
        // happens.
        if self.arrivals.contains_key(&id) {
            return;
        }
        self.arrivals.insert(id, ts);
        self.order.push(id);
    }

    pub fn record_completion(&mut self, id: EventId, ts: Timestamp) {
        // A later completion overwrites an earlier one, matching parse_dump.
        self.completions.insert(id, ts);
    }

    /// Number of distinct arrivals seen so far.
    pub fn sample_count(&self) -> usize {
        self.order.len()
    }

    /// All events in arrival order (same shape as `parse_dump`'s output).
    pub fn events(&self) -> Vec<Event> {
        self.order
            .iter()
            .map(|&id| Event {
                arrival: self.arrivals[&id],
                completion: self.completions.get(&id).copied(),
            })
            .collect()
    }

    /// The most recent `limit` events, oldest first, paired with their ids
    /// (for live per-connection tables that only care about the tail).
    pub fn recent(&self, limit: usize) -> Vec<(EventId, Event)> {
        let skip = self.order.len().saturating_sub(limit);
        self.order[skip..]
            .iter()
            .map(|&id| {
                (
                    id,
                    Event {
                        arrival: self.arrivals[&id],
                        completion: self.completions.get(&id).copied(),
                    },
                )
            })
            .collect()
    }

    /// Current global verdict over everything ingested so far. No client
    /// count is assumed, so this is the `Knowledge::Unknown` reading.
    pub fn classify(&self) -> Verdict {
        classify_events(&self.events())
    }
}

/// Classify a set of events (any order) into a global verdict, assuming no
/// client count. Timestamps are interpreted as nanoseconds and normalized to
/// milliseconds before the strategy sees them.
pub fn classify_events(events: &[Event]) -> Verdict {
    diagnose(&events_to_trace_ms(events), Knowledge::Unknown)
}

fn events_to_trace_ms(events: &[Event]) -> Trace {
    let mut ev = Vec::with_capacity(events.len() * 2);
    for e in events {
        ev.push((e.arrival / NS_PER_MS, Ev::Arrive));
        if let Some(c) = e.completion {
            ev.push((c / NS_PER_MS, Ev::Complete));
        }
    }
    // completions before arrivals on ties, matching `dump::events_to_trace`.
    ev.sort_by_key(|&(t, e)| (t, e == Ev::Arrive));
    Trace(ev)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::verdict::Kind;

    fn ms_to_ns(ms: u64) -> u64 {
        ms * NS_PER_MS
    }

    #[test]
    fn empty_engine_is_undetermined() {
        let engine = VerdictEngine::new();
        assert_eq!(engine.classify().kind, Kind::Undetermined);
        assert_eq!(engine.sample_count(), 0);
    }

    #[test]
    fn closed_loop_classifies_closed() {
        // 2 clients, 10 rounds each: service 10ms, think 1ms, offset 3ms.
        // (mirrors `closed_trace` in lib.rs, in nanoseconds.)
        let mut engine = VerdictEngine::new();
        let mut id = 0u64;
        for off in [0u64, 3] {
            let mut t = off;
            for _ in 0..10 {
                engine.record_arrival(id, ms_to_ns(t));
                engine.record_completion(id, ms_to_ns(t + 10));
                id += 1;
                t += 11;
            }
        }
        assert_eq!(engine.sample_count(), 20);
        assert_eq!(engine.classify().kind, Kind::Closed);
    }

    #[test]
    fn completion_without_arrival_is_ignored() {
        let mut engine = VerdictEngine::new();
        engine.record_completion(7, 1000);
        assert_eq!(engine.sample_count(), 0);
        assert_eq!(engine.events(), Vec::new());
    }

    #[test]
    fn recent_returns_tail_only() {
        let mut engine = VerdictEngine::new();
        for id in 0..10u64 {
            engine.record_arrival(id, id * 10);
            engine.record_completion(id, id * 10 + 1);
        }
        let recent = engine.recent(3);
        assert_eq!(recent.len(), 3);
        assert_eq!(recent[0].0, 7);
        assert_eq!(recent[0].1.arrival, 70);
        assert_eq!(recent[2].0, 9);
        assert_eq!(recent[2].1.arrival, 90);
    }
}
