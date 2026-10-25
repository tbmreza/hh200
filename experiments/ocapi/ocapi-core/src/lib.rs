//! `ocapi-core` is the analysis half of ocapi: parse a traffic dump into events and classify a
//! workload as open- or closed-loop.

mod context;
mod dump;
mod lattice;
mod live;
mod strategy;
mod trace;
mod verdict;

pub use dump::{events_to_inter_arrivals, parse_dump, Event, EventId, ParseError, Timestamp};
pub use live::{classify_events, VerdictEngine};
pub use verdict::{Basis, Kind, Verdict};

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
#[cfg(test)]
mod fixture_tests {
    use super::*;
    use crate::lattice::{Count, Fact, Knowledge};
    use crate::live::events_to_trace;
    use crate::strategy::diagnose;

    /// One test per golden fixture in `fixtures/*.dump`. Each test embeds its
    /// file at compile time (`include_str!`), parses it into events, classifies
    /// it, and asserts the verdict recorded in the fixture's own `#` header and
    /// `fixtures/README.md`. Most go through the public batch path
    /// (`parse_dump` + `classify_events`); the client-count case drives the
    /// count-aware `diagnose` directly.

    #[test]
    fn empty_dump_is_undetermined() {
        let events = parse_dump(include_str!("../fixtures/empty.dump")).unwrap();
        assert_eq!(classify_events(&events), Verdict::undetermined());
    }

    #[test]
    fn lone_arrive_is_undetermined() {
        let events = parse_dump(include_str!("../fixtures/lone_arrive.dump")).unwrap();
        assert_eq!(classify_events(&events), Verdict::undetermined());
    }

    #[test]
    fn single_pair_is_undetermined() {
        let events = parse_dump(include_str!("../fixtures/single_pair.dump")).unwrap();
        assert_eq!(classify_events(&events), Verdict::undetermined());
    }

    #[test]
    fn one_and_a_half_pairs_is_undetermined() {
        let events = parse_dump(include_str!("../fixtures/one_and_a_half_pairs.dump")).unwrap();
        assert_eq!(classify_events(&events), Verdict::undetermined());
    }

    #[test]
    fn one_and_a_half_pairs_mirror_depends_on_client_count() {
        let events =
            parse_dump(include_str!("../fixtures/one_and_a_half_pairs_mirror.dump")).unwrap();
        let trace = events_to_trace(&events);

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

    #[test]
    fn run_closed_dump_classifies_closed() {
        let text = include_str!("../fixtures/run-closed.dump");

        // Empirical capture: every arrival completed, no dangling A at log end.
        let events = parse_dump(text).expect("captured closed-loop dump must parse");
        assert!(
            events.iter().all(|e| e.completion.is_some()),
            "closed-loop capture should leave no arrivals dangling"
        );

        // classify_events normalizes epoch-ns → ms; the closed run reads Closed.
        assert_eq!(classify_events(&events).kind, Kind::Closed);
    }

    #[test]
    fn run_open_dump_classifies_open() {
        let text = include_str!("../fixtures/run-open.dump");

        // Empirical capture: every arrival completed, no dangling A at log end.
        let events = parse_dump(text).expect("captured open-loop dump must parse");
        assert!(
            events.iter().all(|e| e.completion.is_some()),
            "open-loop capture should leave no arrivals dangling"
        );

        // Arrivals are ~100ms apart (10/s); each completion lands 5ms after its
        // own arrival, so no arrival trails a completion within the 2ms window.
        assert_eq!(classify_events(&events).kind, Kind::Open);
    }
}
