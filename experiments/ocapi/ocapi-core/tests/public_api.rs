//! Public-API contract tests.
//!
//! Everything in this file uses *only* the items `ocapi-core` re-exports from
//! its crate root. If this file stops compiling, the public surface is no
//! longer sufficient for a downstream user to (a) read a dump, (b) classify it
//! in one pass, (c) drive the live engine, and (d) pull inter-arrival stats.

use ocapi_core::{classify_events, events_to_inter_arrivals, parse_dump, Kind, VerdictEngine};

#[test]
fn captured_closed_dump_classifies_closed() {
    let events = parse_dump(include_str!("../fixtures/run-closed.dump"))
        .expect("captured closed-loop dump must parse");
    assert_eq!(classify_events(&events).kind, Kind::Closed);
}

#[test]
fn captured_open_dump_classifies_open() {
    let events = parse_dump(include_str!("../fixtures/run-open.dump"))
        .expect("captured open-loop dump must parse");
    assert_eq!(classify_events(&events).kind, Kind::Open);
}

#[test]
fn empty_dump_is_undetermined() {
    let events = parse_dump(include_str!("../fixtures/empty.dump")).expect("empty dump must parse");
    assert_eq!(classify_events(&events).kind, Kind::Undetermined);
}

#[test]
fn live_engine_classifies_a_closed_loop() {
    let mut engine = VerdictEngine::new();

    // 2 clients, 10 rounds each: service 10ms, think 1ms, client B offset 3ms.
    // Timestamps are epoch-like nanoseconds, exactly as the live logger records.
    let mut id = 0u64;
    for off in [0u64, 3] {
        let mut t = off;
        for _ in 0..10 {
            engine.record_arrival(id, t * 1_000_000);
            engine.record_completion(id, (t + 10) * 1_000_000);
            id += 1;
            t += 11;
        }
    }

    assert_eq!(engine.sample_count(), 20);
    assert_eq!(engine.classify().kind, Kind::Closed);
}

#[test]
fn inter_arrival_stats_are_exposed() {
    let events = parse_dump(include_str!("../fixtures/run-open.dump"))
        .expect("captured open-loop dump must parse");
    let (_, gaps) = events_to_inter_arrivals(&events);
    assert!(!gaps.is_empty(), "open capture should have inter-arrival gaps");
    // The open run fires ~10/s, so every gap is a real, positive spacing.
    assert!(gaps.iter().all(|g| g.as_nanos() > 0));
}
