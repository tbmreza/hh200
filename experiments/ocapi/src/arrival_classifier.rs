pub enum Verdict {
    Open,
    Close,
    Unknown,
}
// fn classify(inter_arrivals) -> Verdict

// pub type Timestamp = u64;
//
// pub type SessionId = u32;
//
// #[derive(Debug, Clone, PartialEq, Eq)]
// pub struct Event {
//     pub session_id: SessionId,
//     pub arrival: Timestamp,
//     pub completion: Option<Timestamp>,
// }
//
// /// The global, session-blind projection: events sorted by arrival, gaps between
// /// consecutive arrivals taken as Duration. This is projection (1) from the
// /// discussion — it discards session_id and completion entirely.
// ///
// /// Returns the sequence of inter-arrival durations *and* the first arrival
// /// timestamp, since the first arrival has no predecessor and is otherwise lost.
// /// Without carrying the origin, reconstruction could only recover *relative*
// /// arrival times, not the originals.
// pub fn events_to_inter_arrivals(events: &[Event]) -> (Option<Timestamp>, Vec<Duration>) {
//     if events.is_empty() {
//         return (None, Vec::new());
//     }
//
//     let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
//     arrivals.sort_unstable();
//
//     let first = arrivals[0];
//     let gaps = arrivals
//         .windows(2)
//         .map(|w| Duration::from_nanos(w[1] - w[0]))
//         .collect();
//
//     (Some(first), gaps)
// }
//
// /// Reconstructs the sorted sequence of arrival timestamps from the first
// /// arrival and the inter-arrival gaps. This is the honest inverse of the
// /// arrival-only half of `events_to_inter_arrivals` -- it does NOT reconstruct
// /// session_id or completion, because those are genuinely discarded by the
// /// projection. Roundtripping further than this is not a well-defined property.
// pub fn inter_arrivals_to_arrivals(
//     first: Option<Timestamp>,
//     gaps: &[Duration],
// ) -> Vec<Timestamp> {
//     let Some(first) = first else {
//         debug_assert!(gaps.is_empty(), "gaps present with no first arrival");
//         return Vec::new();
//     };
//
//     let mut arrivals = Vec::with_capacity(gaps.len() + 1);
//     let mut current = first;
//     arrivals.push(current);
//     for gap in gaps {
//         current += gap.as_nanos() as u64;
//         arrivals.push(current);
//     }
//     arrivals
// }
//
// /// Convenience: extract just the sorted arrival timestamps from events,
// /// for comparing against `inter_arrivals_to_arrivals` output.
// pub fn sorted_arrivals(events: &[Event]) -> Vec<Timestamp> {
//     let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
//     arrivals.sort_unstable();
//     arrivals
// }
//
// #[cfg(test)]
// mod proptests {
//     use super::*;
//     use proptest::prelude::*;
//
//     /// Strategy for a single Event. Arrival timestamps are kept in a modest
//     /// range so that `windows(2)` gap sums can't overflow u64 during the
//     /// roundtrip, and session_id / completion are generated but (as noted)
//     /// intentionally NOT checked by the roundtrip property below, since the
//     /// projection is known to discard them.
//     fn event_strategy() -> impl Strategy<Value = Event> {
//         (
//             any::<SessionId>(),
//             0u64..1_000_000_000u64,
//             proptest::option::of(0u64..1_000_000_000u64),
//         )
//             .prop_map(|(session_id, arrival, completion)| Event {
//                 session_id,
//                 arrival,
//                 completion,
//             })
//     }
//
//     fn events_strategy() -> impl Strategy<Value = Vec<Event>> {
//         proptest::collection::vec(event_strategy(), 0..200)
//     }
//
//     proptest! {
//         /// The core roundtrip property: projecting events to (first, gaps)
//         /// and reconstructing arrivals from that must reproduce exactly the
//         /// sorted arrival timestamps of the original events. This holds
//         /// regardless of session_id/completion content, and regardless of
//         /// input order, because the projection sorts internally.
//         #[test]
//         fn roundtrip_recovers_sorted_arrivals(events in events_strategy()) {
//             let (first, gaps) = events_to_inter_arrivals(&events);
//             let reconstructed = inter_arrivals_to_arrivals(first, &gaps);
//             let expected = sorted_arrivals(&events);
//
//             prop_assert_eq!(reconstructed, expected);
//         }
//
//         /// Sanity check on shape: the number of gaps is always one less than
//         /// the number of events (or zero gaps for 0 or 1 events).
//         #[test]
//         fn gap_count_matches_event_count(events in events_strategy()) {
//             let (_, gaps) = events_to_inter_arrivals(&events);
//             let expected_len = events.len().saturating_sub(1);
//             prop_assert_eq!(gaps.len(), expected_len);
//         }
//
//         /// Permutation invariance: shuffling the input events before
//         /// projecting must not change the result, since the projection sorts
//         /// by arrival internally. This is the property that justifies
//         /// calling this a "global, order-of-occurrence" view rather than an
//         /// "input-order" view.
//         #[test]
//         fn projection_is_permutation_invariant(
//             events in events_strategy(),
//             seed in any::<u64>(),
//         ) {
//             use std::collections::hash_map::DefaultHasher;
//             use std::hash::{Hash, Hasher};
//
//             let mut shuffled = events.clone();
//             // Deterministic pseudo-shuffle keyed by `seed`, avoiding a dependency
//             // on proptest's own shuffle strategy for this cross-check.
//             shuffled.sort_by_cached_key(|e| {
//                 let mut h = DefaultHasher::new();
//                 (e.arrival, e.session_id, seed).hash(&mut h);
//                 h.finish()
//             });
//
//             let (first_a, gaps_a) = events_to_inter_arrivals(&events);
//             let (first_b, gaps_b) = events_to_inter_arrivals(&shuffled);
//
//             prop_assert_eq!(first_a, first_b);
//             prop_assert_eq!(gaps_a, gaps_b);
//         }
//
//         /// Regression against the known lossiness: session_id and completion
//         /// are NOT recoverable from (first, gaps) alone. This test doesn't
//         /// assert failure (that's not really testable as a proptest), but
//         /// documents -- by construction -- that reconstruction only ever
//         /// produces Timestamps, never Events. If someone "fixes" the types
//         /// so this compiles into producing Events, that's a signal the
//         /// projection's contract has silently changed.
//         #[test]
//         fn reconstruction_type_is_timestamps_only(events in events_strategy()) {
//             let (first, gaps) = events_to_inter_arrivals(&events);
//             let reconstructed: Vec<Timestamp> = inter_arrivals_to_arrivals(first, &gaps);
//             prop_assert_eq!(reconstructed.len(), events.len());
//         }
//     }
// }

use std::collections::HashMap;
use std::fmt::Write as _;
use std::time::Duration;

/// A monotonic timestamp, modeled as nanoseconds since an arbitrary trace-local
/// epoch. Using a plain u64 instead of std::time::Instant/SystemTime so it's
/// trivially constructible in tests, serializable as plain text, and
/// arithmetic-transparent.
pub type Timestamp = u64;

/// Logger-assigned join key linking an arrival line to its (possible) later
/// completion line in `traffic.dump`. Not semantically meaningful beyond that
/// -- NOT a session_id. A black-box logger (routes.rs) has no notion of which
/// client/session a request belongs to; it only knows "this request, when it
/// arrived, and if/when it completed."
pub type EventId = u64;

/// session_id deliberately dropped from this type. Okapi observes a black-box
/// SUT's traffic; session structure is not something routes.rs can log because
/// it isn't observable at that vantage point. If/when we want session_id-like
/// structure, it should be a *derived, uncertainty-flagged* analysis output
/// (see prior discussion: renewal-gap clustering / concurrency estimation),
/// never a field that implies it was observed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Event {
    pub arrival: Timestamp,
    pub completion: Option<Timestamp>,
}

// ---------------------------------------------------------------------------
// inter_arrivals projection (unchanged in spirit from before, session-free now)
// ---------------------------------------------------------------------------

/// The global, session-blind projection: events sorted by arrival, gaps
/// between consecutive arrivals taken as Duration. Discards `completion`
/// (deliberately -- this view is for arrival-process statistics only; CO /
/// service-time analyses should read `Event.completion` directly rather than
/// go through this projection).
///
/// Returns the gaps *and* the first arrival timestamp, since the first
/// arrival has no predecessor and is otherwise lost -- without carrying the
/// origin, reconstruction could only recover relative arrival times, not the
/// originals.
pub fn events_to_inter_arrivals(events: &[Event]) -> (Option<Timestamp>, Vec<Duration>) {
    if events.is_empty() {
        return (None, Vec::new());
    }

    let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
    arrivals.sort_unstable();

    let first = arrivals[0];
    let gaps = arrivals
        .windows(2)
        .map(|w| Duration::from_nanos(w[1] - w[0]))
        .collect();

    (Some(first), gaps)
}

/// Reconstructs the sorted sequence of arrival timestamps from the first
/// arrival and the inter-arrival gaps. Honest inverse of the arrival-only
/// half of `events_to_inter_arrivals` -- does NOT reconstruct `completion`,
/// which is genuinely discarded by the projection.
pub fn inter_arrivals_to_arrivals(first: Option<Timestamp>, gaps: &[Duration]) -> Vec<Timestamp> {
    let Some(first) = first else {
        debug_assert!(gaps.is_empty(), "gaps present with no first arrival");
        return Vec::new();
    };

    let mut arrivals = Vec::with_capacity(gaps.len() + 1);
    let mut current = first;
    arrivals.push(current);
    for gap in gaps {
        current += gap.as_nanos() as u64;
        arrivals.push(current);
    }
    arrivals
}

/// Convenience: extract just the sorted arrival timestamps from events, for
/// comparing against `inter_arrivals_to_arrivals` output.
pub fn sorted_arrivals(events: &[Event]) -> Vec<Timestamp> {
    let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
    arrivals.sort_unstable();
    arrivals
}

// PICKUP add logging on axum routes for traffic dump textual repr.
// Line-oriented, append-only, two-phase per event:
//
//   # traffic.txt
//   A <id> <arrival_ns>
//   C <id> <completion_ns>
//
// `A` is written the instant a request arrives, `C` is appended later, once (if) the response is sent
// future default behavior is writing to xdg compliant path, overridable with cli arg.
// but for now skip implementing xdg, our first use will be always passing --dump-path arg.
// ---------------------------------------------------------------------------
// traffic.dump textual format
//
// Line-oriented, append-only, two-phase per event:
//
//   # okapi traffic.dump v1
//   A <id> <arrival_ns>
//   C <id> <completion_ns>
//
// `A` is written the instant a request arrives -- routes.rs knows nothing
// else at that point. `C` is appended later, once (if) the response is sent.
// A dangling `A` with no matching `C` is not an error: it's a request that
// was still in flight (or crashed, or the dump was truncated) when the log
// ends, and becomes `Event { completion: None, .. }` on read -- the same
// semantics as `Option<Timestamp>` in the in-memory type. This is the
// meeting-point contract between routes.rs (writer) and
// arrival_classifier.rs (reader): neither side needs to buffer the whole
// trace or coordinate beyond "share this file/stream".
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq, Eq)]
pub enum ParseError {
    /// Line didn't match `A <id> <ts>` / `C <id> <ts>` / a comment.
    Malformed { line_no: usize, line: String },
    /// A `C` line referenced an id with no prior `A` line -- a genuine
    /// contract violation (routes.rs must always log arrival before
    /// completion), not a "still in flight" case.
    CompletionWithoutArrival { line_no: usize, id: EventId },
    /// Same id logged as `A` twice -- routes.rs's id assignment is broken.
    DuplicateArrival { line_no: usize, id: EventId },
}

/// Serializes events to the `traffic.dump` text format. Assigns each event a
/// fresh sequential EventId in slice order (order here is "logger's write
/// order", not sorted by arrival -- a real logger writes in wall-clock order
/// as requests actually arrive, which is arrival order by construction, but
/// we don't assume the caller pre-sorted).
pub fn write_dump(events: &[Event]) -> String {
    let mut out = String::from("# okapi traffic.dump v1\n");
    for (id, event) in events.iter().enumerate() {
        let id = id as EventId;
        let _ = writeln!(out, "A {id} {}", event.arrival);
        if let Some(completion) = event.completion {
            let _ = writeln!(out, "C {id} {completion}");
        }
    }
    out
}

/// Parses a `traffic.dump` text into events. Order of the returned Vec
/// matches EventId order (i.e., logger write order / arrival order), NOT
/// sorted -- callers wanting the sorted-by-arrival view should still go
/// through `events_to_inter_arrivals`, which sorts internally.
pub fn parse_dump(text: &str) -> Result<Vec<Event>, ParseError> {
    let mut arrivals: HashMap<EventId, Timestamp> = HashMap::new();
    let mut completions: HashMap<EventId, Timestamp> = HashMap::new();
    let mut order: Vec<EventId> = Vec::new();

    for (line_no, line) in text.lines().enumerate() {
        let line_no = line_no + 1; // 1-indexed for humans reading errors
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }

        let mut parts = trimmed.split_whitespace();
        let (Some(tag), Some(id_str), Some(ts_str), None) =
            (parts.next(), parts.next(), parts.next(), parts.next())
        else {
            return Err(ParseError::Malformed {
                line_no,
                line: line.to_string(),
            });
        };

        let parse_u64 = |s: &str| {
            s.parse::<u64>().map_err(|_| ParseError::Malformed {
                line_no,
                line: line.to_string(),
            })
        };
        let id: EventId = parse_u64(id_str)?;
        let ts: Timestamp = parse_u64(ts_str)?;

        match tag {
            "A" => {
                if arrivals.insert(id, ts).is_some() {
                    return Err(ParseError::DuplicateArrival { line_no, id });
                }
                order.push(id);
            }
            "C" => {
                if !arrivals.contains_key(&id) {
                    return Err(ParseError::CompletionWithoutArrival { line_no, id });
                }
                // A later C for the same id overwrites -- treated as a
                // corrected/updated completion record rather than an error,
                // since "logger re-emits a line" is a plausible append-only
                // recovery strategy.
                completions.insert(id, ts);
            }
            _ => {
                return Err(ParseError::Malformed {
                    line_no,
                    line: line.to_string(),
                })
            }
        }
    }

    Ok(order
        .into_iter()
        .map(|id| Event {
            arrival: arrivals[&id],
            completion: completions.get(&id).copied(),
        })
        .collect())
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    fn event_strategy() -> impl Strategy<Value = Event> {
        (
            0u64..1_000_000_000u64,
            proptest::option::of(0u64..1_000_000_000u64),
        )
            .prop_map(|(arrival, completion)| Event {
                arrival,
                completion,
            })
    }

    fn events_strategy() -> impl Strategy<Value = Vec<Event>> {
        proptest::collection::vec(event_strategy(), 0..200)
    }

    proptest! {
        // -- inter_arrivals projection properties (carried over) --------

        /// Core roundtrip: projecting to (first, gaps) and reconstructing
        /// must reproduce exactly the sorted arrival timestamps of the
        /// original events. Holds regardless of `completion` content and
        /// input order.
        #[test]
        fn inter_arrivals_roundtrip(events in events_strategy()) {
            let (first, gaps) = events_to_inter_arrivals(&events);
            let reconstructed = inter_arrivals_to_arrivals(first, &gaps);
            let expected = sorted_arrivals(&events);
            prop_assert_eq!(reconstructed, expected);
        }

        #[test]
        fn gap_count_matches_event_count(events in events_strategy()) {
            let (_, gaps) = events_to_inter_arrivals(&events);
            prop_assert_eq!(gaps.len(), events.len().saturating_sub(1));
        }

        // -- traffic.dump textual format properties ----------------------

        /// The real roundtrip this session was about: write_dump then
        /// parse_dump must reproduce the original events exactly (arrival
        /// AND completion, since the text format is not lossy the way the
        /// inter_arrivals projection is -- id-order in equals id-order out).
        #[test]
        fn dump_roundtrip_is_exact(events in events_strategy()) {
            let text = write_dump(&events);
            let parsed = parse_dump(&text).expect("well-formed dump must parse");
            prop_assert_eq!(parsed, events);
        }

        /// A dangling A with no C must parse as `completion: None`, not as
        /// an error -- this is the "still in flight when the log ends" case
        /// the two-phase format exists to support.
        #[test]
        fn dangling_arrival_becomes_none(arrival in 0u64..1_000_000_000u64) {
            let text = format!("# okapi traffic.dump v1\nA 0 {arrival}\n");
            let parsed = parse_dump(&text).expect("dangling A is valid");
            prop_assert_eq!(parsed, vec![Event { arrival, completion: None }]);
        }

        /// A C line with no prior A is a genuine contract violation (not a
        /// "missing data" case) and must be rejected, not silently dropped
        /// or silently accepted as a completion-only event.
        #[test]
        fn completion_without_arrival_is_rejected(
            id in any::<EventId>(),
            ts in 0u64..1_000_000_000u64,
        ) {
            let text = format!("# okapi traffic.dump v1\nC {id} {ts}\n");
            let result = parse_dump(&text);
            prop_assert_eq!(
                result,
                Err(ParseError::CompletionWithoutArrival { line_no: 2, id })
            );
        }

        /// Blank lines and comment lines anywhere in the file must be
        /// transparently ignored -- a logger may want to emit periodic
        /// human-readable markers (rotation, restart) without corrupting
        /// the stream for arrival_classifier.rs.
        #[test]
        fn comments_and_blank_lines_are_ignored(events in events_strategy()) {
            let mut text = write_dump(&events);
            text.push_str("\n# a mid-stream comment\n\n");
            let parsed = parse_dump(&text).expect("comments must not break parsing");
            prop_assert_eq!(parsed, events);
        }

        /// Order preservation: parse_dump returns events in EventId order
        /// (== write_dump's enumeration order), not sorted by arrival. This
        /// is what lets `events_to_inter_arrivals` be the single place that
        /// imposes sort order, rather than every producer/consumer having
        /// its own opinion.
        #[test]
        fn parse_preserves_write_order(events in events_strategy()) {
            let text = write_dump(&events);
            let parsed = parse_dump(&text).unwrap();
            let arrivals: Vec<Timestamp> = parsed.iter().map(|e| e.arrival).collect();
            let original_arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
            prop_assert_eq!(arrivals, original_arrivals);
        }
    }

    // -- a few hand-written edge cases, since proptest strategies won't
    // -- reliably generate specific malformed inputs on their own --------

    #[test]
    fn duplicate_arrival_is_rejected() {
        let text = "# okapi traffic.dump v1\nA 0 100\nA 0 200\n";
        assert_eq!(
            parse_dump(text),
            Err(ParseError::DuplicateArrival { line_no: 3, id: 0 })
        );
    }

    #[test]
    fn malformed_line_is_rejected() {
        let text = "# okapi traffic.dump v1\nX 0 100\n";
        assert_eq!(
            parse_dump(text),
            Err(ParseError::Malformed {
                line_no: 2,
                line: "X 0 100".to_string(),
            })
        );
    }

    #[test]
    fn empty_dump_parses_to_empty_events() {
        let text = "# okapi traffic.dump v1\n";
        assert_eq!(parse_dump(text), Ok(Vec::new()));
    }

    #[test]
    fn later_completion_line_overwrites_earlier_one() {
        // Simulates a logger re-emitting a corrected completion record.
        let text = "# okapi traffic.dump v1\nA 0 100\nC 0 150\nC 0 160\n";
        let parsed = parse_dump(text).unwrap();
        assert_eq!(
            parsed,
            vec![Event {
                arrival: 100,
                completion: Some(160)
            }]
        );
    }
}
