//! The event model, the inter-arrival projection, and the traffic.dump wire format.

use std::collections::HashMap;
#[cfg(test)]
use std::fmt::Write as _;
use std::time::Duration;

pub type Timestamp = u64;

/// A black-box logger (routes.rs) has no notion of which
/// client/session a request belongs to; it only knows "this request, when it
/// arrived, and if/when it completed." therefore not a session ID.
pub type EventId = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Event {
    pub arrival: Timestamp,
    pub completion: Option<Timestamp>,
}

// ---------------------------------------------------------------------------
// inter_arrivals projection (unchanged in spirit from before, session-free now)
// ---------------------------------------------------------------------------

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

#[cfg(test)]
fn inter_arrivals_to_arrivals(first: Option<Timestamp>, gaps: &[Duration]) -> Vec<Timestamp> {
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

#[cfg(test)]
fn sorted_arrivals(events: &[Event]) -> Vec<Timestamp> {
    let mut arrivals: Vec<Timestamp> = events.iter().map(|e| e.arrival).collect();
    arrivals.sort_unstable();
    arrivals
}

// ---------------------------------------------------------------------------
// traffic.dump textual format
//
// Line-oriented, append-only, two-phase per event:
//
//   # ocapi traffic.dump v1
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
// ocapi-core (reader): neither side needs to buffer the whole trace or
// coordinate beyond "share this file/stream".
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

#[cfg(test)]
fn write_dump(events: &[Event]) -> String {
    let mut out = String::from("# ocapi traffic.dump v1\n");
    for (id, event) in events.iter().enumerate() {
        let id = id as EventId;
        let _ = writeln!(out, "A {id} {}", event.arrival);
        if let Some(completion) = event.completion {
            let _ = writeln!(out, "C {id} {completion}");
        }
    }
    out
}

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

        /// The real roundtrip: write_dump then parse_dump must reproduce the
        /// original events exactly (arrival AND completion, since the text
        /// format is not lossy the way the inter_arrivals projection is).
        #[test]
        fn dump_roundtrip_is_exact(events in events_strategy()) {
            let text = write_dump(&events);
            let parsed = parse_dump(&text).expect("well-formed dump must parse");
            prop_assert_eq!(parsed, events);
        }

        /// A dangling A with no C must parse as `completion: None`, not as an
        /// error -- the "still in flight when the log ends" case the two-phase
        /// format exists to support.
        #[test]
        fn dangling_arrival_becomes_none(arrival in 0u64..1_000_000_000u64) {
            let text = format!("# ocapi traffic.dump v1\nA 0 {arrival}\n");
            let parsed = parse_dump(&text).expect("dangling A is valid");
            prop_assert_eq!(parsed, vec![Event { arrival, completion: None }]);
        }

        /// A C line with no prior A is a genuine contract violation and must be
        /// rejected, not silently dropped or accepted as a completion-only event.
        #[test]
        fn completion_without_arrival_is_rejected(
            id in any::<EventId>(),
            ts in 0u64..1_000_000_000u64,
        ) {
            let text = format!("# ocapi traffic.dump v1\nC {id} {ts}\n");
            let result = parse_dump(&text);
            prop_assert_eq!(
                result,
                Err(ParseError::CompletionWithoutArrival { line_no: 2, id })
            );
        }

        /// Blank lines and comment lines anywhere in the file must be
        /// transparently ignored -- a logger may want to emit periodic
        /// human-readable markers (rotation, restart) without corrupting the
        /// stream for ocapi-core.
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
        let text = "# ocapi traffic.dump v1\nA 0 100\nA 0 200\n";
        assert_eq!(
            parse_dump(text),
            Err(ParseError::DuplicateArrival { line_no: 3, id: 0 })
        );
    }

    #[test]
    fn malformed_line_is_rejected() {
        let text = "# ocapi traffic.dump v1\nX 0 100\n";
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
        let text = "# ocapi traffic.dump v1\n";
        assert_eq!(parse_dump(text), Ok(Vec::new()));
    }

    #[test]
    fn later_completion_line_overwrites_earlier_one() {
        // Simulates a logger re-emitting a corrected completion record.
        let text = "# ocapi traffic.dump v1\nA 0 100\nC 0 150\nC 0 160\n";
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
