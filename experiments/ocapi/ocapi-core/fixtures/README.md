# ocapi-core fixtures

Contrived `traffic.dump` inputs for exercising the reader/classifier in
`ocapi-core`. Each `.dump` file is a self-contained trace in the same
line-oriented grammar the live logger writes (`src/logger.rs` in the `ocapi`
crate).

## Grammar

```
A <id> <arrival_ns>
C <id> <completion_ns>
```

- `<id>` is a 0-based `u64` per file. Each file is a standalone trace, so ids
  start at 0 and a valid trace never reuses an arrival id.
- `<arrival_ns>` / `<completion_ns>` are nanosecond `u64` timestamps. The
  classifier normalizes these down to milliseconds, so only relative deltas at
  millisecond resolution matter; fixtures use small readable millisecond-scale
  offsets rather than epoch nanoseconds.
- A `C` line must be preceded by an `A` line for the same id. A dangling `A`
  with no matching `C` is valid and means "still in flight when the log ends".
- `parse_dump` ignores `#` comment lines and blank lines, so every fixture
  carries a `#` header describing itself. This is the only reason `empty.dump`
  is not a zero-byte file: a comment-only file still parses to zero events.

## Files

| File | Case | Reading |
| --- | --- | --- |
| `empty.dump` | Empty trace. | No events, can't tell. |
| `lone_arrive.dump` | `A1` with no completion. | Too little data, can't tell. |
| `single_pair.dump` | No inter-arrival time, verdict "can't tell". | Can't tell. |
| `one_and_a_half_pairs.dump` | `A1 C1 A2`. | One inter-arrival gap is consistent with both an open and a closed workload (intuitively, next arrival A2 arrived after a delay regardless number of clients), can't tell. |
| `one_and_a_half_pairs_mirror.dump` | One and a half pairs, mirror: `A1 A2 C1`. | Open, or closed with at least 2 clients. |

## Before adding a new case

Consider these points first; they keep the folder legible and the tests
dependable:

1. **One concern per file.**
   If a case has variants (like the two "one and a half" orderings), give each
   variant its own file with a distinct name.
2. **Say what the file is for.** Start with a `#` header that names the case
   and states the expected outcome: does it parse cleanly (and to what), is it
   intentionally malformed (and which `ParseError`).
3. **Valid vs malformed is a deliberate choice.** Most fixtures are *valid*
   inputs. A malformed input (orphan `C`, duplicate `A`, `C` before `A`) is
   also a legitimate fixture.
4. **Keep timing legible.** The classifier normalizes ns → ms, so prefer small
   millisecond offsets written in nanoseconds (e.g. `1_000_000`, `2_000_000`,
   `3_000_000`); sub-millisecond deltas would truncate to the same ms and lose
   their meaning. Keep `completion >= arrival` within a pair unless the case is
   specifically about clock skew or reordering.
5. **Respect the grammar's invariants.** 0-based ids, no duplicate `A`, no `C`
   without a prior `A` — unless the case exists to violate one of these.
6. **End the file with a trailing newline**, and use spaces (not tabs) between
   fields, matching the logger's `"A {} {}\n"` format.
7. **Don't edit existing fixtures.** Do bias towards treating them as golden references.

## Captured traces

`make run-closed` / `make run-open` (from `experiments/Makefile`) snapshot real
runs into this folder, overwriting on each invocation:

- `<name>.dump` — the `traffic.dump` the server wrote (`A`/`C` records, epoch
  nanoseconds), i.e. what `ocapi analyze --dump-path` consumes.
- `<name>.stdout` — the container's `tracing` stdout (`method`/`uri`/`status`
  per request) from `docker compose logs`.

Unlike the contrived fixtures above, these are *empirical* captures with epoch
timestamps, not golden references. They are intentionally tracked (not
gitignored) so each overwrite shows up as a diff.
