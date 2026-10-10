# ocapi-core fixtures

Contrived `traffic.dump` inputs for exercising the reader/classifier in
`ocapi-core`. Each `.dump` file is a self-contained trace in the same
line-oriented grammar the live logger writes (`src/logger.rs` in the `ocapi`
crate), hand-written so a reader can see the structure at a glance.

## Grammar

```
A <id> <arrival_ns>
C <id> <completion_ns>
```

- `<id>` is a 0-based `u64` per file. Each file is a standalone trace, so ids
  start at 0 and a valid trace never reuses an arrival id.
- `<arrival_ns>` / `<completion_ns>` are nanosecond `u64` timestamps. Only
  relative deltas matter to the classifier, so fixtures use small readable
  offsets rather than epoch nanoseconds.
- A `C` line must be preceded by an `A` line for the same id. A dangling `A`
  with no matching `C` is valid and means "still in flight when the log ends".
- `parse_dump` ignores `#` comment lines and blank lines, so every fixture
  carries a `#` header describing itself. This is the only reason `empty.dump`
  is not a zero-byte file: a comment-only file still parses to zero events.

## Files

| File | Case |
| --- | --- |
| `empty.dump` | Empty trace — no events at all. |
| `lone_arrive.dump` | Lone arrive — `A1` with no completion. |
| `single_pair.dump` | Single pair — `A1 C1`. No inter-arrival time, verdict "can't tell". |
| `one_and_a_half_pairs.dump` | One and a half pairs — `A1 C1 A2`. |
| `one_and_a_half_pairs_mirror.dump` | One and a half pairs, mirror — `A1 A2 C1`. |

## Before adding a new case

Consider these points first; they keep the folder legible and the tests
dependable:

1. **One concern per file.** Don't pile two unrelated edge cases into one file.
   If a case has variants (like the two "one and a half" orderings), give each
   variant its own file with a distinct name.
2. **Say what the file is for.** Start with a `#` header that names the case
   and states the expected outcome — does it parse cleanly (and to what), is it
   intentionally malformed (and which `ParseError`), and what verdict (Open /
   Closed / Unknown) the classifier should reach if it has one.
3. **Valid vs malformed is a deliberate choice.** Most fixtures are *valid*
   inputs. A malformed input (orphan `C`, duplicate `A`, `C` before `A`) is
   also a legitimate fixture, but it must be labeled as such and named to make
   the expected failure obvious.
4. **Keep timing legible.** Prefer small relative nanosecond offsets (e.g. 1000,
   1100, 2000). Keep `completion >= arrival` within a pair unless the case is
   specifically about clock skew or reordering.
5. **Respect the grammar's invariants.** 0-based ids, no duplicate `A`, no `C`
   without a prior `A` — unless the case exists to violate one of these.
6. **End the file with a trailing newline**, and use spaces (not tabs) between
   fields, matching the logger's `"A {} {}\n"` format.
7. **Don't edit existing fixtures.** Treat them as golden references; tests may
   depend on their exact bytes. Add a new file instead, and consider pairing it
   with a test in `src/lib.rs` that reads it and asserts the expected parse or
   verdict.
