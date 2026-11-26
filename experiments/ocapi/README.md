# ocapi

A controllable HTTP system-under-test for entertaining both open- and closed-model workload traffics.

- "strategy" is our notion of open/closed-model information interpolation: InterArrival and ConcurrencyBound

```

routes -> traffic.dump <- ocapi-core

```

```
cargo t
cargo r -- serve --dump-path ./rotated.log
cargo r -- analyze --dump-path ./rotated.log
cargo r -- doctor
cargo build --release --target x86_64-unknown-linux-musl
```

## Subcommands

- `serve [--dump-path PATH] [--port N] [--tui] [--echo-max-bytes N]` runs the controllable HTTP SUT.
  When `--dump-path` is omitted, the dump is appended to an XDG data directory.
- `analyze [--dump-path PATH]` reads a `traffic.dump` and report arrival
  statistics.
- `doctor` reports where config and dump paths resolve, and whether they are
  usable.

## `/echo`

`ANY /echo` (also `/api/echo`, php-dev-server's path) reflects the request as
JSON. Besides `method`, `uri`, `headers`, `query` and a lossily decoded `body`,
it reports:

- `body_len`, `body_base64`, `body_sha256`, `body_utf8`: the body exactly as
  received, so binary payloads survive. Compare `body_sha256` with `sha256sum`.
- `json`: the parsed body when the content type is JSON.
- `multipart`: for `multipart/form-data`, each part's name, filename, content
  type, length, SHA-256, sniffed type, and text when it is UTF-8.
- `query_all`: every value of every query key (`query` keeps the last).
- `base64`, when `?b64=` is given: a report on one base64 value. `?b64=` alone
  checks the body, `?b64=/image` a JSON Pointer into a JSON body, and
  `?b64=part:NAME` a multipart part. It covers a `data:` URI prefix,
  standard vs URL-safe alphabet, padding, line breaks and other whitespace,
  the first invalid character, whether a strict decoder accepts the value as
  sent (`strict_valid`), and the leniently decoded length, SHA-256 and sniffed
  type (PNG, JPEG, GIF, WebP, PDF), with a plain-language `warnings` list.

```
curl -s 'localhost:8080/echo?b64=/image' -H 'content-type: application/json' \
  -d "{\"image\": \"$(base64 -w0 cat.png)\"}" | jq .base64
```

`/echo` accepts bodies up to 25 MiB (`serve --echo-max-bytes N` to change); a
larger body gets a JSON `413`. Other routes keep axum's 2 MB default.

## Live traffic TUI

`cargo r -- serve --tui` replaces the plain file logger with a live
[ratatui] monitor while still mirroring the `traffic.dump` to disk (so offline
`analyze` keeps working). If stdout is not a TTY, `--tui` silently falls back
to the plain file logger.

The screen shows a sticky top pane (current global verdict, sample count, time
since the last verdict change, dropped-event counter, and request rate), then a
per-connection table, a live inter-arrival sparkline, and a scrolling log of
arrivals/completions and verdict transitions.

- `q` or `Ctrl-C` quits and shuts the server down gracefully.
- `p` pauses display (ingest continues in the background).
- `c` clears the log pane.

While the TUI owns the terminal, `tracing` output is written to
`$XDG_STATE_HOME/ocapi/ocapi.log` (opened in append mode, so an external
`logrotate` `copytruncate` rotation is safe).

[ratatui]: https://ratatui.rs
