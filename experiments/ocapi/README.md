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

- `serve [--dump-path PATH] [--port N] [--tui]` runs the controllable HTTP SUT.
  When `--dump-path` is omitted, the dump is appended to an XDG data directory.
- `analyze [--dump-path PATH]` reads a `traffic.dump` and report arrival
  statistics.
- `doctor` reports where config and dump paths resolve, and whether they are
  usable.

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
