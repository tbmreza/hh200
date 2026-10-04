# ocapi

A controllable HTTP system-under-test for entertaining both open- and closed-model workload traffics.

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

- `serve [--dump-path PATH] [--port N]` runs the controllable HTTP SUT. When
  `--dump-path` is omitted, the dump is appended to an XDG data directory.
- `analyze [--dump-path PATH]` reads a `traffic.dump` and report arrival
  statistics.
- `doctor` reports where config and dump paths resolve, and whether they are
  usable.
