# ocapi

A controllable HTTP system-under-test for entertaining both open- and closed-model workload traffics.

```

routes -> traffic.dump <- arrival_classifier

```

```
cargo t
cargo r -- --dump-path "$(pwd)/traffic.txt"
cargo build --release --target x86_64-unknown-linux-musl
```
