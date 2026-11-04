#!/usr/bin/env bash
set -euo pipefail

# Curl test-suite adapter runner.
#
# Builds hh200 (and its test suite) plus ocapi, starts the ocapi raw replay
# responder, runs the `curl/` tasty group against it, and tears everything
# down. Run directly, or via `make curl-tests`.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

REPLAY_PORT="${HH200_CURL_REPLAY_PORT:-18081}"

# Build hh200 and the test suite.
echo "==> building hh200" >&2
(cd hh200 && stack build --test --no-run-tests)

# Build the ocapi replay server.
echo "==> building ocapi" >&2
(cd experiments/ocapi && cargo build)
OCAPI_BIN="experiments/ocapi/target/debug/ocapi"

# Isolate runtime state (replay fixtures + recorded requests).
TMP="$(mktemp -d)"

echo "==> starting ocapi replay on 127.0.0.1:$REPLAY_PORT" >&2
"$OCAPI_BIN" serve --port 18082 --replay-dir "$TMP" --replay-port "$REPLAY_PORT" \
    >"$TMP/ocapi.log" 2>&1 &
OCAPI_PID=$!

cleanup() {
    kill "$OCAPI_PID" 2>/dev/null || true
    wait "$OCAPI_PID" 2>/dev/null || true
    rm -rf "$TMP"
}
trap cleanup EXIT

# Wait for the replay responder to accept connections.
ready=0
for _ in $(seq 1 50); do
    if (exec 3<>"/dev/tcp/127.0.0.1/$REPLAY_PORT") 2>/dev/null; then
        exec 3>&- 3<&-
        ready=1
        break
    fi
    sleep 0.2
done
if [ "$ready" -ne 1 ]; then
    echo "error: ocapi replay did not become ready on $REPLAY_PORT" >&2
    tail -n 20 "$TMP/ocapi.log" >&2 || true
    exit 1
fi

export OCAPI_REPLAY_DIR="$TMP"
export OCAPI_REPLAY_PORT="$REPLAY_PORT"
export HH200_CURL_DATA_DIR="$ROOT/test/curl/data"

echo "==> running curl test cases" >&2
(cd hh200 && stack test --test-arguments "--pattern curl")
