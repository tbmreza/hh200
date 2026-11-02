#!/usr/bin/env bash
set -euo pipefail

# End-to-end test runner.
#
# Builds the hh200 binary and the ocapi system-under-test server, starts ocapi
# on a fixed port, runs the shelltest golden suite (e2e.test) against it, and
# tears everything down. Run directly, or via `make e2e`.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

PORT="${HH200_E2E_PORT:-18080}"
HOST="127.0.0.1"

if ! command -v shelltest >/dev/null 2>&1; then
    echo "error: shelltest (shelltestrunner) not found on PATH" >&2
    echo "install with: stack install shelltestrunner-1.11" >&2
    exit 1
fi

# Build the hh200 binary and resolve its path.
echo "==> building hh200" >&2
(cd hh200 && stack build)
HH200_BIN="$(cd hh200 && stack path --local-install-root)/bin/hh200"

# Build the ocapi system-under-test server.
echo "==> building ocapi" >&2
(cd experiments/ocapi && cargo build)
OCAPI_BIN="experiments/ocapi/target/debug/ocapi"

# Isolate runtime state (SQLite + ocapi traffic dump + UDS socket).
TMP="$(mktemp -d)"
export HH200_SQLITE="$TMP/app.db"
rm -f /tmp/uds_socket

echo "==> starting ocapi on $HOST:$PORT" >&2
"$OCAPI_BIN" serve --port "$PORT" --dump-path "$TMP/ocapi.dump" \
    >"$TMP/ocapi.log" 2>&1 &
OCAPI_PID=$!

cleanup() {
    kill "$OCAPI_PID" 2>/dev/null || true
    wait "$OCAPI_PID" 2>/dev/null || true
    rm -rf "$TMP"
}
trap cleanup EXIT

# Wait for ocapi to accept connections.
ready=0
for _ in $(seq 1 50); do
    if curl -fsS "http://$HOST:$PORT/health" >/dev/null 2>&1; then
        ready=1
        break
    fi
    sleep 0.2
done
if [ "$ready" -ne 1 ]; then
    echo "error: ocapi did not become ready on $HOST:$PORT" >&2
    tail -n 20 "$TMP/ocapi.log" >&2 || true
    exit 1
fi

export PATH="$(dirname "$HH200_BIN"):$PATH"
echo "==> running golden e2e tests" >&2
shelltest e2e.test
