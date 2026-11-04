#!/usr/bin/env bash
set -euo pipefail

# Refresh vendored curl test data from a pinned upstream tag.
#
#   scripts/sync-curl-tests.sh [TAG]
#
# Copies the tests listed in test/curl/MANIFEST (verbatim, from curl's
# tests/data/testNNN), updates test/curl/COPYING, and rewrites test/curl/UPSTREAM
# with the resolved commit hash.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

TAG="${1:-curl-8_23_0}"
DEST="test/curl/data"
MANIFEST="test/curl/MANIFEST"

if ! command -v git >/dev/null 2>&1; then
    echo "error: git not found on PATH" >&2
    exit 1
fi

TESTS="$(grep -vE '^\s*(#|$)' "$MANIFEST" || true)"
if [ -z "$TESTS" ]; then
    echo "error: $MANIFEST is empty" >&2
    exit 1
fi

TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "==> cloning curl at $TAG" >&2
git clone --quiet --depth 1 --branch "$TAG" https://github.com/curl/curl.git "$TMP/curl"

COMMIT="$(git -C "$TMP/curl" rev-parse HEAD)"
echo "==> upstream commit $COMMIT" >&2

mkdir -p "$DEST"
for n in $TESTS; do
    src="$TMP/curl/tests/data/test$n"
    if [ ! -f "$src" ]; then
        echo "warning: test$n not found upstream, skipping" >&2
        continue
    fi
    cp "$src" "$DEST/test$n"
    echo "  + test$n" >&2
done

cp "$TMP/curl/COPYING" test/curl/COPYING

{
    echo "# Upstream source for vendored curl test data."
    echo ""
    echo "upstream: https://github.com/curl/curl"
    echo "tag: $TAG"
    echo "commit: $COMMIT"
    echo "license: curl (see COPYING)"
    echo ""
    echo "# The files under data/ are copied verbatim from curl's tests/data/testNNN"
    echo "# at the tag above. Only the tests listed in MANIFEST are vendored; see"
    echo "# skiplist.txt for what is deliberately excluded and why."
} > test/curl/UPSTREAM

echo "==> done" >&2
