#!/usr/bin/env bash
set -euo pipefail

# Refresh vendored curl test data from a pinned upstream tag and regenerate the
# .hhs examples used by the tasty curl adapter and by hh200-docs.
#
#   scripts/sync-curl-tests.sh [--docs-only] [TAG]
#
# Vendoring: copies the tests listed in test/curl/MANIFEST (verbatim, from
# curl's tests/data/testNNN), updates test/curl/COPYING, and rewrites
# test/curl/UPSTREAM with the resolved commit hash.
#
# Examples: writes test/curl/examples/testNNN.hhs and, when an hh200-docs
# checkout is available at ../hh200-docs (or $HH200_DOCS_DIR), rewrites
# docs/examples/curl-testNNN.md and the generated section of
# docs/examples/index.md. --docs-only skips the upstream clone and regenerates
# examples from the currently vendored data.

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

DOCS_ONLY=0
if [ "${1:-}" = "--docs-only" ]; then
    DOCS_ONLY=1
    shift
fi
TAG="${1:-curl-8_23_0}"
DEST="test/curl/data"
MANIFEST="test/curl/MANIFEST"

if ! command -v python3 >/dev/null 2>&1; then
    echo "error: python3 not found on PATH (needed to generate examples)" >&2
    exit 1
fi

if [ "$DOCS_ONLY" -eq 0 ]; then
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
fi

GEN_ARGS=(--root "$ROOT")
if [ -n "${HH200_DOCS_DIR:-}" ]; then
    GEN_ARGS+=(--docs-dir "$HH200_DOCS_DIR")
fi

echo "==> generating hh200 examples" >&2
python3 "$ROOT/scripts/gen-curl-examples.py" "${GEN_ARGS[@]}"

echo "==> done" >&2
