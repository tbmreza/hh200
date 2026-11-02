#!/usr/bin/env bash
# Build the eBPF object and embed it as a C array for hh_bpf.c.
# Run this BEFORE `stack build --flag hh200:ebpf`.
set -euo pipefail
cd "$(dirname "$0")"

SRC="hh_bpf.bpf.c"
OBJ="hh_bpf.bpf.o"
INC="hh_bpf.bpf.o.inc"

if ! command -v clang >/dev/null 2>&1; then
  echo "error: clang not found — cannot compile the eBPF object" >&2
  exit 1
fi
if ! command -v xxd >/dev/null 2>&1; then
  echo "error: xxd not found — cannot embed the eBPF object" >&2
  exit 1
fi

echo "==> clang -target bpf -c $SRC"
clang -O2 -g -target bpf -c "$SRC" -o "$OBJ"

echo "==> xxd -i $OBJ > $INC"
xxd -i "$OBJ" > "$INC"

echo "==> wrote $INC"
