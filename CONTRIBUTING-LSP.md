# E2E LSP Tests

To run the end-to-end LSP tests for `hh200` (driven by the `lsp-test` framework), use `stack test`:

```bash
stack test --test-arguments="-p \"E2E LSP Tests\""
```

This will spin up a headless version of our language server connecting over stdio, run the tests that mimic the editor communicating with the server, and verify their assertions.

## Running the server by hand

The editor-facing entrypoint is stdio:

```bash
hh200 lsp            # stdio (default; what editors use)
hh200 lsp --tcp 3000 # TCP on 127.0.0.1:3000 (debug/diagnostic only)
```

`hh200 --lsp-stdio` and `hh200 --lsp=PORT` are retained as deprecated aliases.

## Triaging LSP bug reports

Every hh200 mode (load test, `--call`, the LSP) funnels through the same lexer
(`src/L.x`), parser (`src/P.y`) and `Scanner` analysis, so most bugs reported
through the editor are core bugs wearing an LSP costume. Classify before fixing:

1. **Reproduce in the smallest mode.** If `hh200 --call '<snippet>'` (or a direct
   `Scanner.analyze` / `Scanner.diagnostics` call) reproduces it, the bug is in
   the core, not the LSP.
2. **Locate the layer** — lexer (`L.x`) → parser (`P.y`) → semantic index/queries
   (`Scanner`) → validation (`Validate`) → protocol mapping (`LanguageServer.hs`)
   → transport.
3. **Fix and test at the layer that owns the bug.** Core bugs are fixed once in
   `L.x`/`P.y`/`Scanner` and get a regression test in `test/LspSpec.hs` /
   `test/ScannerSpec.hs` (mode-agnostic, so the fix is covered for CLI, LSP and
   future modes at once). Only position-mapping / transport bugs are fixed in
   `LanguageServer.hs`.

The classic trap is a wrong cursor position: it is either the parser reporting a
bad span (core) or `LanguageServer.hs` mis-converting a 1-based `SrcSpan` to a
0-based LSP `Range`. Check `Scanner.diagnostics` / `definitionAt` directly — if
they return the correct span, the bug is in the offset mapping, not the core.

Priority order for fixes: lexer/parser → `Scanner` semantics → shared runtime
(`Types`/`Execution`/`Http`) → protocol glue → single-mode features. Rank a bug
by (number of modes affected) × severity; a core fix pays off on every surface,
including the LSP.


