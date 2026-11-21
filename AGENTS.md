# AGENTS.md

Notes for agents (and humans) working in this repository.

## Building and testing

- The Haskell package lives in `hh200/`. Build and test from there:
  `stack build --test --no-run-tests && stack test`. CI (`.github/workflows/ci.yml`) runs the same.
- `stack test` runs in `hh200/`, so tests use paths relative to it (`min/`, `../examples`, `../LICENSE`).
- The toolchain needs `downloads.haskell.org` (GHC), Hackage, and `github.com` (the `bel-expr`
  extra-dep in `hh200/stack.yaml`). Linking needs the system GMP library (`libgmp-dev`).
- Regression guards for the open findings below live in `hh200/test/`: `ReleaseScriptSpec`,
  `ExecutionRunSpec`, `CorpusSpec`, and the regression section of `DashboardSpec`. They pin behaviour
  that a fix must keep. A fix that changes behaviour on purpose should add its own test for the new
  behaviour.

## Open findings from a shallow scan

Found by a shallow read of `hh200/` (Nov 2026). Most are unverified beyond reading the code; the
"Guarded by" notes name the tests that should stay green through the fix.

Fixed so far: package-release.sh dropped LICENSE from release tarballs (LICENSE is at the repo root,
the script looked only in `hh200/`). It now falls back to the repo root and refuses to package
without a LICENSE.

### Wrong configs and code

1. **`hh200/package-release.sh` hardcodes `TARGET_ARCH="linux-x86_64"`**, so a build on arm64 or
   macOS is mislabelled. Derive it from `uname -s`/`uname -m`. Guarded by: `ReleaseScriptSpec`
   (the tarball-name match ignores the arch part).
2. **The control socket path is resolved three different ways.** The dashboard honours
   `HH200_CONTROL_SOCKET` (`src/Hh200/Dashboard.hs:160`), but the CLI hardcodes `/tmp/uds_socket`
   (`src/Hh200/Cli.hs:380`), and so does `src/Hh200/Runner.hs:139`. Setting the variable makes the
   dashboard and runs talk to different sockets. Use `controlSocketPath` everywhere. Guarded by:
   `DashboardSpec` control-socket tests.
3. **`ghc` is a library dependency that nothing imports** (`package.yaml`, `hh200.cabal`). The only
   `GHC.*` import is `GHC.Generics`, which comes from `base`. Removing it cuts a very large package
   from the build.
4. **Wrong comment in `src/Hh200/Dashboard.hs:149`:** it says "event every 1.5s", but the code waits
   `threadDelay 2000000` (2 s). Guarded by: `DashboardSpec` SSE tests.
5. **`Debug.Trace` in production paths:** the assertion logic in `src/Hh200/Execution.hs:532-536`
   wraps `jsonSubset` in `trace`, so every JSON assertion prints to stderr. More `trace` calls sit in
   `Runner.hs` (`AT:rid`, `AT:ensureNoActiveRun`) and `Dashboard.hs` (`1st initDb`, `2nd initDb`).
   Guarded by: `ExecutionRunSpec`.
6. **Suspected bug (unverified):** in the same `case` at `Execution.hs:532`, only `ASubsetOfB`
   passes. An expected body that is *identical* to the response gives `Equal` from `jsonSubset`, which
   seems to fail the assertion. Confirm with a test before changing it.
7. **The dashboard serves the SPA through relative paths** (`min/200.html`, `addBase "min"`), so it
   only works when started from `hh200/`. `file-embed` is already a dependency and could embed the
   files. Guarded by: `DashboardSpec` static tests (root, SPA route, `/_app/version.json`).

### Contradictions in the docs

8. **The README's feature #2** promises "regex, random, time batteries", but
   `src/Hh200/hhs-grammar.md` lists `randInt`, `matches`, `timestamp`, `today`, and others as
   "Planned (not yet wired into the evaluator)".
9. **The README's ADR "For review" table lists decisions as open that are already implemented:**
   script path policy (`HH200_SCRIPT_ROOT`, `Runner.hs:71`), one concurrent run (`409` in
   `Dashboard.hs:117`), and `totalRequests` (`Runner.hs:129`).
10. **The versions disagree:** `hh200/CHANGELOG.md` has a placeholder `0.1.0.0 - YYYY-MM-DD`,
    `package.yaml` says `0.0.1`, and the README's packaging example uses `v0.1.0`. The CHANGELOG
    also claims to follow the PVP, which uses four-part versions.
11. **The README diagram leaves out the "Dashboard Server"** that the bullet list right below it
    describes.

### Plainly wrong or stale statements

12. **One README command only works on the author's machine:** the `npx
    @modelcontextprotocol/inspector` line hardcodes `/home/tbmreza/...` and a hash-specific
    `.stack-work` path. Use `$(stack exec -- which hh200)` or `stack path --local-install-root`.
13. **`stack test --test-arguments "--pattern Script"` in the README** probably matches no tests: no
    test group name contains "Script". The closest is "Scanner and Parser".
14. **The README lists php as a development dependency**, but no Makefile, script, or CI file uses
    it.
15. **README typos:** "act a the runner" should be "act as the runner"; "distributed as single
    binary" is missing "a".

### Repo hygiene

16. **Committed files that should not be:** `hh200/trash.out` (ignored by `trash.*` but tracked)
    and `experiments/ocapi/target/{.rustc_info.json,.rustdoc_fingerprint.json,CACHEDIR.TAG}`.
17. **`hh200/next.hhs` is not a valid script:** `POST` has no URL, which the grammar requires.
    `CorpusSpec` only covers `examples/`, so this file is not checked.
18. **`hh200/post_json.hhs` is an exact copy of `examples/post_json.hhs`.**
19. **`hh200/stack.yaml` sets `compiler: ghc-9.10.3` next to the LTS 24.26 snapshot**, which
    already picks that compiler. Its header comment also still says the file was generated by
    `stack init`.
