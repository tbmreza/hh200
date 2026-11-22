# AGENTS.md

Notes for agents (and humans) working in this repository.

## Building and testing

- The Haskell package lives in `hh200/`. Build and test from there:
  `stack build --test --no-run-tests && stack test`. CI (`.github/workflows/ci.yml`) runs the same.
- `stack test` runs in `hh200/`, so tests use paths relative to it (`min/`, `../examples`, `../LICENSE`).
- The toolchain needs `downloads.haskell.org` (GHC), Hackage, and `github.com` (the `bel-expr`
  extra-dep in `hh200/stack.yaml`). Linking needs the system GMP library (`libgmp-dev`).
- Use stack 3.x. Ubuntu's apt `haskell-stack` (2.9.3) can't verify Hackage's current root keys
  ("root.json does not have enough signatures"), and its hpack is older than the one that generated
  `hh200.cabal`, so it ignores `package.yaml`. In a restricted network, where
  `get.haskellstack.org` is blocked, the release tarball from `github.com/commercialhaskell/stack`
  works.
- Regression guards for the open findings below live in `hh200/test/`: `ReleaseScriptSpec`,
  `ExecutionRunSpec`, `CorpusSpec`, and the regression section of `DashboardSpec`. They pin behaviour
  that a fix must keep. A fix that changes behaviour on purpose should add its own test for the new
  behaviour.

## Open findings from a shallow scan

Found by a shallow read of `hh200/` (Nov 2026). Most are unverified beyond reading the code; the
"Guarded by" notes name the tests that should stay green through the fix.

### Wrong configs and code

1. **`hh200/package-release.sh` hardcodes `TARGET_ARCH="linux-x86_64"`**, so a build on arm64 or
   macOS is mislabelled. Derive it from `uname -s`/`uname -m`. Guarded by: `ReleaseScriptSpec`
   (the tarball-name match ignores the arch part).
2. **The control socket path is resolved three different ways.** The dashboard honours
   `HH200_CONTROL_SOCKET` (`src/Hh200/Dashboard.hs:160`), but the CLI hardcodes `/tmp/uds_socket`
   (`src/Hh200/Cli.hs:380`), and so does `src/Hh200/Runner.hs:142`. Setting the variable makes the
   dashboard and runs talk to different sockets. Use `controlSocketPath` everywhere. Guarded by:
   `DashboardSpec` control-socket tests.
3. **`ghc` is a library dependency that nothing imports** (`package.yaml`, `hh200.cabal`). The only
   `GHC.*` import is `GHC.Generics`, which comes from `base`. Removing it cuts a very large package
   from the build.
4. **Wrong comment in `src/Hh200/Dashboard.hs:149`:** it says "event every 1.5s", but the code waits
   `threadDelay 2000000` (2 s). Guarded by: `DashboardSpec` SSE tests.
5. **`Debug.Trace` in production paths:** the assertion logic in `src/Hh200/Execution.hs:532-538`
   wraps `jsonSubset` in `trace`, so every JSON assertion prints to stderr. More `trace` calls sit in
   `Runner.hs` (`AT:rid`, `AT:ensureNoActiveRun`) and `Dashboard.hs` (`1st initDb`, `2nd initDb`).
   Guarded by: `ExecutionRunSpec`. **Blocks the Resend plan (phase 0):** `Execution.hs:477-479`
   also traces the whole request, so a base64 attachment would be dumped to stderr on every call.
6. **The dashboard serves the SPA through relative paths** (`min/200.html`, `addBase "min"`), so it
   only works when started from `hh200/`. `file-embed` is already a dependency and could embed the
   files. Guarded by: `DashboardSpec` static tests (root, SPA route, `/_app/version.json`).
   **Same root cause as the Resend plan's file paths:** paths resolve against the working directory.
   The plan resolves script-referenced files against the script's directory instead (phase 1).

### Contradictions in the docs

7. **The README's feature #2** promises "regex, random, time batteries", but
   `src/Hh200/hhs-grammar.md` lists `randInt`, `matches`, `timestamp`, `today`, and others as
   "Planned (not yet wired into the evaluator)". **Fixed in phase 1:** the README and its ADR row
   now say the helpers are planned, and describe variables and file values.
8. **The README's ADR "For review" table lists decisions as open that are already implemented:**
   script path policy (`HH200_SCRIPT_ROOT`, `Runner.hs:71`), one concurrent run (`409` in
   `Dashboard.hs:117`), and `totalRequests` (`Runner.hs:129`). **Partly fixed in phase 1:** the
   script path policy row moved to the decided table, since it now covers files scripts read too.
   The other two rows are still listed as open.
9. **The versions disagree:** `hh200/CHANGELOG.md` has a placeholder `0.1.0.0 - YYYY-MM-DD`,
    `package.yaml` says `0.0.1`, and the README's packaging example uses `v0.1.0`. The CHANGELOG
    also claims to follow the PVP, which uses four-part versions.
10. **The README diagram leaves out the "Dashboard Server"** that the bullet list right below it
    describes.

### Plainly wrong or stale statements

11. **One README command only works on the author's machine:** the `npx
    @modelcontextprotocol/inspector` line hardcodes `/home/tbmreza/...` and a hash-specific
    `.stack-work` path. Use `$(stack exec -- which hh200)` or `stack path --local-install-root`.
12. **`stack test --test-arguments "--pattern Script"` in the README** probably matches no tests: no
    test group name contains "Script". The closest is "Scanner and Parser".
13. **The README lists php as a development dependency**, but no Makefile, script, or CI file uses
    it.
14. **README typos:** "act a the runner" should be "act as the runner"; "distributed as single
    binary" is missing "a".

### Repo hygiene

15. **Committed files that should not be:** `hh200/trash.out` (ignored by `trash.*` but tracked)
    and `experiments/ocapi/target/{.rustc_info.json,.rustdoc_fingerprint.json,CACHEDIR.TAG}`.
16. **`hh200/next.hhs` is not a valid script:** `POST` has no URL, which the grammar requires.
    `CorpusSpec` only covers `examples/`, so this file is not checked.
17. **`hh200/post_json.hhs` is an exact copy of `examples/post_json.hhs`.**
18. **`hh200/stack.yaml` sets `compiler: ghc-9.10.3` next to the LTS 24.26 snapshot**, which
    already picks that compiler. Its header comment also still says the file was generated by
    `stack init`.
19. **Building needs a UTF-8 locale:** `src/Hh200/McpServer.hs` embeds `hhs-grammar.md` with
    `embedStringFile`, which decodes with the locale, and the file has non-ASCII characters. With
    `LANG=C` or `POSIX` the build fails with `hGetContents: invalid argument`. CI is unaffected
    (GitHub runners use `C.UTF-8`).

### Found while planning curl / Resend support

Found by reading `Execution.hs` and the pinned `bel-expr` (`98d01a5`) in Nov 2026. Each one blocks
the plan below; phase 0 fixes them.

20. **Request bodies are never interpolated.** `handleBody` sends `BS.pack rqBody`
    (`src/Hh200/Execution.hs:390`), so `{{NAME}}` in a JSON body goes out literally. Headers, URL,
    query, form, multipart, and cookies are all rendered through `BEL.render`; the body is the only
    part that isn't.
21. **A user `Content-Type` header produces two `Content-Type` headers.** `handleBody` always
    prepends its own (`text/plain` or form-urlencoded, `Execution.hs:391-393`) to the user's
    headers, and http-client sends both. Default it only when the script sets none, and pick
    `application/json` when the body is a `{...}` object.
22. **Bodies are encoded with `Data.ByteString.Char8.pack`** (`Execution.hs:390`), which truncates
    every character above U+00FF. Any non-Latin-1 text in an email's `subject` or `html` is
    corrupted. Use `Data.Text.Encoding.encodeUtf8`.
23. **Secrets and large bodies leak into logs and the database.** `capture` stores every request
    header, including `Authorization`, in SQLite (`Execution.hs:713`), along with up to 64 KiB of the
    request body. `present ci` (`Execution.hs:468`) prints every call item, and BEL's `eval` calls
    `print expr` on every assertion (`bel-expr` `src/BEL.hs:219`). Redact `Authorization`,
    `Cookie`, and `*-api-key` headers before persisting them, and drop the prints (see finding 5).
24. **BEL renders strings with Haskell `show`, not JSON escaping** (`renderValue`/`show'` in
    `bel-expr` `src/BEL.hs`). A value containing `"`, `\`, or non-ASCII text, interpolated into a
    JSON body, produces invalid JSON or Haskell escapes like `\1234`. Base64 is safe, but a captured
    or external `subject` is not. Body rendering (finding 20) must JSON-escape values that land
    inside a JSON string.

25. **Two more paths still encode text with `Char8`** (found during phase 0, not fixed yet).
    `analyze (Snippet ...)` round-trips the script through `L8.pack`/`L8.unpack` (`Scanner.hs`,
    used by `--call` and tests). Expected response bodies are compared after
    `L8.pack (specResponseBody ci)` (`Execution.hs`, `userAssertions`). Non-Latin-1 text in a
    snippet, or in an expected response body, is truncated. Scripts loaded from a file are not
    affected on the request side.

## Plan: send email with an image attachment through Resend

Goal: hh200 can run this curl snippet, and the same request can be written natively in `.hhs`:

```bash
IMG_BASE64=$(base64 -w 0 ./photo.png)
curl -X POST 'https://api.resend.com/emails' \
  -H 'Authorization: Bearer re_YOUR_API_KEY' \
  -H 'Content-Type: application/json' \
  --data-binary @- <<EOF
{ "from": "sender@example.com", "to": ["recipient@example.com"],
  "subject": "Email with local image", "html": "<p>See attached image.</p>",
  "attachments": [ { "filename": "photo.png", "content": "$IMG_BASE64" } ] }
EOF
```

Phases 0, 1 and 2 are done; phase 3 is not started. Phases go in order; each phase ends with
green `stack test`.

### Design decisions

- **Lower curl onto the existing AST; never run a shell.** The snippet is bash: a variable assignment,
  a command substitution, line continuations, and a heredoc. hh200 accepts a fixed subset of that
  shape and translates it into the same `RequestSpec` that `.hhs` produces, so execution, captures,
  asserts, the dashboard, and load runs all work unchanged. Any construct outside the subset is a
  diagnostic, not a fallback to `/bin/sh`, because the dashboard and MCP server accept scripts from
  other processes.
- **Files are read by hh200, not by `base64`.** `$(base64 -w 0 PATH)` is recognised as one pattern
  and becomes a `base64-file,PATH` value. This follows the hurl-style `file,PATH` prefix that
  `[MultipartFormData]` already uses (`Execution.hs:379-381`) and needs no BEL change. BEL's `match`
  is pure, so a `base64File "..."` BEL function would need IO in an external repo; keep it as a
  later option next to the "Planned" batteries (finding 7).
- **Secrets come from outside the script.** Variables come from `--var NAME=VALUE` and from
  environment variables named `HH200_VAR_NAME`, bound as `NAME`. The opt-in prefix keeps the
  dashboard and MCP server from exposing the server's whole environment to scripts. This mirrors
  hurl's `--variable` and `HURL_*`.
- **An unresolved `{{NAME}}` fails the call before sending.** Today an unbound identifier renders as
  `null`, which would send `Authorization: Bearer null` to a real API.

### Target `.hhs`

```hhs
POST https://api.resend.com/emails
Authorization: Bearer {{RESEND_API_KEY}}
Content-Type: application/json
[Vars]
IMG_BASE64: base64-file,photo.png
{
  "from": "sender@example.com",
  "to": ["recipient@example.com"],
  "subject": "Email with local image",
  "html": "<p>See attached image.</p>",
  "attachments": [ { "filename": "photo.png", "content": "{{IMG_BASE64}}" } ]
}
HTTP 200
[Captures]
EMAIL_ID: {{jsonpath "$.id"}}
```

Run it with `HH200_VAR_RESEND_API_KEY=re_... hh200 resend.hhs`.

### Phase 0: fix the blockers

**Status (Nov 2026): done.** Findings 5, 20, 21, 22, 23, and 24 are fixed. Tests are in
`ExecutionSpec` ("phase 0") and `ExecutionRunSpec` ("request body (phase 0 regressions)"). The BEL
half (no `print`/`trace`, strings rendered raw, a pure `evalText`) is `tbmreza/bel-expr` commit
`55d6e46`, merged into bel-expr's `main` (tbmreza/bel-expr#9). `hh200/stack.yaml` pins it, and
`renderJsonBody` uses `BEL.evalText`, and `stack.yaml.lock` has its regenerated entry.

Findings 5, 20, 21, 22, 23, and 24, in `src/Hh200/Execution.hs` (plus a `bel-expr` bump for the
`print` in finding 23):

- Render `rqBody` through BEL with the call item's env, JSON-escaping values that land inside JSON
  strings (finding 24), and encode it as UTF-8.
- Default `Content-Type` only when the script sets none.
- Redact credential headers in `capture` before they reach SQLite; remove `trace`/`present` output
  from the request path.

Tests: extend `ExecutionRunSpec` with a scotty fixture that echoes what it received. Assert one
`Content-Type` header, a rendered `{{X}}` in the body, and a UTF-8 subject surviving the round
trip. Add a `DashboardSpec` or `Database` test showing `Authorization` stored as redacted.

### Phase 1: native `.hhs` support

1. **External variables.** Add `--var NAME=VALUE` (repeatable) to `src/Hh200/Cli.hs` and read
   `HH200_VAR_*` at startup. Seed them into the initial `Env` that is currently `newEnv`. Thread the
   same seed through `Runner.hs` for dashboard runs, and through `McpServer.hs` if it executes
   scripts. Precedence: `--var` over `HH200_VAR_*`. Captures and `[Vars]` overwrite later, as
   captures do today.
2. **`[Vars]` request square.** Lexer: a `Vars` keyword token (`L.x`). Parser: a `request_sqr`
   alternative (`P.y`). Types: a `RequestSquareVars RhsDict` constructor. `rqSquares` is a positional
   5-tuple (`Types.hs`, `setRequestSquare`, `initRequestSquare`, `buildRequest`); replace it with a
   record before adding a sixth slot. Also update `Scanner.hs`: `getPos`, `tokenSpan`,
   `isBlockStart`, `complete` candidates, `hover`, and `prettyToken` in `P.y`.
3. **Evaluate `[Vars]` before the request is built.** `buildRequest` renders `[Vars]` first, then
   the URL, headers, and body with the extended env. It returns that env so `courseFrom` can `put` it
   and later call items can use the values.
4. **`file,` and `base64-file,` values.** In `[Vars]`, `file,PATH` binds the file's text (UTF-8) and
   `base64-file,PATH` binds `Data.ByteString.Base64.encode` of its bytes. `base64-bytestring` is
   already a dependency.
5. **Path policy.** Resolve `PATH` against the script file's directory, not the working directory
   (finding 6's root cause). Use the working directory only for `--call` snippets and stdin. For
   dashboard runs, generalise `resolveScriptPath` (`Runner.hs:69`) and confine reads under
   `HH200_SCRIPT_ROOT`. Without that, a submitted script can read any file the server can and POST
   it anywhere. Reject a missing file with a diagnostic naming the path; don't send.
6. **Unresolved variables are errors** (see Design decisions), reported with the call item's name.

Tests: `ScannerSpec` for lexing and parsing `[Vars]` and the record refactor. `ExecutionRunSpec`
covers several cases against the echo fixture. A temp file's base64 must equal the `content` field
of the received JSON. `--var` and `HH200_VAR_*` must reach a header. A path escaping the script root
must be refused. An unbound `{{X}}` must fail before any request is sent. Generate the image bytes
in a temp dir in the test rather than committing a binary.

Docs: add `[Vars]`, `file,`, `base64-file,`, and external variables to `src/Hh200/hhs-grammar.md`
with an ```` ```hhs ```` block (`CorpusSpec` parses those automatically). Keep the file's existing
non-ASCII to a minimum (finding 19). Add `examples/resend_email.hhs` (also covered by `CorpusSpec`).
Fix the README feature list (finding 7).

### Phase 2: curl front end

**Status (Nov 2026): done**, in `src/Hh200/Curl.hs`. Run a snippet with `hh200 --curl FILE` (`-` for
stdin), or print it as `.hhs` with `--emit-hhs`. Tests are `CurlFrontendSpec` and `ExecutionRunSpec`
("phase 2: curl snippets"). It differs from the plan below in these ways:
- `--data* @FILE` is refused with a hint to use a heredoc, rather than supported.
- The optional MCP `translate_curl` tool isn't done.
- The harness shares `shellWords`, `curlMethod` and `curlDataArg`. The front end has its own strict,
  positioned lexer.
- Two runtime fixes came with it. A `{{NAME}}` in a URL template rendered with an extra `/`
  (`intercalate "/"`), so `http://h/{{P}}` became `http://h//p`; the parts now join with nothing.
  Non-JSON bodies, such as curl's `-d a=b`, now interpolate raw text (`renderTextBody`), not JSON.

1. **Promote the translator into the library.** Move `tokenize`/`detectMethod`/`detectBody` from
   `hh200/test/CurlTest/Translate.hs` into a new `src/Hh200/Curl.hs`. Make `CurlTest.Translate` call
   it, so curl's vendored conformance suite (`CurlSpec`) keeps exercising the shared code.
2. **Accepted shell subset.** Anything else is a positioned `Diagnostic`, reusing `Types.Diagnostic`
   and `parseWithDiagnostics`' shape:
   - Assignments: `NAME=literal`, `export NAME=...`, and `NAME=$(base64 -w 0 PATH)`. Also accept the
     variants `-w0`, `base64 -i PATH` (macOS), and `base64 < PATH`.
   - Exactly one `curl` command, joined across `\`-newline continuations.
   - Quoting: `'...'` literal, `"..."` with `$NAME`/`${NAME}` expansion, and backslash escapes.
   - Flags: positional URL; `-X`/`--request`; `-H`/`--header`; and `-d`/`--data`/`--data-raw`/
     `--data-binary`/`--json` with a literal, `@file`, or `@-`. Ignore `-s`/`-S`/`-sS`/`-v`. `-F` maps
     onto `[MultipartFormData]` later, not in this phase.
   - `@-` reads the following heredoc: `<<EOF`, `<<-EOF`, or `<<'EOF'` (a quoted delimiter turns off
     `$` expansion, as in bash).
   - Rejected: other `$(...)`, backticks, pipes, `;`, `&&`, `||`, redirections other than the
     heredoc, and more than one `curl`.
3. **Lowering.** A snippet assignment becomes a `[Vars]` entry. `$NAME` becomes `{{NAME}}`, whether
   it is defined in the snippet or comes from external variables. Method follows curl:
   `-X` > `--data*` implies POST > GET. With `-d`/`--data*` and no `Content-Type`, add curl's default
   `application/x-www-form-urlencoded`. `--json` adds `application/json` for both `Content-Type` and
   `Accept`. Lower straight to `RequestSpec`, not via `.hhs` text, because `.hhs` bodies must be
   `{...}` objects and curl bodies need not be.
4. **CLI.** Add `hh200 --curl FILE` (or `-` for stdin) to run the request, and
   `hh200 --curl FILE --emit-hhs` to print the equivalent `.hhs`. `--emit-hhs` fails with a clear
   message when the body is not expressible. Warn, without failing, when an `Authorization` header
   holds a literal token rather than `$NAME`; the snippet's `re_YOUR_API_KEY` would trigger it.
5. **Optional:** an MCP `translate_curl` tool beside `validate_script` in `McpServer.hs`.

Tests: a pure `CurlUnitSpec`-style group in a new `CurlFrontendSpec` for tokenising, the subset and
its rejections, lowering, and `--emit-hhs` output. One `ExecutionRunSpec` case runs the exact
snippet above, with the URL pointed at the echo fixture and `photo.png` in a temp dir. It asserts the
received method, path, a single `Content-Type`, and the `Authorization` value. It also asserts that
`attachments[0].content` decodes to the file's bytes.

### Phase 3: Resend specifics and safety

- **No CI test talks to `api.resend.com`.** An opt-in live test may run only when
  `HH200_VAR_RESEND_API_KEY` is set, sending to Resend's test recipient address.
- **Load runs send real email.** `hh200 --duration`/dashboard runs with `nvu > 1` would send one
  email per iteration. Consider a refusal or confirmation for scripts hitting known side-effecting
  hosts, or document it loudly. An `Idempotency-Key` header limits duplicates when retried.
- **Resend facts to verify against the current docs before relying on them:** success is `200` with
  `{"id": "..."}`; errors are JSON with `statusCode`, `name`, and `message`; rate limiting returns
  `429` (the default limit is low, about 2 requests/s); attachments total about 40 MB after base64.
  The example's `HTTP 200` and `[Captures]` depend on the first.
- **Memory:** `rqBody` is a `String`, so a multi-megabyte attachment costs tens of MB per in-flight
  request. That's acceptable for email-sized files. Switching bodies to `Text` is a separate
  refactor.
