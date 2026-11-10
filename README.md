# hh200 lang

[![CI](https://github.com/tbmreza/hh200/actions/workflows/ci.yml/badge.svg)](https://github.com/tbmreza/hh200/actions/workflows/ci.yml)

hh200 is distributed as single binary, e.g. `npm install -g @mauikut/hh200`.

```
+--------------------------------------------------+
| haskell-stack managed binary                     |
|                                                  |
|  +------------------------------+                |
|  |                              |                |
|  |   +--------------------+     |                |
|  |   |     DSL Grammar    |     |                |
|  |   +--------------------+     |                |
|  |            |                 |                |
|  |            v                 |                |
|  |  +------------------------+  |                |
|  |  | Concurrent HTTP Client |  |                |
|  |  +------------------------+  |                |
|  +---------------+--------------+                |
|                  |                               |
|                  +------------> [sqlite]         |
+--------------------------------------------------+
```

- **DSL Grammar** defines the language HTTP server test designers can use to express test cases.
- **Concurrent HTTP Client** that's capable of generating large HTTP request load safely in _single_ machine (Sidenote is that distributed load generation warrants a version 2).
- **Dashboard Server** with familiar JavaScript.

## Contributing
The project is in ideation phase (Update late 2025: slowly transitioning to a hazily more committal phase; expect target release date sooner rather than later!).
`DRAFT.md` is where I stash my thoughts. `hh200/` works if you want to play with what I got so far.

```yaml
# stack.yaml

snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/24/26.yaml
```

## Adapting curl's Test Suite

hh200 borrows HTTP conformance tests from [curl](https://github.com/curl/curl)'s
test suite to exercise two observable behaviors: the request bytes hh200 sends,
and how hh200 parses/handles a server response. Tests are vendored verbatim and
replayed byte-for-byte by a purpose-built responder in ocapi.

### Layout

```
test/curl/
  UPSTREAM          pinned tag + commit + license attribution
  COPYING           curl's copyright/permission notice (kept intact)
  MANIFEST          test numbers to vendor (one per line)
  skiplist.txt      excluded tests: one "NNN <reason>" per line
  data/             selected tests/data/testNNN, copied verbatim
scripts/
  sync-curl-tests.sh   refresh data/ from the pinned upstream tag
  curl-tests.sh        build hh200+ocapi, start the replay server, run the curl suite
```

### How it works

1. **Parser** (`hh200/test/CurlTest/`, a test-support library): parses curl's
   pseudo-XML `<testcase>` files into a reduced model — `<info><keywords>`,
   `<reply><data>`, `<client><server>/<features>/<command>`, and
   `<verify><protocol>/<errorcode>` — and implements the HTTP-relevant
   preprocessor macros (`%TESTNUMBER`, `%HOSTIP`, `%HTTPPORT`, `%CR`/`%SP`/…,
   and the `crlf` line-ending attribute).

2. **Replay** (`experiments/ocapi/src/replay.rs`): a raw TCP responder started
   with `ocapi serve --replay-dir … --replay-port …` that replays the rendered
   `<reply>` bytes byte-for-byte (so empty, malformed, and split responses
   survive) and records the request it received to
   `<replay-dir>/requests/<N>.txt`.

3. **Translator** (`CurlTest.Translate`): reduces the supported `<command>`
   subset (URL, method via `-X`/`-T -`/`-d`, body via `-d`/`--data-binary`/
   `<stdin>`) to an hh200 request, and derives the expected outcome from the
   reply status and `<errorcode>`.

4. **Test cases** (`CurlSpec`): each vendored test becomes one tasty case named
   `curl.testNNNN <keywords>` (`curl/testNNNN` in the plan). It runs the
   translated script against the replay server, checks the outcome (status
   assertion / HTTP error), and compares the recorded request to
   `<verify><protocol>`.

The request comparison is *normalized*: method, request-target, and body are
compared exactly (after macro expansion); headers are limited to
`Host`/`Content-Type`/`Content-Length` plus any `-H` the test sets. Headers
hh200 adds that curl would not (e.g. the unconditional `Content-Type` on GET)
are surfaced as non-fatal findings rather than failures.

### Scope and skip list

Only single-request HTTP tests are in scope. Everything else — non-HTTP servers
(FTP/SMTP/IMAP/…), TLS/auth backends, proxies, cookie jars, redirects (`-L`),
and curl-only flags — is recorded in `skiplist.txt` (manual exclusions) or
auto-skipped by the parser (unsupported `<server>`/`<features>`/`<command>`).

### Running

```
stack test --test-arguments "--pattern Script"
make curl-tests          # build hh200+ocapi, start the replay server, run the curl suite
make sync-curl-tests     # refresh vendored data from the pinned upstream tag
npx @modelcontextprotocol/inspector /home/tbmreza/gh/hh200/hh200/.stack-work/install/x86_64-linux/fec37efdeab1d21ab9f47f0ea78c1dd062848e3bc32ad42bcd33e4b02a90d6d2/9.10.3/bin/hh200 --mcp-stdio
```

## Features
The following defining features sum up hh200 in trade-off terms.

#### 1. Fail fast (compromising test percentage)
Well-functioning system-under-test is the only thing that should matter; we're dodging the need for skipping cases in test scripts.

#### 2. Regex, random, time batteries (compromising binary size)
hh200 comes integrated with a full expression language BEL evaluator.

## See also

<details>
<summary>
hurl https://github.com/Orange-OpenSource/hurl
</summary>
Requests in "simple plain text format". You could invoke hurl HTTP client
binary from your favorite general purpose language to achieve, for example,
parallel execution of hurl scripts.
</details>

<details>
<summary>
httpie https://github.com/httpie/cli
</summary>
"Make CLI interaction with web services as human-friendly as possible".
httpie resonates with people who have worked with curl or wget and find
their flags and quote escapes unpleasant.
</details>

<details>
<summary>
grafana/k6 https://github.com/grafana/k6
</summary>
Load testing engine providing JavaScript programming interface. To fully
live the term "load testing" (say, 6-digit number of virtual users), it can
act a the runner in an orchestrated, distributed load testing grid to
generate the traffic.
</details>

## LR grammar

hh200 grammar builds on [hurl's](https://hurl.dev/docs/grammar.html), which we're going to just trust to be consistent with
its parser implementation (a [handwritten](https://github.com/Orange-OpenSource/hurl/blob/master/packages/hurl_core/src/parser/primitives.rs) recursive descent parser).

### Syntax decision notes
URL fragments agree with https://hurl.dev/docs/hurl-file.html#special-characters-in-strings


### Development dependencies
- shelltestrunner (https://github.com/simonmichael/shelltestrunner latest github release: 1.11)
- php (latest debian stable: 8.4)

#### Database seeding

Location of `package.json` manifest isn't set in stone yet, but `releases/README.md` npm packaging was tested on the file being in root.

Whether there's value in using the same manifest for both npm packaging and db development setup is also to be seen.

### Package Build
Notable aspects:
- Uses `bel-expr` expression language
- Bundles with Language Server Protocol (`lsp`) for IDE support
```
stack build --ghc-options="-O2"
./package-release.sh <version string>  # ./package-release.sh v0.1.0

```

### Development
Developing a rule in the grammar is an activity of conservatively editing `src/L.x` and `src/P.y` at the following sites.

```haskell
-- src/L.x
tokens :-
    ...

data Token =
    ...
  deriving (Eq, Show)
```
```haskell
-- src/P.y
%token
    ...  { ... }

rule : ...
```
```sh
stack purge  # rm -rf .stack-work
HH200_SQLITE=$HOME/gh/hh200/live/prisma/app.db stack run -- --call 'POST http://localhost:9999/api/echo \n {"k":9}'
ghciwatch --command "stack repl" --watch . --error-file errors.err --clear  # fast feedback loop!
```
## Experiments
### Ocapi
Ocapi argument line is `L1: passive impossibility; L2: witness; T: active identifiability`

## ADR

Decided architecture records, split by component. Items still open are listed
under [For review](#for-review).

### Ocapi
| Decision                     | Context / trade-off                                                      | Evidence                      |
|---|---|---|
| TUI live verdict             | Simple streaming/batching once core is done.                             |                               |

### hh200
| Decision                                   | Context / trade-off                                                        | Evidence                                                 |
|---|---|---|
| Haskell + Stack build                      | Implementation language; against a pinned Stackage LTS.   | `package.yaml`, `hh200/stack.yaml`                       |
| Single-binary distribution                 | Ships as one binary, `npm install -g @mauikut/hh200`.                      | README, `releases/`, `package.json`                      |
| Alex lexer + Happy parser                  | Lexer/parser specs for the main language grammar.     | `src/L.x`, `src/P.y`                                     |
| BEL embedded expression engine             | `bel-expr` provides regex/random/time batteries; compromises binary size.  | `package.yaml`, `src/Hh200/Types.hs`                     |
| Grammar derived from hurl's                | Ecosystem story.                  | `src/P.y`, README "LR grammar"                           |
| Shared `http-client` Manager               | One Manager threaded as a Reader for connection reuse.                     | `src/Hh200/Http.hs`, `src/Hh200/Execution.hs`            |
| `ProcM = MaybeT (RWST Manager Log Env IO)` | Reader=manager, Writer=log, State=env; `MaybeT` short-circuits on failure. | `src/Hh200/Execution.hs`                                 |
| Fail fast, no skip                         | Failing fast, dodging skip-cases; compromises test percentage.             | `src/Hh200/Execution.hs`, README                         |
| Courier worker pool (`forkIO`)             | One courier per virtual user runs the whole Script.                        | `src/Hh200/TokenBucketWorkerPool.hs`, `src/Hh200/Cli.hs` |
| Unix Domain Socket control channel         | Pause/resume/stop via `/tmp/uds_socket`.                                   | `src/Hh200/Cli.hs`, `src/Hh200/Dashboard.hs`             |
| SQLite persistence (`sqlite-simple`)       | Runs in SQLite; `HH200_SQLITE` or XDG data dir.                            | `src/Hh200/Database.hs`                                  |
| Scotty dashboard                           | Serves a SvelteKit SPA from `min/` with a JSON API.                        | `src/Hh200/Dashboard.hs`                                 |
| SSE stream                                 | Hardcoded placeholder: `/api/sse` streams a static JSON blob; live run events are not wired.                                          | `src/Hh200/Dashboard.hs`                                 |
| LSP server (stdio default, TCP debug)      | Launched via `hh200 lsp` (stdio) or `hh200 lsp --tcp PORT`. Advertises `textDocumentSync` (Full), diagnostics, range formatting, definition and references. Hover/completion/rename still exist in `Scanner` but aren't wired to the protocol. | `src/Hh200/LanguageServer.hs`, `src/Hh200/Scanner.hs`, `src/Hh200/Cli.hs` |
| JSON structural subset assertion           | Response body asserted as `⊆`, not exact equality.                         | `src/Hh200/Execution.hs` `jsonSubset`                    |
| Case-insensitive header keys               | Header maps keyed by `CaseInsensitive.CI ByteString`.                      | `src/Hh200/Execution.hs`                                 |
| Runtime host info collection               | hostname/os/arch/uptime gathered for debugging leads.                      | `src/Hh200/Scanner.hs`                                   |
| Single-machine load (distributed = v2)     | Distributed load generation deferred to a future version 2.                | README diagram                                           |

### For review

Open design decisions for the Run screen (start-a-run-from-the-server) vertical slice.

| Decision (open) | Recommended default | Trade-off |
|---|---|---|
| Launch mechanism | In-process `forkIO` of couriers, reusing `courier` + a shared `TVar RunState` | Lifecycle/leak risk inside the server vs. subprocess isolation (a subprocess must poll SQLite for status). |
| Rate-limit source & precedence | `--rate` flag + optional script-header key; precedence `env > flag > header > default (unlimited)` | No rate limit exists today, so the plan endpoint returns "unknown" until this lands. |
| Script path policy | `HH200_SCRIPT_ROOT` dir + `canonicalizePath`, reject `..`/escape | Usability vs. arbitrary file read / path traversal. |
| Concurrent runs | One at a time; `409` if a run is already `running` | Prevents parallel load tests but matches the single `RunState` model. |
| Run name uniqueness | Non-unique; auto-generate when blank | Possible display ambiguity vs. blocking re-runs. |
| `totalRequests` semantics | `nvu × length (callItems script)` for single-shot | More accurate than `nvu`, but requires counting parsed `CallItem`s. |
