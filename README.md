# hh200 lang

[![CI](https://github.com/tbmreza/hh200/actions/workflows/ci.yml/badge.svg)](https://github.com/tbmreza/hh200/actions/workflows/ci.yml)

hh200 is distributed as single binary, e.g. `npm install -g @mauikut/hh200`.

```
+---------------------------------------------------------------------------+
| haskell-stack managed binary                                              |
|                                                                           |
|  +------------------------------+                                         |
|  |                              |                                         |
|  |   +--------------------+     |                                         |
|  |   |     DSL Grammar    |     |                                         |
|  |   +--------------------+     |                                         |
|  |            |                 |                                         |
|  |            v                 |      +-------------------------------+  |
|  |  +------------------------+  |      |                               |  |
|  |  | Concurrent HTTP Client |  |      |    Network Monitoring eBPF    |  |
|  |  +------------------------+  |      |                               |  |
|  +---------------+--------------+      +---------------+---------------+  |
|                  |                                     |                  |
|                  +------------> [sqlite] <-------------+                  |
+---------------------------------------------------------------------------+
```

where the right-hand side half is the part that supports a web viewing dashboard and is optional.
- **DSL Grammar** defines the language HTTP server test designers can use to express test cases.
- **Concurrent HTTP Client** that's capable of generating large HTTP request load safely in _single_ machine (Sidenote is that distributed load generation warrants a version 2).
- **Network Monitoring 🐝 eBPF** in safe kernel-extending program. Requires user sudo.
- **Dashboard Server** with familiar JavaScript.

## Contributing
The project is in ideation phase (Update late 2025: slowly transitioning to a hazily more committal phase; expect target release date sooner rather than later!).
`DRAFT.md` is where I stash my thoughts. `hh200/` works if you want to play with what I got so far.

```sh
stack test --test-arguments "--pattern Script"
shelltest easy.test  # https://github.com/simonmichael/shelltestrunner
```
```yaml
# stack.yaml

snapshot:
  url: https://raw.githubusercontent.com/commercialhaskell/stackage-snapshots/master/lts/24/26.yaml
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
- shelltestrunner (latest github release: 1.11)
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
## ADR

| Decision                                   | Context / trade-off | Evidence |
|---|---|---|
| Haskell + Stack build                      | Implementation language; managed by Stack against a pinned Stackage LTS. | `package.yaml`, `hh200/stack.yaml` |
| Single-binary distribution                 | Ships as one binary, `npm install -g @mauikut/hh200`. | README, `releases/`, `package.json` |
| Alex lexer + Happy parser                  | Hand-written lexer/parser specs instead of a recursive-descent parser. | `src/L.x`, `src/P.y` |
| Grammar derived from hurl's                | Trusts hurl's grammar to match its parser implementation. | `src/P.y`, README "LR grammar" |
| BEL embedded expression engine             | `bel-expr` provides regex/random/time batteries; compromises binary size. | `package.yaml`, `src/Hh200/Types.hs` |
| Shared `http-client` Manager               | One Manager threaded as a Reader for connection reuse. | `src/Hh200/Http.hs`, `src/Hh200/Execution.hs` |
| `ProcM = MaybeT (RWST Manager Log Env IO)` | Reader=manager, Writer=log, State=env; `MaybeT` short-circuits on failure. | `src/Hh200/Execution.hs` |
| Fail fast, no skip                         | Failing fast, dodging skip-cases; compromises test percentage. | `src/Hh200/Execution.hs`, README |
| Courier worker pool (`forkIO`)             | One courier per virtual user runs the whole Script. | `src/Hh200/TokenBucketWorkerPool.hs`, `src/Hh200/Cli.hs` |
| STM token-bucket rate limiter              | Async refill thread spread across sub-second ticks. | `src/Hh200/TokenBucketWorkerPool.hs` |
| STM `TVar RunState` control                | `Running/Paused/Stopped` flag shared across workers. | `src/Hh200/TokenBucketWorkerPool.hs` |
| Unix Domain Socket control channel         | Pause/resume/stop via `/tmp/uds_socket`. | `src/Hh200/Cli.hs`, `src/Hh200/Dashboard.hs` |
| SQLite persistence (`sqlite-simple`)       | Runs/metrics in SQLite; `HH200_SQLITE` or XDG data dir. | `src/Hh200/Database.hs` |
| Scotty dashboard                           | Serves a SvelteKit SPA from `min/` with a JSON API. | `src/Hh200/Dashboard.hs` |
| SSE live stream + CSV export               | Server-sent events for live data; CSV download. | `src/Hh200/Dashboard.hs` |
| LSP server (TCP + stdio)                   | IDE support via the `lsp` package. | `src/Hh200/LanguageServer.hs`, `src/Hh200/Scanner.hs` |
| eBPF monitoring in C/C++                   | Kernel-extending monitoring linked into the binary; requires sudo. | `packets/`, `hh200.cabal` |
| JSON structural subset assertion           | Response body asserted as `⊆`, not exact equality. | `src/Hh200/Execution.hs` `jsonSubset` |
| Case-insensitive header keys               | Header maps keyed by `CaseInsensitive.CI ByteString`. | `src/Hh200/Execution.hs` |
| Runtime host info collection               | hostname/os/arch/uptime gathered for debugging leads. | `src/Hh200/Scanner.hs` |
| Single-machine load (distributed = v2)     | Distributed load generation deferred to a future version 2. | README diagram |
