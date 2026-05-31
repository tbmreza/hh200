# hh200 lang

[![CI](https://github.com/tbmreza/hh200/actions/workflows/ci.yml/badge.svg)](https://github.com/tbmreza/hh200/actions/workflows/ci.yml)

hh200 is distributed as single binary, e.g. `npm install -g @mauikut/hh200`.

```
+---------------------------------------------------------------------------+
| haskell-stack managed binary                                              |
|                                                                           |
|  +------------------------------+      +-------------------------------+  |
|  | HASKELL RUNTIME              |      |                               |  |
|  |                              |      |                               |  |
|  |   +--------------------+     |      |  +-------------------------+  |  |
|  |   |     DSL Grammar    |     |      |  | Network Monitoring eBPF |  |  |
|  |   +--------------------+     |      |  +-------------------------+  |  |
|  |            |                 |      |              |                |  |
|  |            v                 |      |              v                |  |
|  |  +------------------------+  |      |   +----------------------+    |  |
|  |  | Concurrent HTTP Client |  |      |   |   Dashboard Server   |    |  |
|  |  +------------------------+  |      |   +----------------------+    |  |
|  +---------------+--------------+      +---------------+---------------+  |
|                  |                                     |                  |
|                  +------------> [sqlite] <-------------+                  |
+---------------------------------------------------------------------------+
```

where the right-hand side half is the part that supports a web viewing dashboard and is optional.
- **DSL Grammar** defines the language HTTP server test designers can use to express test cases.
- **Concurrent HTTP Client** that's capable of generating large HTTP request load safely in _single_ machine (Sidenote is that distributed load generation warrants a version 2).
- **Network Monitoring 🐝 eBPF** in safe kernel-level programming. Requires user sudo.
- **Dashboard Server** in C using the fuzzed, safe-to-forget network framework [cesanta/mongoose](https://github.com/cesanta/mongoose).

## Contributing
The project is in ideation phase (Update late 2025: slowly transitioning to a hazily more committal phase; expect target release date sooner rather than later!).
`DRAFT.md` is where I stash my thoughts. `hh200/` works if you want to play with what I got so far.

```sh
stack test --test-arguments "--pattern Script"
shelltest easy.test
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
- sequelize (latest npm release: 6 stable)

### Build Dependencies
Notable aspects:
- Uses `bel-expr` expression language
- Integrates with Language Server Protocol (`lsp`) for IDE support


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
``` sh
stack purge  # rm -rf .stack-work
stack run
ghciwatch --command "stack repl" --watch . --error-file errors.err --clear  # fast feedback loop!
stack exec hh200 -- --version +RTS -l -RTS  # generates .eventlog
```
