# hh200 lang

[![CI](https://github.com/tbmreza/hh200/actions/workflows/ci.yml/badge.svg)](https://github.com/tbmreza/hh200/actions/workflows/ci.yml)

## Contributing
The project is in ideation phase (Update late 2025: slowly transitioning to a hazily more committal phase; expect target release date sooner rather than later!).
`DRAFT.md` is where I stash my thoughts. `hh200/` works if you want to play with what I got so far.

### Surprises
Some nuggets that are less than mundane when I learned them.
- Haskell std lib trace doesn't print if the last value is unused, and doesn't print at all on panicking path.
- Show in haskell is not intended to be overriden. https://stackoverflow.com/q/9288883


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


### Host system dependencies
- alex == 3.5.2.0
- happy == 2.1.4

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
```

## Modelling parallel test users

Haskell distincts parallelism (executing computations simultaneously to improve
performance) from concurrency (managing multiple independent computations that may interact,
such as through I/O or shared resources).

hh200 doesn't try to speak in the same granularity as haskell or any parallelism-supporting
languages. We could reexport our host language's semantics with our syntax;
this option is always option for future implementations of hh200. But for now when we say
"HTTP server test with parallel users", we are thinking about a specific semantics for the
following example program:

```
#! ["user1", "user2"] row

"download image.jpg"
GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
HTTP [200 201] ("/downloads/img-{{row}}.jpg" fresh)

```
