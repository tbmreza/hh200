# hh200 lang

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
```

## Modelling parallel test users

Haskell distinguishes between parallelism—executing computations simultaneously to improve
performance—and concurrency—managing multiple independent computations that may interact,
such as through I/O or shared resources.

hh200 doesn't try to speak in the same granularity as haskell or any parallelism-supporting
programming languages. We could reexport our host language's semantics with our syntax;
this option is always option for future implementations of hh200. But for now when we say
"HTTP server test with parallel users", we are thinking about a specific semantics for the
following example program:

```
#! ["user1", "user2"] row

"download image.jpg"
GET https://fastly.picsum.photos/id/19/200/200.jpg?hmac=U8dBrPCcPP89QG1EanVOKG3qBsZwAvtCLUrfeXdE0FI
HTTP [200 201] ("/downloads/img-{{row}}.jpg" fresh)

```
