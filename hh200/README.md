# hh200 lang

## LR grammar

hh200 grammar builds on [hurl's](https://hurl.dev/docs/grammar.html), which we're going to just trust to be consistent with
its parser implementation (a [handwritten](https://github.com/Orange-OpenSource/hurl/blob/master/packages/hurl_core/src/parser/primitives.rs) recursive descent parser).

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

## Erlang runtime
### Development
The shell might interfere with how we interact with `/bin/erl_call` sometimes, for example with the
out-of-context message "erl_call:.:{N}: not enough arguments". Use another shell or look closer to
what your shell does every step of the way.

```sh
erl -sname hh200 -setcookie $(cat ~/.erlang.cookie)
epmd -names
erl_call -sname hh200 -a 'init stop'
```
erl_call -n hh200 -c $(cat ~/.erlang.cookie) -e rt:dbg().
erl_call -sname hh200 -a 'rt dbg []'
erl_call -sname hh200 -c $(cat ~/.erlang.cookie) -a 'rt dbg []'
