# hh200 lang

## LR grammar

hh200 grammar builds on [hurl's](https://hurl.dev/docs/grammar.html), which we're going to just trust to be consistent with
its parser implementation (a [handwritten](https://github.com/Orange-OpenSource/hurl/blob/master/packages/hurl_core/src/parser/primitives.rs) recursive descent parser).

### Host system dependencies
- alex == 3.5.2.0
- happy == 2.1.4

### Development
Adding a rule to the grammar is a 3-step process. As an illustration:
```
```
``` sh
stack purge
stack run
```
