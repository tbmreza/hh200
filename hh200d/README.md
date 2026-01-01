# hh200d

This is the language server for the hh200 DSL.

## Building

This project uses `stack` to build. Before building, you need to generate the parser files from the BNFC grammar.

### 1. Install BNFC

If you don't have `bnfc` installed, you can install it using `cabal`:

```
cabal install BNFC
```

Make sure that `~/.cabal/bin` is in your `PATH`.

### 2. Clone Dependencies

This project has a local dependency on the `bel-expr` library. Clone it into the project root:

```
git clone https://github.com/tbmreza/bel-expr.git ../bel-expr
```

### 3. Generate Parser Files

To generate the parser files, run the following command from the project root:

```
bnfc --haskell -m -o hh200d/src/Hh200 hh200d/src/Hh200.cf
```

### 4. Build the Project

Once the parser files have been generated, you can build the project using `stack`:

```
cd hh200d
stack build
```
