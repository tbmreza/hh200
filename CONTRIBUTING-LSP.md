# E2E LSP Tests

To run the end-to-end LSP tests for `hh200` (driven by the `lsp-test` framework), use `stack test`:

```bash
stack test --test-arguments="-p \"E2E LSP Tests\""
```

This will spin up a headless version of our language server connecting over stdio, run the tests that mimic the editor communicating with the server, and verify their assertions.
