# hh200d

hh200 language server implementation

```sh
stack install
hh200d --port 9994
```

```lua
:lua vim.lsp.start({ name = 'hh200d', cmd = vim.lsp.rpc.connect('127.0.0.1', 9994), root_dir = vim.fs.dirname(vim.fs.find({'stack.yaml', '.git'}, { path = startpath })[1]), })
:lua vim.lsp.start({ name = 'hh200d', cmd = vim.lsp.rpc.connect('127.0.0.1', 9994), root_dir = vim.fn.getcwd(), })
:lua vim.lsp.start({ name = 'hh200d', cmd = vim.lsp.rpc.connect('127.0.0.1', 9994), settings = { someOption = 12 } })

```

## Roadmap

- [ ] **Diagnostics**: Real-time syntax and semantic error reporting.
- [ ] **Hover Documentation**: Show type information and documentation when hovering over symbols.
- [ ] **Go to Definition**: Jump to the definition of variables, functions, and types.
- [ ] **Code Completion**: Context-aware autocompletion for keywords and symbols.
- [ ] **Rename**: Safe global renaming of symbols.
- [ ] **Formatting**: Automatic code formatting.
- [ ] **Document Symbols**: Outline view of symbols in the current file.
