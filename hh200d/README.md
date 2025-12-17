# hh200d LSP Server

This is a minimal Haskell LSP server bootstrapped with `stack`.

## Prerequisites

- `stack`
- `nvim` (NeoVim)

## Building

```bash
stack build
```

## Neovim Configuration

To attach this LSP server to your buffer, use the following Lua snippet in your Neovim config (or run it as a command `:lua ...`). This assumes you are opening a file within this project directory.

```lua
vim.api.nvim_create_autocmd("FileType", {
  pattern = "haskell",
  callback = function()
    vim.lsp.start({
      name = "hh200d",
      cmd = { "stack", "exec", "--", "hh200d-exe" },
      root_dir = vim.fs.dirname(vim.fs.find({ "stack.yaml", ".git" }, { path = vim.api.nvim_buf_get_name(0), upward = true })[1]),
    })
  end,
})
```

Alternatively, for a one-off test on the current buffer:

```lua
:lua vim.lsp.start({ name = 'hh200d', cmd = {'stack', 'exec', '--', 'hh200d-exe'}, root_dir = vim.loop.cwd() })
```

Verify attachment with `:LspInfo`. You should see "hh200d" listed under "Active Clients" or "attached to this buffer".
