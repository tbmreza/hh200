# hh200-lsp

This is a bootstrap implementation of the Language Server Protocol (LSP) server for the `hh200` language.

## Status

**Bootstrap**: The server currently implements basic lifecycle management and a proof-of-concept diagnostic pipeline. The backing compiler (`hh200`) is simulated for now.

## Testing

This server is primarily tested on **Neovim**.

### Neovim Configuration

To use this LSP server with Neovim, you can add the following configuration to your `init.lua` or a file in `lua/plugins/`. Ensure you have `nvim-lspconfig` installed.

First, build the server:
```bash
cd lsp
stack install
```
This will install `hh200-lsp` to your local bin path (usually `~/.local/bin`). Ensure this is in your `$PATH`.

Then, configure Neovim:

```lua
local lspconfig = require'lspconfig'
local configs = require'lspconfig.configs'

if not configs.hh200 then
  configs.hh200 = {
    default_config = {
      cmd = {'hh200-lsp'},
      filetypes = {'hh200'},
      root_dir = lspconfig.util.root_pattern('.git', 'package.yaml'),
      settings = {},
    },
  }
end

lspconfig.hh200.setup{
  on_attach = function(client, bufnr)
    print("hh200 LSP attached!")
  end,
}
```

## Features

- **Diagnostics**: Checks for specific keywords (e.g., "todo", "fixme") to demonstrate the `publishDiagnostics` capability.
