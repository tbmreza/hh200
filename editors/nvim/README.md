# Neovim Support for hh200

Syntax highlighting plus a dependency-free language-server client for `hh200`.

## What's here

```
editors/nvim/
  ftdetect/hh200.vim      filetype detection for *.hhs (and legacy *.hh200)
  ftplugin/hh200.vim      indentation + LSP autostart
  lua/hh200/init.lua      the LSP client (the single "seam" to the compiler)
  syntax/hh200.vim        syntax highlighting
```

## Installation

Copy the directory contents into your Neovim config:

```bash
cp -r editors/nvim/* ~/.config/nvim/
```

Or point your plugin manager at the repo and it will pick up the same layout.

## Usage

Open a file with a `.hhs` extension. Syntax highlighting, diagnostics,
`gofmt`-style range formatting, go-to-definition and find-references activate
automatically as long as the `hh200` binary is on your `$PATH`.

## The contract with the compiler

The editor only knows one thing about the compiler: **it starts `hh200 lsp`
over stdio and reads capabilities from the `initialize` response.** That command
is encoded in exactly one place, `lua/hh200/init.lua` (`command()`). If the
binary's subcommand ever changes, edit that one line.

If you would rather not install the plugin directory, the equivalent 3-line
config is:

```lua
vim.api.nvim_create_autocmd('FileType', {
  pattern = 'hh200',
  callback = function()
    vim.lsp.start({ name = 'hh200d', cmd = { 'hh200', 'lsp' } })
  end,
})
```

Requires Neovim 0.8+ (0.9+ recommended).
