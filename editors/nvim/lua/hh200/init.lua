-- hh200 language-server client for Neovim.
--
-- Dependency-free: uses only Neovim core `vim.lsp` (no nvim-lspconfig).
--
-- This module is the *single seam* between the editor and the compiler.
-- The one fact the editor needs to know about the hh200 binary lives in
-- `command()` below. If the compiler's CLI changes (e.g. the subcommand is
-- renamed), update only that table.

local M = {}

local SERVER_NAME = 'hh200d'

-- The only place that encodes how to launch the server.
-- stdio is the editor-facing transport; TCP is a debug affordance only.
local function command()
  return { 'hh200', 'lsp' }
end

-- Resolve a stable project root so the client attaches once per workspace.
-- Falls back to the current working directory when nothing better is found.
local function resolve_root_dir()
  local root = vim.fs and vim.fs.dirname(vim.fs.find({ '.git', 'package.yaml', 'hh200.cabal', '.hg' }, { upward = true })[1])
  return root or vim.fn.getcwd()
end

--- Start the hh200 language server for the current buffer (idempotent).
-- @param opts optional table: { root_dir = string }
function M.start(opts)
  opts = opts or {}

  if not vim.lsp or not vim.lsp.start then
    vim.notify('hh200: this plugin needs Neovim >= 0.8 (vim.lsp.start)', vim.log.levels.ERROR)
    return
  end

  local buf = vim.api.nvim_get_current_buf()

  -- Reuse an already-running client for this buffer instead of respawning.
  local get_clients = vim.lsp.get_clients or vim.lsp.get_active_clients
  local clients = get_clients({ bufnr = buf, name = SERVER_NAME })
  if #clients > 0 then
    return
  end

  vim.lsp.start({
    name = SERVER_NAME,
    cmd = command(),
    root_dir = opts.root_dir or resolve_root_dir(),
    -- Diagnostics/formatting/goto-def are advertised by the server; we don't
    -- assume any capability beyond what initialize reports.
  })
end

--- Convenience: wire FileType hh200 to start the server.
-- Use this if you are *not* installing the shipped ftplugin directory.
function M.setup(opts)
  vim.api.nvim_create_autocmd('FileType', {
    pattern = 'hh200',
    callback = function()
      M.start(opts)
    end,
  })
end

return M
