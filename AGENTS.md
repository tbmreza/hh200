# AGENTS.md

This file contains context and instructions for AI agents working on this repository.

## Project Overview
This repository contains the `hh200` project, which includes a core Haskell application, a Node.js/PHP development server, and editor support (VS Code, Neovim, Emacs, Zsh, Monarch).

## Directory Structure
*   `hh200/`: Core Haskell project.
*   `dev-server/`: Node.js application (Express/SQLite3) and a PHP echo server (`echo.php`).
*   `editors/`: Editor support files.
    *   `monarch/`: Monarch syntax highlighting.
    *   `vscode/`: VS Code extension.
    *   `nvim/`: Neovim plugins (syntax, filetype).
    *   `emacs/`: Emacs configuration.
    *   `zsh/`: Zsh syntax highlighting.
*   `bin/`: Contains scripts and utilities. Note that `bin/windows` is a text file with documentation references, not an executable.
*   `.github/workflows/`: CI configurations.

## Build and Run Instructions

### Haskell Core (`hh200/`)
*   **Build System**: Uses `stack`.
*   **Dependencies**: Requires `alex` (v3.5.2.0) and `happy` (v2.1.4) in the system PATH.
*   **Commands**:
    *   Build: `stack build`
    *   Test: `stack test`
*   **Grammar/Lexer**: Defined in `hh200/src/P.y` (Happy) and `hh200/src/L.x` (Alex).
*   **Stack Config**: `hh200/stack.yaml`. Note that `bel-expr` is a remote GitHub dependency used to replace a local path for CI compatibility.

### Dev Server (`dev-server/`)
*   **Database**: `metrics.db` (SQLite3). Initialized with `populate_db.js`.
*   **Start Node.js Server**: `npm run start` (Runs on Port 3000).
*   **Start PHP Echo Server**: `php -S localhost:9999 -f echo.php`.

## Configuration & Standards
*   **Indentation**: Preferred style is **4 spaces** (not tabs).
*   **Git Ignore**:
    *   Root `.gitignore` handles global ignores (OS, Editors, Node.js) and Haskell artifacts.
    *   Subdirectory `.gitignore` files handle local specific files (e.g., `metrics.db` in `dev-server/`, `errors.err`).

## Editor Specifics
*   **VS Code**: Settings in `editors/vscode/package.json` (`configurationDefaults`). Uses TextMate grammar.
*   **Neovim**:
    *   Filetype plugin: `editors/nvim/ftplugin/hh200.vim` (indentation).
    *   Syntax/Filetype detection: `editors/nvim`.
*   **Emacs**: `editors/emacs/.dir-locals.el` for project settings.
*   **Monarch**: Syntax definitions in `editors/monarch/hh200.json`.
*   **Zsh**: Syntax highlighting script at `editors/zsh/hh200-highlighter.zsh`.

## CI/CD
*   **Workflow**: `.github/workflows/ci.yml`.
*   **Triggers**: `stack build` and `stack test` on the `main` branch.
*   **Badge**: Workflow is named "build" to ensure the status badge displays "build | passing".
