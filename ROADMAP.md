# Roadmap

This document outlines the development plan for `hh200`, a "fail-fast", batteries-included DSL for testing HTTP servers.

## Vision
`hh200` prioritizes reliability and ease of use for developers testing their APIs. It integrates a full expression language (`bel-expr`), regex support, and plans for high-concurrency load testing.

## Current Status (v0.0.1)
- **Core DSL**: Basic HTTP request/response parsing and execution.
- **Batteries**: Integration with `bel-expr` for expression evaluation (Regex, Random, Time).
- **Execution**: Basic sequential execution.
- **Editors**: Initial support for VS Code, Neovim, Emacs, and Zsh.
- **Dependencies**: Uses `http-client` and `http-client-tls`.

## Short-term Goals (v0.1.0)
The primary goal is to stabilize the core features to match standard HTTP client expectations and enable robust testing scenarios.

### Core Features
- [ ] **Cookie Management**:
    - [ ] Implement `Set-Cookie` parsing and storage.
    - [ ] Support `Secure` attribute (send only over HTTPS).
    - [ ] Proper cookie jar handling across requests.
- [ ] **HTTPS Support**:
    - [ ] Distinct handling of TLS vs. non-TLS connections.
    - [ ] Verify `Connection: close` behavior by default.
- [ ] **Assertions & Captures**:
    - [ ] Stabilize JSONPath integration (`aeson-jsonpath`).
    - [ ] Implement "eyeball" assertions (human verification step).
    - [ ] Enhance capture logic for dynamic values (e.g., tokens).

### Syntax Enhancements
- [ ] **Control Flow**:
    - [ ] `unless` logic for conditional execution.
    - [ ] Loop structures (`until`, `step`).
    - [ ] Scoped variables (`let`).
- [ ] **Output**:
    - [ ] Support writing response bodies to files (e.g., `POST ... write out.png`).

## Medium-term Goals
### Parallelism & Performance
- [ ] **Parallel Execution Models**:
    - [ ] "Shotgun" mode: Thread-based parallelism using `async` and `QSemN`.
    - [ ] "RPS" mode: Duration-bound virtual users.
    - [ ] Semantic definition for `#! ["user1", "user2"]` syntax.
- [ ] **Performance**:
    - [ ] Optimize for higher concurrency (aiming towards 1M parallel requests).

### Infrastructure & Tooling
- [ ] **CI/CD**:
    - [ ] Automated release builds.
    - [ ] Self-maintaining fork of `httpLbs` (sync via GitHub Actions).
- [ ] **Developer Experience**:
    - [ ] Better error reporting (parser and runtime).
    - [ ] Language Server Protocol (LSP) implementation.
    - [ ] Code formatter.

## Long-term Ideas
- [ ] Distributed load testing (grid capabilities).
- [ ] First-class callables/functions in the DSL.
- [ ] Advanced tree-based statement evaluation.

## References
- `DRAFT.md`: Contains raw notes, design stashes, and syntax ideas.
- `README.md`: Project philosophy and comparison with other tools.
