# Project Plan: Road to Alpha

This document outlines the work required to bring `hh200` to an "alpha quality" release. The primary goal of this release is to provide a stable and demonstrable version for research paper artifact reviewers.

## Alpha Quality Criteria

A release is considered "alpha quality" when it meets the following criteria:

1.  **Core Functionality:** The system can successfully execute a `POST` request with cookies and a JSON body, and save the response to a file. This serves as the litmus test for the release.
2.  **Reproducible Dependencies:** All external dependencies, particularly `bel-expr` and `http-client-tls`, are sourced from Git repositories with pinned commits, not local folders. This ensures a consistent and reproducible build environment for reviewers.
3.  **Reviewer-Focused Documentation:** A clear, concise guide exists to help reviewers set up the project, run the core functionality test, and verify its success.

## Sprints

The work is broken down into a series of 2-day sprints, each focused on a specific, high-priority goal. The plan prioritizes architectural decisions and core features that are difficult to change later.

### Sprint 1: Foundational Setup & Core Feature Implementation (2 days)
*   **Goal:** Implement the foundational syntax and logic for the alpha release's core features.
*   **Tasks:**
    *   **Dependency Configuration:** Modify `hh200/stack.yaml` to change the local `extra-deps` to use git repositories with placeholder URLs.
    *   **JSON Body Support:** Update the lexer and parser to correctly handle nested JSON bodies in `POST` requests.
    *   **Cookie and Output Syntax:** Introduce `Cookie` and `Output` directives to the language grammar.

### Sprint 2: Bug Fixing and Test Infrastructure (2 days)
*   **Goal:** Solidify the implementation by fixing known bugs and establishing a reliable testing process.
*   **Tasks:**
    *   Thoroughly test and debug the features from Sprint 1.
    *   Set up a working test environment, including a functional dev server.
    *   Write a new, comprehensive test case that validates the full alpha litmus test.

### Sprint 3: Documentation and Final Polish (2 days)
*   **Goal:** Prepare the project for review by an external audience.
*   **Tasks:**
    *   Write the "Artifact Reviewer's Guide," providing clear, step-by-step instructions.
    *   Review and update all existing documentation for clarity and accuracy.
    *   Clean up the codebase and prepare for the release.
