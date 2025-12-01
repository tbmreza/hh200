# Monarch Syntax Highlighting for hh200

This directory contains the [Monarch](https://microsoft.github.io/monaco-editor/monarch.html) syntax highlighting definition for the `hh200` language. Monarch is the syntax highlighter used by the Monaco Editor.

## Usage

To use this syntax definition in a web application or a VS Code extension that uses the Monaco Editor (e.g., in a webview), follow these steps:

1.  **Load the JSON file:** Import or fetch the `hh200.json` file.
2.  **Register the language:** Use `monaco.languages.register`.
3.  **Set the tokens provider:** Use `monaco.languages.setMonarchTokensProvider`.

### Example (JavaScript/TypeScript)

```javascript
import * as monaco from 'monaco-editor';
import hh200Def from './hh200.json'; // Assuming you have a loader or fetch the JSON

// 1. Register the language
monaco.languages.register({ id: 'hh200' });

// 2. Set the tokens provider
// Note: hh200.json contains the language definition object directly.
monaco.languages.setMonarchTokensProvider('hh200', hh200Def);

// 3. Define a theme (Optional)
// You can map the token types (e.g., "keyword", "string") to colors.
monaco.editor.defineTheme('hh200-theme', {
    base: 'vs-dark',
    inherit: true,
    rules: [
        { token: 'keyword', foreground: 'C586C0' },
        { token: 'string', foreground: 'CE9178' },
        { token: 'string.link', foreground: '40A1E0', underline: true },
        { token: 'comment', foreground: '6A9955' },
        { token: 'number', foreground: 'B5CEA8' },
        { token: 'identifier', foreground: '9CDCFE' },
        { token: 'delimiter', foreground: 'D4D4D4' }
    ]
});

// 4. Create the editor
monaco.editor.create(document.getElementById('container'), {
    value: 'GET http://example.com',
    language: 'hh200',
    theme: 'hh200-theme'
});
```

## Structure

The `hh200.json` file adheres to the Monarch JSON format. It defines a `tokenizer` with various states:

*   **root**: The main entry point.
*   **whitespace**: Handles spaces.
*   **comments**: Handles `#` comments.
*   **methods**: Matches HTTP methods (GET, POST, etc.).
*   **keywords**: Matches language keywords (then, HTTP, Configs, etc.).
*   **strings**: Matches quoted strings and URLs.
*   **numbers**: Matches numeric values.
*   **identifiers**: Matches variable names and other identifiers.
*   **delimiters**: Matches punctuation.
