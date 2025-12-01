# VS Code Syntax Highlighting for hh200

This directory contains the VS Code extension structure for `hh200` syntax highlighting.

## Development

To test this extension:

1.  Open this folder (`editors/vscode`) in VS Code.
2.  Press `F5` to start debugging. A new window will open with the extension loaded.
3.  Open a `.hh200` file to see the highlighting.

## Packaging

To package the extension, you can use `vsce`:

```bash
npm install -g vsce
vsce package
```
