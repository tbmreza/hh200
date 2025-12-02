# Zsh Syntax Highlighting for hh200

This directory contains a custom highlighter for `zsh-syntax-highlighting`.

## Usage

1.  Ensure you have [zsh-syntax-highlighting](https://github.com/zsh-users/zsh-syntax-highlighting) installed.
2.  Source the `hh200-highlighter.zsh` script in your `.zshrc`.
3.  Add `hh200` to your `ZSH_HIGHLIGHT_HIGHLIGHTERS` array.

```zsh
source /path/to/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
source /path/to/editors/zsh/hh200-highlighter.zsh

ZSH_HIGHLIGHT_HIGHLIGHTERS+=(hh200)
```
