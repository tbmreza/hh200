# AUR: hh200-bin

Repackages the GitHub release tarball; no build step. Users install with `yay -S hh200-bin`.
Shell completions for bash, zsh and fish are installed with it.

After a new GitHub release exists (needs `pacman-contrib` and `namcap`):

```sh
./bump.sh v0.2.0
```

Pass the release tag. Pre-release tags such as `v0.2.0-beta.1` become pkgver `0.2.0beta.1`,
which pacman sorts before `0.2.0`. Passing the current tag again bumps `pkgrel`, for
packaging-only changes. Never re-upload an asset under an existing tag: the published checksum
would stop matching for everyone.

Then copy `PKGBUILD` and `.SRCINFO` into a clone of `ssh://aur@aur.archlinux.org/hh200-bin.git`,
commit and push.

The dashboard won't work from this package: its SPA isn't in the tarball and is served from a
relative `min/` path (AGENTS.md open fix 4).

## Pinned AUR comment

> If `hh200 --version` doesn't change after an upgrade, an older copy is earlier on your PATH:
> `install.sh` puts one in `~/.local/bin`. Check with `which -a hh200` and remove the stale one.
> If pacman says `/usr/bin/hh200 exists in filesystem`, it was installed with `npm install -g`;
> run `npm uninstall -g @mauikut/hh200` first.
