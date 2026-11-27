# AUR: hh200-bin

Repackages the GitHub release tarball; no build step. Users install with `yay -S hh200-bin`.

After a new GitHub release exists (needs `pacman-contrib` and `namcap`):

```sh
./bump.sh 0.2.0
```

Then copy `PKGBUILD` and `.SRCINFO` into a clone of `ssh://aur@aur.archlinux.org/hh200-bin.git`,
commit and push.

The dashboard won't work from this package: its SPA isn't in the tarball and is served from a
relative `min/` path (AGENTS.md open fix 4).
