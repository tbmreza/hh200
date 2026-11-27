# AUR: hh200-bin

Repackages the GitHub release tarball (`hh200-v<ver>-linux-x86_64.tar.gz`). No build step.

New version, after the GitHub release exists (needs `pacman-contrib`, `namcap`):

```sh
./bump.sh 0.2.0
```

Publish (maintainer, needs the AUR account's SSH key):

```sh
git clone ssh://aur@aur.archlinux.org/hh200-bin.git
cp PKGBUILD .SRCINFO hh200-bin/ && cd hh200-bin
git add PKGBUILD .SRCINFO && git commit -m "Update to <ver>" && git push
```

Notes:
- The binary links glibc, gcc-libs, gmp and zlib dynamically; it is not a static musl build.
- The dashboard SPA (`hh200/min/`) is not in the tarball and is served by relative path
  (AGENTS.md open fix 4), so the dashboard does not work from `/usr/bin`.
- Once a release tarball ships `LICENSE` (current `package-release.sh` does), the separate
  LICENSE source can be dropped in favour of the tarball's copy.
