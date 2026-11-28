## npmjs

The npm package ships the GitHub release binary, the same build as the AUR package and
`install.sh`. `npm pack` and `npm publish` run `npm-fetch-release.sh` (the `prepack` hook), which
downloads `hh200-v<version>-linux-x86_64.tar.gz` for the `version` in the root `package.json`,
checks it against the release's `.sha256` and puts the binary in `bin/hh200`. Publish the GitHub
release first, then set `version` to the tag without its `v`:

```sh
npm version 0.2.0 --no-git-tag-version   # must match release tag v0.2.0
npm pack --dry-run
npm pack
npm i <path_to_mauikut_tgz>
# npm login --scope=@mauikut
# npm publish
```

The root `package.json` lists the database tooling (`sequelize`, `sqlite3`, ...) under
`devDependencies`, so `npm install -g @mauikut/hh200` installs only the binary.

Ideally we test releases for every channel where our deps are available.

- [ ] debian (apt)
- [ ] arch (AUR `hh200-bin`, see `aur/`)
- [ ] alpine (docker, github & gitlab ci/cd)
- [ ] nix (macos, windows wsl)
- [ ] windows (winget, scoop)

Awesome but not essential for now:
- [ ] wasm
- [ ] android (as an app that manages and runs .hhs scripts)

Where we don't see prospect users:
- embedded systems (maybe even impossible with haskell GC)
