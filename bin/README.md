Ideally we test releases for every channel where erlang/otp is available.

- [ ] debian (apt)
- [ ] alpine (docker, github & gitlab ci/cd)
- [ ] nix (macos, windows wsl)
- [ ] windows (winget, scoop)

Awesome but not essential for now:
- [ ] wasm (replacing standard otp with wasm-targetting beam language)
- [ ] android (as an app that manages and runs .hhs scripts)

Where we don't see prospect users:
- embedded systems (maybe even impossible with haskell GC and erlang/otp runtime)
