#!/bin/sh
# Usage: ./bump.sh <release tag>     e.g. ./bump.sh v0.2.0   or   ./bump.sh v0.2.0-beta.1
#
# A new tag sets pkgver from it and resets pkgrel to 1. Passing the current tag
# again bumps pkgrel instead (for packaging-only changes). Checks that the
# release asset exists before changing anything, then updates checksums and
# .SRCINFO. Needs pacman-contrib (updpkgsums) and namcap. Does not commit.
set -eu
[ $# -eq 1 ] || { echo "usage: $0 <release tag, e.g. v0.2.0>" >&2; exit 1; }
cd "$(dirname "$0")"

tag="v${1#v}"
# pkgver may not contain '-'. Dropping it keeps the order right:
# pacman sorts 0.2.0beta.1 before 0.2.0.
ver=$(printf '%s' "${tag#v}" | tr -d -- '-')
case "$ver" in
  *[!A-Za-z0-9._]*|'') echo "can't turn tag '$tag' into a pkgver" >&2; exit 1 ;;
esac

asset="https://github.com/tbmreza/hh200/releases/download/${tag}/hh200-${tag}-linux-x86_64.tar.gz"
# One-byte GET: the asset host GitHub redirects to rejects HEAD requests.
if ! curl -fsL -r 0-0 -o /dev/null "$asset"; then
  echo "release asset not found: $asset" >&2
  echo "publish the GitHub release first" >&2
  exit 1
fi

old_tag=$(sed -n 's/^_tag=\([^ ]*\).*/\1/p' PKGBUILD)
if [ "$tag" = "$old_tag" ]; then
  rel=$(( $(sed -n 's/^pkgrel=//p' PKGBUILD) + 1 ))
else
  rel=1
fi
sed -i "s/^pkgver=.*/pkgver=$ver/; s/^_tag=[^ ]*/_tag=$tag/; s/^pkgrel=.*/pkgrel=$rel/" PKGBUILD

updpkgsums
makepkg --printsrcinfo > .SRCINFO
namcap PKGBUILD || true
echo "==> $ver-$rel ($tag)"
git --no-pager diff -- PKGBUILD .SRCINFO 2>/dev/null || true
