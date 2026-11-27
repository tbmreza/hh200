#!/bin/sh
# Usage: ./bump.sh 0.2.0
# Updates pkgver/pkgrel, checksums and .SRCINFO in place. Needs pacman-contrib
# (updpkgsums) and namcap. Does not commit: copy PKGBUILD and .SRCINFO into
# your clone of ssh://aur@aur.archlinux.org/hh200-bin.git, review, then push.
set -eu
[ $# -eq 1 ] || { echo "usage: $0 <version, without the v>" >&2; exit 1; }
cd "$(dirname "$0")"
sed -i "s/^pkgver=.*/pkgver=$1/; s/^pkgrel=.*/pkgrel=1/" PKGBUILD
updpkgsums
makepkg --printsrcinfo > .SRCINFO
namcap PKGBUILD || true
git --no-pager diff -- PKGBUILD .SRCINFO
