#!/bin/sh
# npm's prepack hook (package.json): puts the GitHub release binary for
# v$npm_package_version into bin/hh200, so npm ships the same build as the
# AUR package and install.sh. Fails if the release or its .sha256 is missing
# or doesn't match. Run from the repository root (npm does this).
#
#   npm pack          # or npm publish; both run prepack
#
# HH200_RELEASE_TAG overrides the tag, for testing.
set -eu

version="${npm_package_version:-}"
[ -n "$version" ] || { echo "npm_package_version is unset; run via npm pack/publish" >&2; exit 1; }
tag="${HH200_RELEASE_TAG:-v$version}"

case "$tag" in
  "v$version") ;;
  *) echo "warning: packing release $tag as npm version $version" >&2 ;;
esac

asset="hh200-${tag}-linux-x86_64.tar.gz"
url="https://github.com/tbmreza/hh200/releases/download/${tag}/${asset}"

tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT INT TERM

echo "==> fetching $url" >&2
curl -fsSL -o "$tmp/$asset" "$url" \
  || { echo "release asset not found: $url (publish the GitHub release first)" >&2; exit 1; }
curl -fsSL -o "$tmp/$asset.sha256" "$url.sha256" \
  || { echo "checksum not found: $url.sha256" >&2; exit 1; }
(cd "$tmp" && sha256sum -c "$asset.sha256" >&2)

tar -xzf "$tmp/$asset" -C "$tmp"
mkdir -p bin
install -m 755 "$tmp/hh200-${tag}-linux-x86_64/hh200" bin/hh200
echo "==> bin/hh200 from $tag" >&2
