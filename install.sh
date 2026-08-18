#!/bin/sh
# install.sh - Installer script for hh200, a HTTP load testing CLI tool.
# This script is designed to be fully POSIX-compliant and run cleanly on minimal
# environments like the curlimages/curl Alpine image in GitLab CI.
#
# Usage:
#   curl -fsSL <url>/install.sh | sh

# Fail loudly and early:
# -e: exit immediately if any command exits with a non-zero status
# -u: treat unset variables as an error
set -eu

REPO="tbmreza/hh200"
VERSION="${HH200_VERSION:-latest}"
BIN_DIR="${XDG_BIN_HOME:-$HOME/.local/bin}"
ARCH="linux-x86_64"

if [ "$VERSION" = "latest" ]; then
  VERSION=$(curl -fsSL "https://api.github.com/repos/${REPO}/releases/latest" \
    | grep '"tag_name":' | sed -E 's/.*"([^"]+)".*/\1/')
fi

ASSET="hh200-${VERSION}-${ARCH}.tar.gz"
URL="https://github.com/${REPO}/releases/download/${VERSION}/${ASSET}"

mkdir -p "$BIN_DIR"
curl -fsSL "$URL" -o /tmp/${ASSET}
tar -xzf /tmp/${ASSET} -C /tmp
mv /tmp/hh200-${VERSION}-${ARCH}/hh200 "$BIN_DIR/hh200"
chmod +x "$BIN_DIR/hh200"

echo "hh200 ${VERSION} installed to $BIN_DIR"
