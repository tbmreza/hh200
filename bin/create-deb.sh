#!/bin/bash
set -e

# Usage: ./bin/create-deb.sh <binary_path> <version> <arch>
# Assumes it is run from the repo root and hh200/package.yaml exists

BINARY_PATH=${1:-"$(find .stack-work -type f -name hh200 | head -n 1)"}
VERSION=${2:-"0.0.1"}
ARCH=${3:-"amd64"}

if [ -z "$BINARY_PATH" ] || [ ! -f "$BINARY_PATH" ]; then
    echo "Error: Binary not found at $BINARY_PATH"
    exit 1
fi

PACKAGE_NAME="hh200"
WORK_DIR="build_deb/${PACKAGE_NAME}_${VERSION}_${ARCH}"

# Extract Maintainer from package.yaml if possible
MAINTAINER="Reza Handzalah <rezahandzalah@gmail.com>"
if [ -f "hh200/package.yaml" ]; then
    EXTRACTED_MAINT=$(grep '^maintainer:' hh200/package.yaml | sed 's/maintainer:[[:space:]]*"\(.*\)"/\1/')
    if [ ! -z "$EXTRACTED_MAINT" ]; then
        MAINTAINER="$EXTRACTED_MAINT"
    fi
fi

# Clean previous build
rm -rf "build_deb"
mkdir -p "$WORK_DIR/DEBIAN"
mkdir -p "$WORK_DIR/usr/bin"

# Copy binary
cp "$BINARY_PATH" "$WORK_DIR/usr/bin/$PACKAGE_NAME"
chmod 755 "$WORK_DIR/usr/bin/$PACKAGE_NAME"

# Create control file
cat <<EOF > "$WORK_DIR/DEBIAN/control"
Package: $PACKAGE_NAME
Version: $VERSION
Section: utils
Priority: optional
Architecture: $ARCH
Maintainer: $MAINTAINER
Description: Statically-checked DSL for testing HTTP servers
 hh200 is a domain-specific language designed to make testing HTTP servers
 reliable and type-safe.
EOF

# Build package
mkdir -p dist
dpkg-deb --build "$WORK_DIR" "dist/${PACKAGE_NAME}_${VERSION}_${ARCH}.deb"

echo "Package created at dist/${PACKAGE_NAME}_${VERSION}_${ARCH}.deb"
