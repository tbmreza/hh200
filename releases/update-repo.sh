#!/bin/bash
set -e

# Usage: ./bin/update-repo.sh <repo_dir>
REPO_DIR=${1:-"gh-pages"}

if [ ! -d "$REPO_DIR" ]; then
    echo "Error: Directory $REPO_DIR does not exist"
    exit 1
fi

cd "$REPO_DIR"

# Ensure we have deb files
if ! ls *.deb 1> /dev/null 2>&1; then
    echo "Warning: No .deb files found in $REPO_DIR"
fi

# Generate Packages
echo "Scanning packages..."
dpkg-scanpackages . /dev/null > Packages
gzip -k -f Packages

# Generate Release
echo "Generating Release file..."
cat <<EOF > Release
Origin: hh200
Label: hh200
Suite: stable
Codename: stable
Architectures: amd64
Components: main
Description: Official hh200 repository
Date: $(date -R)
EOF

# Calculate hashes
echo "MD5Sum:" >> Release
md5sum Packages Packages.gz | awk '{print " " $1 " " $3 " " $2}' >> Release
echo "SHA256:" >> Release
sha256sum Packages Packages.gz | awk '{print " " $1 " " $3 " " $2}' >> Release

echo "Repository updated in $REPO_DIR"
