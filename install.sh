#!/bin/sh
# install.sh - Installer script for hh200, a HTTP load testing CLI tool.
# This script is designed to be fully POSIX-compliant and run cleanly on minimal
# environments like the curlimages/curl Alpine image in GitLab CI.
#
# Usage:
#   curl -fsSL <url>/install.sh | sh
#
# ==============================================================================
# HOW TO TEST THIS SCRIPT (no docker-compose version)
# ==============================================================================
# You can test this script locally inside a matching 'curlimages/curl' container
# to ensure everything works correctly under Alpine/musl and a POSIX environment:
#
# 1. Start the container and run the script directly:
#    docker run --rm -v "$(pwd):/workspace" -w /workspace curlimages/curl:latest sh install.sh
#
# 2. Simulate standard piping execution (curl ... | sh):
#    docker run --rm -v "$(pwd):/workspace" -w /workspace curlimages/curl:latest sh -c "cat install.sh | sh"
#
# 3. Test with a mock Release API response (highly recommended to bypass rate limits
#    and verify behavior when no public asset is built yet):
#    - Create a file 'mock_release.json' containing:
#      {
#        "assets": [
#          {
#            "browser_download_url": "file:///workspace/mock_bin?hh200-linux-amd64"
#          }
#        ]
#      }
#    - Create a dummy file 'mock_bin' with executable permissions.
#    - Run:
#      docker run --rm -v "$(pwd):/workspace" -e HH200_RELEASE_API_URL="file:///workspace/mock_release.json" -w /workspace curlimages/curl:latest sh install.sh
# ==============================================================================

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

# # Initialize empty temporary file paths to avoid unbound variable errors in cleanup
# TMP_JSON=""
# TMP_BIN=""
#
# # Clean up temporary files on exit
# cleanup() {
#     if [ -n "${TMP_JSON:-}" ] && [ -f "$TMP_JSON" ]; then
#         rm -f "$TMP_JSON"
#     fi
#     if [ -n "${TMP_BIN:-}" ] && [ -f "$TMP_BIN" ]; then
#         rm -f "$TMP_BIN"
#     fi
# }
# trap cleanup EXIT INT TERM
#
# # 1. Detect OS/arch (specifically Linux on x86_64/arm64 for GitLab runners)
# detect_os_arch() {
#     OS_RAW=$(uname -s)
#     ARCH_RAW=$(uname -m)
#
#     case "$OS_RAW" in
#         Linux)
#             OS="linux"
#             ;;
#         *)
#             echo "Error: Operating system '$OS_RAW' is not supported." >&2
#             echo "This installer currently only supports Linux (e.g. GitLab runners)." >&2
#             exit 1
#             ;;
#     esac
#
#     case "$ARCH_RAW" in
#         x86_64)
#             ARCH="amd64"
#             ;;
#         aarch64|arm64)
#             ARCH="arm64"
#             ;;
#         *)
#             echo "Error: Architecture '$ARCH_RAW' is not supported." >&2
#             exit 1
#             ;;
#     esac
# }
#
# detect_os_arch
#
# echo "Detected system: ${OS}-${ARCH}"
#
# # 2. Query GitHub Releases API to find the latest release asset URL
# REPO="tbmreza/hh200"
# API_URL="${HH200_RELEASE_API_URL:-https://api.github.com/repos/${REPO}/releases/latest}"
# BINARY_NAME="hh200-${OS}-${ARCH}"
#
# echo "Fetching latest release metadata for ${BINARY_NAME}..."
#
# # Create a secure temporary file to store the API response
# # Use standard POSIX-friendly temp file creation or fallback
# TMP_JSON=$(mktemp 2>/dev/null || mktemp -t 'hh200-json' 2>/dev/null || echo "/tmp/hh200-release.json")
#
# # Fetch the release JSON
# if ! curl -fsSL -H "Accept: application/vnd.github.v3+json" "$API_URL" > "$TMP_JSON"; then
#     echo "Error: Failed to fetch release metadata from GitHub API." >&2
#     exit 1
# fi
#
# # Extract the browser_download_url matching our target binary
# # We use standard POSIX awk to parse the JSON string:
# # It splits the line on double quotes (") and extracts the 4th field when the
# # line matches the "browser_download_url" key and ends with the exact binary name followed by a quote.
# # This prevents false positives matching checksum or signature files (e.g. hh200-linux-amd64.sha256).
# DOWNLOAD_URL=$(awk -F'"' '/"browser_download_url"/ && /'"${BINARY_NAME}"'("|\?)/ {print $4; exit}' "$TMP_JSON")
#
# if [ -z "$DOWNLOAD_URL" ]; then
#     echo "Error: Could not find download URL for asset '${BINARY_NAME}' in the latest release." >&2
#     exit 1
# fi
#
# # 3. Download the binary and install it to $HOME/.local/bin
# INSTALL_DIR="${HOME}/.local/bin"
# TARGET_PATH="${INSTALL_DIR}/hh200"
#
# echo "Downloading ${BINARY_NAME}..."
# TMP_BIN=$(mktemp 2>/dev/null || mktemp -t 'hh200-bin' 2>/dev/null || echo "/tmp/hh200-bin")
#
# if ! curl -fsSL "$DOWNLOAD_URL" > "$TMP_BIN"; then
#     echo "Error: Failed to download binary from ${DOWNLOAD_URL}" >&2
#     exit 1
# fi
#
# # Ensure the download wasn't empty
# if [ ! -s "$TMP_BIN" ]; then
#     echo "Error: Downloaded binary is empty." >&2
#     exit 1
# fi
#
# # Create installation directory if missing
# mkdir -p "$INSTALL_DIR"
#
# # Move the downloaded binary to target path and make it executable
# mv "$TMP_BIN" "$TARGET_PATH"
# chmod +x "$TARGET_PATH"
#
# # 4. Set up an XDG-compliant app data directory and empty/init SQLite db file
# DB_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/hh200"
# DB_PATH="${DB_DIR}/hh200.sqlite"
#
# # Create database directory if missing
# mkdir -p "$DB_DIR"
#
# # Idempotent: only initialize the SQLite file if it doesn't already exist
# if [ ! -f "$DB_PATH" ]; then
#     echo "Initializing database at ${DB_PATH}..."
#     touch "$DB_PATH"
# else
#     echo "Database already exists at ${DB_PATH}. Skipping initialization."
# fi
#
# # 5. Success message
# echo "=================================================="
# echo "hh200 has been successfully installed!"
# echo "=================================================="
# echo "Executable path: ${TARGET_PATH}"
# echo "Database path:   ${DB_PATH}"
# echo ""
# echo "Please ensure ${INSTALL_DIR} is in your PATH."
# echo "You can add it by adding the following to your shell profile (e.g., .profile or .bashrc):"
# echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
# echo "=================================================="
