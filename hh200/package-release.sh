#!/usr/bin/env bash
set -euo pipefail

# ---- config -----------------------------------------------------------
BIN_NAME="hh200"
TARGET_ARCH="linux-x86_64"
# -------------------------------------------------------------------------

VERSION="${1:-${HH200_VERSION:-}}"
if [ -z "${VERSION}" ]; then
  echo "Usage: $0 <version>   (or set HH200_VERSION)" >&2
  exit 1
fi

echo "==> Packaging ${BIN_NAME} ${VERSION} (${TARGET_ARCH})"

# 1. Ask stack where the built binary lives (the one stack-specific line)
LOCAL_INSTALL_ROOT="$(stack path --local-install-root)"
SRC_BIN="${LOCAL_INSTALL_ROOT}/bin/${BIN_NAME}"

if [ ! -f "${SRC_BIN}" ]; then
  echo "Binary not found at: ${SRC_BIN}" >&2
  echo "Did you run 'stack build --ghc-options=-O2' first?" >&2
  exit 1
fi

# 2. Strip debug symbols (best-effort)
echo "==> Stripping symbols"
strip --strip-all "${SRC_BIN}" || echo "warning: strip failed or unavailable, continuing"

# 3. Stage into dist/<target>/
ROOT_DIR="$(pwd)"
TARGET_NAME="${BIN_NAME}-${VERSION}-${TARGET_ARCH}"
STAGE_DIR="${ROOT_DIR}/dist/${TARGET_NAME}"
TARBALL="${ROOT_DIR}/dist/${TARGET_NAME}.tar.gz"

rm -rf "${STAGE_DIR}"
mkdir -p "${STAGE_DIR}"
cp "${SRC_BIN}" "${STAGE_DIR}/${BIN_NAME}"
chmod +x "${STAGE_DIR}/${BIN_NAME}"

# Docs to ship: look in the working directory first, then at the repository
# root (one level above this script), since LICENSE lives at the repo root
# while the script is normally run from hh200/.
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/.." && pwd)"

find_doc() {
  for dir in "${ROOT_DIR}" "${REPO_ROOT}"; do
    if [ -f "${dir}/$1" ]; then
      echo "${dir}/$1"
      return 0
    fi
  done
  return 1
}

if README_SRC="$(find_doc README.md)"; then
  cp "${README_SRC}" "${STAGE_DIR}/README.md"
else
  echo "warning: README.md not found, packaging without it" >&2
fi

# The MIT license requires the notice to ship with the binary: no LICENSE, no release.
if LICENSE_SRC="$(find_doc LICENSE)"; then
  cp "${LICENSE_SRC}" "${STAGE_DIR}/LICENSE"
else
  echo "LICENSE not found in ${ROOT_DIR} or ${REPO_ROOT}; refusing to package" >&2
  exit 1
fi

# 4. tar.gz it (relative paths only, so extraction doesn't leak absolute dirs)
echo "==> Creating tarball"
tar -czf "${TARBALL}" -C "${ROOT_DIR}/dist" "${TARGET_NAME}"

# 5. sha256 checksum, written next to the tarball
echo "==> Writing checksum"
if command -v sha256sum >/dev/null 2>&1; then
  ( cd "${ROOT_DIR}/dist" && sha256sum "${TARGET_NAME}.tar.gz" > "${TARGET_NAME}.tar.gz.sha256" )
elif command -v shasum >/dev/null 2>&1; then
  ( cd "${ROOT_DIR}/dist" && shasum -a 256 "${TARGET_NAME}.tar.gz" > "${TARGET_NAME}.tar.gz.sha256" )
else
  echo "warning: no sha256sum/shasum found, skipping checksum" >&2
fi

echo "==> Done: ${TARBALL}"
echo "==> Done: ${TARBALL}.sha256"
