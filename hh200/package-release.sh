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

for f in README.md LICENSE; do
  [ -f "${ROOT_DIR}/${f}" ] && cp "${ROOT_DIR}/${f}" "${STAGE_DIR}/${f}"
done

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
