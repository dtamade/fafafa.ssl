#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_android_openssl_path_check_draft.sh"
MISSING_NDK_ROOT="/tmp/fafafa-missing-ndk-root-$$"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] Android OpenSSL path check draft non-dry-run failure contract"

rm -rf "$MISSING_NDK_ROOT"

set +e
OUT="$(cd /tmp && bash "$SCRIPT" \
  --ndk-root "$MISSING_NDK_ROOT" \
  --openssl-root /opt/android-openssl \
  --skip-module-tests \
  --skip-phase2-dryrun 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -eq 0 ]]; then
  fail "non-dry-run should fail when Android NDK root does not exist"
fi

if [[ "$OUT" != *"[FAIL] missing Android NDK root: $MISSING_NDK_ROOT"* ]]; then
  echo "$OUT"
  fail "failure output should mention missing Android NDK root path"
fi

echo "[PASS] android openssl path check non-dry-run failure contract passed"
