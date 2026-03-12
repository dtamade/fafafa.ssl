#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_windows_winssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] Windows WinSSL path check draft dry-run contract"

OUT="$(cd /tmp && bash "$SCRIPT" \
  --dry-run \
  --skip-module-tests \
  --skip-phase2-dryrun \
  --modules PKCS7 \
  --msys2-root C:/msys64 2>&1)"

if [[ "$OUT" != *"[WARN] non-Windows environment detected"* ]]; then
  fail "windows path check should emit non-Windows warning on Linux in dry-run"
fi

if [[ "$OUT" != *"[PASS] windows winssl path check draft finished"* ]]; then
  fail "windows path check dry-run should finish with PASS line"
fi

if [[ "$OUT" != *"cd '$ROOT_DIR'"* ]]; then
  fail "windows path check commands should execute under project root"
fi

echo "[PASS] windows winssl path check dry-run contract passed"
