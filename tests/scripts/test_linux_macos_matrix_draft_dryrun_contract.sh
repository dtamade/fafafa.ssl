#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] Linux/macOS matrix draft dry-run contract"

LINUX_OUT="$(cd /tmp && bash "$ROOT_DIR/scripts/run_linux_openssl_matrix_draft.sh" --dry-run --skip-compile --skip-phase2-dryrun --modules PKCS7 2>&1)"
if [[ "$LINUX_OUT" != *"[PASS] linux openssl matrix draft finished"* ]]; then
  fail "linux matrix dry-run should finish with PASS line"
fi
if [[ "$LINUX_OUT" != *"cd '$ROOT_DIR'"* ]]; then
  fail "linux matrix commands should execute under project root"
fi

echo "[PASS] run_linux_openssl_matrix_draft.sh"

MACOS_OUT="$(cd /tmp && bash "$ROOT_DIR/scripts/run_macos_openssl_path_check_draft.sh" --dry-run --skip-module-tests --skip-phase2-dryrun --modules PKCS7 --openssl-root /opt/homebrew/opt/openssl@3 2>&1)"
if [[ "$MACOS_OUT" != *"[WARN] non-macOS environment detected"* ]]; then
  fail "macOS path check should emit non-macOS warning on Linux in dry-run"
fi
if [[ "$MACOS_OUT" != *"[PASS] macOS openssl path check draft finished"* ]]; then
  fail "macOS path check dry-run should finish with PASS line"
fi
if [[ "$MACOS_OUT" != *"cd '$ROOT_DIR'"* ]]; then
  fail "macOS path check commands should execute under project root"
fi

echo "[PASS] run_macos_openssl_path_check_draft.sh"

echo "[PASS] linux/macos dry-run contracts passed"
