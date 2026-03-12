#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_windows_winssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] Windows WinSSL path check draft non-dry-run failure contract"

set +e
OUT="$(cd /tmp && bash "$SCRIPT" --skip-module-tests --skip-phase2-dryrun 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -eq 0 ]]; then
  fail "non-dry-run should fail on non-Windows environment"
fi

if [[ "$OUT" != *"[FAIL] this script is intended for Windows/MSYS2 (current: "* ]]; then
  echo "$OUT"
  fail "failure output should mention non-Windows environment restriction"
fi

echo "[PASS] windows winssl path check non-dry-run failure contract passed"
