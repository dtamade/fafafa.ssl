#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_windows_winssl_blocker_batch_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] windows winssl blocker batch draft non-dry-run failure contract"

set +e
OUT="$(cd "$ROOT_DIR" && bash "$SCRIPT" --strict 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -eq 0 ]]; then
  fail "non-dry-run should fail on non-Windows environment"
fi

if [[ "$OUT" != *"[FAIL] this script is intended for Windows/Win64 RTL (current:"* ]]; then
  echo "$OUT"
  fail "failure output should mention Windows/Win64 RTL requirement"
fi

echo "[PASS] windows winssl blocker batch draft non-dry-run failure contract passed"
