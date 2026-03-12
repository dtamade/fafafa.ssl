#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_windows_winssl_blocker_batch_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] windows winssl blocker batch tool host fallback contract"

if ! rg -F --quiet -- 'LAZBUILD_EXE="${LAZBUILD_EXE:-lazbuild}"' "$SCRIPT"; then
  fail "script should define LAZBUILD_EXE with env-overridable default"
fi

if ! rg -F --quiet -- 'FPC_EXE="${FPC_EXE:-fpc}"' "$SCRIPT"; then
  fail "script should define FPC_EXE with env-overridable default"
fi

if ! rg -F --quiet -- 'if ! command -v "$LAZBUILD_EXE" >/dev/null 2>&1; then' "$SCRIPT"; then
  fail "script should probe lazbuild host availability"
fi

if ! rg -F --quiet -- 'if ! command -v "$FPC_EXE" >/dev/null 2>&1; then' "$SCRIPT"; then
  fail "script should probe fpc host availability"
fi

if ! rg -F --quiet -- '[WARN] missing lazbuild host in PATH (dry-run placeholder):' "$SCRIPT"; then
  fail "script should keep dry-run preview when lazbuild host is missing"
fi

if ! rg -F --quiet -- '[WARN] missing fpc host in PATH (dry-run placeholder):' "$SCRIPT"; then
  fail "script should keep dry-run preview when fpc host is missing"
fi

if ! rg -F --quiet -- '$ENV_PREFIX $LAZBUILD_EXE --cpu=x86_64 --os=win64' "$SCRIPT"; then
  fail "p133-p135 commands should execute via resolved LAZBUILD_EXE"
fi

if ! rg -F --quiet -- '$ENV_PREFIX $FPC_EXE -Twin64 -Px86_64' "$SCRIPT"; then
  fail "p136 command should execute via resolved FPC_EXE"
fi

echo "[PASS] windows winssl blocker batch tool host fallback contract passed"
