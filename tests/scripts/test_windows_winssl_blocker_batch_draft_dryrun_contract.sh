#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_windows_winssl_blocker_batch_draft.sh"
WORK_REL="tmp/test_windows_winssl_blocker_batch_dryrun"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] windows winssl blocker batch draft dry-run contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="winssl_blocker_dryrun_$$"
OUT_REL="$WORK_REL/reports/summary.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"

OUT="$(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --reports-dir "$WORK_REL/reports" \
  --output "$OUT_REL" \
  --dry-run 2>&1)"

[[ -f "$OUT_FILE" ]] || fail "dry-run should generate summary report"
if [[ "$OUT" != *"[WARN] non-Windows environment detected"* ]]; then
  echo "$OUT"
  fail "dry-run on Linux should emit non-Windows warning"
fi
if [[ "$OUT" != *"[PASS] windows winssl blocker batch draft report generated"* ]]; then
  echo "$OUT"
  fail "dry-run should emit PASS report line"
fi

if ! rg -F --quiet "P1-33" "$OUT_FILE"; then
  sed -n '1,260p' "$OUT_FILE" || true
  fail "summary should include P1-33 mapping"
fi
if ! rg -F --quiet "lazbuild --cpu=x86_64 --os=win64 test_winssl_certificate_loading.lpi" "$OUT_FILE"; then
  sed -n '1,260p' "$OUT_FILE" || true
  fail "summary should include p133 command"
fi
if ! rg -F --quiet "fpc -Twin64 -Px86_64" "$OUT_FILE"; then
  sed -n '1,260p' "$OUT_FILE" || true
  fail "summary should include p136 win64 command"
fi

echo "[PASS] windows winssl blocker batch draft dry-run contract passed"
