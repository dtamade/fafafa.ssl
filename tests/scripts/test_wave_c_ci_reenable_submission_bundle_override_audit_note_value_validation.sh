#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_override_audit_note_value_validation"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave c b149 override audit note value validation contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_override_audit_note_validation_$$"
OUT_REL="$WORK_REL/reports/b149.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"
ERR_LOG="$WORK_DIR/stderr.log"
INVALID_VALUE="INVALID_AUDIT_NOTE"

rm -f "$OUT_FILE" "$ERR_LOG"

set +e
(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance \
  --override-b147-projected-audit-note "$INVALID_VALUE" \
  >/dev/null 2>"$ERR_LOG")
EC=$?
set -e

if [[ "$EC" -eq 0 ]]; then
  echo "[FAIL] invalid override value should fail"
  [[ -f "$ERR_LOG" ]] && sed -n '1,120p' "$ERR_LOG" || true
  exit 1
fi

if ! rg -F --quiet -- "invalid value for --override-b147-projected-audit-note" "$ERR_LOG"; then
  echo "[FAIL] missing invalid value error message"
  sed -n '1,160p' "$ERR_LOG" || true
  exit 1
fi

if [[ -f "$OUT_FILE" ]]; then
  fail "invalid override should not generate b149 report"
fi

echo "[PASS] wave c b149 override audit note value validation contract passed"
