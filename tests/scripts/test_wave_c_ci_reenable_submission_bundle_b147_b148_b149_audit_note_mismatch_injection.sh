#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_b147_b148_b149_audit_note_mismatch_injection"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,420p' "$file" || true
    exit 1
  fi
}

extract_summary_value() {
  local file="$1"
  local key="$2"
  rg -o -- "- ${key}: [A-Z0-9_]+" "$file" \
    | head -1 \
    | sed -E "s/- ${key}: ([A-Z0-9_]+)/\\1/" || true
}

echo "[TEST] wave c b149 b147-b148-b149 audit note mismatch injection contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_b147_b148_b149_mismatch_injection_$$"
OUT_REL="$WORK_REL/reports/b149.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"
OVERRIDE_NOTE="B148_ALERT_MISSING"

rm -f "$OUT_FILE"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance \
  --override-b147-projected-audit-note "$OVERRIDE_NOTE" >/dev/null)

[[ -f "$OUT_FILE" ]] || fail "b149 report should be generated"

assert_contains "$OUT_FILE" "- b147_projected_b149_audit_alert_note: $OVERRIDE_NOTE"
assert_contains "$OUT_FILE" "- b149_audit_alert_note_sync_state: MISMATCH"
assert_contains "$OUT_FILE" "- b147_b148_b149_audit_note_consistency: MISMATCH"

actual_b149_note="$(extract_summary_value "$OUT_FILE" "b149_audit_alert_note")"
[[ -n "$actual_b149_note" ]] || fail "b149 note should be present"
[[ "$actual_b149_note" != "$OVERRIDE_NOTE" ]] || fail "override note should differ from b149 note in mismatch injection scenario"

echo "[PASS] wave c b149 b147-b148-b149 audit note mismatch injection contract passed"
