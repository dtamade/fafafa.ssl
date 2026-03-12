#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_b147_b148_b149_audit_note_consistency"
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

extract_marked_state() {
  local file="$1"
  local key="$2"
  rg -o "${key}:[[:space:]]*\*\*[A-Z0-9_]+\*\*" "$file" \
    | head -1 \
    | sed -E 's/.*\*\*([A-Z0-9_]+)\*\*/\1/' || true
}

echo "[TEST] wave c b149 b147-b148-b149 audit note consistency contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_b147_b148_b149_consistency_$$"
OUT_REL="$WORK_REL/reports/b149.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"
B148_REPORT="$ROOT_DIR/tmp/wave_c_ci_reenable_reports/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"

rm -f "$OUT_FILE" "$B148_REPORT"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance >/dev/null)

[[ -f "$OUT_FILE" ]] || fail "b149 report should be generated"
[[ -f "$B148_REPORT" ]] || fail "b148 report should be generated"

assert_contains "$OUT_FILE" "- b148_b149_audit_alert_note_sync_state:"
assert_contains "$OUT_FILE" "- b147_b148_b149_audit_note_consistency:"

b148_sync_state="$(extract_marked_state "$B148_REPORT" "b149_audit_alert_note_sync_state")"
[[ -n "$b148_sync_state" ]] || fail "b148 sync state should be present"

b149_b148_sync_state="$(extract_summary_value "$OUT_FILE" "b148_b149_audit_alert_note_sync_state")"
chain_consistency="$(extract_summary_value "$OUT_FILE" "b147_b148_b149_audit_note_consistency")"
b147_projected_note="$(extract_summary_value "$OUT_FILE" "b147_projected_b149_audit_alert_note")"
b149_note="$(extract_summary_value "$OUT_FILE" "b149_audit_alert_note")"

[[ -n "$b149_b148_sync_state" ]] || fail "b149 summary should contain b148 sync passthrough"
[[ -n "$chain_consistency" ]] || fail "b149 summary should contain chain consistency"
[[ -n "$b147_projected_note" ]] || fail "b149 summary should contain b147 projected note"
[[ -n "$b149_note" ]] || fail "b149 summary should contain b149 note"

[[ "$b149_b148_sync_state" == "$b148_sync_state" ]] || fail "b149 b148 sync passthrough should equal b148 report"
[[ "$b147_projected_note" == "$b149_note" ]] || fail "b147 projected note should equal b149 note"
[[ "$chain_consistency" == "MATCH" ]] || fail "three-stage chain consistency should be MATCH"

echo "[PASS] wave c b149 b147-b148-b149 audit note consistency contract passed"
