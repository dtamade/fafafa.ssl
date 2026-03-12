#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_override_audit_note_allowed_values_batch"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

extract_summary_value() {
  local file="$1"
  local key="$2"
  rg -o -- "- ${key}: [A-Z0-9_]+" "$file" \
    | head -1 \
    | sed -E "s/- ${key}: ([A-Z0-9_]+)/\\1/" || true
}

echo "[TEST] wave c b149 override audit note allowed values batch contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

allowed_values=(
  "B148_ALERT_WARN_REVIEW_REQUIRED"
  "B148_ALERT_CLEAR"
  "B148_ALERT_MISSING"
  "B148_ALERT_UNKNOWN"
)

for override_note in "${allowed_values[@]}"; do
  run_tag="${override_note,,}"
  run_tag="${run_tag//[^a-z0-9]/_}"
  run_id="b149_override_allowed_${run_tag}_$$"
  out_rel="$WORK_REL/reports/${run_tag}.md"
  out_file="$ROOT_DIR/$out_rel"

  rm -f "$out_file"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --run-id "$run_id" \
    --output "$out_rel" \
    --skip-local-guard-batch \
    --skip-docs-governance \
    --override-b147-projected-audit-note "$override_note" >/dev/null)

  [[ -f "$out_file" ]] || fail "report should be generated for override: $override_note"

  projected_note="$(extract_summary_value "$out_file" "b147_projected_b149_audit_alert_note")"
  b149_note="$(extract_summary_value "$out_file" "b149_audit_alert_note")"
  sync_state="$(extract_summary_value "$out_file" "b149_audit_alert_note_sync_state")"

  [[ "$projected_note" == "$override_note" ]] || fail "projected note mismatch for override: $override_note"
  [[ -n "$b149_note" ]] || fail "b149 note missing for override: $override_note"
  [[ -n "$sync_state" ]] || fail "sync state missing for override: $override_note"

  expected_sync="MISMATCH"
  if [[ "$override_note" == "$b149_note" ]]; then
    expected_sync="MATCH"
  fi

  [[ "$sync_state" == "$expected_sync" ]] || fail "sync state mismatch for override: $override_note (expect $expected_sync got $sync_state)"
done

echo "[PASS] wave c b149 override audit note allowed values batch contract passed"
