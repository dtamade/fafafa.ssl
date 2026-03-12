#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_b147_audit_note_sync"
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
    sed -n '1,360p' "$file" || true
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

echo "[TEST] wave c b149 b147 audit note sync summary contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_b147_audit_note_sync_$$"
OUT_REL="$WORK_REL/reports/b149.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"

rm -f "$OUT_FILE"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance >/dev/null)

[[ -f "$OUT_FILE" ]] || fail "b149 report should be generated"

assert_contains "$OUT_FILE" "- b147_projected_b149_audit_alert_note:"
assert_contains "$OUT_FILE" "- b149_audit_alert_note_sync_state: MATCH"

projected_note="$(extract_summary_value "$OUT_FILE" "b147_projected_b149_audit_alert_note")"
actual_note="$(extract_summary_value "$OUT_FILE" "b149_audit_alert_note")"

[[ -n "$projected_note" ]] || fail "projected note should be present"
[[ -n "$actual_note" ]] || fail "actual note should be present"
[[ "$projected_note" == "$actual_note" ]] || fail "projected note should match actual note"

echo "[PASS] wave c b149 b147 audit note sync summary contract passed"
