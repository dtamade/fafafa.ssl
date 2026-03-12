#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_b148_alert_state_summary"
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
    sed -n '1,320p' "$file" || true
    exit 1
  fi
}

extract_marked_state() {
  local file="$1"
  local key="$2"
  rg -o "${key}:[[:space:]]*\\*\\*[A-Z_]+\\*\\*" "$file" \
    | head -1 \
    | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true
}

echo "[TEST] wave c b149 summary includes b148 alert state"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_b148_alert_state_$$"
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

EXPECTED_ALERT_STATE="$(extract_marked_state "$B148_REPORT" "alert_state")"
[[ -n "$EXPECTED_ALERT_STATE" ]] || fail "b148 alert_state should be present"

assert_contains "$OUT_FILE" "- b148_alert_state: $EXPECTED_ALERT_STATE"

echo "[PASS] wave c b149 summary includes b148 alert state passed"
