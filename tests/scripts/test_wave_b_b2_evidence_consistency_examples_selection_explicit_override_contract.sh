#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
WORK_REL="tmp/test_wave_b_evidence_examples_selection_override_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_evidence_selection_override_$$"
OVERRIDE_REL="$REPORTS_REL/custom_examples_override.json"
OVERRIDE_FILE="$ROOT_DIR/$OVERRIDE_REL"
OUTPUT_REL="$REPORTS_REL/evidence_selection_override_${RUN_ID}.md"
OUTPUT_FILE="$ROOT_DIR/$OUTPUT_REL"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b evidence consistency examples selection explicit override contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

cat > "$OVERRIDE_FILE" <<EOF_JSON
{
  "run_id": "$RUN_ID",
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

cat > "$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md" <<EOF_CROSS
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
EOF_CROSS

cat > "$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md" <<EOF_CLOSE
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
EOF_CLOSE

FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
FAFAFA_WAVE_B_EXAMPLES_REPORT_REL="$OVERRIDE_REL" \
  bash "$SCRIPT" --run-id "$RUN_ID" --output "$OUTPUT_REL" >/dev/null

[[ -f "$OUTPUT_FILE" ]] || fail "evidence report should be generated"
assert_contains "$OUTPUT_FILE" "- linux_examples_selection: explicit_override"
assert_contains "$OUTPUT_FILE" "| linux_examples_json | $OVERRIDE_REL | YES | $RUN_ID | YES | ok |"

echo "[PASS] wave-b evidence consistency examples selection explicit override contract passed"
