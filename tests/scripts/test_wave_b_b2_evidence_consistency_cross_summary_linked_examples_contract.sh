#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
WORK_REL="tmp/test_wave_b_evidence_cross_linked_examples_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_cross_linked_examples_$$"
OUTPUT_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"

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
    sed -n '1,320p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b evidence consistency cross-summary linked examples contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

cat > "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" <<EOF_JSON
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
- linux_examples_json: $REPORTS_REL/examples_compile_ci_gate.json
- linux_examples_selection: static_same_run_fallback
EOF_CROSS

cat > "$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md" <<EOF_CLOSE
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
EOF_CLOSE

set +e
FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
  bash "$SCRIPT" --run-id "$RUN_ID" --strict >/dev/null 2>&1
EC=$?
set -e

if [[ $EC -eq 0 ]]; then
  fail "strict mode should fail when cross-summary embeds mismatched linux examples linkage"
fi

[[ -f "$OUTPUT_FILE" ]] || fail "evidence report should be generated"
assert_contains "$OUTPUT_FILE" "- consistency_status: **INCONSISTENT**"
assert_contains "$OUTPUT_FILE" "- linked_evidence_mismatch: 1"
assert_contains "$OUTPUT_FILE" "| cross_summary | $REPORTS_REL/wave_b_cross_platform_summary_${RUN_ID}.md | YES | $RUN_ID | YES | ok; linked linux_examples_json mismatch; linked linux_examples_selection mismatch |"
assert_contains "$OUTPUT_FILE" "| linux_examples_json | $REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json | YES | $RUN_ID | YES | ok |"

echo "[PASS] wave-b evidence consistency cross-summary linked examples contract passed"
