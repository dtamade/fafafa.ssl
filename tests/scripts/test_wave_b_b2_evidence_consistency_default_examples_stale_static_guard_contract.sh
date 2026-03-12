#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
WORK_REL="tmp/test_wave_b_b2_evidence_default_stale_guard_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wb2_examples_default_guard_$$"
OTHER_RUN_ID="wb2_examples_default_other_$$"

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

assert_not_contains() {
  local file="$1"
  local pattern="$2"
  if rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] unexpected pattern present: $pattern"
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b b2 evidence consistency default examples stale static guard contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

cat > "$REPORTS_DIR/examples_compile_ci_gate.json" <<EOF_JSON
{
  "run_id": "$OTHER_RUN_ID",
  "summary": {
    "total": 1,
    "passed": 0,
    "failed": 1,
    "skipped": 0,
    "pass_rate": "0.0%"
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

set +e
FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
  bash "$SCRIPT" --run-id "$RUN_ID" --strict >/dev/null 2>&1
EC=$?
set -e

if [[ $EC -eq 0 ]]; then
  fail "strict mode should fail when default examples falls back to stale static alias"
fi

OUTPUT_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
[[ -f "$OUTPUT_FILE" ]] || fail "evidence report should be generated"

assert_contains "$OUTPUT_FILE" "| linux_examples_json | $REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json | NO | n/a | n/a | missing |"
assert_not_contains "$OUTPUT_FILE" "| linux_examples_json | $REPORTS_REL/examples_compile_ci_gate.json |"
assert_contains "$OUTPUT_FILE" "- consistency_status: **INCONSISTENT**"

echo "[PASS] wave-b b2 evidence consistency default examples stale static guard contract passed"
