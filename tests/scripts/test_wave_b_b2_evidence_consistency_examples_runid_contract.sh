#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
WORK_REL="tmp/test_wave_b_b2_evidence_examples_runid_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="wb2_examples_target_$$"
OTHER_RUN_ID="wb2_examples_other_$$"
OUTPUT_REL="$WORK_REL/evidence_examples_runid.md"
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

echo "[TEST] wave-b b2 evidence consistency examples run-id contract"

mkdir -p "$WORK_DIR"

cat > "$WORK_DIR/linux_summary.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

cat > "$WORK_DIR/examples.json" <<EOF_JSON
{
  "run_id": "$OTHER_RUN_ID",
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

cat > "$WORK_DIR/cross_summary.md" <<EOF_CROSS
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
EOF_CROSS

cat > "$WORK_DIR/closure_report.md" <<EOF_CLOSE
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
EOF_CLOSE

set +e
OUT="$(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure_report.md" \
  --output "$OUTPUT_REL" \
  --strict 2>&1)"
EC=$?
set -e

if [[ $EC -eq 0 ]]; then
  echo "$OUT"
  fail "strict mode should fail when linux examples json carries a mismatched run_id"
fi

[[ -f "$OUTPUT_FILE" ]] || fail "evidence report should be generated"
assert_contains "$OUTPUT_FILE" "| linux_examples_json | $WORK_REL/examples.json | YES | $OTHER_RUN_ID | NO | run_id mismatch |"
assert_contains "$OUTPUT_FILE" "- consistency_status: **INCONSISTENT**"

echo "[PASS] wave-b b2 evidence consistency examples run-id contract passed"
