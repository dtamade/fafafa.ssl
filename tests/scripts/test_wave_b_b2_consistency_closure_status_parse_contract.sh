#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_closure_status_parse_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_closure_status_parse"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 75,
    "failed": 0,
    "skipped": 0,
    "pass_rate": "100%"
  }
}
EOF

cat > "$WORK_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- linux_summary: $WORK_REL/linux_summary.md
- linux_examples_json: $WORK_REL/examples.json
EOF

cat > "$WORK_DIR/closure.md" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure.md" \
  --strict \
  --output "$OUTPUT_REL" >/dev/null 2>&1
exit_code=$?
set -e

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected consistency report to be generated"
fi

if [[ "$exit_code" -eq 0 ]]; then
  fail "strict consistency should reject a closure report that is missing closure_status metadata"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when closure_status metadata is missing"
fi

if ! rg -n "^- runid_mismatch_or_parse_issue: 1$" "$OUTPUT_ABS" >/dev/null; then
  fail "missing closure_status metadata should count as one parse issue"
fi

if ! rg -n "^- closure_status_note: closure_status missing$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should surface the missing closure_status note explicitly"
fi

if ! rg -n "^\\| closure_report \\| $WORK_REL/closure.md \\| YES \\| $RUN_ID \\| YES \\| closure_status missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "closure report row should expose the missing closure_status parse issue"
fi

echo "[PASS] wave-b-b2 consistency closure_status parse contract passed"
