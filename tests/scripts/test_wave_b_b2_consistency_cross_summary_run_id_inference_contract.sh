#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_run_id_inference_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_runid_truth"
CUSTOM_LINUX_SUMMARY_REL="$WORK_REL/custom_linux_summary.md"
CUSTOM_LINUX_SUMMARY_ABS="$ROOT_DIR/$CUSTOM_LINUX_SUMMARY_REL"
CUSTOM_EXAMPLES_REL="$WORK_REL/custom_examples.json"
CUSTOM_EXAMPLES_ABS="$ROOT_DIR/$CUSTOM_EXAMPLES_REL"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$CUSTOM_LINUX_SUMMARY_ABS" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

cat > "$CUSTOM_EXAMPLES_ABS" <<'EOF'
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

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$CUSTOM_LINUX_SUMMARY_REL" \
  --linux-examples "$CUSTOM_EXAMPLES_REL" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$CUSTOM_LINUX_SUMMARY_REL" \
  --output "$WORK_REL/closure.md" >/dev/null

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure.md" \
  --strict \
  --output "$OUTPUT_REL" >/dev/null 2>&1
exit_code=$?
set -e

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected consistency report to be generated"
fi

if [[ "$exit_code" -ne 0 ]]; then
  fail "consistency should infer the existing run_id from cross-summary-declared active linux summary instead of failing strict mode"
fi

if ! rg -n "^- run_id: $RUN_ID$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should inherit the real run_id instead of minting a fresh timestamp"
fi

if ! rg -n "^- consistency_status: \\*\\*CONSISTENT\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should stay CONSISTENT when cross summary, closure report, and active linux evidence already belong to the same batch"
fi

if ! rg -n "^\\| linux_summary \\| $CUSTOM_LINUX_SUMMARY_REL \\| YES \\| $RUN_ID \\| YES \\| ok \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should reuse the active custom linux summary path and match its run_id"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/cross_summary.md \\| YES \\| $RUN_ID \\| YES \\| ok \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary row should match the inferred run_id"
fi

if ! rg -n "^\\| closure_report \\| $WORK_REL/closure.md \\| YES \\| $RUN_ID \\| YES \\| ok \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "closure report row should match the inferred run_id"
fi

echo "[PASS] wave-b-b2 consistency cross summary run_id inference contract passed"
