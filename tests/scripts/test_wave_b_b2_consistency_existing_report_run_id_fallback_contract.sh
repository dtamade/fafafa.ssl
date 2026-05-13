#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_existing_report_run_id_fallback_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_existing_report_runid_fallback"
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

rm -f "$CUSTOM_LINUX_SUMMARY_ABS"

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

if [[ "$exit_code" -eq 0 ]]; then
  fail "consistency should still fail strict mode when the active custom linux summary is missing"
fi

if ! rg -n "^- run_id: $RUN_ID$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should recover the real run_id from the existing report chain instead of minting a fresh timestamp"
fi

if ! rg -n "^- required_missing: 1$" "$OUTPUT_ABS" >/dev/null; then
  fail "only the missing active linux summary should count as required missing evidence in this scenario"
fi

if ! rg -n "^- runid_mismatch_or_parse_issue: 0$" "$OUTPUT_ABS" >/dev/null; then
  fail "existing cross summary and closure report should not be polluted into extra run_id mismatches"
fi

if ! rg -n "^\\| linux_summary \\| $CUSTOM_LINUX_SUMMARY_REL \\| NO \\| n/a \\| NO \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should still flag the missing active custom linux summary"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/cross_summary.md \\| YES \\| $RUN_ID \\| YES \\| ok \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary row should keep matching the recovered run_id"
fi

if ! rg -n "^\\| closure_report \\| $WORK_REL/closure.md \\| YES \\| $RUN_ID \\| YES \\| ok \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "closure report row should keep matching the recovered run_id"
fi

echo "[PASS] wave-b-b2 consistency existing report run_id fallback contract passed"
