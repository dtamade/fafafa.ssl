#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_linux_summary_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_linux_summary"
CUSTOM_LINUX_SUMMARY_REL="$WORK_REL/custom_linux_summary.md"
CUSTOM_LINUX_SUMMARY_ABS="$ROOT_DIR/$CUSTOM_LINUX_SUMMARY_REL"
DEFAULT_LINUX_SUMMARY_REL="test-reports/wave_b_ci_gate_summary_${RUN_ID}.md"
DEFAULT_LINUX_SUMMARY_ABS="$ROOT_DIR/$DEFAULT_LINUX_SUMMARY_REL"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

mkdir -p "$WORK_DIR" "$(dirname "$DEFAULT_LINUX_SUMMARY_ABS")"
trap 'rm -rf "$WORK_DIR"; rm -f "$DEFAULT_LINUX_SUMMARY_ABS"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$CUSTOM_LINUX_SUMMARY_ABS" <<EOF
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

cat > "$DEFAULT_LINUX_SUMMARY_ABS" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$CUSTOM_LINUX_SUMMARY_REL" \
  --linux-examples "$WORK_REL/examples.json" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$CUSTOM_LINUX_SUMMARY_REL" \
  --output "$WORK_REL/closure.md" >/dev/null

rm -f "$CUSTOM_LINUX_SUMMARY_ABS"

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
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
  fail "consistency should fail strict mode when cross summary already declares an active custom linux summary but that summary is missing"
fi

if ! rg -n "^\\| linux_summary \\| $CUSTOM_LINUX_SUMMARY_REL \\| NO \\| n/a \\| NO \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should inherit the active custom linux summary path from cross summary and mark it missing"
fi

if ! rg -n "consistency_status: \\*\\*INCONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when the active custom linux summary is missing"
fi

echo "[PASS] wave-b-b2 consistency cross summary linux summary path contract passed"
