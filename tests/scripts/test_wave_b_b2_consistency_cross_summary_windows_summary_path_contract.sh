#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_windows_summary_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_windows_summary"
WINDOWS_SUMMARY_REL="$WORK_REL/windows_evidence/wave_b_windows_gate_summary_${RUN_ID}.md"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

mkdir -p "$WORK_DIR" "$ROOT_DIR/$WORK_REL/windows_evidence"
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

cat > "$ROOT_DIR/$WINDOWS_SUMMARY_REL" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WINDOWS_SUMMARY_REL" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --windows-summary "$WINDOWS_SUMMARY_REL" \
  --output "$WORK_REL/closure.md" >/dev/null

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
  fail "consistency should fail strict mode when cross summary already declares an active custom windows summary but its runtime artifacts are missing"
fi

if ! rg -n "^\\| windows_summary \\| $WINDOWS_SUMMARY_REL \\| YES \\| $RUN_ID \\| YES \\| ok \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should inherit the active custom windows summary path from cross summary"
fi

if ! rg -n "^\\| windows_quick_log \\| $WORK_REL/windows_evidence/winssl_quick_smoke_${RUN_ID}\\.log \\| NO \\| n/a \\| n/a \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should require the sibling windows quick log for the active custom windows summary"
fi

if ! rg -n "^\\| windows_runtime_transcript \\| $WORK_REL/windows_evidence/winssl_runtime_suite_${RUN_ID}\\.log \\| NO \\| n/a \\| n/a \\| missing \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should require the sibling windows runtime transcript for the active custom windows summary"
fi

if ! rg -n "consistency_status: \\*\\*INCONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when active custom Windows evidence lacks runtime artifacts"
fi

echo "[PASS] wave-b-b2 consistency cross summary windows summary path contract passed"
