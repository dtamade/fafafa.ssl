#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_closure_windows_runtime_fail_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
WINDOWS_DIR="$WORK_DIR/windows_evidence"
OUTPUT_DIR="$WORK_DIR/out"
RUN_ID="handoff_closure_windows_runtime_fail"
REPORT_ABS="$OUTPUT_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"
CLOSURE_ABS="$OUTPUT_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"
CROSS_ABS="$OUTPUT_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"

mkdir -p "$WINDOWS_DIR" "$OUTPUT_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
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

cat > "$WORK_DIR/macos_summary.md" <<EOF
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$WINDOWS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$WINDOWS_DIR/winssl_quick_smoke_${RUN_ID}.log" <<EOF
[quick-smoke] run_id=$RUN_ID
EOF

cat > "$WINDOWS_DIR/winssl_runtime_suite_${RUN_ID}.log" <<'EOF'
[WINSSL-RUNTIME] suite_start total=8
[WINSSL-RUNTIME] suite_summary passed=7 failed=1 total=8 success_rate=87.5
[WINSSL-RUNTIME] suite_end status=FAIL phase=runtime
EOF

bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --windows-summary "$WORK_REL/windows_evidence/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --output-dir "$WORK_REL/out" >/dev/null

if [[ ! -f "$CLOSURE_ABS" ]]; then
  fail "expected closure readiness report"
fi

if [[ ! -f "$REPORT_ABS" ]]; then
  fail "expected handoff report"
fi

if [[ ! -f "$CROSS_ABS" ]]; then
  fail "expected cross summary report"
fi

if ! rg -n "^\\| windows \\| FAIL \\| summary parsed; runtime_transcript: $WORK_REL/windows_evidence/winssl_runtime_suite_${RUN_ID}\\.log \\(suite_end_status=FAIL\\) \\| $WORK_REL/windows_evidence/wave_b_windows_gate_summary_${RUN_ID}\\.md \\|$" "$CLOSURE_ABS" >/dev/null; then
  fail "handoff bundle should generate a closure report that inherits the sibling Windows runtime FAIL truth"
fi

if ! rg -n "^\\| windows \\| FAIL \\| summary: $WORK_REL/windows_evidence/wave_b_windows_gate_summary_${RUN_ID}\\.md \\(overall=PASS\\); runtime_transcript: $WORK_REL/windows_evidence/winssl_runtime_suite_${RUN_ID}\\.log \\(suite_end_status=FAIL\\) \\|$" "$CROSS_ABS" >/dev/null; then
  fail "cross summary should continue to promote Windows to FAIL for the same runtime transcript"
fi

if ! rg -n "^- handoff_state: \\*\\*NEEDS_GATE_REPAIR\\*\\*$" "$REPORT_ABS" >/dev/null; then
  fail "handoff bundle should remain in NEEDS_GATE_REPAIR when Windows runtime transcript ends in FAIL"
fi

echo "[PASS] prepare_wave_b_b2 handoff-bundle closure Windows runtime fail contract passed"
