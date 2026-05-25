#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_windows_runtime_fail_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
WINDOWS_DIR="$WORK_DIR/windows-evidence"
OUTPUT_DIR="$WORK_DIR/handoff-out"
RUN_ID="handoff_windows_runtime_fail"
HANDOFF_REPORT="$OUTPUT_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"
SUMMARY_REPORT="$OUTPUT_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"

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

cat > "$WINDOWS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | ok |
| modules | **PASS** | ok |
| examples | **PASS** | ok |
EOF

cat > "$WINDOWS_DIR/winssl_quick_smoke_${RUN_ID}.log" <<EOF
[quick-smoke] run_id=$RUN_ID
EOF

cat > "$WINDOWS_DIR/winssl_runtime_suite_${RUN_ID}.log" <<'EOF'
[WINSSL-RUNTIME] suite_start total=8
[WINSSL-RUNTIME] suite_summary passed=7 failed=1 total=8 success_rate=87.5
[WINSSL-RUNTIME] suite_end status=FAIL phase=runtime
EOF

ABS_WINDOWS_SUMMARY="$WINDOWS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md"

(cd /tmp && bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$ABS_WINDOWS_SUMMARY" \
  --output-dir "$OUTPUT_DIR" >/dev/null)

if [[ ! -f "$SUMMARY_REPORT" ]]; then
  fail "expected cross summary report"
fi

if [[ ! -f "$HANDOFF_REPORT" ]]; then
  fail "expected handoff bundle report"
fi

if ! rg -n "^\\| windows \\| FAIL \\|" "$SUMMARY_REPORT" >/dev/null; then
  fail "prepare handoff bundle should propagate Windows runtime FAIL into the generated cross summary"
fi

if ! rg -n "handoff_state: \\*\\*NEEDS_GATE_REPAIR\\*\\*" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff bundle should not remain CLOSED when the sibling Windows runtime transcript ends in FAIL"
fi

if rg -n "handoff_state: \\*\\*CLOSED\\*\\*" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff bundle must not report CLOSED for an opt-in Windows runtime failure"
fi

if ! rg -n "broader runtime suite 已返回 FAIL|runtime transcript 已明确 FAIL|WinSSL broader runtime suite 已失败|在 Windows runner 执行 live gate 并回填 Windows summary" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff bundle next actions should explicitly surface the Windows broader runtime failure"
fi

echo "[PASS] prepare_wave_b_b2 handoff bundle Windows runtime fail contract passed"
