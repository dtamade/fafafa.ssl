#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_cross_platform_summary_windows_runtime_fail_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="cross_summary_windows_runtime_fail"
OUTPUT_ABS="$WORK_DIR/cross_summary.md"

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

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | fixture |
| run_all_module_tests | **PASS** | fixture |
| verify_examples_compile | **PASS** | fixture |
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

cat > "$WORK_DIR/windows_summary.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | fixture |
| modules | **PASS** | fixture |
| examples | **PASS** | fixture |
EOF

cat > "$WORK_DIR/winssl_runtime_suite_${RUN_ID}.log" <<'EOF'
[WINSSL-RUNTIME] suite_start total=8
[WINSSL-RUNTIME] suite_summary passed=7 failed=1 total=8 success_rate=87.5
[WINSSL-RUNTIME] suite_end status=FAIL phase=runtime
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$WORK_REL/windows_summary.md" \
  --windows-runtime-transcript "$WORK_REL/winssl_runtime_suite_${RUN_ID}.log" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected cross summary output"
fi

if ! rg -n "^\\| windows \\| FAIL \\| summary: $WORK_REL/windows_summary\\.md \\(overall=PASS\\); runtime_transcript: $WORK_REL/winssl_runtime_suite_${RUN_ID}\\.log \\(suite_end_status=FAIL\\) \\|$" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary should promote windows state to FAIL when the explicit runtime transcript ends in FAIL"
fi

if ! rg -n "Windows 为 READY/FAIL/DRY_RUN/PENDING" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary next actions should reopen the Windows repair lane when runtime transcript promotes windows to FAIL"
fi

if rg -n "当前三平台 platform summary 状态已对齐" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary should not claim aligned platform summaries when the Windows runtime transcript failed"
fi

echo "[PASS] wave-b cross-platform summary Windows runtime fail contract passed"
