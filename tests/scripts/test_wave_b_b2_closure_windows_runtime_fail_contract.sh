#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_closure_windows_runtime_fail_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="closure_windows_runtime_fail"
OUTPUT_ABS="$WORK_DIR/closure.md"

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

cat > "$WORK_DIR/macos_summary.md" <<EOF
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$WORK_DIR/windows_summary.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$WORK_DIR/winssl_runtime_suite_${RUN_ID}.log" <<'EOF'
[WINSSL-RUNTIME] suite_start total=8
[WINSSL-RUNTIME] suite_summary passed=7 failed=1 total=8 success_rate=87.5
[WINSSL-RUNTIME] suite_end status=FAIL phase=runtime
EOF

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --windows-summary "$WORK_REL/windows_summary.md" \
  --output "$WORK_REL/closure.md" >/dev/null

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected closure readiness report"
fi

if ! rg -n "^\\| windows \\| FAIL \\| summary parsed; runtime_transcript: $WORK_REL/winssl_runtime_suite_${RUN_ID}\\.log \\(suite_end_status=FAIL\\) \\| $WORK_REL/windows_summary\\.md \\|$" "$OUTPUT_ABS" >/dev/null; then
  fail "closure readiness should demote Windows to FAIL when the sibling runtime transcript ends in FAIL"
fi

if ! rg -n "^- closure_status: \\*\\*IN_PROGRESS\\*\\*$" "$OUTPUT_ABS" >/dev/null; then
  fail "closure readiness should stay IN_PROGRESS when Windows runtime truth reopens the gate"
fi

if ! rg -n "Windows 为 READY/DRY_RUN/FAIL/PENDING" "$OUTPUT_ABS" >/dev/null; then
  fail "closure next actions should reopen the Windows repair lane after runtime transcript failure"
fi

if rg -n "当前三平台 summary 状态已闭环" "$OUTPUT_ABS" >/dev/null; then
  fail "closure readiness should not claim summary closure when runtime transcript forces Windows back to FAIL"
fi

echo "[PASS] wave-b-b2 closure Windows runtime fail contract passed"
