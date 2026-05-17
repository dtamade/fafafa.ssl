#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_closure_next_actions_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
IN_PROGRESS_RUN_ID="closure_next_actions_truth"
CLOSED_RUN_ID="closure_next_actions_closed"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

write_linux_summary() {
  local target="$1"
  local run_id="$2"
  local overall="$3"
  cat > "$target" <<EOF
# Wave B CI Gate Summary

- run_id: $run_id
- Overall Status: $overall
EOF
}

write_platform_summary() {
  local target="$1"
  local title="$2"
  local run_id="$3"
  local overall="$4"
  cat > "$target" <<EOF
# $title

- run_id: $run_id
- overall: $overall
EOF
}

IN_PROGRESS_DIR="$WORK_DIR/in_progress"
mkdir -p "$IN_PROGRESS_DIR"
write_linux_summary "$IN_PROGRESS_DIR/linux_summary.md" "$IN_PROGRESS_RUN_ID" "PASS"

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$IN_PROGRESS_RUN_ID" \
  --linux-summary "$WORK_REL/in_progress/linux_summary.md" \
  --output "$WORK_REL/in_progress/closure_report.md" >/dev/null

IN_PROGRESS_REPORT="$IN_PROGRESS_DIR/closure_report.md"
if [[ ! -f "$IN_PROGRESS_REPORT" ]]; then
  fail "expected in-progress closure readiness report to be generated"
fi

if rg -n "scripts/generate_wave_b_cross_platform_summary\\.sh" "$IN_PROGRESS_REPORT" >/dev/null; then
  fail "closure readiness report should no longer direct operators to rerun the stale cross-summary-only entrypoint"
fi

if ! rg -n "prepare_wave_b_b2_handoff_bundle\\.sh" "$IN_PROGRESS_REPORT" >/dev/null; then
  fail "closure readiness report should point operators back to the current handoff-bundle prepare entrypoint"
fi

CLOSED_DIR="$WORK_DIR/closed"
mkdir -p "$CLOSED_DIR"
write_linux_summary "$CLOSED_DIR/linux_summary.md" "$CLOSED_RUN_ID" "PASS"
write_platform_summary "$CLOSED_DIR/macos_summary.md" "Wave B macOS Gate Summary" "$CLOSED_RUN_ID" "PASS"
write_platform_summary "$CLOSED_DIR/windows_summary.md" "Wave B Windows Gate Summary" "$CLOSED_RUN_ID" "PASS"

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$CLOSED_RUN_ID" \
  --linux-summary "$WORK_REL/closed/linux_summary.md" \
  --macos-summary "$WORK_REL/closed/macos_summary.md" \
  --windows-summary "$WORK_REL/closed/windows_summary.md" \
  --output "$WORK_REL/closed/closure_report.md" >/dev/null

CLOSED_REPORT="$CLOSED_DIR/closure_report.md"
if [[ ! -f "$CLOSED_REPORT" ]]; then
  fail "expected closed closure readiness report to be generated"
fi

if ! rg -n "^- closure_status: \\*\\*CLOSED\\*\\*$" "$CLOSED_REPORT" >/dev/null; then
  fail "closed platform summaries should still yield closure_status CLOSED"
fi

if ! rg -n "当前三平台 summary 状态已闭环" "$CLOSED_REPORT" >/dev/null; then
  fail "closed closure readiness should narrow the closed wording to summary-state closure"
fi

if ! rg -n "完整交接仍需结合 consistency / handoff bundle 判断" "$CLOSED_REPORT" >/dev/null; then
  fail "closed closure readiness should remind operators that full handoff closure still depends on consistency/handoff bundle"
fi

if rg -n "当前三平台 summary 已闭环" "$CLOSED_REPORT" >/dev/null; then
  fail "closed closure readiness should no longer over-claim that the whole handoff chain is already closed"
fi

if rg -n "macOS runner|Windows runner" "$CLOSED_REPORT" >/dev/null; then
  fail "closed closure readiness should not keep stale platform rerun instructions"
fi

echo "[PASS] wave-b-b2 closure next actions contract passed"
