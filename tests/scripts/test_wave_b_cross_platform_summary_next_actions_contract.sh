#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_cross_platform_summary_next_actions_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
LINUX_FAIL_RUN_ID="cross_summary_linux_fail"
CLOSED_RUN_ID="cross_summary_closed"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

write_examples_json() {
  local target="$1"
  cat > "$target" <<'EOF'
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
}

write_linux_summary() {
  local target="$1"
  local run_id="$2"
  local overall="$3"
  cat > "$target" <<EOF
# Wave B CI Gate Summary

- run_id: $run_id
- Overall Status: $overall

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **$overall** | fixture |
| run_all_module_tests | **$overall** | fixture |
| verify_examples_compile | **$overall** | fixture |
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

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **$overall** | fixture |
| modules | **$overall** | fixture |
| examples | **$overall** | fixture |
EOF
}

LINUX_FAIL_DIR="$WORK_DIR/linux_fail"
mkdir -p "$LINUX_FAIL_DIR"
write_linux_summary "$LINUX_FAIL_DIR/linux_summary.md" "$LINUX_FAIL_RUN_ID" "FAIL"
write_examples_json "$LINUX_FAIL_DIR/examples.json"
write_platform_summary "$LINUX_FAIL_DIR/macos_summary.md" "Wave B macOS Gate Summary" "$LINUX_FAIL_RUN_ID" "PASS"
write_platform_summary "$LINUX_FAIL_DIR/windows_summary.md" "Wave B Windows Gate Summary" "$LINUX_FAIL_RUN_ID" "PASS"

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$LINUX_FAIL_RUN_ID" \
  --linux-summary "$WORK_REL/linux_fail/linux_summary.md" \
  --linux-examples "$WORK_REL/linux_fail/examples.json" \
  --macos-summary "$WORK_REL/linux_fail/macos_summary.md" \
  --windows-summary "$WORK_REL/linux_fail/windows_summary.md" \
  --output "$WORK_REL/linux_fail/cross_summary.md" >/dev/null

LINUX_FAIL_REPORT="$LINUX_FAIL_DIR/cross_summary.md"
if [[ ! -f "$LINUX_FAIL_REPORT" ]]; then
  fail "expected linux-fail cross summary report"
fi

if ! rg -n "Linux.*FAIL" "$LINUX_FAIL_REPORT" >/dev/null; then
  fail "cross summary next actions should explicitly mention Linux FAIL when Linux baseline is the blocking platform"
fi

if ! rg -n "prepare_wave_b_b2_handoff_bundle\\.sh" "$LINUX_FAIL_REPORT" >/dev/null; then
  fail "cross summary next actions should point operators back to the prepare handoff entrypoint"
fi

if rg -n "重新运行本脚本" "$LINUX_FAIL_REPORT" >/dev/null; then
  fail "cross summary next actions should no longer direct operators to rerun only the lower-level summary script"
fi

CLOSED_DIR="$WORK_DIR/closed"
mkdir -p "$CLOSED_DIR"
write_linux_summary "$CLOSED_DIR/linux_summary.md" "$CLOSED_RUN_ID" "PASS"
write_examples_json "$CLOSED_DIR/examples.json"
write_platform_summary "$CLOSED_DIR/macos_summary.md" "Wave B macOS Gate Summary" "$CLOSED_RUN_ID" "PASS"
write_platform_summary "$CLOSED_DIR/windows_summary.md" "Wave B Windows Gate Summary" "$CLOSED_RUN_ID" "PASS"

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$CLOSED_RUN_ID" \
  --linux-summary "$WORK_REL/closed/linux_summary.md" \
  --linux-examples "$WORK_REL/closed/examples.json" \
  --macos-summary "$WORK_REL/closed/macos_summary.md" \
  --windows-summary "$WORK_REL/closed/windows_summary.md" \
  --output "$WORK_REL/closed/cross_summary.md" >/dev/null

CLOSED_REPORT="$CLOSED_DIR/cross_summary.md"
if [[ ! -f "$CLOSED_REPORT" ]]; then
  fail "expected closed cross summary report"
fi

if ! rg -n "当前三平台 cross-platform evidence 已对齐" "$CLOSED_REPORT" >/dev/null; then
  fail "closed cross summary should acknowledge the aligned three-platform state"
fi

if rg -n "macOS runner|Windows runner" "$CLOSED_REPORT" >/dev/null; then
  fail "closed cross summary should not keep telling operators to rerun platform lanes that are already PASS"
fi

echo "[PASS] wave-b cross-platform summary next actions contract passed"
