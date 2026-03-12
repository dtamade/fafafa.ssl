#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_continuous_monitor_output_governance_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
SCRIPT_SRC="$ROOT_DIR/scripts/continuous_test_monitor.sh"
SCRIPT_DIR="$WORK_DIR/scripts"
SCRIPT_COPY="$SCRIPT_DIR/continuous_test_monitor.sh"
FAKE_RUNNER="$SCRIPT_DIR/run_all_module_tests.sh"
REPORTS_DIR="$WORK_DIR/reports"
CAPTURE_FILE="$WORK_DIR/runner_env.txt"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] continuous test monitor output governance runtime contract"

mkdir -p "$SCRIPT_DIR"
cp "$SCRIPT_SRC" "$SCRIPT_COPY"
chmod +x "$SCRIPT_COPY"

cat > "$FAKE_RUNNER" <<EOF_RUNNER
#!/usr/bin/env bash
set -euo pipefail
{
  echo "unit=\$FAFAFA_FPC_UNIT_OUTPUT_DIR"
  echo "bin=\$FAFAFA_TEST_BIN_DIR"
  echo "args=\$*"
} > "$CAPTURE_FILE"
echo "总测试数: 3"
echo "通过: 3"
echo "失败: 0"
echo "跳过: 0"
EOF_RUNNER
chmod +x "$FAKE_RUNNER"

OUT="$(
  cd "$WORK_DIR"
  FAFAFA_CONTINUOUS_MONITOR_REPORTS_DIR="$REPORTS_DIR" \
    bash "$SCRIPT_COPY" --max-runs 1 --interval 0 --modules PKCS7 2>&1
)"

[[ -f "$REPORTS_DIR/monitor/test_history.csv" ]] || fail "history file should be written under monitor dir"
[[ -f "$REPORTS_DIR/monitor/monitor_summary.txt" ]] || fail "summary file should be written under monitor dir"
[[ -f "$REPORTS_DIR/monitor/trend_report.txt" ]] || fail "trend report should be written under monitor dir"
[[ -f "$CAPTURE_FILE" ]] || fail "fake runner should capture passthrough env"

if ! rg -F --quiet -- "unit=$REPORTS_DIR/runs/continuous_monitor_units_" "$CAPTURE_FILE"; then
  cat "$CAPTURE_FILE"
  fail "unit output dir should be scoped under reports/runs"
fi

if ! rg -F --quiet -- "bin=$REPORTS_DIR/runs/continuous_monitor_bin_" "$CAPTURE_FILE"; then
  cat "$CAPTURE_FILE"
  fail "bin output dir should be scoped under reports/runs"
fi

if ! rg -F --quiet -- "args=--modules PKCS7" "$CAPTURE_FILE"; then
  cat "$CAPTURE_FILE"
  fail "modules arg should be passed through to runner"
fi

if ! rg -F --quiet -- ",1,3,3,0,0,100.0" "$REPORTS_DIR/monitor/test_history.csv"; then
  cat "$REPORTS_DIR/monitor/test_history.csv"
  fail "history csv should record the single synthetic run"
fi

if ! rg -F --quiet -- "总运行次数: 1" "$REPORTS_DIR/monitor/trend_report.txt"; then
  cat "$REPORTS_DIR/monitor/trend_report.txt"
  fail "trend report should summarize the single run"
fi

if ! rg -F --quiet -- "监控完成。总运行次数: 1" "$REPORTS_DIR/monitor/monitor_summary.txt"; then
  cat "$REPORTS_DIR/monitor/monitor_summary.txt"
  fail "summary file should record final run count"
fi

if [[ "$OUT" != *"持续测试监控"* ]]; then
  printf '%s\n' "$OUT"
  fail "runtime output should include monitor banner"
fi

echo "[PASS] continuous test monitor output governance runtime contract passed"
