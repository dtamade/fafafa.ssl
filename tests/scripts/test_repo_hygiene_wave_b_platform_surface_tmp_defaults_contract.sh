#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,260p' "$file" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] repo hygiene wave b platform surface tmp defaults contract"

MACOS="$ROOT_DIR/scripts/run_wave_b_macos_gate.sh"
WINDOWS="$ROOT_DIR/scripts/run_wave_b_windows_gate.ps1"
ARCHIVE="$ROOT_DIR/scripts/archive_ci_artifacts_draft.sh"
MONITOR="$ROOT_DIR/scripts/continuous_test_monitor.sh"
TLS13_CI="$ROOT_DIR/scripts/run_tls13_signer_gate_ci.sh"
TLS13_BUNDLE="$ROOT_DIR/scripts/run_tls13_signer_gate_bundle.sh"

assert_contains "$MACOS" 'OUTPUT_DIR_REL="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$MACOS" '--output-dir DIR           输出目录（相对项目根，默认: tmp/wave_b_reports）'
assert_contains "$MACOS" 'SUMMARY_REL="$OUTPUT_DIR_REL/wave_b_macos_gate_summary_${RUN_ID}.md"'

assert_contains "$WINDOWS" '[string]$OutputDir = ""'
assert_contains "$WINDOWS" 'if ([string]::IsNullOrWhiteSpace($OutputDir)) {'
assert_contains "$WINDOWS" '$OutputDir = $env:FAFAFA_WAVE_B_REPORTS_DIR'
assert_contains "$WINDOWS" '$OutputDir = "tmp/wave_b_reports"'
assert_contains "$WINDOWS" '$SummaryFile = Join-Path $OutDirAbs "wave_b_windows_gate_summary_${RunId}.md"'

assert_contains "$ARCHIVE" 'WAVE_B_REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$ARCHIVE" 'TLS13_SIGNER_GATE_REPORTS_DIR="${FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR:-tmp/tls13_signer_gate_reports}"'
assert_contains "$ARCHIVE" 'LEGACY_REPORTS_DIR="${FAFAFA_ARCHIVE_LEGACY_REPORTS_DIR:-test-reports}"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_ci_gate_summary_*.md"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_macos_gate_summary_*.md"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_windows_gate_summary_*.md"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/winssl_blocker_batch_*.md"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_compile_*.log"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_modules_*.log"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_examples_*.log"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_macos_*.log"'
assert_contains "$ARCHIVE" '"$WAVE_B_REPORTS_DIR/wave_b_windows_*.log"'
assert_contains "$ARCHIVE" '"$TLS13_SIGNER_GATE_REPORTS_DIR/tls13_signer_gate_bundle_*.md"'
assert_contains "$ARCHIVE" '"$TLS13_SIGNER_GATE_REPORTS_DIR/tls13_signer_gate_snapshot_*.md"'
assert_contains "$ARCHIVE" '"$TLS13_SIGNER_GATE_REPORTS_DIR/tls13_signer_gate_status_*.json"'
assert_contains "$ARCHIVE" '"$TLS13_SIGNER_GATE_REPORTS_DIR/wave_b_tls13_sign_bench_*.log"'
assert_contains "$ARCHIVE" '"$TLS13_SIGNER_GATE_REPORTS_DIR/wave_b_tls13_signer_*.json"'
assert_contains "$ARCHIVE" '"$TLS13_SIGNER_GATE_REPORTS_DIR/tls13_signer_bench_history_*.md"'

assert_contains "$TLS13_CI" 'FAFAFA_WAVE_B_REPORTS_DIR="$OUTPUT_DIR_REL"'
assert_contains "$TLS13_CI" 'FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR="$OUTPUT_DIR_REL"'
assert_contains "$TLS13_BUNDLE" '"FAFAFA_WAVE_B_REPORTS_DIR=$REPORTS_DIR"'
assert_contains "$TLS13_BUNDLE" '"FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR=$REPORTS_DIR"'

assert_contains "$MONITOR" 'REPORTS_DIR="${FAFAFA_CONTINUOUS_MONITOR_REPORTS_DIR:-$PROJECT_ROOT/tmp/continuous_test_monitor_reports}"'
assert_contains "$MONITOR" 'MONITOR_DIR="$REPORTS_DIR/monitor"'
assert_contains "$MONITOR" 'HISTORY_FILE="$MONITOR_DIR/test_history.csv"'
assert_contains "$MONITOR" 'SUMMARY_FILE="$MONITOR_DIR/monitor_summary.txt"'
assert_contains "$MONITOR" 'TREND_FILE="$MONITOR_DIR/trend_report.txt"'
assert_contains "$MONITOR" 'RUNS_DIR="$REPORTS_DIR/runs"'

echo "[PASS] repo hygiene wave b platform surface tmp defaults contract passed"
