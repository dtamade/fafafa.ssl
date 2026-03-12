#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TLS13_WORKFLOW="$ROOT_DIR/.github/workflows/tls13-signer-gate.yml"
WAVE_B_WORKFLOW="$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml.disabled"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] missing pattern '$pattern' in $file"
    sed -n '1,320p' "$file" || true
    fail "expected pattern not found"
  fi
}

assert_not_contains() {
  local file="$1"
  local pattern="$2"
  if rg -F --quiet -- "$pattern" "$file"; then
    echo "[INFO] unexpected pattern '$pattern' in $file"
    sed -n '1,320p' "$file" || true
    fail "unexpected pattern found"
  fi
}

echo "[TEST] repo hygiene workflow wave b tls13 tmp report paths contract"

assert_contains "$TLS13_WORKFLOW" '--reports-dir tmp/tls13_signer_gate_reports'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/tls13_signer_gate_bundle_*.md'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/tls13_signer_gate_snapshot_*.md'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/tls13_signer_gate_status_*.json'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/wave_b_ci_gate_summary_tls13_signer_*.md'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/wave_b_tls13_signer_*.json'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/tls13_signer_bench_history_*.md'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/wave_b_tls13_sign_purity_*.log'
assert_contains "$TLS13_WORKFLOW" 'tmp/tls13_signer_gate_reports/wave_b_tls13_sign_bench_*.log'
assert_contains "$TLS13_WORKFLOW" 'LATEST_BUNDLE=$(ls -1t tmp/tls13_signer_gate_reports/tls13_signer_gate_bundle_*.md 2>/dev/null | head -1 || true)'
assert_contains "$TLS13_WORKFLOW" 'LATEST_SNAPSHOT=$(ls -1t tmp/tls13_signer_gate_reports/tls13_signer_gate_snapshot_*.md 2>/dev/null | head -1 || true)'
assert_contains "$TLS13_WORKFLOW" 'LATEST_STATUS=$(ls -1t tmp/tls13_signer_gate_reports/tls13_signer_gate_status_*.json 2>/dev/null | head -1 || true)'
assert_contains "$TLS13_WORKFLOW" 'LATEST_BENCH_JSON=$(ls -1t tmp/tls13_signer_gate_reports/wave_b_tls13_signer_*.json 2>/dev/null | head -1 || true)'
assert_not_contains "$TLS13_WORKFLOW" 'test-reports/'

assert_contains "$WAVE_B_WORKFLOW" '--examples-report "tmp/wave_b_reports/examples_compile_ci_gate_${RUN_ID}.json"'
assert_contains "$WAVE_B_WORKFLOW" '--summary-out "tmp/wave_b_reports/wave_b_ci_gate_summary_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_ci_gate_summary_${{ needs.setup.outputs.run_id }}.md'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/examples_compile_ci_gate_${{ needs.setup.outputs.run_id }}.json'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_compile_*.log'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_modules_*.log'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_examples_*.log'
assert_contains "$WAVE_B_WORKFLOW" '--output-dir tmp/wave_b_reports'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_macos_gate_summary_${{ needs.setup.outputs.run_id }}.md'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_macos_*.log'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/examples_compile_gate_macos_${{ needs.setup.outputs.run_id }}.json'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_windows_gate_summary_${{ needs.setup.outputs.run_id }}.md'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_windows_*.log'
assert_contains "$WAVE_B_WORKFLOW" 'REPORTS_DIR="tmp/wave_b_reports"'
assert_contains "$WAVE_B_WORKFLOW" 'mkdir -p "$REPORTS_DIR"'
assert_contains "$WAVE_B_WORKFLOW" 'cp -f artifacts/linux/* "$REPORTS_DIR"/ 2>/dev/null || true'
assert_contains "$WAVE_B_WORKFLOW" 'cp -f artifacts/macos/* "$REPORTS_DIR"/ 2>/dev/null || true'
assert_contains "$WAVE_B_WORKFLOW" 'cp -f artifacts/windows/* "$REPORTS_DIR"/ 2>/dev/null || true'
assert_contains "$WAVE_B_WORKFLOW" 'LINUX_SUMMARY="$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" 'LINUX_EXAMPLES="$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json"'
assert_contains "$WAVE_B_WORKFLOW" 'if [[ -f "$REPORTS_DIR/wave_b_macos_gate_summary_${RUN_ID}.md" ]]; then'
assert_contains "$WAVE_B_WORKFLOW" 'MACOS_ARGS=(--macos-summary "$REPORTS_DIR/wave_b_macos_gate_summary_${RUN_ID}.md")'
assert_contains "$WAVE_B_WORKFLOW" 'if [[ -f "$REPORTS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md" ]]; then'
assert_contains "$WAVE_B_WORKFLOW" 'WINDOWS_ARGS=(--windows-summary "$REPORTS_DIR/wave_b_windows_gate_summary_${RUN_ID}.md")'
assert_contains "$WAVE_B_WORKFLOW" '--output "$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" '--output "$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" '--cross-summary "$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" '--closure-report "$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" '--output "$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_cross_platform_summary_${{ needs.setup.outputs.run_id }}.md'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_b2_closure_readiness_${{ needs.setup.outputs.run_id }}.md'
assert_contains "$WAVE_B_WORKFLOW" 'tmp/wave_b_reports/wave_b_b2_evidence_consistency_${{ needs.setup.outputs.run_id }}.md'
assert_not_contains "$WAVE_B_WORKFLOW" 'test-reports/'

echo "[PASS] repo hygiene workflow wave b tls13 tmp report paths contract passed"
