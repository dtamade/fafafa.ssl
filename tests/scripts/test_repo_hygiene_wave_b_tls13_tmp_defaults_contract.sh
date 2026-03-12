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

echo "[TEST] repo hygiene wave b tls13 tmp defaults contract"

WAVE_B_GATE="$ROOT_DIR/scripts/run_wave_b_ci_gate.sh"
WINSSL_BLOCKER="$ROOT_DIR/scripts/run_windows_winssl_blocker_batch_draft.sh"
CROSS_SUMMARY="$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh"
CLOSURE="$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh"
CONSISTENCY="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
HANDOFF="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
TLS13_CI="$ROOT_DIR/scripts/run_tls13_signer_gate_ci.sh"
TLS13_SNAPSHOT="$ROOT_DIR/scripts/generate_tls13_signer_gate_snapshot.sh"
TLS13_STATUS="$ROOT_DIR/scripts/export_tls13_signer_gate_status_json.sh"
TLS13_BUNDLE="$ROOT_DIR/scripts/run_tls13_signer_gate_bundle.sh"
TLS13_HISTORY="$ROOT_DIR/scripts/summarize_tls13_signer_bench_history.sh"

assert_contains "$WAVE_B_GATE" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$WAVE_B_GATE" 'EXAMPLES_REPORT_REL="${FAFAFA_WAVE_B_EXAMPLES_REPORT_REL:-$REPORTS_DIR/examples_compile_ci_gate.json}"'
assert_contains "$WAVE_B_GATE" 'SUMMARY_OUT_REL="${FAFAFA_WAVE_B_SUMMARY_OUT_REL:-$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md}"'

assert_contains "$WINSSL_BLOCKER" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$WINSSL_BLOCKER" 'OUTPUT_FILE="$REPORTS_DIR/winssl_blocker_batch_${RUN_ID}.md"'

assert_contains "$CROSS_SUMMARY" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$CROSS_SUMMARY" 'OUTPUT_FILE="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"'

assert_contains "$CLOSURE" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$CLOSURE" 'OUTPUT_FILE="$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"'

assert_contains "$CONSISTENCY" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$CONSISTENCY" 'CROSS_SUMMARY="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"'
assert_contains "$CONSISTENCY" 'CLOSURE_REPORT="$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"'
assert_contains "$CONSISTENCY" 'OUTPUT_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"'

assert_contains "$HANDOFF" 'REPORTS_DIR="${FAFAFA_WAVE_B_REPORTS_DIR:-tmp/wave_b_reports}"'
assert_contains "$HANDOFF" 'OUTPUT_DIR="$REPORTS_DIR"'
assert_contains "$HANDOFF" 'CROSS_SUMMARY="${OUTPUT_DIR}/wave_b_cross_platform_summary_${RUN_ID}.md"'
assert_contains "$HANDOFF" 'CLOSURE_REPORT="${OUTPUT_DIR}/wave_b_b2_closure_readiness_${RUN_ID}.md"'
assert_contains "$HANDOFF" 'CONSISTENCY_REPORT="${OUTPUT_DIR}/wave_b_b2_evidence_consistency_${RUN_ID}.md"'
assert_contains "$HANDOFF" 'BUNDLE_REPORT="${OUTPUT_DIR}/wave_b_b2_handoff_bundle_${RUN_ID}.md"'

assert_contains "$TLS13_CI" 'OUTPUT_DIR_REL="${FAFAFA_TLS13_SIGNER_GATE_OUTPUT_DIR:-tmp/tls13_signer_gate_reports}"'
assert_contains "$TLS13_CI" 'SUMMARY_REL="$OUTPUT_DIR_REL/wave_b_ci_gate_summary_tls13_signer_${RUN_ID}.md"'
assert_contains "$TLS13_CI" 'BENCH_JSON_REL="$OUTPUT_DIR_REL/wave_b_tls13_signer_${RUN_ID}.json"'
assert_contains "$TLS13_CI" 'HISTORY_MD_REL="$OUTPUT_DIR_REL/tls13_signer_bench_history_${RUN_ID}.md"'
assert_contains "$TLS13_CI" 'FAFAFA_WAVE_B_REPORTS_DIR="$OUTPUT_DIR_REL" bash scripts/run_wave_b_ci_gate.sh'

assert_contains "$TLS13_SNAPSHOT" 'REPORTS_DIR="${FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR:-tmp/tls13_signer_gate_reports}"'
assert_contains "$TLS13_SNAPSHOT" 'OUTPUT_FILE="$REPORTS_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md"'

assert_contains "$TLS13_STATUS" 'REPORTS_DIR="${FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR:-tmp/tls13_signer_gate_reports}"'
assert_contains "$TLS13_STATUS" 'OUTPUT_JSON="$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.json"'
assert_contains "$TLS13_STATUS" 'SNAPSHOT_FILE="$(ls -1t "$REPORTS_DIR"/tls13_signer_gate_snapshot_*.md 2>/dev/null | head -1 || true)"'

assert_contains "$TLS13_BUNDLE" 'DEFAULT_REPORTS_DIR="tmp/tls13_signer_gate_reports"'
assert_contains "$TLS13_BUNDLE" 'REPORTS_DIR="${FAFAFA_TLS13_SIGNER_GATE_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'
assert_contains "$TLS13_BUNDLE" 'OUTPUT_FILE="$REPORTS_DIR/tls13_signer_gate_bundle_${RUN_ID}.md"'
assert_contains "$TLS13_BUNDLE" 'ci_summary="$REPORTS_DIR/wave_b_ci_gate_summary_tls13_signer_${RUN_ID}.md"'
assert_contains "$TLS13_BUNDLE" 'ci_bench_json="$REPORTS_DIR/wave_b_tls13_signer_${RUN_ID}.json"'
assert_contains "$TLS13_BUNDLE" 'ci_history="$REPORTS_DIR/tls13_signer_bench_history_${RUN_ID}.md"'
assert_contains "$TLS13_BUNDLE" 'snapshot_report="$REPORTS_DIR/tls13_signer_gate_snapshot_${RUN_ID}.md"'
assert_contains "$TLS13_BUNDLE" 'status_json="$REPORTS_DIR/tls13_signer_gate_status_${RUN_ID}.json"'

assert_contains "$TLS13_HISTORY" 'PATTERN="${FAFAFA_TLS13_SIGN_BENCH_HISTORY_GLOB:-tmp/tls13_signer_gate_reports/wave_b_tls13_sign_bench_*.log}"'

echo "[PASS] repo hygiene wave b tls13 tmp defaults contract passed"
