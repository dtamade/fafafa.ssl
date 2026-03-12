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
    sed -n '1,240p' "$file" || true
    fail "expected pattern not found"
  fi
}

echo "[TEST] repo hygiene wave c quick enablement tmp defaults contract"

BUNDLE="$ROOT_DIR/scripts/run_wave_c_quick_sprint_bundle.sh"
B107="$ROOT_DIR/scripts/evaluate_wave_c_b101_thresholds.sh"
B108="$ROOT_DIR/scripts/check_wave_c_default_on_readiness.sh"
B109="$ROOT_DIR/scripts/prepare_wave_c_b109_canary_rollout.sh"
B110="$ROOT_DIR/scripts/run_wave_c_b110_rollback_drill.sh"
B115="$ROOT_DIR/scripts/check_wave_c_workflow_enable_prereq.sh"
B116="$ROOT_DIR/scripts/prepare_wave_c_b116_enablement_packet.sh"
B119="$ROOT_DIR/scripts/check_wave_c_first_run_preflight.sh"
B120="$ROOT_DIR/scripts/check_wave_c_post_trigger_observability.sh"

assert_contains "$BUNDLE" 'DEFAULT_REPORTS_DIR="tmp/wave_c_quick_sprint_reports"'
assert_contains "$BUNDLE" 'REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-$DEFAULT_REPORTS_DIR}"'
assert_contains "$BUNDLE" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_quick_sprint_bundle_${RUN_ID}.md"'

assert_contains "$B107" 'REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"'
assert_contains "$B107" 'VALIDATION_GLOB="${FAFAFA_WAVE_C_B101_VALIDATION_GLOB:-tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md}"'
assert_contains "$B107" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b107_threshold_eval_${RUN_ID}.md"'

assert_contains "$B108" 'REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"'
assert_contains "$B108" 'VALIDATION_GLOB="${FAFAFA_WAVE_C_B101_VALIDATION_GLOB:-tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md}"'
assert_contains "$B108" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b108_default_on_readiness_${RUN_ID}.md"'

assert_contains "$B109" 'REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"'
assert_contains "$B109" 'VALIDATION_GLOB="${FAFAFA_WAVE_C_B101_VALIDATION_GLOB:-tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md}"'
assert_contains "$B109" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b109_canary_rollout_${RUN_ID}.md"'

assert_contains "$B110" 'REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"'
assert_contains "$B110" 'VALIDATION_GLOB="${FAFAFA_WAVE_C_B101_VALIDATION_GLOB:-tmp/wave_c_b101_reports_*/wave_c_b101_validation_*.md}"'
assert_contains "$B110" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b110_rollback_drill_${RUN_ID}.md"'

assert_contains "$B115" 'REPORTS_DIR="${FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR:-tmp/wave_c_enablement_reports}"'
assert_contains "$B115" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b115_workflow_enable_prereq_${RUN_ID}.md"'

assert_contains "$B116" 'REPORTS_DIR="${FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR:-tmp/wave_c_enablement_reports}"'
assert_contains "$B116" 'PREREQ_REPORT="$(ls -1t "$REPORTS_DIR"/wave_c_b115_workflow_enable_prereq_*.md 2>/dev/null | head -1 || true)"'
assert_contains "$B116" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b116_enablement_request_packet_${RUN_ID}.md"'

assert_contains "$B119" 'REPORTS_DIR="${FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR:-tmp/wave_c_enablement_reports}"'
assert_contains "$B119" 'QUICK_SPRINT_REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"'
assert_contains "$B119" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b119_first_run_preflight_${RUN_ID}.md"'
assert_contains "$B119" 'latest_bundle="$(ls -1t "$QUICK_SPRINT_REPORTS_DIR"/wave_c_quick_sprint_bundle_*.md 2>/dev/null | head -1 || true)"'

assert_contains "$B120" 'REPORTS_DIR="${FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR:-tmp/wave_c_quick_sprint_reports}"'
assert_contains "$B120" 'OUTPUT_FILE="$REPORTS_DIR/wave_c_b120_post_trigger_observability_${RUN_ID}.md"'

echo "[PASS] repo hygiene wave c quick enablement tmp defaults contract passed"
