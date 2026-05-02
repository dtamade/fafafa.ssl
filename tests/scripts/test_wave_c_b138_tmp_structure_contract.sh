#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SCRIPT_FILE="$PROJECT_ROOT/scripts/run_wave_c_pre_ci_reenable_full_gate.sh"

assert_contains() {
  local pattern="$1"
  if ! rg -F --quiet -- "$pattern" "$SCRIPT_FILE"; then
    echo "[FAIL] missing expected pattern in run_wave_c_pre_ci_reenable_full_gate.sh: $pattern"
    sed -n '1,220p' "$SCRIPT_FILE" || true
    exit 1
  fi
}

assert_contains 'OUTPUT_FILE="tmp/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md"'
assert_contains 'FULL_GATE_DIR="tmp/test-reports"'
assert_contains 'oncall_report="$FULL_GATE_DIR/wave_c_b129_oncall_check_${RUN_ID}.md"'
assert_contains 'snapshot_report="$FULL_GATE_DIR/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md"'
assert_contains 'packet_report="$FULL_GATE_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}.md"'
assert_contains 'oncall_log="$FULL_GATE_DIR/wave_c_b129_oncall_check_${RUN_ID}.b138.log"'
assert_contains 'packet_log="$FULL_GATE_DIR/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}.b138.log"'

echo "[PASS] Wave C B138 tmp structure contract passed"
