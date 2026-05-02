#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
SCRIPT_FILE="$PROJECT_ROOT/scripts/run_wave_c_local_guard_ops_pack.sh"

assert_contains() {
  local pattern="$1"
  if ! rg -F --quiet -- "$pattern" "$SCRIPT_FILE"; then
    echo "[FAIL] missing expected pattern in run_wave_c_local_guard_ops_pack.sh: $pattern"
    sed -n '1,220p' "$SCRIPT_FILE" || true
    exit 1
  fi
}

assert_contains 'OUTPUT_FILE="tmp/test-reports/wave_c_b144_local_guard_ops_pack_${RUN_ID}.md"'
assert_contains 'OPS_DIR="tmp/test-reports"'
assert_contains 'b140_report="$OPS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.md"'
assert_contains 'b142_json="$OPS_DIR/wave_c_b142_local_guard_status_${RUN_ID}.json"'
assert_contains 'b143_report="$OPS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.md"'
assert_contains 'b140_log="$OPS_DIR/wave_c_b140_local_guard_consistency_${RUN_ID}.b144.log"'
assert_contains 'b143_log="$OPS_DIR/wave_c_b143_alert_thresholds_${RUN_ID}.b144.log"'

echo "[PASS] run_wave_c_local_guard_ops_pack tmp structure contract passed"
