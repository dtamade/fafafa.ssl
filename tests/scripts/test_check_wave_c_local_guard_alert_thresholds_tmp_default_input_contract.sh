#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="contract_guard_alerts_$(date +%s)_$$"
INPUT_JSON="$PROJECT_ROOT/tmp/test-reports/wave_c_b142_local_guard_status_${RUN_ID}.json"
OUT_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b143_alert_thresholds_${RUN_ID}_out.md"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$INPUT_JSON" "$OUT_FILE"' EXIT

cat > "$INPUT_JSON" <<EOF_JSON
{
  "run_id": "$RUN_ID",
  "generated_at": "2026-03-16 00:00:00 +0800",
  "overall_state": "HEALTHY",
  "workflow_state": "DISABLED",
  "oncall_state": "PASS",
  "snapshot_state": "GREEN",
  "full_gate_state": "PASS",
  "consistency_state": "CONSISTENT",
  "evidence": {}
}
EOF_JSON

cd "$PROJECT_ROOT"

bash scripts/check_wave_c_local_guard_alert_thresholds.sh --run-id "$RUN_ID" --strict --output "$OUT_FILE"

if [[ ! -f "$OUT_FILE" ]]; then
  echo "[FAIL] B143 report not generated"
  exit 1
fi

if ! rg -F --quiet -- "- input_json: tmp/test-reports/wave_c_b142_local_guard_status_${RUN_ID}.json" "$OUT_FILE"; then
  echo "[FAIL] B143 should default to tmp/test-reports input json"
  sed -n '1,200p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- alert_level: **NONE**' "$OUT_FILE"; then
  echo "[FAIL] B143 should stay NONE for healthy tmp input"
  sed -n '1,200p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] check_wave_c_local_guard_alert_thresholds tmp default input contract passed"
