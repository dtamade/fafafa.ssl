#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="contract_b137_$(date +%s)_$$"
OUT_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b137_pre_ci_reenable_packet_${RUN_ID}_out.md"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$PROJECT_ROOT"/tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md "$OUT_FILE"' EXIT

cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md" <<EOF_B129
# B129
- overall: **PASS**
EOF_B129
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md" <<EOF_B132
# B132
- snapshot_state: **GREEN**
EOF_B132

cd "$PROJECT_ROOT"

bash scripts/prepare_wave_c_b137_pre_ci_reenable_packet.sh --run-id "$RUN_ID" --strict --output "$OUT_FILE"

if ! rg -F --quiet -- '- packet_state: **READY_FOR_APPROVAL**' "$OUT_FILE"; then
  echo "[FAIL] B137 should be READY_FOR_APPROVAL with tmp inputs"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "- oncall_report: tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md" "$OUT_FILE"; then
  echo "[FAIL] B137 should prefer tmp oncall report"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] Wave C B137 tmp lookup contract passed"
