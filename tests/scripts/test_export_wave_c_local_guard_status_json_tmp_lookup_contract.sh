#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="contract_guard_status_$(date +%s)_$$"
OUT_JSON="$PROJECT_ROOT/tmp/test-reports/wave_c_b142_local_guard_status_${RUN_ID}_out.json"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$PROJECT_ROOT"/tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b140_local_guard_consistency_${RUN_ID}.md "$OUT_JSON"' EXIT

cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md" <<EOF_ONCALL
# Wave C B129 Oncall
- overall: **PASS**
EOF_ONCALL
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_ID}.md" <<EOF_SNAPSHOT
# Wave C B132 Snapshot
- snapshot_state: **GREEN**
EOF_SNAPSHOT
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_ID}.md" <<EOF_FULL
# Wave C B138 Full Gate
- overall: **PASS**
EOF_FULL
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b140_local_guard_consistency_${RUN_ID}.md" <<EOF_CONS
# Wave C B140 Consistency
- consistency_state: **CONSISTENT**
EOF_CONS

cd "$PROJECT_ROOT"

bash scripts/export_wave_c_local_guard_status_json.sh --run-id "$RUN_ID" --strict --output "$OUT_JSON"

if [[ ! -f "$OUT_JSON" ]]; then
  echo "[FAIL] B142 json not generated"
  exit 1
fi

if ! rg -F --quiet -- '"overall_state": "HEALTHY"' "$OUT_JSON"; then
  echo "[FAIL] B142 should be HEALTHY with tmp/test-reports inputs"
  cat "$OUT_JSON"
  exit 1
fi

if ! rg -F --quiet -- "\"oncall_report\": \"tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md\"" "$OUT_JSON"; then
  echo "[FAIL] B142 should resolve evidence from tmp/test-reports"
  cat "$OUT_JSON"
  exit 1
fi

echo "[PASS] export_wave_c_local_guard_status_json tmp lookup contract passed"
