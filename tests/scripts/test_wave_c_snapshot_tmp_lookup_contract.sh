#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="contract_snapshot_$(date +%s)_$$"
OUT_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_ID}_out.md"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$PROJECT_ROOT"/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b126_local_guard_history_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md "$OUT_FILE"' EXIT

cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.md" <<EOF_B123
# B123
- local_first_state: **LOCAL_READY**
EOF_B123
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md" <<EOF_B124
# B124
- local_drift_state: **LOCAL_STABLE**
EOF_B124
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_ID}.md" <<EOF_B125
# B125
- overall: **PASS**
EOF_B125
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b126_local_guard_history_${RUN_ID}.md" <<EOF_B126
# B126
- trend_state: **STABLE**
EOF_B126
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md" <<EOF_B129
# B129
- overall: **PASS**
EOF_B129

cd "$PROJECT_ROOT"

bash scripts/generate_wave_c_local_first_status_snapshot.sh --run-id "$RUN_ID" --strict --output "$OUT_FILE"

if ! rg -F --quiet -- '- snapshot_state: **GREEN**' "$OUT_FILE"; then
  echo "[FAIL] B132 should be GREEN with tmp inputs"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "- B129: tmp/test-reports/wave_c_b129_oncall_check_${RUN_ID}.md" "$OUT_FILE"; then
  echo "[FAIL] B132 should pick latest B129 from tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] Wave C B132 tmp lookup contract passed"
