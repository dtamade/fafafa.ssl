#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_OLD="stable_b142_$(date +%s)_$$"
RUN_NEW="inflight_b142_$(date +%s)_$$"
SANDBOX_ROOT="$(mktemp -d)"
OUT_JSON="$SANDBOX_ROOT/tmp/test-reports/wave_c_b142_local_guard_status_consistent_out.json"

mkdir -p "$SANDBOX_ROOT/scripts" "$SANDBOX_ROOT/.github/workflows" "$SANDBOX_ROOT/tmp/test-reports"
cp "$PROJECT_ROOT/scripts/export_wave_c_local_guard_status_json.sh" "$SANDBOX_ROOT/scripts/"
trap 'rm -rf "$SANDBOX_ROOT"' EXIT

touch "$SANDBOX_ROOT/.github/workflows/wave-c-quick-sprint-manual.yml.disabled"

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_OLD}.md" <<EOF_B129_OLD
# B129
- overall: **PASS**
EOF_B129_OLD

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_OLD}.md" <<EOF_B132_OLD
# B132
- snapshot_state: **GREEN**

## Latest Evidence

- B129: tmp/test-reports/wave_c_b129_oncall_check_${RUN_OLD}.md
EOF_B132_OLD

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_OLD}.md" <<EOF_B138_OLD
# B138
- overall: **PASS**
EOF_B138_OLD

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b140_local_guard_consistency_${RUN_OLD}.md" <<EOF_B140_OLD
# B140
- consistency_state: **CONSISTENT**
EOF_B140_OLD

cat > "$SANDBOX_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_NEW}.md" <<EOF_B129_NEW
# B129
- overall: **FAIL**
EOF_B129_NEW

touch -d '2 hours ago' \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_OLD}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_OLD}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b138_pre_ci_reenable_full_gate_${RUN_OLD}.md" \
  "$SANDBOX_ROOT/tmp/test-reports/wave_c_b140_local_guard_consistency_${RUN_OLD}.md"
touch -d '1 hour ago' "$SANDBOX_ROOT/tmp/test-reports/wave_c_b129_oncall_check_${RUN_NEW}.md"

cd "$SANDBOX_ROOT"

bash scripts/export_wave_c_local_guard_status_json.sh --run-id consistent_b142 --strict --output "$OUT_JSON"

if ! rg -F --quiet -- '"overall_state": "HEALTHY"' "$OUT_JSON"; then
  echo "[FAIL] B142 should stay HEALTHY by using the oncall report referenced by the latest snapshot"
  cat "$OUT_JSON" || true
  exit 1
fi

if ! rg -F --quiet -- "\"oncall_report\": \"tmp/test-reports/wave_c_b129_oncall_check_${RUN_OLD}.md\"" "$OUT_JSON"; then
  echo "[FAIL] B142 should derive default oncall evidence from the selected snapshot"
  cat "$OUT_JSON" || true
  exit 1
fi

if ! rg -F --quiet -- "\"snapshot_report\": \"tmp/test-reports/wave_c_b132_local_first_status_snapshot_${RUN_OLD}.md\"" "$OUT_JSON"; then
  echo "[FAIL] B142 should keep the stable snapshot input"
  cat "$OUT_JSON" || true
  exit 1
fi

echo "[PASS] export_wave_c_local_guard_status_json consistent snapshot contract passed"
