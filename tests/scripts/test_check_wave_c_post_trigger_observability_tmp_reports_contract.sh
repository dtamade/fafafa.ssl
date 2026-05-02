#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="contract_obs_$(date +%s)_$$"
OUT_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b120_post_trigger_observability_${RUN_ID}_out.md"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$PROJECT_ROOT"/tmp/test-reports/wave_c_b107_threshold_eval_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b108_default_on_readiness_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b109_canary_rollout_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_b110_rollback_drill_${RUN_ID}.md "$PROJECT_ROOT"/tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_ID}.md "$OUT_FILE"' EXIT

cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b107_threshold_eval_${RUN_ID}.md" <<EOF_B107
# Wave C B107 Threshold Evaluation Report
- overall: **PASS**
EOF_B107
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b108_default_on_readiness_${RUN_ID}.md" <<EOF_B108
# Wave C B108 Default-On Readiness
- readiness: **READY**
EOF_B108
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b109_canary_rollout_${RUN_ID}.md" <<EOF_B109
# Wave C B109 Controlled Canary Rollout
- rollout_state: **CANARY_READY**
EOF_B109
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_b110_rollback_drill_${RUN_ID}.md" <<EOF_B110
# Wave C B110 Rollback Drill Report
- drill_status: **PASS**
EOF_B110
cat > "$PROJECT_ROOT/tmp/test-reports/wave_c_quick_sprint_bundle_${RUN_ID}.md" <<EOF_BUNDLE
# Wave C Quick Sprint Bundle
- overall: **PASS**
EOF_BUNDLE

cd "$PROJECT_ROOT"

bash scripts/check_wave_c_post_trigger_observability.sh --run-id "$RUN_ID" --strict --output "$OUT_FILE"

if ! rg -F --quiet -- '- state: **READY**' "$OUT_FILE"; then
  echo "[FAIL] B120 should be READY when artifacts exist in tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b109_canary_rollout_${RUN_ID}.md | PASS |" "$OUT_FILE"; then
  echo "[FAIL] B120 should resolve artifact paths under tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] check_wave_c_post_trigger_observability tmp/test-reports contract passed"
