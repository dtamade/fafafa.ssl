#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b113_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b107_threshold_eval.md" <<'EOF_B107'
# Wave C B107 Threshold Evaluation Report

- overall: **PASS**
EOF_B107

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b108_default_on_readiness.md" <<'EOF_B108'
# Wave C B108 Default-On Readiness

- readiness: **READY**
EOF_B108

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b109_canary_rollout.md" <<'EOF_B109'
# Wave C B109 Controlled Canary Rollout

- rollout_state: **CANARY_READY**
EOF_B109

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b110_rollback_drill.md" <<'EOF_B110'
# Wave C B110 Rollback Drill Report

- drill_status: **PASS**
EOF_B110

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_quick_sprint_bundle.md" <<'EOF_BUNDLE'
# Wave C Quick Sprint Bundle

- overall: **PASS**
EOF_BUNDLE

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/signoff.md"

cd "$PROJECT_ROOT"

bash scripts/prepare_wave_c_b113_release_signoff.sh \
  --run-id 20260315_contract \
  --reports-dir "$TEST_DIR" \
  --threshold-report "$TEST_DIR/wave_c_b107_threshold_eval.md" \
  --readiness-report "$TEST_DIR/wave_c_b108_default_on_readiness.md" \
  --rollout-report "$TEST_DIR/wave_c_b109_canary_rollout.md" \
  --rollback-report "$TEST_DIR/wave_c_b110_rollback_drill.md" \
  --bundle-report "$TEST_DIR/wave_c_quick_sprint_bundle.md" \
  --output "$TEST_DIR/signoff.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] signoff record not generated"
  exit 1
fi

if ! rg -F --quiet -- '- signoff_state: READY_FOR_APPROVAL' "$OUTPUT_FILE"; then
  echo "[FAIL] signoff state should be READY_FOR_APPROVAL"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- allow_canary_execution: YES' "$OUTPUT_FILE"; then
  echo "[FAIL] allow_canary_execution should be YES"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| B110 rollback drill | PASS |' "$OUTPUT_FILE"; then
  echo "[FAIL] B110 evidence row should be PASS"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] prepare_wave_c_b113_release_signoff unified input contract passed"
