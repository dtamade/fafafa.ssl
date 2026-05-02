#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b109_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b107_threshold_eval_unified.md" <<'EOF_B107'
# Wave C B107 Threshold Evaluation Report

- overall: **PASS**
EOF_B107

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b108_default_on_readiness_unified.md" <<'EOF_B108'
# Wave C B108 Default-On Readiness

- readiness: **READY**
EOF_B108

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_unified.md" <<'EOF_B101'
# Wave C B101 Validation Playbook Report

- run_id: 20260315_foo
- full_gate: true
- overall: **PASS**

## Benchmark Snapshot

- hit_rate_percent: 99.9
- speedup_factor_x: 5.9
EOF_B101

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/wave_c_b109.md"

cd "$PROJECT_ROOT"

bash scripts/prepare_wave_c_b109_canary_rollout.sh \
  --reports-dir "$TEST_DIR" \
  --threshold-report "$TEST_DIR/wave_c_b107_threshold_eval_unified.md" \
  --readiness-report "$TEST_DIR/wave_c_b108_default_on_readiness_unified.md" \
  --validation-report "$TEST_DIR/wave_c_b101_validation_unified.md" \
  --run-id 20260315_contract \
  --strict \
  --output "$TEST_DIR/wave_c_b109.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B109 rollout report not generated"
  exit 1
fi

if ! rg -F --quiet -- '- rollout_state: **CANARY_READY**' "$OUTPUT_FILE"; then
  echo "[FAIL] rollout_state should be CANARY_READY"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '--report-glob' "$OUTPUT_FILE"; then
  echo "[FAIL] operator commands should include report_glob guidance"
  sed -n '1,260p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '--require-full-gate' "$OUTPUT_FILE"; then
  echo "[FAIL] operator commands should require full-gate evidence"
  sed -n '1,260p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] prepare_wave_c_b109_canary_rollout unified input contract passed"
