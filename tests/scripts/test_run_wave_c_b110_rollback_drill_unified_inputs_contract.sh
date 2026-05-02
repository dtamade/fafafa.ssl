#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b110_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b109_canary_rollout_unified.md" <<'EOF_B109'
# Wave C B109 Controlled Canary Rollout

- rollout_state: **CANARY_READY**
EOF_B109

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b108_default_on_readiness_unified.md" <<'EOF_B108'
# Wave C B108 Default-On Readiness

- readiness: **READY**
EOF_B108

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b107_threshold_eval_unified.md" <<'EOF_B107'
# Wave C B107 Threshold Evaluation Report

- overall: **PASS**
EOF_B107

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_unified.md" <<'EOF_B101'
# Wave C B101 Validation Playbook Report

- run_id: 20260315_foo
- full_gate: true
- overall: **PASS**

## Benchmark Snapshot

- hit_rate_percent: 99.9
- speedup_factor_x: 5.9
EOF_B101

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/wave_c_b110.md"

cd "$PROJECT_ROOT"

bash scripts/run_wave_c_b110_rollback_drill.sh \
  --reports-dir "$TEST_DIR" \
  --rollout-report "$TEST_DIR/wave_c_b109_canary_rollout_unified.md" \
  --readiness-report "$TEST_DIR/wave_c_b108_default_on_readiness_unified.md" \
  --threshold-report "$TEST_DIR/wave_c_b107_threshold_eval_unified.md" \
  --validation-report "$TEST_DIR/wave_c_b101_validation_unified.md" \
  --run-id 20260315_contract \
  --strict \
  --output "$TEST_DIR/wave_c_b110.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B110 rollback report not generated"
  exit 1
fi

if ! rg -F --quiet -- '- drill_status: **PASS**' "$OUTPUT_FILE"; then
  echo "[FAIL] rollback drill should pass for unified ready inputs"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "| recovery_recheck | PASS |" "$OUTPUT_FILE"; then
  echo "[FAIL] recovery recheck should pass"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "wave_c_b110_recheck_20260315_contract.md" "$OUTPUT_FILE"; then
  echo "[FAIL] report should reference deterministic recheck report path"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] run_wave_c_b110_rollback_drill unified input contract passed"
