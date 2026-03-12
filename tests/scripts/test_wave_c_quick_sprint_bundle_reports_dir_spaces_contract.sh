#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_quick_sprint_bundle_space_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
QUICK_REL="$WORK_REL/quick reports"
QUICK_DIR="$ROOT_DIR/$QUICK_REL"
VALIDATION_GLOB_REL="$WORK_REL/b101 reports/*/wave_c_b101_validation_*.md"
RUN_ID="wave_c_quick_sprint_space_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave c quick sprint bundle reports-dir spaces contract"

mkdir -p \
  "$WORK_DIR/b101 reports/run1" \
  "$WORK_DIR/b101 reports/run2" \
  "$WORK_DIR/b101 reports/run3"

for run in run1 run2 run3; do
  cat > "$WORK_DIR/b101 reports/${run}/wave_c_b101_validation_${run}.md" <<EOF_REPORT
# Wave C B101 Validation

- run_id: ${run}
- overall: **PASS**
- hit_rate_percent: 99.9
- speedup_factor_x: 3.5
EOF_REPORT
done

if ! FAFAFA_WAVE_C_B101_VALIDATION_GLOB="$VALIDATION_GLOB_REL" \
  bash scripts/run_wave_c_quick_sprint_bundle.sh --run-id "$RUN_ID" --reports-dir "$QUICK_REL" --strict; then
  find "$WORK_DIR" -maxdepth 3 -type f | sort || true
  if [[ -f "$QUICK_DIR/wave_c_b107_threshold_eval_${RUN_ID}.log" ]]; then
    sed -n '1,160p' "$QUICK_DIR/wave_c_b107_threshold_eval_${RUN_ID}.log" || true
  fi
  fail "bundle should accept reports-dir paths containing spaces"
fi

for suffix in \
  "wave_c_b107_threshold_eval_${RUN_ID}.md" \
  "wave_c_b108_default_on_readiness_${RUN_ID}.md" \
  "wave_c_b109_canary_rollout_${RUN_ID}.md" \
  "wave_c_b110_rollback_drill_${RUN_ID}.md" \
  "wave_c_quick_sprint_bundle_${RUN_ID}.md"; do
  [[ -f "$QUICK_DIR/$suffix" ]] || fail "missing expected output under spaced reports dir: $suffix"
done

if ! rg -F --quiet -- '- overall: **PASS**' "$QUICK_DIR/wave_c_quick_sprint_bundle_${RUN_ID}.md"; then
  sed -n '1,220p' "$QUICK_DIR/wave_c_quick_sprint_bundle_${RUN_ID}.md" || true
  fail "bundle report should record PASS under spaced reports dir"
fi

echo "[PASS] wave c quick sprint bundle reports-dir spaces contract passed"
