#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_bundle_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_20260315_100000.md" <<'EOF_A'
# Wave C B101 Validation Playbook Report

- run_id: 20260315_100000
- full_gate: true
- overall: **PASS**

## Benchmark Snapshot

- hit_rate_percent: 99.9
- speedup_factor_x: 5.0
EOF_A

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_20260315_110000.md" <<'EOF_B'
# Wave C B101 Validation Playbook Report

- run_id: 20260315_110000
- full_gate: true
- overall: **PASS**

## Benchmark Snapshot

- hit_rate_percent: 99.9
- speedup_factor_x: 5.5
EOF_B

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_20260315_120000.md" <<'EOF_C'
# Wave C B101 Validation Playbook Report

- run_id: 20260315_120000
- full_gate: true
- overall: **PASS**

## Benchmark Snapshot

- hit_rate_percent: 99.9
- speedup_factor_x: 5.9
EOF_C

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/wave_c_bundle.md"

cd "$PROJECT_ROOT"

bash scripts/run_wave_c_quick_sprint_bundle.sh \
  --reports-dir "$TEST_DIR" \
  --report-glob 'wave_c_b101_validation_20260315_*.md' \
  --require-full-gate \
  --validation-report "$TEST_DIR/wave_c_b101_validation_20260315_120000.md" \
  --run-id 20260315_contract \
  --strict \
  --output "$TEST_DIR/wave_c_bundle.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] quick sprint bundle report not generated"
  exit 1
fi

if ! rg -F --quiet -- '- overall: **PASS**' "$OUTPUT_FILE"; then
  echo "[FAIL] quick sprint bundle should pass for unified inputs"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| B107 threshold | 0 |' "$OUTPUT_FILE"; then
  echo "[FAIL] B107 step should succeed"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| B110 rollback drill | 0 |' "$OUTPUT_FILE"; then
  echo "[FAIL] B110 step should succeed"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] run_wave_c_quick_sprint_bundle unified input contract passed"
