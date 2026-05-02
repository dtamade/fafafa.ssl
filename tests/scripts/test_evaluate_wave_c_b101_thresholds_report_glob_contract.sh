#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b107_glob_$(date +%s)"
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

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_contract_noise.md" <<'EOF_NOISE'
# Wave C B101 Validation Playbook Report

- run_id: contract_noise
- full_gate: true
- overall: **FAIL**

## Benchmark Snapshot

- hit_rate_percent: 0.0
- speedup_factor_x: 0.1
EOF_NOISE

cat > "$PROJECT_ROOT/$TEST_DIR/wave_c_b101_validation_20260315_120000.md" <<'EOF_PARTIAL'
# Wave C B101 Validation Playbook Report

- run_id: 20260315_120000
- full_gate: false
- overall: **PASS**

## Benchmark Snapshot

- hit_rate_percent: 99.9
- speedup_factor_x: 9.9
EOF_PARTIAL

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b107.md"

cd "$PROJECT_ROOT"

bash scripts/evaluate_wave_c_b101_thresholds.sh \
  --reports-dir "$TEST_DIR" \
  --report-glob 'wave_c_b101_validation_20260315_*.md' \
  --require-full-gate \
  --min-passing-runs 2 \
  --strict \
  --run-id contract \
  --output "$TEST_DIR/b107.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] threshold report not generated"
  exit 1
fi

if ! rg -F --quiet '| 20260315_100000 | PASS | 99.9 | 5.0 | YES |' "$OUTPUT_FILE"; then
  echo "[FAIL] expected first matching run in report"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet '| 20260315_110000 | PASS | 99.9 | 5.5 | YES |' "$OUTPUT_FILE"; then
  echo "[FAIL] expected second matching run in report"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if rg -F --quiet 'contract_noise' "$OUTPUT_FILE"; then
  echo "[FAIL] non-matching noise report should be excluded by report_glob"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if rg -F --quiet '20260315_120000' "$OUTPUT_FILE"; then
  echo "[FAIL] partial run should be excluded when --require-full-gate is enabled"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- passing_runs: 2' "$OUTPUT_FILE"; then
  echo "[FAIL] passing_runs should count only matching reports"
  sed -n '1,120p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] evaluate_wave_c_b101_thresholds report_glob contract passed"
