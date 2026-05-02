#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="$(date +%Y%m%d_%H%M%S)_$$"
OUT_FILE="$PROJECT_ROOT/tmp/test-reports/wave_c_b125_local_guard_bundle_${RUN_ID}.md"
LEGACY_OUT="$PROJECT_ROOT/test-reports/wave_c_b125_local_guard_bundle_${RUN_ID}.md"

mkdir -p "$PROJECT_ROOT/tmp/test-reports"
trap 'rm -f "$OUT_FILE" "$LEGACY_OUT" "$PROJECT_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.md" "$PROJECT_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md" "$PROJECT_ROOT/tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.log" "$PROJECT_ROOT/tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.log" "$PROJECT_ROOT/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.md" "$PROJECT_ROOT/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md" "$PROJECT_ROOT/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.log" "$PROJECT_ROOT/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.log"' EXIT

cd "$PROJECT_ROOT"

bash scripts/run_wave_c_local_first_guard_bundle.sh --run-id "$RUN_ID"

if [[ ! -f "$OUT_FILE" ]]; then
  echo "[FAIL] B125 default output should land in tmp/test-reports"
  exit 1
fi

if [[ -f "$LEGACY_OUT" ]]; then
  echo "[FAIL] B125 should not write default bundle output into legacy test-reports"
  exit 1
fi

if ! rg -F --quiet -- "| B123 local continuity |" "$OUT_FILE"; then
  echo "[FAIL] B125 output missing B123 step row"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.md" "$OUT_FILE"; then
  echo "[FAIL] B125 should default B123 report path to tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.md" "$OUT_FILE"; then
  echo "[FAIL] B125 should default B124 report path to tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b123_local_first_continuity_${RUN_ID}.log" "$OUT_FILE"; then
  echo "[FAIL] B125 should default B123 log path to tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b124_local_drift_watch_${RUN_ID}.log" "$OUT_FILE"; then
  echo "[FAIL] B125 should default B124 log path to tmp/test-reports"
  sed -n '1,220p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] run_wave_c_local_first_guard_bundle tmp default contract passed"
