#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_b_ci_gate.sh"

echo "[TEST] wave_b_ci_gate run-id passthrough contract"

RUN_ID="20991231_235959"
WORK_REL="tmp/test_wave_b_ci_gate_run_id_passthrough_contract"
SUMMARY_REL="$WORK_REL/wave_b_ci_gate_summary_${RUN_ID}.md"
EXAMPLES_REL="$WORK_REL/examples_compile_ci_gate_${RUN_ID}.json"

rm -rf "$ROOT_DIR/$WORK_REL"
mkdir -p "$ROOT_DIR/$WORK_REL"

bash "$SCRIPT" \
  --dry-run \
  --run-id "$RUN_ID" \
  --reports-dir "$WORK_REL" \
  --examples-report "$EXAMPLES_REL" \
  --summary-out "$SUMMARY_REL" >/dev/null

if [[ ! -f "$ROOT_DIR/$SUMMARY_REL" ]]; then
  echo "[FAIL] expected summary file: $SUMMARY_REL"
  exit 1
fi

if ! rg -F --quiet -- "Run ID: \`$RUN_ID\`" "$ROOT_DIR/$SUMMARY_REL"; then
  echo "[FAIL] expected summary to contain injected run id: $RUN_ID"
  exit 1
fi

echo "[PASS] wave_b_ci_gate run-id passthrough contract passed"
