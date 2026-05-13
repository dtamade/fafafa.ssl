#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_closure_next_actions_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="closure_next_actions_truth"
OUTPUT_ABS="$WORK_DIR/closure_report.md"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --output "$OUTPUT_ABS" >/dev/null

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected closure readiness report to be generated"
fi

if rg -n "scripts/generate_wave_b_cross_platform_summary\\.sh" "$OUTPUT_ABS" >/dev/null; then
  fail "closure readiness report should no longer direct operators to rerun the stale cross-summary-only entrypoint"
fi

if ! rg -n "prepare_wave_b_b2_handoff_bundle\\.sh" "$OUTPUT_ABS" >/dev/null; then
  fail "closure readiness report should point operators back to the current handoff-bundle prepare entrypoint"
fi

echo "[PASS] wave-b-b2 closure next actions contract passed"
