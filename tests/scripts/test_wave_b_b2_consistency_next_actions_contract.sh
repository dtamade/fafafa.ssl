#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_next_actions_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_next_actions_truth"
OUTPUT_ABS="$WORK_DIR/consistency.md"

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

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 75,
    "failed": 0,
    "skipped": 0,
    "pass_rate": "100%"
  }
}
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --output "$WORK_REL/cross.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --output "$WORK_REL/closure.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --cross-summary "$WORK_REL/cross.md" \
  --closure-report "$WORK_REL/closure.md" \
  --output "$OUTPUT_ABS" >/dev/null

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected consistency report to be generated"
fi

if ! rg -n "^## Next Actions$" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should include next-action guidance when closure is still in progress"
fi

if ! rg -n "closure_status_note=IN_PROGRESS" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency next actions should explain that IN_PROGRESS closure means the handoff is not closed yet"
fi

if ! rg -n "prepare_wave_b_b2_handoff_bundle\\.sh" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency next actions should point operators back to the current handoff-bundle prepare entrypoint"
fi

echo "[PASS] wave-b-b2 consistency next actions contract passed"
