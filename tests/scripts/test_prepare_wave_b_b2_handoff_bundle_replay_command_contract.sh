#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_replay_command_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="handoff_replay_command_truth"
OUTPUT_DIR_REL="$WORK_REL/custom-out"
OUTPUT_DIR_ABS="$ROOT_DIR/$OUTPUT_DIR_REL"
WINDOWS_SUMMARY_REL="$WORK_REL/windows_evidence/custom_windows_summary.md"
HANDOFF_REPORT_ABS="$OUTPUT_DIR_ABS/wave_b_b2_handoff_bundle_${RUN_ID}.md"

mkdir -p "$WORK_DIR" "$OUTPUT_DIR_ABS" "$ROOT_DIR/$WORK_REL/windows_evidence"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/custom_linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

cat > "$WORK_DIR/custom_examples.json" <<'EOF'
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

cat > "$ROOT_DIR/$WINDOWS_SUMMARY_REL" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

: > "$ROOT_DIR/$WORK_REL/windows_evidence/winssl_quick_smoke_${RUN_ID}.log"
: > "$ROOT_DIR/$WORK_REL/windows_evidence/winssl_runtime_suite_${RUN_ID}.log"

bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/custom_linux_summary.md" \
  --linux-examples "$WORK_REL/custom_examples.json" \
  --windows-summary "$WINDOWS_SUMMARY_REL" \
  --output-dir "$OUTPUT_DIR_REL" >/dev/null

if [[ ! -f "$HANDOFF_REPORT_ABS" ]]; then
  fail "expected handoff bundle report to be generated"
fi

if ! rg -n --fixed-strings "scripts/prepare_wave_b_b2_handoff_bundle.sh --run-id $RUN_ID --linux-summary $WORK_REL/custom_linux_summary.md --linux-examples $WORK_REL/custom_examples.json --windows-summary $WINDOWS_SUMMARY_REL --output-dir $OUTPUT_DIR_REL --strict" "$HANDOFF_REPORT_ABS" >/dev/null; then
  fail "handoff bundle replay command should preserve the batch-defining custom linux/windows/output-dir arguments"
fi

echo "[PASS] prepare_wave_b_b2 handoff bundle replay command contract passed"
