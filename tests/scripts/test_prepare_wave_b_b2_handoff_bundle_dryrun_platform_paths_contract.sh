#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_handoff_dryrun_platform_paths_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_handoff_dryrun_platforms_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave-b handoff dry-run platform paths contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

cat > "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" <<EOF_JSON
{
  "run_id": "$RUN_ID",
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

cat > "$WORK_DIR/macos_summary.md" <<EOF_SUMMARY
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF_SUMMARY

cat > "$WORK_DIR/windows_summary.md" <<EOF_SUMMARY
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF_SUMMARY

OUT="$(FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --windows-summary "$WORK_REL/windows_summary.md" \
  --dry-run 2>&1)"

[[ "$OUT" == *"[DRY-RUN] macos_summary=$WORK_REL/macos_summary.md"* ]] || fail "dry-run should expose macOS summary path"
[[ "$OUT" == *"[DRY-RUN] windows_summary=$WORK_REL/windows_summary.md"* ]] || fail "dry-run should expose Windows summary path"

echo "[PASS] wave-b handoff dry-run platform paths contract passed"
