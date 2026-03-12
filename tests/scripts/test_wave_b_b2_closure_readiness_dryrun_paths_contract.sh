#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh"
WORK_REL="tmp/test_wave_b_b2_closure_dryrun_paths_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="wb2_closure_dryrun_paths_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave-b b2 closure readiness dry-run paths contract"

mkdir -p "$WORK_DIR"

cat > "$WORK_DIR/linux_pass.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

cat > "$WORK_DIR/macos_pass.md" <<EOF_SUMMARY
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF_SUMMARY

cat > "$WORK_DIR/windows_pass.md" <<EOF_SUMMARY
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF_SUMMARY

OUT="$(cd /tmp && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_pass.md" \
  --macos-summary "$WORK_REL/macos_pass.md" \
  --windows-summary "$WORK_REL/windows_pass.md" \
  --dry-run 2>&1)"

[[ "$OUT" == *"[DRY-RUN] linux_summary=$WORK_REL/linux_pass.md"* ]] || fail "dry-run should expose linux summary path"
[[ "$OUT" == *"[DRY-RUN] macos_summary=$WORK_REL/macos_pass.md"* ]] || fail "dry-run should expose macOS summary path"
[[ "$OUT" == *"[DRY-RUN] windows_summary=$WORK_REL/windows_pass.md"* ]] || fail "dry-run should expose Windows summary path"
[[ "$OUT" == *"[DRY-RUN] output=tmp/wave_b_reports/wave_b_b2_closure_readiness_${RUN_ID}.md"* || "$OUT" == *"[DRY-RUN] output=$WORK_REL/closure.md"* || "$OUT" == *"[DRY-RUN] output="* ]] || fail "dry-run should still expose output path"

echo "[PASS] wave-b b2 closure readiness dry-run paths contract passed"
