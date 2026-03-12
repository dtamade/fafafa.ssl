#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh"
WORK_REL="tmp/test_wave_b_evidence_dryrun_paths_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="wave_b_evidence_dryrun_paths_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave-b evidence dry-run paths contract"

mkdir -p "$WORK_DIR"

cat > "$WORK_DIR/linux_summary.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF_SUMMARY

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

cat > "$WORK_DIR/cross_summary.md" <<EOF_SUMMARY
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
EOF_SUMMARY

cat > "$WORK_DIR/closure_report.md" <<EOF_SUMMARY
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
EOF_SUMMARY

cat > "$WORK_DIR/examples.json" <<EOF_JSON
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

OUT="$(cd /tmp && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --windows-summary "$WORK_REL/windows_summary.md" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure_report.md" \
  --dry-run 2>&1)"

[[ "$OUT" == *"[DRY-RUN] linux_summary=$WORK_REL/linux_summary.md"* ]] || fail "dry-run should expose linux summary path"
[[ "$OUT" == *"[DRY-RUN] macos_summary=$WORK_REL/macos_summary.md"* ]] || fail "dry-run should expose macOS summary path"
[[ "$OUT" == *"[DRY-RUN] windows_summary=$WORK_REL/windows_summary.md"* ]] || fail "dry-run should expose Windows summary path"
[[ "$OUT" == *"[DRY-RUN] cross_summary=$WORK_REL/cross_summary.md"* ]] || fail "dry-run should expose cross summary path"
[[ "$OUT" == *"[DRY-RUN] closure_report=$WORK_REL/closure_report.md"* ]] || fail "dry-run should expose closure report path"

echo "[PASS] wave-b evidence dry-run paths contract passed"
