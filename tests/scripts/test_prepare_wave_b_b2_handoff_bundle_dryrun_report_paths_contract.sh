#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_handoff_dryrun_report_paths_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_handoff_dryrun_paths_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave-b handoff dry-run report paths contract"

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

OUT="$(FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" bash "$SCRIPT" --run-id "$RUN_ID" --linux-summary "$REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md" --dry-run 2>&1)"

[[ "$OUT" == *"[DRY-RUN] cross_summary=$REPORTS_REL/wave_b_cross_platform_summary_${RUN_ID}.md"* ]] || fail "dry-run should expose cross summary path"
[[ "$OUT" == *"[DRY-RUN] closure_report=$REPORTS_REL/wave_b_b2_closure_readiness_${RUN_ID}.md"* ]] || fail "dry-run should expose closure report path"
[[ "$OUT" == *"[DRY-RUN] consistency_report=$REPORTS_REL/wave_b_b2_evidence_consistency_${RUN_ID}.md"* ]] || fail "dry-run should expose consistency report path"
[[ "$OUT" == *"[DRY-RUN] bundle_report=$REPORTS_REL/wave_b_b2_handoff_bundle_${RUN_ID}.md"* ]] || fail "dry-run should expose bundle report path"

echo "[PASS] wave-b handoff dry-run report paths contract passed"
