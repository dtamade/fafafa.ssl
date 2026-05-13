#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_explicit_missing_evidence_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="prepare_explicit_missing_evidence"
OUTPUT_DIR_REL="$WORK_REL/out"
OUTPUT_DIR_ABS="$ROOT_DIR/$OUTPUT_DIR_REL"
MISSING_MACOS_SUMMARY_REL="$WORK_REL/missing_macos_summary.md"
MISSING_WINDOWS_SUMMARY_REL="$WORK_REL/missing_windows_summary.md"
CROSS_SUMMARY_ABS="$OUTPUT_DIR_ABS/wave_b_cross_platform_summary_${RUN_ID}.md"
CONSISTENCY_ABS="$OUTPUT_DIR_ABS/wave_b_b2_evidence_consistency_${RUN_ID}.md"
BUNDLE_ABS="$OUTPUT_DIR_ABS/wave_b_b2_handoff_bundle_${RUN_ID}.md"
WINDOWS_QUICK_REL="$WORK_REL/winssl_quick_smoke_${RUN_ID}.log"
WINDOWS_RUNTIME_REL="$WORK_REL/winssl_runtime_suite_${RUN_ID}.log"

mkdir -p "$WORK_DIR" "$OUTPUT_DIR_ABS"
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

bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$MISSING_MACOS_SUMMARY_REL" \
  --windows-summary "$MISSING_WINDOWS_SUMMARY_REL" \
  --output-dir "$OUTPUT_DIR_REL" >/dev/null

for file in "$CROSS_SUMMARY_ABS" "$CONSISTENCY_ABS" "$BUNDLE_ABS"; do
  if [[ ! -f "$file" ]]; then
    fail "expected generated artifact: $file"
  fi
done

if ! rg -n "^\\| macos \\| PENDING \\| summary: $MISSING_MACOS_SUMMARY_REL \\(missing file\\) \\|$" "$CROSS_SUMMARY_ABS" >/dev/null; then
  fail "prepare should preserve explicit missing macOS summary into cross summary truth"
fi

if ! rg -n "^\\| windows \\| PENDING \\| summary: $MISSING_WINDOWS_SUMMARY_REL \\(missing file\\) \\|$" "$CROSS_SUMMARY_ABS" >/dev/null; then
  fail "prepare should preserve explicit missing Windows summary into cross summary truth"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$CONSISTENCY_ABS" >/dev/null; then
  fail "consistency should become INCONSISTENT when prepare forwards explicit missing summary evidence"
fi

if ! rg -n "^- required_missing: 4$" "$CONSISTENCY_ABS" >/dev/null; then
  fail "explicit missing macOS summary plus Windows summary and its companion runtime artifacts should count as four required missing artifacts"
fi

if ! rg -n "^\\| macos_summary \\| $MISSING_MACOS_SUMMARY_REL \\| NO \\| n/a \\| NO \\| missing \\|" "$CONSISTENCY_ABS" >/dev/null; then
  fail "consistency should track the explicit missing macOS summary path instead of a default path"
fi

if ! rg -n "^\\| windows_summary \\| $MISSING_WINDOWS_SUMMARY_REL \\| NO \\| n/a \\| NO \\| missing \\|" "$CONSISTENCY_ABS" >/dev/null; then
  fail "consistency should track the explicit missing Windows summary path instead of a default path"
fi

if ! rg -n "^\\| windows_quick_log \\| $WINDOWS_QUICK_REL \\| NO \\| n/a \\| n/a \\| missing \\|" "$CONSISTENCY_ABS" >/dev/null; then
  fail "consistency should derive the explicit Windows companion quick-log path from the explicit summary path"
fi

if ! rg -n "^\\| windows_runtime_transcript \\| $WINDOWS_RUNTIME_REL \\| NO \\| n/a \\| n/a \\| missing \\|" "$CONSISTENCY_ABS" >/dev/null; then
  fail "consistency should derive the explicit Windows companion runtime-transcript path from the explicit summary path"
fi

if ! rg -n "^- handoff_state: \\*\\*NEEDS_EVIDENCE_SYNC\\*\\*$" "$BUNDLE_ABS" >/dev/null; then
  fail "handoff bundle should fall to NEEDS_EVIDENCE_SYNC when explicit missing evidence makes consistency inconsistent"
fi

if ! rg -n "^\\| winssl_quick_smoke_${RUN_ID}\\.log \\| $WINDOWS_QUICK_REL \\| NO \\|$" "$BUNDLE_ABS" >/dev/null; then
  fail "handoff bundle should list the explicit Windows quick-smoke companion artifact even when it is missing"
fi

if ! rg -n "^\\| winssl_runtime_suite_${RUN_ID}\\.log \\| $WINDOWS_RUNTIME_REL \\| NO \\|$" "$BUNDLE_ABS" >/dev/null; then
  fail "handoff bundle should list the explicit Windows runtime-suite companion artifact even when it is missing"
fi

echo "[PASS] prepare_wave_b_b2 handoff bundle explicit missing evidence contract passed"
