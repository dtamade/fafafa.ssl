#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_cross_platform_summary_explicit_missing_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="cross_summary_explicit_missing_truth"
OUTPUT_REL="$WORK_REL/cross_summary.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"
MISSING_MACOS_SUMMARY_REL="$WORK_REL/missing_macos_summary.md"
MISSING_WINDOWS_SUMMARY_REL="$WORK_REL/missing_windows_summary.md"
MISSING_MACOS_PROBE_REL="$WORK_REL/missing_macos_probe.json"
PROBE_OUTPUT_REL="$WORK_REL/cross_summary_probe.md"
PROBE_OUTPUT_ABS="$ROOT_DIR/$PROBE_OUTPUT_REL"

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
  --macos-summary "$MISSING_MACOS_SUMMARY_REL" \
  --windows-summary "$MISSING_WINDOWS_SUMMARY_REL" \
  --output "$OUTPUT_REL" >/dev/null

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected cross summary to be generated for explicit missing summaries"
fi

if ! rg -n "^\\| macos \\| PENDING \\| summary: $MISSING_MACOS_SUMMARY_REL \\(missing file\\) \\|$" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary should surface an explicit missing macOS summary as missing-file evidence instead of generic no-evidence"
fi

if ! rg -n "^\\| windows \\| PENDING \\| summary: $MISSING_WINDOWS_SUMMARY_REL \\(missing file\\) \\|$" "$OUTPUT_ABS" >/dev/null; then
  fail "cross summary should surface an explicit missing Windows summary as missing-file evidence instead of generic no-evidence"
fi

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-probe "$MISSING_MACOS_PROBE_REL" \
  --output "$PROBE_OUTPUT_REL" >/dev/null

if [[ ! -f "$PROBE_OUTPUT_ABS" ]]; then
  fail "expected cross summary to be generated for explicit missing macOS probe"
fi

if ! rg -n "^\\| macos \\| PENDING \\| probe: $MISSING_MACOS_PROBE_REL \\(missing file\\) \\|$" "$PROBE_OUTPUT_ABS" >/dev/null; then
  fail "cross summary should surface an explicit missing macOS probe as missing-file evidence instead of generic no-evidence"
fi

echo "[PASS] wave-b cross-platform summary explicit missing evidence contract passed"
