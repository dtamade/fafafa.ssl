#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_macos_probe_fallback_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="handoff_macos_probe_contract"
PROBE_REL="test-reports/wave_b_macos_gate_probe_${RUN_ID}.json"
PROBE_ABS="$ROOT_DIR/$PROBE_REL"
OUTPUT_DIR="$WORK_DIR/out"

mkdir -p "$WORK_DIR" "$(dirname "$PROBE_ABS")" "$OUTPUT_DIR"
trap 'rm -rf "$WORK_DIR"; rm -f "$PROBE_ABS"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
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

cat > "$PROBE_ABS" <<'EOF'
{
  "status": "error"
}
EOF

(cd /tmp && bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --output-dir "$OUTPUT_DIR" >/dev/null)

CROSS_SUMMARY="$OUTPUT_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"
HANDOFF_REPORT="$OUTPUT_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"

if [[ ! -f "$CROSS_SUMMARY" ]]; then
  fail "expected cross summary to be generated"
fi

if [[ ! -f "$HANDOFF_REPORT" ]]; then
  fail "expected handoff bundle report to be generated"
fi

if ! rg -n "\\| macos \\| PROBE_ONLY \\| probe: $PROBE_REL \\(status=error\\) \\|" "$CROSS_SUMMARY" >/dev/null; then
  fail "cross summary should preserve macOS probe-only evidence when no macOS summary exists"
fi

if ! rg -n "\\| wave_b_macos_gate_probe_${RUN_ID}\\.json \\| $PROBE_REL \\| YES \\|" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff bundle should list the macOS probe artifact when it is used as fallback evidence"
fi

echo "[PASS] prepare_wave_b_b2 macOS probe fallback contract passed"
