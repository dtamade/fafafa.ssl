#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_macos_probe_consistency_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="handoff_macos_probe_consistency"
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

CONSISTENCY_REPORT="$OUTPUT_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
if [[ ! -f "$CONSISTENCY_REPORT" ]]; then
  fail "expected consistency report to be generated"
fi

if ! rg -n "\\| macos_probe \\| $PROBE_REL \\| YES \\| n/a \\| n/a \\| json_valid=YES \\|" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should surface the macOS probe artifact when probe-only evidence feeds the cross summary"
fi

if ! rg -n "consistency_status: \\*\\*CONSISTENT\\*\\*" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should remain CONSISTENT when the macOS probe artifact exists and is valid"
fi

echo "[PASS] prepare_wave_b_b2 macOS probe consistency contract passed"
