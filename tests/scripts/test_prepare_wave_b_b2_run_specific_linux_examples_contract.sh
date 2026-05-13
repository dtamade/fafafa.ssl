#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_run_specific_linux_examples_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="handoff_run_specific_linux_examples"
RUN_SPECIFIC_JSON_REL="test-reports/examples_compile_ci_gate_${RUN_ID}.json"
RUN_SPECIFIC_JSON_ABS="$ROOT_DIR/$RUN_SPECIFIC_JSON_REL"
OUTPUT_DIR="$WORK_DIR/out"

mkdir -p "$WORK_DIR" "$(dirname "$RUN_SPECIFIC_JSON_ABS")" "$OUTPUT_DIR"
trap 'rm -rf "$WORK_DIR"; rm -f "$RUN_SPECIFIC_JSON_ABS"' EXIT

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

cat > "$RUN_SPECIFIC_JSON_ABS" <<'EOF'
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

(cd /tmp && bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --output-dir "$OUTPUT_DIR" >/dev/null)

CROSS_SUMMARY="$OUTPUT_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"
CONSISTENCY_REPORT="$OUTPUT_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"

if [[ ! -f "$CROSS_SUMMARY" ]]; then
  fail "expected cross summary to be generated"
fi

if [[ ! -f "$CONSISTENCY_REPORT" ]]; then
  fail "expected consistency report to be generated"
fi

if ! rg -n "^- linux_examples_json: $RUN_SPECIFIC_JSON_REL$" "$CROSS_SUMMARY" >/dev/null; then
  fail "cross summary should prefer the run-specific linux examples json when it exists"
fi

if ! rg -n "\\| passed \\| 75 \\|" "$CROSS_SUMMARY" >/dev/null; then
  fail "cross summary should read metrics from the run-specific linux examples json"
fi

if ! rg -n "\\| linux_examples_json \\| $RUN_SPECIFIC_JSON_REL \\| YES \\|" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should track the run-specific linux examples json path"
fi

echo "[PASS] prepare_wave_b_b2 run-specific linux examples contract passed"
