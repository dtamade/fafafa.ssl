#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
SUMMARY_RUN_ID="handoff_infer_run_id"
RUN_SPECIFIC_JSON_REL="test-reports/examples_compile_ci_gate_${SUMMARY_RUN_ID}.json"
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

- run_id: $SUMMARY_RUN_ID
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
  --linux-summary "$WORK_REL/linux_summary.md" \
  --output-dir "$OUTPUT_DIR" >/dev/null)

CROSS_SUMMARY="$OUTPUT_DIR/wave_b_cross_platform_summary_${SUMMARY_RUN_ID}.md"
CLOSURE_REPORT="$OUTPUT_DIR/wave_b_b2_closure_readiness_${SUMMARY_RUN_ID}.md"
CONSISTENCY_REPORT="$OUTPUT_DIR/wave_b_b2_evidence_consistency_${SUMMARY_RUN_ID}.md"
BUNDLE_REPORT="$OUTPUT_DIR/wave_b_b2_handoff_bundle_${SUMMARY_RUN_ID}.md"

for file in "$CROSS_SUMMARY" "$CLOSURE_REPORT" "$CONSISTENCY_REPORT" "$BUNDLE_REPORT"; do
  if [[ ! -f "$file" ]]; then
    fail "expected derived-run-id artifact: $file"
  fi
done

if ! rg -n "^- run_id: $SUMMARY_RUN_ID$" "$CROSS_SUMMARY" >/dev/null; then
  fail "cross summary should inherit run_id from the Linux summary"
fi

if ! rg -n "^- run_id: $SUMMARY_RUN_ID$" "$CLOSURE_REPORT" >/dev/null; then
  fail "closure report should inherit run_id from the Linux summary"
fi

if ! rg -n "^- run_id: $SUMMARY_RUN_ID$" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should inherit run_id from the Linux summary"
fi

if ! rg -n "^- consistency_status: \\*\\*CONSISTENT\\*\\*$" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should stay CONSISTENT when only Linux evidence is present but aligned"
fi

if ! rg -n "^- run_id: $SUMMARY_RUN_ID$" "$BUNDLE_REPORT" >/dev/null; then
  fail "handoff bundle should inherit run_id from the Linux summary"
fi

echo "[PASS] prepare_wave_b_b2 infers run_id from Linux summary contract passed"
