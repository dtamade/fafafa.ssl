#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_partial_linux_examples_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_partial_linux_examples"
CUSTOM_JSON_REL="$WORK_REL/partial_examples.json"
CUSTOM_JSON_ABS="$ROOT_DIR/$CUSTOM_JSON_REL"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$WORK_DIR"
trap cleanup EXIT

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

cat > "$CUSTOM_JSON_ABS" <<'EOF'
{
  "summary": {
    "total": 75,
    "tested": 1,
    "passed": 1,
    "failed": 0,
    "skipped": 0,
    "remaining": 74,
    "stopped_early": true,
    "pass_rate": "100%"
  }
}
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$CUSTOM_JSON_REL" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --output "$WORK_REL/closure.md" >/dev/null

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --cross-summary "$WORK_REL/cross_summary.md" \
  --closure-report "$WORK_REL/closure.md" \
  --strict \
  --output "$OUTPUT_REL" >/dev/null 2>&1
exit_code=$?
set -e

if [[ ! -f "$OUTPUT_ABS" ]]; then
  fail "expected consistency report to be generated"
fi

if [[ "$exit_code" -eq 0 ]]; then
  fail "strict consistency should fail when the active linux examples report is explicitly partial"
fi

if ! rg -n "^- linux_examples_json: $CUSTOM_JSON_REL$" "$ROOT_DIR/$WORK_REL/cross_summary.md" >/dev/null; then
  fail "cross summary should record the active partial linux examples json path"
fi

if ! rg -n "^\\| linux_examples_json \\| $CUSTOM_JSON_REL \\| YES \\| n/a \\| n/a \\| json_valid=YES; partial_examples_report=YES" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should surface partial linux examples evidence instead of treating it as an ordinary valid json"
fi

if ! rg -n "consistency_status: \\*\\*INCONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when active linux examples evidence is partial"
fi

echo "[PASS] wave-b-b2 consistency partial linux examples contract passed"
