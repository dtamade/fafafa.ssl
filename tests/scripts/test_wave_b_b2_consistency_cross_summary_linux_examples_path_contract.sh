#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_linux_examples_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_linux_examples"
CUSTOM_JSON_REL="$WORK_REL/custom_examples.json"
CUSTOM_JSON_ABS="$ROOT_DIR/$CUSTOM_JSON_REL"
GENERIC_JSON_REL="test-reports/examples_compile_ci_gate.json"
GENERIC_JSON_ABS="$ROOT_DIR/$GENERIC_JSON_REL"
BACKUP_JSON_ABS="$ROOT_DIR/tmp/test_wave_b_b2_consistency_cross_summary_linux_examples_backup_$$.json"
OUTPUT_REL="$WORK_REL/consistency.md"
OUTPUT_ABS="$ROOT_DIR/$OUTPUT_REL"

cleanup() {
  if [[ -f "$BACKUP_JSON_ABS" ]]; then
    mv "$BACKUP_JSON_ABS" "$GENERIC_JSON_ABS"
  else
    rm -f "$GENERIC_JSON_ABS"
  fi
  rm -rf "$WORK_DIR"
}

mkdir -p "$WORK_DIR" "$(dirname "$GENERIC_JSON_ABS")" "$(dirname "$BACKUP_JSON_ABS")"
trap cleanup EXIT

if [[ -f "$GENERIC_JSON_ABS" ]]; then
  cp "$GENERIC_JSON_ABS" "$BACKUP_JSON_ABS"
fi

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
    "passed": 73,
    "failed": 0,
    "skipped": 2,
    "pass_rate": "97.3%"
  }
}
EOF

cat > "$GENERIC_JSON_ABS" <<'EOF'
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
  --linux-examples "$CUSTOM_JSON_REL" \
  --output "$WORK_REL/cross_summary.md" >/dev/null

bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --output "$WORK_REL/closure.md" >/dev/null

printf '{ bad json\n' > "$CUSTOM_JSON_ABS"

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
  fail "consistency should fail strict mode when the active custom linux examples json recorded by cross summary becomes invalid"
fi

if ! rg -n "^- linux_examples_json: $CUSTOM_JSON_REL$" "$ROOT_DIR/$WORK_REL/cross_summary.md" >/dev/null; then
  fail "cross summary should record the active custom linux examples json path"
fi

if ! rg -n "^\\| linux_examples_json \\| $CUSTOM_JSON_REL \\| YES \\| n/a \\| n/a \\| json_valid=NO \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should inherit the active custom linux examples json path from cross summary and mark it invalid"
fi

if ! rg -n "consistency_status: \\*\\*INCONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should become INCONSISTENT when the active custom linux examples json is invalid"
fi

echo "[PASS] wave-b-b2 consistency cross summary linux examples path contract passed"
