#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_generic_linux_examples_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_generic_linux_examples"
GENERIC_JSON_REL="test-reports/examples_compile_ci_gate.json"
GENERIC_JSON_ABS="$ROOT_DIR/$GENERIC_JSON_REL"
BACKUP_JSON_ABS="$ROOT_DIR/tmp/test_wave_b_b2_consistency_generic_linux_examples_backup_$$.json"
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

cat > "$GENERIC_JSON_ABS" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 74,
    "failed": 0,
    "skipped": 1,
    "pass_rate": "98.7%"
  }
}
EOF

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
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

if [[ "$exit_code" -ne 0 ]]; then
  fail "generic linux examples fallback should keep strict consistency green when cross summary already uses the generic json"
fi

if ! rg -n "^- linux_examples_json: $GENERIC_JSON_REL$" "$ROOT_DIR/$WORK_REL/cross_summary.md" >/dev/null; then
  fail "cross summary should prove that the generic linux examples json is the active evidence path"
fi

if ! rg -n "^\\| linux_examples_json \\| $GENERIC_JSON_REL \\| YES \\|" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should track the generic linux examples json path when run-specific json is absent"
fi

if ! rg -n "consistency_status: \\*\\*CONSISTENT\\*\\*" "$OUTPUT_ABS" >/dev/null; then
  fail "consistency report should stay CONSISTENT when generic linux examples fallback is the active evidence path"
fi

echo "[PASS] wave-b-b2 consistency generic linux examples fallback contract passed"
