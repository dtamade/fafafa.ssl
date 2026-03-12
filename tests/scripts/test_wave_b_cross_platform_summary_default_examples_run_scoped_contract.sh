#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh"
WORK_REL="tmp/test_wave_b_cross_examples_default_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_cross_examples_$$"
OTHER_RUN="wave_b_cross_examples_other_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b cross-platform summary default examples run-scoped contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" <<EOF_SUMMARY
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
EOF_SUMMARY

cat > "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" <<EOF_TARGET
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
EOF_TARGET

cat > "$REPORTS_DIR/examples_compile_ci_gate.json" <<EOF_STALE
{
  "run_id": "$OTHER_RUN",
  "summary": {
    "total": 1,
    "passed": 0,
    "failed": 1,
    "skipped": 0,
    "pass_rate": "0.0%"
  }
}
EOF_STALE

FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
  bash "$SCRIPT" --run-id "$RUN_ID" >/dev/null

OUTPUT_FILE="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"
[[ -f "$OUTPUT_FILE" ]] || fail "cross-platform summary should be generated"

assert_contains "$OUTPUT_FILE" "- linux_examples_json: $REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json"
assert_contains "$OUTPUT_FILE" '| total | 75 |'
assert_contains "$OUTPUT_FILE" '| pass_rate | 94.7% |'

echo "[PASS] wave-b cross-platform summary default examples run-scoped contract passed"
