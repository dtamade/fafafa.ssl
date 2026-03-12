#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_handoff_examples_warning_override_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_handoff_warning_override_$$"
OVERRIDE_REL="$REPORTS_REL/custom_examples_override.json"
OVERRIDE_FILE="$ROOT_DIR/$OVERRIDE_REL"
WARNING_TEXT="explicit override in use; verify owner run_id/path manually"

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
    sed -n '1,320p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave-b handoff examples warning explicit override contract"

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

cat > "$OVERRIDE_FILE" <<EOF_JSON
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
EOF_JSON

FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
FAFAFA_WAVE_B_EXAMPLES_REPORT_REL="$OVERRIDE_REL" \
  bash "$SCRIPT" --run-id "$RUN_ID" --linux-summary "$REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md" --output-dir "$REPORTS_REL" >/dev/null

BUNDLE_FILE="$REPORTS_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"
CONSISTENCY_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
[[ -f "$BUNDLE_FILE" ]] || fail "handoff bundle should be generated"
[[ -f "$CONSISTENCY_FILE" ]] || fail "consistency report should be generated"

assert_contains "$BUNDLE_FILE" "- linux_examples_selection: explicit_override"
assert_contains "$BUNDLE_FILE" "- linux_examples_warning: $WARNING_TEXT"
assert_contains "$CONSISTENCY_FILE" "- linux_examples_warning: $WARNING_TEXT"

echo "[PASS] wave-b handoff examples warning explicit override contract passed"
