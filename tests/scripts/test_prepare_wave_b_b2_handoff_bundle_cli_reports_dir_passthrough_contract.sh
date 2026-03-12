#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_handoff_cli_reports_dir_passthrough_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports dir"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_handoff_cli_reports_$$"
LEGACY_DIR="$ROOT_DIR/tmp/wave_b_reports"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f \
    "$LEGACY_DIR/wave_b_cross_platform_summary_${RUN_ID}.md" \
    "$LEGACY_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md" \
    "$LEGACY_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md" \
    "$LEGACY_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"
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

echo "[TEST] wave b handoff cli reports-dir passthrough contract"

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

cat > "$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json" <<EOF_JSON
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

bash "$SCRIPT" --run-id "$RUN_ID" --reports-dir "$REPORTS_REL" >/dev/null

BUNDLE_FILE="$REPORTS_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"
CROSS_FILE="$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md"
CLOSURE_FILE="$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"
CONSISTENCY_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"

[[ -f "$BUNDLE_FILE" ]] || fail "handoff bundle should be generated under cli reports dir"
[[ -f "$CROSS_FILE" ]] || fail "cross summary should be generated under cli reports dir"
[[ -f "$CLOSURE_FILE" ]] || fail "closure report should be generated under cli reports dir"
[[ -f "$CONSISTENCY_FILE" ]] || fail "consistency report should be generated under cli reports dir"

[[ ! -f "$LEGACY_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md" ]] || fail "handoff bundle should not be written under default reports dir"
[[ ! -f "$LEGACY_DIR/wave_b_cross_platform_summary_${RUN_ID}.md" ]] || fail "cross summary should not be written under default reports dir"
[[ ! -f "$LEGACY_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md" ]] || fail "closure report should not be written under default reports dir"
[[ ! -f "$LEGACY_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md" ]] || fail "consistency report should not be written under default reports dir"

assert_contains "$BUNDLE_FILE" "- linux_examples_selection: run_scoped_exact"
assert_contains "$BUNDLE_FILE" "| wave_b_ci_gate_summary_${RUN_ID}.md | $REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md | YES |"
assert_contains "$CONSISTENCY_FILE" "| linux_examples_json | $REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json | YES | $RUN_ID | YES | ok |"
assert_contains "$CROSS_FILE" "- linux_examples_json: $REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json"

echo "[PASS] wave b handoff cli reports-dir passthrough contract passed"
