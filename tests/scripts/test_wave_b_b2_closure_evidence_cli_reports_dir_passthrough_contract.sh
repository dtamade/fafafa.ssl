#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_closure_evidence_cli_reports_dir_passthrough_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports dir"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_closure_evidence_cli_$$"
LEGACY_DIR="$ROOT_DIR/tmp/wave_b_reports"

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f \
    "$LEGACY_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md" \
    "$LEGACY_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
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

echo "[TEST] wave b b2 closure/evidence cli reports-dir passthrough contract"

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

cat > "$REPORTS_DIR/wave_b_cross_platform_summary_${RUN_ID}.md" <<EOF_CROSS
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
EOF_CROSS

cd "$ROOT_DIR"
bash scripts/check_wave_b_b2_closure_readiness.sh --run-id "$RUN_ID" --reports-dir "$REPORTS_REL"
bash scripts/check_wave_b_b2_evidence_consistency.sh --run-id "$RUN_ID" --reports-dir "$REPORTS_REL"

CLOSURE_FILE="$REPORTS_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md"
EVIDENCE_FILE="$REPORTS_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
[[ -f "$CLOSURE_FILE" ]] || fail "closure report should be generated under cli reports dir"
[[ -f "$EVIDENCE_FILE" ]] || fail "evidence report should be generated under cli reports dir"
[[ ! -f "$LEGACY_DIR/wave_b_b2_closure_readiness_${RUN_ID}.md" ]] || fail "closure report should not be written under default reports dir"
[[ ! -f "$LEGACY_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md" ]] || fail "evidence report should not be written under default reports dir"

assert_contains "$CLOSURE_FILE" "| linux | PASS | summary parsed | $REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md |"
assert_contains "$EVIDENCE_FILE" "| linux_summary | $REPORTS_REL/wave_b_ci_gate_summary_${RUN_ID}.md | YES | $RUN_ID | YES | ok |"
assert_contains "$EVIDENCE_FILE" "| cross_summary | $REPORTS_REL/wave_b_cross_platform_summary_${RUN_ID}.md | YES | $RUN_ID | YES | ok |"
assert_contains "$EVIDENCE_FILE" "| closure_report | $REPORTS_REL/wave_b_b2_closure_readiness_${RUN_ID}.md | YES | $RUN_ID | YES | ok |"
assert_contains "$EVIDENCE_FILE" "| linux_examples_json | $REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json | YES | $RUN_ID | YES | ok |"

echo "[PASS] wave b b2 closure/evidence cli reports-dir passthrough contract passed"
