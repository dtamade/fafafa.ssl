#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_run_id_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="consistency_cross_summary_run_id_truth"
BAD_RUN_ID="consistency_cross_summary_run_id_other"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

write_linux_summary() {
  local target="$1"
  cat > "$target" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF
}

write_examples_json() {
  local target="$1"
  cat > "$target" <<'EOF'
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
}

write_cross_summary() {
  local target="$1"
  local run_id_line="$2"
  cat > "$target" <<EOF
# Wave B Cross-Platform Summary

${run_id_line}
- linux_summary: $WORK_REL/linux_summary.md
- linux_examples_json: $WORK_REL/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PENDING | no evidence |
EOF
}

write_closure_report() {
  local target="$1"
  cat > "$target" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **IN_PROGRESS**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | $WORK_REL/linux_summary.md |
| macos | PENDING | no evidence | |
| windows | PENDING | no evidence | |
EOF
}

run_scenario() {
  local scenario="$1"
  local run_id_line="$2"
  local expected_note="$3"
  local expected_parsed="$4"

  local scenario_dir="$WORK_DIR/$scenario"
  local output_rel="$WORK_REL/$scenario/consistency.md"
  local output_abs="$ROOT_DIR/$output_rel"
  local exit_code=0

  mkdir -p "$scenario_dir"
  write_linux_summary "$scenario_dir/linux_summary.md"
  write_examples_json "$scenario_dir/examples.json"
  write_cross_summary "$scenario_dir/cross_summary.md" "$run_id_line"
  write_closure_report "$scenario_dir/closure.md"

  set +e
  bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
    --run-id "$RUN_ID" \
    --linux-summary "$WORK_REL/$scenario/linux_summary.md" \
    --linux-examples "$WORK_REL/$scenario/examples.json" \
    --cross-summary "$WORK_REL/$scenario/cross_summary.md" \
    --closure-report "$WORK_REL/$scenario/closure.md" \
    --strict \
    --output "$output_rel" >/dev/null 2>&1
  exit_code=$?
  set -e

  if [[ ! -f "$output_abs" ]]; then
    fail "expected consistency report for scenario $scenario"
  fi

  if [[ "$exit_code" -eq 0 ]]; then
    fail "strict consistency should reject cross_summary run_id issue in scenario $scenario"
  fi

  if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$output_abs" >/dev/null; then
    fail "consistency should become INCONSISTENT in scenario $scenario"
  fi

  if ! rg -n "^- runid_mismatch_or_parse_issue: 1$" "$output_abs" >/dev/null; then
    fail "cross_summary run_id issue should count as one parse issue in scenario $scenario"
  fi

  if ! rg -n "^- closure_status_note: IN_PROGRESS$" "$output_abs" >/dev/null; then
    fail "closure_status_note should stay tied to the valid closure report in scenario $scenario"
  fi

  if ! rg -n "^\\| cross_summary \\| $WORK_REL/$scenario/cross_summary\\.md \\| YES \\| ${expected_parsed} \\| NO \\| ${expected_note} \\|" "$output_abs" >/dev/null; then
    fail "cross_summary row should surface ${expected_note} in scenario $scenario"
  fi

  if ! rg -n "closure_status_note=IN_PROGRESS" "$output_abs" >/dev/null; then
    fail "next actions should preserve the in-progress closure guidance in scenario $scenario"
  fi

  if rg -n "当前 closure 已闭环" "$output_abs" >/dev/null; then
    fail "next actions must not claim closure is already closed in scenario $scenario"
  fi
}

run_scenario "run_id_missing" "" "run_id not found" "n/a"
run_scenario "run_id_mismatch" "- run_id: $BAD_RUN_ID" "run_id mismatch" "$BAD_RUN_ID"

echo "[PASS] wave-b-b2 consistency cross_summary run-id contract passed"
