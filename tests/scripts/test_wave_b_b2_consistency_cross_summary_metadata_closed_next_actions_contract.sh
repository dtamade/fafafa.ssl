#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_metadata_closed_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

write_linux_summary() {
  local path="$1"
  local run_id="$2"
  cat > "$path" <<EOF
# Wave B CI Gate Summary

- run_id: $run_id
- Overall Status: PASS
EOF
}

write_examples_json() {
  local path="$1"
  cat > "$path" <<'EOF'
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

write_closure_report() {
  local path="$1"
  local run_id="$2"
  local linux_summary_rel="$3"
  cat > "$path" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $run_id
- closure_status: **CLOSED**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | $linux_summary_rel |
| macos | PENDING | no evidence | |
| windows | PENDING | no evidence | |
EOF
}

run_scenario() {
  local scenario="$1"
  local run_id="$2"
  local expected_note="$3"

  local scenario_dir="$WORK_DIR/$scenario"
  local output_rel="$WORK_REL/$scenario/consistency.md"
  local output_abs="$ROOT_DIR/$output_rel"
  local exit_code=0

  mkdir -p "$scenario_dir"
  write_linux_summary "$scenario_dir/linux_summary.md" "$run_id"
  write_examples_json "$scenario_dir/examples.json"
  write_closure_report "$scenario_dir/closure.md" "$run_id" "$WORK_REL/$scenario/linux_summary.md"

  case "$scenario" in
    linux_examples_missing)
      cat > "$scenario_dir/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $run_id
- generated_at: fake
- linux_summary: $WORK_REL/$scenario/linux_summary.md

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/$scenario/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PENDING | no evidence |
EOF
      ;;
    macos_probe_metadata_missing)
      cat > "$scenario_dir/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $run_id
- generated_at: fake
- linux_summary: $WORK_REL/$scenario/linux_summary.md
- linux_examples_json: $WORK_REL/$scenario/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/$scenario/linux_summary.md |
| macos | PROBE_ONLY | probe metadata lost |
| windows | PENDING | no evidence |
EOF
      ;;
    windows_active_metadata_missing)
      cat > "$scenario_dir/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $run_id
- generated_at: fake
- linux_summary: $WORK_REL/$scenario/linux_summary.md
- linux_examples_json: $WORK_REL/$scenario/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/$scenario/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PASS | metadata lost |
EOF
      ;;
    *)
      fail "unknown scenario: $scenario"
      ;;
  esac

  set +e
  bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
    --run-id "$run_id" \
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
    fail "strict consistency should reject cross_summary metadata/path issue in scenario $scenario"
  fi

  if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$output_abs" >/dev/null; then
    fail "consistency should become INCONSISTENT in scenario $scenario"
  fi

  if ! rg -n "^- runid_mismatch_or_parse_issue: 1$" "$output_abs" >/dev/null; then
    fail "cross_summary metadata/path issue should count as one parse issue in scenario $scenario"
  fi

  if ! rg -n "^- closure_status_note: CLOSED$" "$output_abs" >/dev/null; then
    fail "closure_status_note should stay CLOSED when the closure report is valid in scenario $scenario"
  fi

  if ! rg -n "^\\| cross_summary \\| $WORK_REL/$scenario/cross_summary\\.md \\| YES \\| $run_id \\| YES \\| $expected_note \\|" "$output_abs" >/dev/null; then
    fail "cross_summary row should surface $expected_note in scenario $scenario"
  fi

  if ! rg -n "当前 closure 已闭环，但 evidence consistency 仍未对齐" "$output_abs" >/dev/null; then
    fail "next actions should preserve the closed-closure guidance in scenario $scenario"
  fi

  if rg -n "closure_status_note=IN_PROGRESS|当前 evidence consistency 与 closure 元数据至少有一层未对齐" "$output_abs" >/dev/null; then
    fail "next actions must not fall back to IN_PROGRESS or generic guidance in scenario $scenario"
  fi
}

run_scenario "linux_examples_missing" "consistency_cross_summary_linux_examples_closed_truth" "linux_examples_json missing"
run_scenario "macos_probe_metadata_missing" "consistency_cross_summary_macos_probe_closed_truth" "macos probe metadata missing"
run_scenario "windows_active_metadata_missing" "consistency_cross_summary_windows_active_closed_truth" "windows active evidence metadata missing"

echo "[PASS] wave-b-b2 consistency cross summary metadata closed next-actions contract passed"
