#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_missing_report_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="handoff_missing_report_truth"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

write_linux_summary() {
  local path="$1"
  cat > "$path" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
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

write_cross_summary_stub() {
  local path="$1"
  cat > "$path" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
RUN_ID=""
LINUX_SUMMARY=""
LINUX_EXAMPLES=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) OUTPUT="$2"; shift 2 ;;
    --run-id) RUN_ID="$2"; shift 2 ;;
    --linux-summary) LINUX_SUMMARY="$2"; shift 2 ;;
    --linux-examples) LINUX_EXAMPLES="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$(dirname "$OUTPUT")"
cat > "$OUTPUT" <<OUT
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
- linux_summary: $LINUX_SUMMARY
- linux_examples_json: $LINUX_EXAMPLES
OUT
EOF
}

write_closure_stub() {
  local path="$1"
  cat > "$path" <<EOF
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
while [[ \$# -gt 0 ]]; do
  case "\$1" in
    --output) OUTPUT="\$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "\$(dirname "\$OUTPUT")"
cat > "\$OUTPUT" <<OUT
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **CLOSED**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | linux.md |
| macos | PASS | ok | macos.md |
| windows | PASS | ok | windows.md |
OUT
EOF
}

write_consistency_stub() {
  local path="$1"
  cat > "$path" <<EOF
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
while [[ \$# -gt 0 ]]; do
  case "\$1" in
    --output) OUTPUT="\$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "\$(dirname "\$OUTPUT")"
cat > "\$OUTPUT" <<OUT
# Wave B / B2 Evidence Consistency

- run_id: $RUN_ID
- generated_at: fake
- consistency_status: **CONSISTENT**
- strict_mode: false
- required_missing: 0
- runid_mismatch_or_parse_issue: 0
- closure_status_note: CLOSED
OUT
EOF
}

write_missing_output_stub() {
  local path="$1"
  cat > "$path" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
OUTPUT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) OUTPUT="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$(dirname "$OUTPUT")"
exit 0
EOF
}

run_scenario() {
  local scenario="$1"
  local missing_report="$2"
  local expect_note="$3"

  local scenario_dir="$WORK_DIR/$scenario"
  local fake_root="$scenario_dir/fake_project"
  local fake_scripts="$fake_root/scripts"
  local report_abs="$scenario_dir/out/wave_b_b2_handoff_bundle_${RUN_ID}.md"

  mkdir -p "$fake_scripts" "$scenario_dir/out"

  cp "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" "$fake_scripts/prepare_wave_b_b2_handoff_bundle.sh"
  write_cross_summary_stub "$fake_scripts/generate_wave_b_cross_platform_summary.sh"

  if [[ "$missing_report" == "closure" ]]; then
    write_missing_output_stub "$fake_scripts/check_wave_b_b2_closure_readiness.sh"
    write_consistency_stub "$fake_scripts/check_wave_b_b2_evidence_consistency.sh"
  else
    write_closure_stub "$fake_scripts/check_wave_b_b2_closure_readiness.sh"
    write_missing_output_stub "$fake_scripts/check_wave_b_b2_evidence_consistency.sh"
  fi

  write_linux_summary "$scenario_dir/linux_summary.md"
  write_examples_json "$scenario_dir/examples.json"

  (
    cd "$fake_root" &&
    bash "$fake_scripts/prepare_wave_b_b2_handoff_bundle.sh" \
      --run-id "$RUN_ID" \
      --linux-summary "$scenario_dir/linux_summary.md" \
      --linux-examples "$scenario_dir/examples.json" \
      --output-dir "$scenario_dir/out" >/dev/null
  )

  if [[ ! -f "$report_abs" ]]; then
    fail "expected handoff bundle report for scenario $scenario"
  fi

  if ! rg -n "^- handoff_state: \\*\\*NEEDS_REPORT_REPAIR\\*\\*$" "$report_abs" >/dev/null; then
    fail "handoff bundle should downgrade to NEEDS_REPORT_REPAIR when downstream report file is missing in scenario $scenario"
  fi

  if ! rg -n "report_chain_note: .*${expect_note}" "$report_abs" >/dev/null; then
    fail "handoff bundle should surface ${expect_note} in scenario $scenario"
  fi

  if ! rg -n "修复或重建下游 report metadata（.*${expect_note}.*）" "$report_abs" >/dev/null; then
    fail "next actions should stay on the generic report-repair branch and mention ${expect_note} in scenario $scenario"
  fi

  if rg -n "^- handoff_state: \\*\\*READY_FOR_RUNNER\\*\\*$|^- handoff_state: \\*\\*CLOSED\\*\\*$|^- handoff_state: \\*\\*NEEDS_EVIDENCE_SYNC\\*\\*$" "$report_abs" >/dev/null; then
    fail "handoff bundle must not keep a normal or evidence-sync state when downstream report file is missing in scenario $scenario"
  fi
}

run_scenario "closure_report_missing" "closure" "closure_report missing"
run_scenario "consistency_report_missing" "consistency" "consistency_report missing"

echo "[PASS] prepare_wave_b_b2 handoff bundle missing-report contract passed"
