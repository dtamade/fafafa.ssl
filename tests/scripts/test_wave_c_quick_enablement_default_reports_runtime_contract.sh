#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_quick_enablement_default_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
QUICK_REL="$WORK_REL/quick_reports"
QUICK_DIR="$ROOT_DIR/$QUICK_REL"
ENABLE_REL="$WORK_REL/enable_reports"
ENABLE_DIR="$ROOT_DIR/$ENABLE_REL"
B101_GLOB_REL="$WORK_REL/b101_reports/*/wave_c_b101_validation_*.md"
LEGACY_DIR="$ROOT_DIR/test-reports"

RUN_BUNDLE="wave_c_quick_enablement_bundle_$$"
RUN_ENABLE="wave_c_quick_enablement_enable_$$"
RUN_PREFLIGHT="wave_c_quick_enablement_preflight_$$"
WORKFLOW_FILE_REL="$WORK_REL/wave-c-quick-sprint-manual.yml.disabled"
SIGNOFF_REL="$WORK_REL/signoff.md"
ACCEPTANCE_REL="$WORK_REL/acceptance.md"
ROOT_WORKFLOW="$ROOT_DIR/.github/workflows/wave-c-quick-sprint-manual.yml"
CREATED_ROOT_WORKFLOW=0

cleanup() {
  rm -rf "$WORK_DIR"
  rm -f     "$LEGACY_DIR/wave_c_quick_sprint_bundle_${RUN_BUNDLE}.md"     "$LEGACY_DIR/wave_c_b107_threshold_eval_${RUN_BUNDLE}.md"     "$LEGACY_DIR/wave_c_b108_default_on_readiness_${RUN_BUNDLE}.md"     "$LEGACY_DIR/wave_c_b109_canary_rollout_${RUN_BUNDLE}.md"     "$LEGACY_DIR/wave_c_b110_rollback_drill_${RUN_BUNDLE}.md"     "$LEGACY_DIR/wave_c_b115_workflow_enable_prereq_${RUN_ENABLE}.md"     "$LEGACY_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md"     "$LEGACY_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md"     "$LEGACY_DIR/wave_c_b120_post_trigger_observability_${RUN_BUNDLE}.md"
  if [[ "$CREATED_ROOT_WORKFLOW" -eq 1 ]]; then
    rm -f "$ROOT_WORKFLOW"
  fi
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave c quick enablement default reports runtime contract"

mkdir -p   "$WORK_DIR/b101_reports/run1"   "$WORK_DIR/b101_reports/run2"   "$WORK_DIR/b101_reports/run3"

for run in run1 run2 run3; do
  cat > "$WORK_DIR/b101_reports/${run}/wave_c_b101_validation_${run}.md" <<EOF_REPORT
# Wave C B101 Validation

- run_id: ${run}
- overall: **PASS**
- hit_rate_percent: 99.9
- speedup_factor_x: 3.5
EOF_REPORT
done

cat > "$ROOT_DIR/$SIGNOFF_REL" <<'EOF_SIGNOFF'
# Signoff

- signoff_state: APPROVED
EOF_SIGNOFF

cat > "$ROOT_DIR/$ACCEPTANCE_REL" <<'EOF_ACCEPT'
# Acceptance

Quick bundle overall: PASS
EOF_ACCEPT

cat > "$ROOT_DIR/$WORKFLOW_FILE_REL" <<'EOF_WF'
name: Wave C quick sprint manual
on:
  workflow_dispatch:
EOF_WF

if [[ ! -f "$ROOT_WORKFLOW" ]]; then
  mkdir -p "$(dirname "$ROOT_WORKFLOW")"
  cat > "$ROOT_WORKFLOW" <<'EOF_ROOT_WF'
name: Wave C quick sprint manual
on:
  workflow_dispatch:
EOF_ROOT_WF
  CREATED_ROOT_WORKFLOW=1
fi

cd "$ROOT_DIR"
FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR="$QUICK_REL" FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR="$ENABLE_REL" FAFAFA_WAVE_C_B101_VALIDATION_GLOB="$B101_GLOB_REL" bash scripts/run_wave_c_quick_sprint_bundle.sh --run-id "$RUN_BUNDLE"

FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR="$ENABLE_REL" bash scripts/check_wave_c_workflow_enable_prereq.sh   --run-id "$RUN_ENABLE"   --signoff-record "$SIGNOFF_REL"   --acceptance "$ACCEPTANCE_REL"   --workflow "$WORKFLOW_FILE_REL"

FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR="$ENABLE_REL" bash scripts/prepare_wave_c_b116_enablement_packet.sh   --run-id "$RUN_ENABLE"   --signoff-record "$SIGNOFF_REL"

FAFAFA_WAVE_C_ENABLEMENT_REPORTS_DIR="$ENABLE_REL" FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR="$QUICK_REL" bash scripts/check_wave_c_first_run_preflight.sh --run-id "$RUN_PREFLIGHT"

FAFAFA_WAVE_C_QUICK_SPRINT_REPORTS_DIR="$QUICK_REL" bash scripts/check_wave_c_post_trigger_observability.sh --run-id "$RUN_BUNDLE"

[[ -f "$QUICK_DIR/wave_c_b107_threshold_eval_${RUN_BUNDLE}.md" ]] || fail "default B107 report should be written under quick reports dir"
[[ -f "$QUICK_DIR/wave_c_b108_default_on_readiness_${RUN_BUNDLE}.md" ]] || fail "default B108 report should be written under quick reports dir"
[[ -f "$QUICK_DIR/wave_c_b109_canary_rollout_${RUN_BUNDLE}.md" ]] || fail "default B109 report should be written under quick reports dir"
[[ -f "$QUICK_DIR/wave_c_b110_rollback_drill_${RUN_BUNDLE}.md" ]] || fail "default B110 report should be written under quick reports dir"
[[ -f "$QUICK_DIR/wave_c_quick_sprint_bundle_${RUN_BUNDLE}.md" ]] || fail "default bundle report should be written under quick reports dir"
[[ -f "$QUICK_DIR/wave_c_b120_post_trigger_observability_${RUN_BUNDLE}.md" ]] || fail "default B120 report should be written under quick reports dir"

[[ -f "$ENABLE_DIR/wave_c_b115_workflow_enable_prereq_${RUN_ENABLE}.md" ]] || fail "default B115 report should be written under enablement reports dir"
[[ -f "$ENABLE_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md" ]] || fail "default B116 report should be written under enablement reports dir"
[[ -f "$ENABLE_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md" ]] || fail "default B119 report should be written under enablement reports dir"

[[ ! -f "$LEGACY_DIR/wave_c_quick_sprint_bundle_${RUN_BUNDLE}.md" ]] || fail "default bundle report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b115_workflow_enable_prereq_${RUN_ENABLE}.md" ]] || fail "default B115 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md" ]] || fail "default B116 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md" ]] || fail "default B119 report should no longer be written under test-reports"
[[ ! -f "$LEGACY_DIR/wave_c_b120_post_trigger_observability_${RUN_BUNDLE}.md" ]] || fail "default B120 report should no longer be written under test-reports"

if ! rg -F --quiet -- "- prereq_report: $ENABLE_REL/wave_c_b115_workflow_enable_prereq_${RUN_ENABLE}.md" "$ENABLE_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md"; then
  sed -n '1,220p' "$ENABLE_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md" || true
  fail "B116 should default to the latest B115 report from enablement reports dir"
fi

if ! rg -F --quiet -- "- latest_bundle: $QUICK_REL/wave_c_quick_sprint_bundle_${RUN_BUNDLE}.md" "$ENABLE_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md"; then
  sed -n '1,220p' "$ENABLE_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md" || true
  fail "B119 should default to the latest quick sprint bundle from quick reports dir"
fi

echo "[PASS] wave c quick enablement default reports runtime contract passed"
