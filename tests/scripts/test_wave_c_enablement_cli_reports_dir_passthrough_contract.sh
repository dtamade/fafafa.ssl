#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_enablement_cli_reports_dir_passthrough_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
QUICK_REL="$WORK_REL/quick reports"
QUICK_DIR="$ROOT_DIR/$QUICK_REL"
ENABLE_REL="$WORK_REL/enable reports"
ENABLE_DIR="$ROOT_DIR/$ENABLE_REL"
B101_GLOB_REL="$WORK_REL/b101_reports/*/wave_c_b101_validation_*.md"

RUN_BUNDLE="wave_c_enablement_cli_bundle_$$"
RUN_ENABLE="wave_c_enablement_cli_enable_$$"
RUN_PREFLIGHT="wave_c_enablement_cli_preflight_$$"
WORKFLOW_FILE_REL="$WORK_REL/wave-c-quick-sprint-manual.yml.disabled"
SIGNOFF_REL="$WORK_REL/signoff.md"
ACCEPTANCE_REL="$WORK_REL/acceptance.md"
ROOT_WORKFLOW="$ROOT_DIR/.github/workflows/wave-c-quick-sprint-manual.yml"
CREATED_ROOT_WORKFLOW=0

cleanup() {
  rm -rf "$WORK_DIR"
  if [[ "$CREATED_ROOT_WORKFLOW" -eq 1 ]]; then
    rm -f "$ROOT_WORKFLOW"
  fi
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

echo "[TEST] wave c enablement cli reports-dir passthrough contract"

mkdir -p "$WORK_DIR/b101_reports/run1" "$WORK_DIR/b101_reports/run2"

for run in run1 run2; do
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
FAFAFA_WAVE_C_B101_VALIDATION_GLOB="$B101_GLOB_REL" \
  bash scripts/run_wave_c_quick_sprint_bundle.sh --run-id "$RUN_BUNDLE" --reports-dir "$QUICK_REL"

bash scripts/check_wave_c_workflow_enable_prereq.sh \
  --run-id "$RUN_ENABLE" \
  --reports-dir "$ENABLE_REL" \
  --signoff-record "$SIGNOFF_REL" \
  --acceptance "$ACCEPTANCE_REL" \
  --workflow "$WORKFLOW_FILE_REL"

bash scripts/prepare_wave_c_b116_enablement_packet.sh \
  --run-id "$RUN_ENABLE" \
  --reports-dir "$ENABLE_REL" \
  --signoff-record "$SIGNOFF_REL"

bash scripts/check_wave_c_first_run_preflight.sh \
  --run-id "$RUN_PREFLIGHT" \
  --reports-dir "$ENABLE_REL" \
  --quick-reports-dir "$QUICK_REL"

[[ -f "$QUICK_DIR/wave_c_quick_sprint_bundle_${RUN_BUNDLE}.md" ]] || fail "bundle should be generated under cli quick reports dir"
[[ -f "$ENABLE_DIR/wave_c_b115_workflow_enable_prereq_${RUN_ENABLE}.md" ]] || fail "B115 should be generated under cli enablement reports dir"
[[ -f "$ENABLE_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md" ]] || fail "B116 should be generated under cli enablement reports dir"
[[ -f "$ENABLE_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md" ]] || fail "B119 should be generated under cli enablement reports dir"

assert_contains "$ENABLE_DIR/wave_c_b116_enablement_request_packet_${RUN_ENABLE}.md" "- prereq_report: $ENABLE_REL/wave_c_b115_workflow_enable_prereq_${RUN_ENABLE}.md"
assert_contains "$ENABLE_DIR/wave_c_b119_first_run_preflight_${RUN_PREFLIGHT}.md" "- latest_bundle: $QUICK_REL/wave_c_quick_sprint_bundle_${RUN_BUNDLE}.md"

echo "[PASS] wave c enablement cli reports-dir passthrough contract passed"
