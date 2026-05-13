#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_absolute_output_path_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
ABS_OUT_BASE="$WORK_DIR/absolute-output"

mkdir -p "$ABS_OUT_BASE"
ABS_OUT_BASE="$(cd "$ABS_OUT_BASE" && pwd)"

trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

write_fixtures() {
  cat > "$WORK_DIR/linux_summary.md" <<'EOF'
# Wave B CI Gate Summary

- run_id: abs_output_contract
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | all compiled |
| run_all_module_tests | **PASS** | all passed |
| verify_examples_compile | **PASS** | 75/75 |
EOF

  cat > "$WORK_DIR/examples.json" <<'EOF'
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

  cat > "$WORK_DIR/macos_summary.md" <<'EOF'
# Wave B macOS Gate Summary

- run_id: abs_output_contract
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | all compiled |
| modules | **PASS** | all passed |
| examples | **PASS** | 75/75 |
EOF
}

assert_file_exists() {
  local file="$1"
  local label="$2"
  if [[ ! -f "$file" ]]; then
    fail "$label should be written to absolute path: $file"
  fi
}

write_fixtures

CROSS_OUT="$ABS_OUT_BASE/cross_platform_summary.md"
CLOSURE_OUT="$ABS_OUT_BASE/closure_readiness.md"
CONSISTENCY_OUT="$ABS_OUT_BASE/evidence_consistency.md"
BUNDLE_OUT_DIR="$ABS_OUT_BASE/handoff-bundle"

mkdir -p "$BUNDLE_OUT_DIR"

(cd /tmp && bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id abs_output_contract \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --output "$CROSS_OUT" >/dev/null)

assert_file_exists "$CROSS_OUT" "cross-platform summary"

(cd /tmp && bash "$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh" \
  --run-id abs_output_contract \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --output "$CLOSURE_OUT" >/dev/null)

assert_file_exists "$CLOSURE_OUT" "closure readiness report"

(cd /tmp && bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id abs_output_contract \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --cross-summary "$CROSS_OUT" \
  --closure-report "$CLOSURE_OUT" \
  --output "$CONSISTENCY_OUT" >/dev/null)

assert_file_exists "$CONSISTENCY_OUT" "evidence consistency report"

(cd /tmp && bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id abs_output_contract \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --output-dir "$BUNDLE_OUT_DIR" >/dev/null)

assert_file_exists "$BUNDLE_OUT_DIR/wave_b_cross_platform_summary_abs_output_contract.md" "handoff cross summary"
assert_file_exists "$BUNDLE_OUT_DIR/wave_b_b2_closure_readiness_abs_output_contract.md" "handoff closure readiness"
assert_file_exists "$BUNDLE_OUT_DIR/wave_b_b2_evidence_consistency_abs_output_contract.md" "handoff evidence consistency"
assert_file_exists "$BUNDLE_OUT_DIR/wave_b_b2_handoff_bundle_abs_output_contract.md" "handoff bundle report"

echo "[PASS] wave-b-b2 absolute output path contract passed"
