#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh"
WORK_REL="tmp/test_wave_b_b2_handoff_run_id_scoped_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
OUT_DIR_REL="$WORK_REL/out"
TARGET_RUN="wb2_handoff_target_$$"
OTHER_RUN="wb2_handoff_other_$$"

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

echo "[TEST] wave-b b2 handoff bundle run-id scoped linux summary contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${TARGET_RUN}.md" <<EOF_TARGET
# Wave B CI Gate Summary

- run_id: $TARGET_RUN
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | target |
| run_all_module_tests | **PASS** | target |
| verify_examples_compile | **PASS** | target |
EOF_TARGET

cat > "$REPORTS_DIR/examples_compile_ci_gate.json" <<'EOF_JSON'
{
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 0,
    "skipped": 4,
    "pass_rate": "94.7%"
  }
}
EOF_JSON

sleep 1

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${OTHER_RUN}.md" <<EOF_OTHER
# Wave B CI Gate Summary

- run_id: $OTHER_RUN
- Overall Status: FAIL

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **FAIL** | distractor |
| run_all_module_tests | **FAIL** | distractor |
| verify_examples_compile | **FAIL** | distractor |
EOF_OTHER

cd "$ROOT_DIR"
FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
  bash "$SCRIPT" \
    --run-id "$TARGET_RUN" \
    --linux-examples "$REPORTS_REL/examples_compile_ci_gate.json" \
    --output-dir "$OUT_DIR_REL" >/dev/null

CROSS_FILE="$ROOT_DIR/$OUT_DIR_REL/wave_b_cross_platform_summary_${TARGET_RUN}.md"
[[ -f "$CROSS_FILE" ]] || fail "cross summary should be generated"

assert_contains "$CROSS_FILE" "- linux_summary: $REPORTS_REL/wave_b_ci_gate_summary_${TARGET_RUN}.md"

BUNDLE_FILE="$ROOT_DIR/$OUT_DIR_REL/wave_b_b2_handoff_bundle_${TARGET_RUN}.md"
[[ -f "$BUNDLE_FILE" ]] || fail "handoff bundle should be generated"
assert_contains "$BUNDLE_FILE" "| wave_b_ci_gate_summary_${TARGET_RUN}.md | $REPORTS_REL/wave_b_ci_gate_summary_${TARGET_RUN}.md | YES |"

echo "[PASS] wave-b b2 handoff bundle run-id scoped linux summary contract passed"
