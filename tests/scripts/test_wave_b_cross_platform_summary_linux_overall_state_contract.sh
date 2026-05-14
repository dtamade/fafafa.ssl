#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_cross_platform_summary_linux_overall_state_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"

mkdir -p "$WORK_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
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

write_linux_summary_missing_overall() {
  local target="$1"
  local run_id="$2"
  cat > "$target" <<EOF
# Wave B CI Gate Summary

- run_id: $run_id

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | fixture |
| run_all_module_tests | **PASS** | fixture |
| verify_examples_compile | **PASS** | fixture |
EOF
}

write_linux_summary_invalid_overall() {
  local target="$1"
  local run_id="$2"
  cat > "$target" <<EOF
# Wave B CI Gate Summary

- run_id: $run_id
- Overall Status: BROKEN

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | fixture |
| run_all_module_tests | **PASS** | fixture |
| verify_examples_compile | **PASS** | fixture |
EOF
}

MISSING_RUN_ID="cross_summary_linux_missing_overall"
MISSING_DIR="$WORK_DIR/missing"
mkdir -p "$MISSING_DIR"
write_linux_summary_missing_overall "$MISSING_DIR/linux_summary.md" "$MISSING_RUN_ID"
write_examples_json "$MISSING_DIR/examples.json"

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$MISSING_RUN_ID" \
  --linux-summary "$WORK_REL/missing/linux_summary.md" \
  --linux-examples "$WORK_REL/missing/examples.json" \
  --output "$WORK_REL/missing/cross_summary.md" >/dev/null

MISSING_OUTPUT="$MISSING_DIR/cross_summary.md"
if [[ ! -f "$MISSING_OUTPUT" ]]; then
  fail "expected missing-overall cross summary report"
fi

if ! rg -n "^\\| linux \\| READY \\| $WORK_REL/missing/linux_summary\\.md \\|$" "$MISSING_OUTPUT" >/dev/null; then
  fail "cross summary should normalize a Linux summary without Overall Status into linux state READY"
fi

if ! rg -n "^\\| overall \\| READY \\| PENDING \\| PENDING \\|$" "$MISSING_OUTPUT" >/dev/null; then
  fail "cross summary checklist overall row should stay in the legal READY state when Linux Overall Status is missing"
fi

if rg -n "UNKNOWN" "$MISSING_OUTPUT" >/dev/null; then
  fail "cross summary should not leak UNKNOWN into the report when Linux Overall Status is missing"
fi

INVALID_RUN_ID="cross_summary_linux_invalid_overall"
INVALID_DIR="$WORK_DIR/invalid"
mkdir -p "$INVALID_DIR"
write_linux_summary_invalid_overall "$INVALID_DIR/linux_summary.md" "$INVALID_RUN_ID"
write_examples_json "$INVALID_DIR/examples.json"

bash "$ROOT_DIR/scripts/generate_wave_b_cross_platform_summary.sh" \
  --run-id "$INVALID_RUN_ID" \
  --linux-summary "$WORK_REL/invalid/linux_summary.md" \
  --linux-examples "$WORK_REL/invalid/examples.json" \
  --output "$WORK_REL/invalid/cross_summary.md" >/dev/null

INVALID_OUTPUT="$INVALID_DIR/cross_summary.md"
if [[ ! -f "$INVALID_OUTPUT" ]]; then
  fail "expected invalid-overall cross summary report"
fi

if ! rg -n "^\\| linux \\| READY \\| $WORK_REL/invalid/linux_summary\\.md \\|$" "$INVALID_OUTPUT" >/dev/null; then
  fail "cross summary should normalize an invalid Linux Overall Status into linux state READY"
fi

if ! rg -n "^\\| overall \\| READY \\| PENDING \\| PENDING \\|$" "$INVALID_OUTPUT" >/dev/null; then
  fail "cross summary checklist overall row should stay in the legal READY state when Linux Overall Status is invalid"
fi

if rg -n "BROKEN" "$INVALID_OUTPUT" >/dev/null; then
  fail "cross summary should not leak raw invalid Linux Overall Status into the report"
fi

echo "[PASS] wave-b cross-platform summary linux overall state contract passed"
