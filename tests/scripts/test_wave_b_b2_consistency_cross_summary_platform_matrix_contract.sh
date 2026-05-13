#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_platform_matrix_$(date +%s)"
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

write_closure() {
  local path="$1"
  local run_id="$2"
  local linux_rel="$3"
  cat > "$path" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $run_id
- closure_status: **IN_PROGRESS**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | $linux_rel |
| macos | PENDING | no evidence | |
| windows | PENDING | no evidence | |
EOF
}

# Scenario 1: cross summary platform table is missing the windows row entirely.
MISSING_ROW_RUN_ID="consistency_cross_summary_missing_row"
MISSING_ROW_DIR="$WORK_DIR/missing_row"
mkdir -p "$MISSING_ROW_DIR"
write_linux_summary "$MISSING_ROW_DIR/linux_summary.md" "$MISSING_ROW_RUN_ID"
write_examples_json "$MISSING_ROW_DIR/examples.json"
write_closure "$MISSING_ROW_DIR/closure.md" "$MISSING_ROW_RUN_ID" "$WORK_REL/missing_row/linux_summary.md"
cat > "$MISSING_ROW_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $MISSING_ROW_RUN_ID
- generated_at: fake
- linux_summary: $WORK_REL/missing_row/linux_summary.md
- linux_examples_json: $WORK_REL/missing_row/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/missing_row/linux_summary.md |
| macos | PENDING | no evidence |
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$MISSING_ROW_RUN_ID" \
  --linux-summary "$WORK_REL/missing_row/linux_summary.md" \
  --linux-examples "$WORK_REL/missing_row/examples.json" \
  --cross-summary "$WORK_REL/missing_row/cross_summary.md" \
  --closure-report "$WORK_REL/missing_row/closure.md" \
  --strict \
  --output "$WORK_REL/missing_row/consistency.md" >/dev/null 2>&1
missing_row_exit=$?
set -e

MISSING_ROW_OUTPUT="$ROOT_DIR/$WORK_REL/missing_row/consistency.md"
if [[ ! -f "$MISSING_ROW_OUTPUT" ]]; then
  fail "expected missing-row consistency report"
fi

if [[ "$missing_row_exit" -eq 0 ]]; then
  fail "consistency should fail strict mode when cross summary platform table is missing a required row"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/missing_row/cross_summary\\.md \\| YES \\| $MISSING_ROW_RUN_ID \\| YES \\| cross_summary platform state missing: windows \\|" "$MISSING_ROW_OUTPUT" >/dev/null; then
  fail "cross_summary row should expose the missing windows platform state"
fi

# Scenario 2: cross summary linux row has an invalid state.
INVALID_LINUX_RUN_ID="consistency_cross_summary_invalid_linux_state"
INVALID_LINUX_DIR="$WORK_DIR/invalid_linux"
mkdir -p "$INVALID_LINUX_DIR"
write_linux_summary "$INVALID_LINUX_DIR/linux_summary.md" "$INVALID_LINUX_RUN_ID"
write_examples_json "$INVALID_LINUX_DIR/examples.json"
write_closure "$INVALID_LINUX_DIR/closure.md" "$INVALID_LINUX_RUN_ID" "$WORK_REL/invalid_linux/linux_summary.md"
cat > "$INVALID_LINUX_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $INVALID_LINUX_RUN_ID
- generated_at: fake
- linux_summary: $WORK_REL/invalid_linux/linux_summary.md
- linux_examples_json: $WORK_REL/invalid_linux/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | BROKEN | $WORK_REL/invalid_linux/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PENDING | no evidence |
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$INVALID_LINUX_RUN_ID" \
  --linux-summary "$WORK_REL/invalid_linux/linux_summary.md" \
  --linux-examples "$WORK_REL/invalid_linux/examples.json" \
  --cross-summary "$WORK_REL/invalid_linux/cross_summary.md" \
  --closure-report "$WORK_REL/invalid_linux/closure.md" \
  --strict \
  --output "$WORK_REL/invalid_linux/consistency.md" >/dev/null 2>&1
invalid_linux_exit=$?
set -e

INVALID_LINUX_OUTPUT="$ROOT_DIR/$WORK_REL/invalid_linux/consistency.md"
if [[ ! -f "$INVALID_LINUX_OUTPUT" ]]; then
  fail "expected invalid-linux-state consistency report"
fi

if [[ "$invalid_linux_exit" -eq 0 ]]; then
  fail "consistency should fail strict mode when cross summary linux row uses an invalid platform state"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/invalid_linux/cross_summary\\.md \\| YES \\| $INVALID_LINUX_RUN_ID \\| YES \\| invalid linux state: BROKEN \\|" "$INVALID_LINUX_OUTPUT" >/dev/null; then
  fail "cross_summary row should expose the invalid linux state"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$INVALID_LINUX_OUTPUT" >/dev/null; then
  fail "invalid cross summary linux state should make consistency INCONSISTENT"
fi

echo "[PASS] wave-b-b2 consistency cross summary platform matrix contract passed"
