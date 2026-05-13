#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_b2_consistency_cross_summary_platform_evidence_metadata_$(date +%s)"
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
  cat > "$path" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $run_id
- closure_status: **IN_PROGRESS**
- strict_mode: false

## Platform Status

| platform | state | note | summary |
|----------|-------|------|---------|
| linux | PASS | ok | $path.linux |
| macos | PENDING | no evidence | |
| windows | PENDING | no evidence | |
EOF
}

# Scenario 1: macOS probe state is active but the evidence path is lost.
MACOS_RUN_ID="consistency_cross_summary_macos_probe_metadata"
MACOS_DIR="$WORK_DIR/macos"
mkdir -p "$MACOS_DIR"
write_linux_summary "$MACOS_DIR/linux_summary.md" "$MACOS_RUN_ID"
write_examples_json "$MACOS_DIR/examples.json"
write_closure_report "$MACOS_DIR/closure.md" "$MACOS_RUN_ID"
sed -i "s|$MACOS_DIR/closure.md.linux|$WORK_REL/macos/linux_summary.md|" "$MACOS_DIR/closure.md"
cat > "$MACOS_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $MACOS_RUN_ID
- generated_at: fake
- linux_summary: $WORK_REL/macos/linux_summary.md
- linux_examples_json: $WORK_REL/macos/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/macos/linux_summary.md |
| macos | PROBE_ONLY | probe metadata lost |
| windows | PENDING | no evidence |
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$MACOS_RUN_ID" \
  --linux-summary "$WORK_REL/macos/linux_summary.md" \
  --linux-examples "$WORK_REL/macos/examples.json" \
  --cross-summary "$WORK_REL/macos/cross_summary.md" \
  --closure-report "$WORK_REL/macos/closure.md" \
  --strict \
  --output "$WORK_REL/macos/consistency.md" >/dev/null 2>&1
macos_exit=$?
set -e

MACOS_OUTPUT="$ROOT_DIR/$WORK_REL/macos/consistency.md"
if [[ ! -f "$MACOS_OUTPUT" ]]; then
  fail "expected macOS metadata consistency report"
fi

if [[ "$macos_exit" -eq 0 ]]; then
  fail "consistency should fail strict mode when cross summary marks macOS probe evidence active but loses the probe path metadata"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$MACOS_OUTPUT" >/dev/null; then
  fail "macOS active probe metadata loss should make consistency INCONSISTENT"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/macos/cross_summary\\.md \\| YES \\| $MACOS_RUN_ID \\| YES \\| macos probe metadata missing \\|" "$MACOS_OUTPUT" >/dev/null; then
  fail "cross_summary row should expose the missing macOS probe metadata"
fi

# Scenario 2: Windows state is active but the evidence path is lost.
WINDOWS_RUN_ID="consistency_cross_summary_windows_metadata"
WINDOWS_DIR="$WORK_DIR/windows"
mkdir -p "$WINDOWS_DIR"
write_linux_summary "$WINDOWS_DIR/linux_summary.md" "$WINDOWS_RUN_ID"
write_examples_json "$WINDOWS_DIR/examples.json"
write_closure_report "$WINDOWS_DIR/closure.md" "$WINDOWS_RUN_ID"
sed -i "s|$WINDOWS_DIR/closure.md.linux|$WORK_REL/windows/linux_summary.md|" "$WINDOWS_DIR/closure.md"
cat > "$WINDOWS_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $WINDOWS_RUN_ID
- generated_at: fake
- linux_summary: $WORK_REL/windows/linux_summary.md
- linux_examples_json: $WORK_REL/windows/examples.json

## 1) Platform Evidence Status

| platform | state | evidence |
|----------|-------|----------|
| linux | PASS | $WORK_REL/windows/linux_summary.md |
| macos | PENDING | no evidence |
| windows | PASS | metadata lost |
EOF

set +e
bash "$ROOT_DIR/scripts/check_wave_b_b2_evidence_consistency.sh" \
  --run-id "$WINDOWS_RUN_ID" \
  --linux-summary "$WORK_REL/windows/linux_summary.md" \
  --linux-examples "$WORK_REL/windows/examples.json" \
  --cross-summary "$WORK_REL/windows/cross_summary.md" \
  --closure-report "$WORK_REL/windows/closure.md" \
  --strict \
  --output "$WORK_REL/windows/consistency.md" >/dev/null 2>&1
windows_exit=$?
set -e

WINDOWS_OUTPUT="$ROOT_DIR/$WORK_REL/windows/consistency.md"
if [[ ! -f "$WINDOWS_OUTPUT" ]]; then
  fail "expected Windows metadata consistency report"
fi

if [[ "$windows_exit" -eq 0 ]]; then
  fail "consistency should fail strict mode when cross summary marks Windows evidence active but loses the summary path metadata"
fi

if ! rg -n "^- consistency_status: \\*\\*INCONSISTENT\\*\\*$" "$WINDOWS_OUTPUT" >/dev/null; then
  fail "Windows active summary metadata loss should make consistency INCONSISTENT"
fi

if ! rg -n "^\\| cross_summary \\| $WORK_REL/windows/cross_summary\\.md \\| YES \\| $WINDOWS_RUN_ID \\| YES \\| windows active evidence metadata missing \\|" "$WINDOWS_OUTPUT" >/dev/null; then
  fail "cross_summary row should expose the missing Windows evidence metadata"
fi

echo "[PASS] wave-b-b2 consistency cross summary platform evidence metadata contract passed"
