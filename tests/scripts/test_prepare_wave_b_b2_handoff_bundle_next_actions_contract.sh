#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_next_actions_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
PARTIAL_RUN_ID="handoff_next_actions_partial"
CLOSED_RUN_ID="handoff_next_actions_closed"

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

write_platform_summary() {
  local path="$1"
  local run_id="$2"
  cat > "$path" <<EOF
# Wave B Platform Gate Summary

- run_id: $run_id
- overall: PASS
EOF
}

# Scenario 1: Windows already PASS, macOS still pending.
PARTIAL_DIR="$WORK_DIR/partial"
mkdir -p "$PARTIAL_DIR/out"
write_linux_summary "$PARTIAL_DIR/linux_summary.md" "$PARTIAL_RUN_ID"
write_examples_json "$PARTIAL_DIR/examples.json"
write_platform_summary "$PARTIAL_DIR/windows_summary.md" "$PARTIAL_RUN_ID"
: > "$PARTIAL_DIR/winssl_quick_smoke_${PARTIAL_RUN_ID}.log"
: > "$PARTIAL_DIR/winssl_runtime_suite_${PARTIAL_RUN_ID}.log"

bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$PARTIAL_RUN_ID" \
  --linux-summary "$WORK_REL/partial/linux_summary.md" \
  --linux-examples "$WORK_REL/partial/examples.json" \
  --windows-summary "$WORK_REL/partial/windows_summary.md" \
  --output-dir "$WORK_REL/partial/out" >/dev/null

PARTIAL_REPORT="$PARTIAL_DIR/out/wave_b_b2_handoff_bundle_${PARTIAL_RUN_ID}.md"
if [[ ! -f "$PARTIAL_REPORT" ]]; then
  fail "expected partial handoff bundle report"
fi

if ! rg -n "^1\\. 在 macOS runner 执行 live gate 并回填 macOS summary。$" "$PARTIAL_REPORT" >/dev/null; then
  fail "partial bundle should still prompt macOS live gate when macOS is pending"
fi

if rg -n "^2\\. 在 Windows runner 执行 live gate 并回填 Windows summary。$" "$PARTIAL_REPORT" >/dev/null; then
  fail "partial bundle should not prompt Windows live gate when Windows is already PASS"
fi

if ! rg -n "^2\\. 回填后重新执行 'scripts/prepare_wave_b_b2_handoff_bundle\\.sh .* --strict'。$" "$PARTIAL_REPORT" >/dev/null; then
  fail "partial bundle should still keep the replay command as the final next action"
fi

# Scenario 2: Whole bundle already CLOSED.
CLOSED_DIR="$WORK_DIR/closed"
mkdir -p "$CLOSED_DIR/out"
write_linux_summary "$CLOSED_DIR/linux_summary.md" "$CLOSED_RUN_ID"
write_examples_json "$CLOSED_DIR/examples.json"
write_platform_summary "$CLOSED_DIR/macos_summary.md" "$CLOSED_RUN_ID"
write_platform_summary "$CLOSED_DIR/windows_summary.md" "$CLOSED_RUN_ID"
: > "$CLOSED_DIR/winssl_quick_smoke_${CLOSED_RUN_ID}.log"
: > "$CLOSED_DIR/winssl_runtime_suite_${CLOSED_RUN_ID}.log"

bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$CLOSED_RUN_ID" \
  --linux-summary "$WORK_REL/closed/linux_summary.md" \
  --linux-examples "$WORK_REL/closed/examples.json" \
  --macos-summary "$WORK_REL/closed/macos_summary.md" \
  --windows-summary "$WORK_REL/closed/windows_summary.md" \
  --output-dir "$WORK_REL/closed/out" >/dev/null

CLOSED_REPORT="$CLOSED_DIR/out/wave_b_b2_handoff_bundle_${CLOSED_RUN_ID}.md"
if [[ ! -f "$CLOSED_REPORT" ]]; then
  fail "expected closed handoff bundle report"
fi

if ! rg -n "^- handoff_state: \\*\\*CLOSED\\*\\*$" "$CLOSED_REPORT" >/dev/null; then
  fail "closed scenario should produce a CLOSED handoff bundle"
fi

if rg -n "macOS runner|Windows runner" "$CLOSED_REPORT" >/dev/null; then
  fail "closed bundle should not keep stale macOS/Windows runner instructions"
fi

if ! rg -n "^1\\. 当前批次已闭环；如需复核，可重新执行 'scripts/prepare_wave_b_b2_handoff_bundle\\.sh .* --strict'。$" "$CLOSED_REPORT" >/dev/null; then
  fail "closed bundle should offer only an optional replay command"
fi

echo "[PASS] prepare_wave_b_b2 handoff bundle next actions contract passed"
