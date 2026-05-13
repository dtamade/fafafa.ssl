#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_gate_repair_state_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="handoff_gate_repair_state_truth"
REPORT_ABS="$WORK_DIR/out/wave_b_b2_handoff_bundle_${RUN_ID}.md"

mkdir -p "$WORK_DIR/out"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: FAIL

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **FAIL** | fixture |
| run_all_module_tests | **PASS** | fixture |
| verify_examples_compile | **PASS** | fixture |
EOF

cat > "$WORK_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 74,
    "failed": 1,
    "skipped": 0,
    "pass_rate": "98.7%"
  }
}
EOF

cat > "$WORK_DIR/macos_summary.md" <<EOF
# Wave B macOS Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$WORK_DIR/windows_summary.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

: > "$WORK_DIR/winssl_quick_smoke_${RUN_ID}.log"
: > "$WORK_DIR/winssl_runtime_suite_${RUN_ID}.log"

bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --macos-summary "$WORK_REL/macos_summary.md" \
  --windows-summary "$WORK_REL/windows_summary.md" \
  --output-dir "$WORK_REL/out" >/dev/null

if [[ ! -f "$REPORT_ABS" ]]; then
  fail "expected handoff bundle report"
fi

if ! rg -n "^- consistency_status: CONSISTENT$" "$REPORT_ABS" >/dev/null; then
  fail "fixture should keep evidence consistency green so the handoff_state check isolates gate-repair semantics"
fi

if ! rg -n "^- handoff_state: \\*\\*NEEDS_GATE_REPAIR\\*\\*$" "$REPORT_ABS" >/dev/null; then
  fail "handoff bundle should not stay READY_FOR_RUNNER when an existing platform summary already reports FAIL"
fi

if rg -n "^- handoff_state: \\*\\*READY_FOR_RUNNER\\*\\*$" "$REPORT_ABS" >/dev/null; then
  fail "handoff bundle must reserve READY_FOR_RUNNER for missing runner evidence, not failed platform gates"
fi

echo "[PASS] prepare_wave_b_b2 handoff bundle gate repair state contract passed"
