#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_prepare_wave_b_b2_handoff_bundle_windows_companion_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
WINDOWS_EVIDENCE_DIR="$WORK_DIR/windows-evidence"
OUTPUT_DIR="$WORK_DIR/handoff-out"
RUN_ID="handoff_windows_companion_contract"

mkdir -p "$WINDOWS_EVIDENCE_DIR" "$OUTPUT_DIR"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cat > "$WORK_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile_all_modules | **PASS** | ok |
| run_all_module_tests | **PASS** | ok |
| verify_examples_compile | **PASS** | ok |
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

cat > "$WINDOWS_EVIDENCE_DIR/wave_b_windows_gate_summary_${RUN_ID}.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS

## Gate Steps

| step | status | notes |
|------|--------|-------|
| compile | **PASS** | ok |
| modules | **PASS** | ok |
| examples | **PASS** | ok |
EOF

cat > "$WINDOWS_EVIDENCE_DIR/winssl_quick_smoke_${RUN_ID}.log" <<EOF
[quick-smoke] run_id=$RUN_ID
EOF

cat > "$WINDOWS_EVIDENCE_DIR/winssl_runtime_suite_${RUN_ID}.log" <<EOF
[runtime-suite] run_id=$RUN_ID
EOF

ABS_WINDOWS_SUMMARY="$WINDOWS_EVIDENCE_DIR/wave_b_windows_gate_summary_${RUN_ID}.md"
ABS_OUTPUT_DIR="$OUTPUT_DIR"

(cd /tmp && bash "$ROOT_DIR/scripts/prepare_wave_b_b2_handoff_bundle.sh" \
  --run-id "$RUN_ID" \
  --linux-summary "$WORK_REL/linux_summary.md" \
  --linux-examples "$WORK_REL/examples.json" \
  --windows-summary "$ABS_WINDOWS_SUMMARY" \
  --output-dir "$ABS_OUTPUT_DIR" >/dev/null)

CONSISTENCY_REPORT="$OUTPUT_DIR/wave_b_b2_evidence_consistency_${RUN_ID}.md"
HANDOFF_REPORT="$OUTPUT_DIR/wave_b_b2_handoff_bundle_${RUN_ID}.md"

if [[ ! -f "$CONSISTENCY_REPORT" ]]; then
  fail "expected consistency report to be generated"
fi

if [[ ! -f "$HANDOFF_REPORT" ]]; then
  fail "expected handoff bundle report to be generated"
fi

if ! rg -n "consistency_status: \\*\\*CONSISTENT\\*\\*" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should stay CONSISTENT when companion logs exist next to the provided windows summary"
fi

if ! rg -n "\\| windows_quick_log \\| $WINDOWS_EVIDENCE_DIR/winssl_quick_smoke_${RUN_ID}\\.log \\| YES \\|" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should track the sibling windows quick log path"
fi

if ! rg -n "\\| windows_runtime_transcript \\| $WINDOWS_EVIDENCE_DIR/winssl_runtime_suite_${RUN_ID}\\.log \\| YES \\|" "$CONSISTENCY_REPORT" >/dev/null; then
  fail "consistency report should track the sibling windows runtime transcript path"
fi

if ! rg -n "handoff_state: \\*\\*READY_FOR_RUNNER\\*\\*" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff state should remain READY_FOR_RUNNER when consistency is green but macOS evidence is still absent"
fi

if ! rg -n "\\| winssl_quick_smoke_${RUN_ID}\\.log \\| $WINDOWS_EVIDENCE_DIR/winssl_quick_smoke_${RUN_ID}\\.log \\| YES \\|" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff bundle should list the Windows quick-smoke companion artifact when it exists"
fi

if ! rg -n "\\| winssl_runtime_suite_${RUN_ID}\\.log \\| $WINDOWS_EVIDENCE_DIR/winssl_runtime_suite_${RUN_ID}\\.log \\| YES \\|" "$HANDOFF_REPORT" >/dev/null; then
  fail "handoff bundle should list the Windows runtime-suite companion artifact when it exists"
fi

echo "[PASS] prepare_wave_b_b2_handoff_bundle windows companion path contract passed"
