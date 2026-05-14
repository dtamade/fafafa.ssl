#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_quick_sprint_run_id_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
REPORTS_REL="tmp/test-reports"
REPORTS_DIR="$FAKE_ROOT/$REPORTS_REL"
OUTPUT_FILE_REL="$REPORTS_REL/quick_sprint_bundle.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MARKER="$FAKE_ROOT/wave_c_quick_sprint_injected.marker"
RUN_ID_B107_LOG="$WORK_DIR/b107_run_id.log"
RUN_ID_B108_LOG="$WORK_DIR/b108_run_id.log"
RUN_ID_B109_LOG="$WORK_DIR/b109_run_id.log"
RUN_ID_B110_LOG="$WORK_DIR/b110_run_id.log"
FULL_GATE_LOG="$WORK_DIR/b107_require_full_gate.log"
MALICIOUS_RUN_ID="wavec_quick; touch wave_c_quick_sprint_injected.marker; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS" "$REPORTS_DIR"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_c_quick_sprint_bundle.sh" "$FAKE_SCRIPTS/"

cat > "$REPORTS_DIR/validation.md" <<'EOF'
# validation
EOF

cat > "$FAKE_SCRIPTS/evaluate_wave_c_b101_thresholds.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
require_full_gate="false"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --require-full-gate) require_full_gate="true"; shift ;;
    --reports-dir|--report-glob) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B107_RUN_ID_LOG:?}"
printf '%s\n' "$require_full_gate" > "${FAFAFA_B107_FULL_GATE_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
# Wave C B107 Threshold Evaluation Report
- overall: **PASS**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/check_wave_c_default_on_readiness.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --reports-dir|--threshold-report|--validation-report) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B108_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
# Wave C B108 Default-On Readiness
- readiness: **READY**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/prepare_wave_c_b109_canary_rollout.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --reports-dir|--threshold-report|--readiness-report|--validation-report) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B109_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
# Wave C B109 Controlled Canary Rollout
- rollout_state: **CANARY_READY**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/run_wave_c_b110_rollback_drill.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --reports-dir|--threshold-report|--readiness-report|--rollout-report|--validation-report) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B110_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
# Wave C B110 Rollback Drill Report
- drill_status: **PASS**
REPORT
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

set +e
(
  cd "$FAKE_ROOT"
  FAFAFA_B107_RUN_ID_LOG="$RUN_ID_B107_LOG" \
  FAFAFA_B107_FULL_GATE_LOG="$FULL_GATE_LOG" \
  FAFAFA_B108_RUN_ID_LOG="$RUN_ID_B108_LOG" \
  FAFAFA_B109_RUN_ID_LOG="$RUN_ID_B109_LOG" \
  FAFAFA_B110_RUN_ID_LOG="$RUN_ID_B110_LOG" \
  bash scripts/run_wave_c_quick_sprint_bundle.sh \
    --reports-dir "$REPORTS_REL" \
    --report-glob 'wave_c_b101_validation_*.md' \
    --require-full-gate \
    --validation-report "$REPORTS_REL/validation.md" \
    --run-id "$MALICIOUS_RUN_ID" \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "quick sprint bundle should stay green with fake green nested runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "quick sprint bundle should not execute shell content embedded in --run-id"
fi

if [[ ! -f "$RUN_ID_B107_LOG" || ! -f "$RUN_ID_B108_LOG" || ! -f "$RUN_ID_B109_LOG" || ! -f "$RUN_ID_B110_LOG" ]]; then
  fail "fake nested B107/B108/B109/B110 runners should observe run-id values"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B107_LOG" >/dev/null; then
  fail "quick sprint bundle should pass the full run-id payload as data to B107"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B108_LOG" >/dev/null; then
  fail "quick sprint bundle should pass the full run-id payload as data to B108"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B109_LOG" >/dev/null; then
  fail "quick sprint bundle should pass the full run-id payload as data to B109"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B110_LOG" >/dev/null; then
  fail "quick sprint bundle should pass the full run-id payload as data to B110"
fi

if [[ ! -f "$FULL_GATE_LOG" ]] || ! rg -Fx -- "true" "$FULL_GATE_LOG" >/dev/null; then
  fail "quick sprint bundle should still pass --require-full-gate to B107"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected quick sprint bundle report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "quick sprint bundle report should stay PASS in the fake green scenario"
fi

echo "[PASS] quick sprint bundle run-id injection contract passed"
