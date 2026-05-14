#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b149_run_id_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_DOCS="$FAKE_ROOT/docs/test_reports"
OUTPUT_FILE_REL="tmp/test-reports/b149.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MARKER="$FAKE_ROOT/wave_c_b149_injected.marker"
RUN_ID_B146_LOG="$WORK_DIR/b146_run_id.log"
RUN_ID_B147_LOG="$WORK_DIR/b147_run_id.log"
RUN_ID_B148_LOG="$WORK_DIR/b148_run_id.log"
MALICIOUS_RUN_ID="wavec_b149; touch wave_c_b149_injected.marker; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS" "$FAKE_DOCS" "$FAKE_ROOT/tmp/test-reports"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_ROOT/tmp/signoff.md" <<'EOF'
# signoff
EOF
cat > "$FAKE_ROOT/tmp/prereq.md" <<'EOF'
# prereq
EOF
cat > "$FAKE_ROOT/tmp/packet.md" <<'EOF'
# packet
EOF

cat > "$FAKE_SCRIPTS/prepare_wave_c_ci_reenable_submission_pack.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --signoff-record|--prereq-report|--packet-report) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B146_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- submission_state: **READY_TO_SUBMIT**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/check_wave_c_ci_reenable_submission_pack.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --input) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B147_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- check_state: **PASS**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/generate_wave_c_ci_reenable_approval_brief.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --input) shift 2 ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B148_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- brief_state: **READY_FOR_APPROVAL**
REPORT
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

set +e
(
  cd "$FAKE_ROOT"
  FAFAFA_B146_RUN_ID_LOG="$RUN_ID_B146_LOG" \
  FAFAFA_B147_RUN_ID_LOG="$RUN_ID_B147_LOG" \
  FAFAFA_B148_RUN_ID_LOG="$RUN_ID_B148_LOG" \
  bash scripts/run_wave_c_ci_reenable_submission_bundle.sh \
    --run-id "$MALICIOUS_RUN_ID" \
    --signoff-record tmp/signoff.md \
    --prereq-report tmp/prereq.md \
    --packet-report tmp/packet.md \
    --approval-input tmp/packet.md \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave c B149 submission bundle should stay green with fake green nested runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave c B149 submission bundle should not execute shell content embedded in --run-id"
fi

if [[ ! -f "$RUN_ID_B146_LOG" || ! -f "$RUN_ID_B147_LOG" || ! -f "$RUN_ID_B148_LOG" ]]; then
  fail "fake nested B146/B147/B148 runners should observe run-id values"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B146_LOG" >/dev/null; then
  fail "B149 submission bundle should pass the full run-id payload as data to B146"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B147_LOG" >/dev/null; then
  fail "B149 submission bundle should pass the full run-id payload as data to B147"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B148_LOG" >/dev/null; then
  fail "B149 submission bundle should pass the full run-id payload as data to B148"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B149 submission bundle report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B149 submission bundle report should stay PASS in the fake green scenario"
fi

echo "[PASS] wave c B149 submission bundle run-id injection contract passed"
