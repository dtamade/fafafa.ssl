#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b138_run_id_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
OUTPUT_FILE_REL="tmp/test-reports/full_gate.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MARKER="$FAKE_ROOT/wave_c_b138_injected.marker"
RUN_ID_B129_LOG="$WORK_DIR/b129_run_id.log"
RUN_ID_B132_LOG="$WORK_DIR/b132_run_id.log"
RUN_ID_B137_LOG="$WORK_DIR/b137_run_id.log"
MALICIOUS_RUN_ID="wavec_b138; touch wave_c_b138_injected.marker; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS" "$FAKE_ROOT/tmp/test-reports"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_c_pre_ci_reenable_full_gate.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_SCRIPTS/run_wave_c_local_guard_oncall_check.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --strict|--quiet) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B129_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- overall: **PASS**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/generate_wave_c_local_first_status_snapshot.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B132_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- snapshot_state: **GREEN**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/prepare_wave_c_b137_pre_ci_reenable_packet.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    --oncall-report|--snapshot-report) shift 2 ;;
    --strict) shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B137_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- packet_state: **READY_FOR_APPROVAL**
REPORT
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

set +e
(
  cd "$FAKE_ROOT"
  FAFAFA_B129_RUN_ID_LOG="$RUN_ID_B129_LOG" \
  FAFAFA_B132_RUN_ID_LOG="$RUN_ID_B132_LOG" \
  FAFAFA_B137_RUN_ID_LOG="$RUN_ID_B137_LOG" \
  bash scripts/run_wave_c_pre_ci_reenable_full_gate.sh \
    --run-id "$MALICIOUS_RUN_ID" \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave c B138 full gate should stay green with fake green nested runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave c B138 full gate should not execute shell content embedded in --run-id"
fi

if [[ ! -f "$RUN_ID_B129_LOG" || ! -f "$RUN_ID_B132_LOG" || ! -f "$RUN_ID_B137_LOG" ]]; then
  fail "fake nested B129/B132/B137 runners should observe run-id values"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B129_LOG" >/dev/null; then
  fail "B138 full gate should pass the full run-id payload as data to B129"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B132_LOG" >/dev/null; then
  fail "B138 full gate should pass the full run-id payload as data to B132"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B137_LOG" >/dev/null; then
  fail "B138 full gate should pass the full run-id payload as data to B137"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B138 full-gate report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B138 full-gate report should stay PASS in the fake green scenario"
fi

echo "[PASS] wave c B138 full gate run-id injection contract passed"
