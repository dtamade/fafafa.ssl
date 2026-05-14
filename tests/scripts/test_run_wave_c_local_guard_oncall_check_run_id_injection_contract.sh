#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b129_run_id_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_WORKFLOWS="$FAKE_ROOT/.github/workflows"
OUTPUT_FILE_REL="out/oncall.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MARKER="$FAKE_ROOT/wave_c_b129_injected.marker"
RUN_ID_B125_LOG="$WORK_DIR/b125_run_id.log"
RUN_ID_B126_LOG="$WORK_DIR/b126_run_id.log"
MALICIOUS_RUN_ID="wavec_b129; touch wave_c_b129_injected.marker; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS" "$FAKE_WORKFLOWS"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_c_local_guard_oncall_check.sh" "$FAKE_SCRIPTS/"
touch "$FAKE_WORKFLOWS/wave-c-quick-sprint-manual.yml.disabled"

cat > "$FAKE_SCRIPTS/run_wave_c_local_first_guard_bundle.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B125_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- overall: **PASS**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/summarize_wave_c_local_guard_history.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B126_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- trend_state: **STABLE**
REPORT
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

set +e
(
  cd "$FAKE_ROOT"
  FAFAFA_B125_RUN_ID_LOG="$RUN_ID_B125_LOG" \
  FAFAFA_B126_RUN_ID_LOG="$RUN_ID_B126_LOG" \
  bash scripts/run_wave_c_local_guard_oncall_check.sh \
    --run-id "$MALICIOUS_RUN_ID" \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave c B129 oncall should stay green with fake green nested runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave c B129 oncall should not execute shell content embedded in --run-id"
fi

if [[ ! -f "$RUN_ID_B125_LOG" || ! -f "$RUN_ID_B126_LOG" ]]; then
  fail "fake nested B125/B126 runners should observe run-id values"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B125_LOG" >/dev/null; then
  fail "B129 oncall should pass the full run-id payload as data to B125"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B126_LOG" >/dev/null; then
  fail "B129 oncall should pass the full run-id payload as data to B126"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B129 oncall report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B129 oncall report should stay PASS in the fake green scenario"
fi

echo "[PASS] wave c B129 oncall run-id injection contract passed"
