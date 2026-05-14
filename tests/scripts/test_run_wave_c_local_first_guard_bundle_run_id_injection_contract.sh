#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b125_run_id_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
REPORTS_DIR="out"
OUTPUT_FILE_REL="$REPORTS_DIR/bundle.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MARKER="$FAKE_ROOT/wave_c_b125_injected.marker"
RUN_ID_B123_LOG="$WORK_DIR/b123_run_id.log"
RUN_ID_B124_LOG="$WORK_DIR/b124_run_id.log"
MALICIOUS_RUN_ID="wavec_b125; touch wave_c_b125_injected.marker; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_c_local_first_guard_bundle.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_SCRIPTS/check_wave_c_local_first_continuity.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B123_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- local_first_state: **LOCAL_READY**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/check_wave_c_local_drift_watch.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B124_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- local_drift_state: **LOCAL_STABLE**
REPORT
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

set +e
(
  cd "$FAKE_ROOT"
  FAFAFA_B123_RUN_ID_LOG="$RUN_ID_B123_LOG" \
  FAFAFA_B124_RUN_ID_LOG="$RUN_ID_B124_LOG" \
  bash scripts/run_wave_c_local_first_guard_bundle.sh \
    --run-id "$MALICIOUS_RUN_ID" \
    --reports-dir "$REPORTS_DIR" \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave c B125 bundle should stay green with fake green nested runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave c B125 bundle should not execute shell content embedded in --run-id"
fi

if [[ ! -f "$RUN_ID_B123_LOG" || ! -f "$RUN_ID_B124_LOG" ]]; then
  fail "fake nested B123/B124 runners should observe run-id values"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B123_LOG" >/dev/null; then
  fail "B125 bundle should pass the full run-id payload as data to B123"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B124_LOG" >/dev/null; then
  fail "B125 bundle should pass the full run-id payload as data to B124"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B125 bundle report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B125 bundle report should stay PASS in the fake green scenario"
fi

echo "[PASS] wave c B125 run-id injection contract passed"
