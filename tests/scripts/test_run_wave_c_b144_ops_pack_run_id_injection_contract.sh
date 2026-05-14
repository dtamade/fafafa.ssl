#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b144_run_id_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
OUTPUT_FILE_REL="tmp/test-reports/ops_pack.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MARKER="$FAKE_ROOT/wave_c_b144_injected.marker"
RUN_ID_B138_LOG="$WORK_DIR/b138_run_id.log"
RUN_ID_B140_LOG="$WORK_DIR/b140_run_id.log"
RUN_ID_B142_LOG="$WORK_DIR/b142_run_id.log"
RUN_ID_B143_LOG="$WORK_DIR/b143_run_id.log"
RUN_ID_B139_LOG="$WORK_DIR/b139_run_id.log"
MALICIOUS_RUN_ID="wavec_b144; touch wave_c_b144_injected.marker; #"
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

cp "$ROOT_DIR/scripts/run_wave_c_local_guard_ops_pack.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_SCRIPTS/run_wave_c_pre_ci_reenable_full_gate.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B138_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- overall: **PASS**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/check_wave_c_local_guard_consistency.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B140_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- consistency_state: **CONSISTENT**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/export_wave_c_local_guard_status_json.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B142_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'JSON'
{"overall_state":"HEALTHY"}
JSON
exit 0
EOF

cat > "$FAKE_SCRIPTS/check_wave_c_local_guard_alert_thresholds.sh" <<'EOF'
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
printf '%s\n' "$run_id" > "${FAFAFA_B143_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- alert_level: **NONE**
REPORT
exit 0
EOF

cat > "$FAKE_SCRIPTS/cleanup_wave_c_local_guard_reports.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
run_id=""
output=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --run-id) run_id="$2"; shift 2 ;;
    --output) output="$2"; shift 2 ;;
    *) shift ;;
  esac
done
printf '%s\n' "$run_id" > "${FAFAFA_B139_RUN_ID_LOG:?}"
mkdir -p "$(dirname "$output")"
cat > "$output" <<'REPORT'
- mode: DRY_RUN
REPORT
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh

set +e
(
  cd "$FAKE_ROOT"
  FAFAFA_B138_RUN_ID_LOG="$RUN_ID_B138_LOG" \
  FAFAFA_B140_RUN_ID_LOG="$RUN_ID_B140_LOG" \
  FAFAFA_B142_RUN_ID_LOG="$RUN_ID_B142_LOG" \
  FAFAFA_B143_RUN_ID_LOG="$RUN_ID_B143_LOG" \
  FAFAFA_B139_RUN_ID_LOG="$RUN_ID_B139_LOG" \
  bash scripts/run_wave_c_local_guard_ops_pack.sh \
    --run-id "$MALICIOUS_RUN_ID" \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave c B144 ops pack should stay green with fake green nested runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave c B144 ops pack should not execute shell content embedded in --run-id"
fi

if [[ ! -f "$RUN_ID_B138_LOG" || ! -f "$RUN_ID_B140_LOG" || ! -f "$RUN_ID_B142_LOG" || ! -f "$RUN_ID_B143_LOG" || ! -f "$RUN_ID_B139_LOG" ]]; then
  fail "fake nested B138/B140/B142/B143/B139 runners should observe run-id values"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B138_LOG" >/dev/null; then
  fail "B144 ops pack should pass the full run-id payload as data to B138"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B140_LOG" >/dev/null; then
  fail "B144 ops pack should pass the full run-id payload as data to B140"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B142_LOG" >/dev/null; then
  fail "B144 ops pack should pass the full run-id payload as data to B142"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B143_LOG" >/dev/null; then
  fail "B144 ops pack should pass the full run-id payload as data to B143"
fi

if ! rg -Fx -- "$MALICIOUS_RUN_ID" "$RUN_ID_B139_LOG" >/dev/null; then
  fail "B144 ops pack should pass the full run-id payload as data to B139"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B144 ops-pack report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B144 ops-pack report should stay PASS in the fake green scenario"
fi

echo "[PASS] wave c B144 ops pack run-id injection contract passed"
