#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b101_modules_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_BENCH="$FAKE_ROOT/tests/benchmarks"
FAKE_REPORTS_REL="tmp/test-reports"
FAKE_REPORTS_DIR="$FAKE_ROOT/$FAKE_REPORTS_REL"
OUTPUT_FILE_REL="$FAKE_REPORTS_REL/b101_modules.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
MODULES_LOG="$WORK_DIR/modules.log"
FULL_GATE_FLAG_LOG="$WORK_DIR/full_gate_fast_local.log"
MARKER="$FAKE_ROOT/wave_c_b101_modules_injected.marker"
MALICIOUS_MODULES="PKCS7; touch wave_c_b101_modules_injected.marker; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS" "$FAKE_BENCH" "$FAKE_REPORTS_DIR"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_c_b101_validation_playbook.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_SCRIPTS/compile_all_modules.py" <<'EOF'
#!/usr/bin/env python3
import sys
print("compile ok")
sys.exit(0)
EOF

cat > "$FAKE_SCRIPTS/run_all_module_tests.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
modules=""
fast_local="false"
while [[ $# -gt 0 ]]; do
  case "$1" in
    --modules) modules="$2"; shift 2 ;;
    --fast-local) fast_local="true"; shift ;;
    *) shift ;;
  esac
done
printf '%s\n' "$modules" > "${FAFAFA_B101_MODULES_LOG:?}"
printf '%s|%s|%s|%s\n' \
  "$fast_local" \
  "${FAFAFA_MODULE_TEST_REPORTS_DIR:-}" \
  "${FAFAFA_MODULE_TEST_BIN_DIR:-}" \
  "${FAFAFA_FPC_UNIT_OUTPUT_DIR:-}" > "${FAFAFA_B101_FAST_LOCAL_LOG:?}"
exit 0
EOF

cat > "$FAKE_ROOT/fpc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
outdir=""
for arg in "$@"; do
  case "$arg" in
    -FE*) outdir="${arg#-FE}" ;;
  esac
done
mkdir -p "$outdir"
cat > "$outdir/benchmark_cert_verify_cache" <<'RUNNER'
#!/usr/bin/env bash
echo "Hit Rate: 99.9%"
echo "Speedup Factor: 5.0x"
RUNNER
chmod +x "$outdir/benchmark_cert_verify_cache"
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/compile_all_modules.py" "$FAKE_SCRIPTS/run_all_module_tests.sh" "$FAKE_ROOT/fpc"
touch "$FAKE_BENCH/benchmark_cert_verify_cache.pas"

set +e
(
  cd "$FAKE_ROOT"
  PATH="$FAKE_ROOT:$PATH" \
  FAFAFA_B101_MODULES_LOG="$MODULES_LOG" \
  FAFAFA_B101_FAST_LOCAL_LOG="$FULL_GATE_FLAG_LOG" \
  bash scripts/run_wave_c_b101_validation_playbook.sh \
    --full-gate \
    --fast-local \
    --modules "$MALICIOUS_MODULES" \
    --run-id contract_modules \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "B101 validation playbook should stay green with fake green tooling"
fi

if [[ -e "$MARKER" ]]; then
  fail "B101 validation playbook should not execute shell content embedded in --modules"
fi

if [[ ! -f "$MODULES_LOG" ]]; then
  fail "fake module runner should observe the modules payload"
fi

if ! rg -Fx -- "$MALICIOUS_MODULES" "$MODULES_LOG" >/dev/null; then
  fail "B101 validation playbook should pass the full modules payload as data"
fi

if [[ ! -f "$FULL_GATE_FLAG_LOG" ]]; then
  fail "fake module runner should capture fast-local env wiring"
fi

if ! rg -Fx -- "true|$FAKE_ROOT/tmp/wave_c_b101_module_reports_contract_modules|$FAKE_ROOT/tmp/wave_c_b101_module_bin_contract_modules|$FAKE_ROOT/tmp/wave_c_b101_module_units_contract_modules" "$FULL_GATE_FLAG_LOG" >/dev/null; then
  fail "B101 validation playbook should preserve fast-local env directories for module tests"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B101 validation report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B101 validation report should stay PASS in the fake green scenario"
fi

echo "[PASS] B101 validation playbook modules injection contract passed"
