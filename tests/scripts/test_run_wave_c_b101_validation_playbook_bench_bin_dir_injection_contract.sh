#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_c_b101_bench_dir_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_BENCH="$FAKE_ROOT/tests/benchmarks"
FAKE_REPORTS_REL="tmp/test-reports"
FAKE_REPORTS_DIR="$FAKE_ROOT/$FAKE_REPORTS_REL"
OUTPUT_FILE_REL="$FAKE_REPORTS_REL/b101_bench.md"
OUTPUT_FILE_ABS="$FAKE_ROOT/$OUTPUT_FILE_REL"
FPC_LOG="$WORK_DIR/fpc_outdir.log"
BENCH_ENV_LOG="$WORK_DIR/bench_env.log"
MARKER="$FAKE_ROOT/wave_c_b101_bench_injected.marker"
MALICIOUS_BENCH_DIR="tmp/bench'; touch wave_c_b101_bench_injected.marker; #'"
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
touch "$FAKE_BENCH/benchmark_cert_verify_cache.pas"

cat > "$FAKE_ROOT/fpc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
outdir=""
for arg in "$@"; do
  case "$arg" in
    -FE*) outdir="${arg#-FE}" ;;
  esac
done
printf '%s\n' "$outdir" > "${FAFAFA_B101_FPC_OUTDIR_LOG:?}"
mkdir -p "$outdir"
cat > "$outdir/benchmark_cert_verify_cache" <<'RUNNER'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "${FAFAFA_PROJECT_ROOT:-}" > "${FAFAFA_B101_BENCH_ENV_LOG:?}"
echo "Hit Rate: 99.9%"
echo "Speedup Factor: 5.2x"
RUNNER
chmod +x "$outdir/benchmark_cert_verify_cache"
exit 0
EOF

chmod +x "$FAKE_ROOT/fpc"

set +e
(
  cd "$FAKE_ROOT"
  PATH="$FAKE_ROOT:$PATH" \
  FAFAFA_B101_FPC_OUTDIR_LOG="$FPC_LOG" \
  FAFAFA_B101_BENCH_ENV_LOG="$BENCH_ENV_LOG" \
  bash scripts/run_wave_c_b101_validation_playbook.sh \
    --fast-local \
    --bench-bin-dir "$MALICIOUS_BENCH_DIR" \
    --run-id contract_bench \
    --output "$OUTPUT_FILE_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "B101 validation playbook should stay green with fake benchmark tooling"
fi

if [[ -e "$MARKER" ]]; then
  fail "B101 validation playbook should not execute shell content embedded in --bench-bin-dir"
fi

expected_bench_dir="$FAKE_ROOT/$MALICIOUS_BENCH_DIR"

if [[ ! -f "$FPC_LOG" ]] || ! rg -Fx -- "$expected_bench_dir" "$FPC_LOG" >/dev/null; then
  fail "fake fpc should receive the full bench-bin-dir payload as data"
fi

if [[ ! -f "$BENCH_ENV_LOG" ]] || ! rg -Fx -- "$FAKE_ROOT" "$BENCH_ENV_LOG" >/dev/null; then
  fail "benchmark runner should receive FAFAFA_PROJECT_ROOT as data"
fi

if [[ ! -f "$OUTPUT_FILE_ABS" ]]; then
  fail "expected B101 validation report to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$OUTPUT_FILE_ABS" >/dev/null; then
  fail "B101 validation report should stay PASS in the fake green scenario"
fi

echo "[PASS] B101 validation playbook bench-bin-dir injection contract passed"
