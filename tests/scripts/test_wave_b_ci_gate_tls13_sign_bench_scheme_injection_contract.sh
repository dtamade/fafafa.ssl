#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_tls13_sign_bench_scheme_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_BIN="$WORK_DIR/fakebin"
RUN_ID="wave_b_ci_gate_tls13_bench_scheme_injection"
SUMMARY_REL="tmp/wave_b_ci_gate_summary_${RUN_ID}.md"
SUMMARY_ABS="$FAKE_ROOT/$SUMMARY_REL"
FLAG_FILE="$WORK_DIR/bench_scheme_injected.flag"
FAKE_BASH_ARGS_LOG="$WORK_DIR/fake_bash_args.log"
FAKE_BASH_SCHEME_LOG="$WORK_DIR/fake_bash_scheme.log"
MALICIOUS_SCHEME="rsa_pkcs1_sha256'; touch '$FLAG_FILE'; echo '"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

mkdir -p "$FAKE_SCRIPTS" "$FAKE_BIN"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_b_ci_gate.sh" "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh"

cat > "$FAKE_BIN/bash" <<'EOF'
#!/usr/bin/bash
set -euo pipefail
printf '%s\n' "$@" > "${FAFAFA_FAKE_BASH_ARGS_LOG:?}"
printf '%s\n' "${FAFAFA_TLS13_SIGN_BENCH_SCHEME:-}" > "${FAFAFA_FAKE_BASH_SCHEME_LOG:?}"
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" "$FAKE_BIN/bash"

set +e
(
  cd "$FAKE_ROOT"
  PATH="$FAKE_BIN:$PATH" \
  FAFAFA_FAKE_BASH_ARGS_LOG="$FAKE_BASH_ARGS_LOG" \
  FAFAFA_FAKE_BASH_SCHEME_LOG="$FAKE_BASH_SCHEME_LOG" \
  /usr/bin/bash "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" \
    --only-tls13-sign-bench \
    --run-id "$RUN_ID" \
    --reports-dir tmp \
    --tls13-sign-bench-scheme "$MALICIOUS_SCHEME" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b linux gate should stay green with fake tls13 bench runner"
fi

if [[ -e "$FLAG_FILE" ]]; then
  fail "wave b linux gate should not execute shell content embedded in --tls13-sign-bench-scheme"
fi

if [[ ! -f "$FAKE_BASH_ARGS_LOG" ]]; then
  fail "fake nested bash should have been invoked for tls13 bench step"
fi

if ! rg -Fx -- "scripts/run_freepascal_tls13_servercertverify_bench.sh" "$FAKE_BASH_ARGS_LOG" >/dev/null; then
  fail "wave b linux gate should still invoke the tls13 bench runner"
fi

if [[ ! -f "$FAKE_BASH_SCHEME_LOG" ]]; then
  fail "fake tls13 bench runner should observe scheme env"
fi

if ! rg -Fx -- "$MALICIOUS_SCHEME" "$FAKE_BASH_SCHEME_LOG" >/dev/null; then
  fail "wave b linux gate should pass the full bench scheme payload as env data"
fi

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "wave b linux gate should still emit a summary after safe bench passthrough"
fi

echo "[PASS] wave b linux gate tls13 bench scheme injection contract passed"
