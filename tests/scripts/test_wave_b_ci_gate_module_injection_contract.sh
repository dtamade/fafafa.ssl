#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_module_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_BIN="$WORK_DIR/fakebin"
RUN_ID="wave_b_ci_gate_module_injection"
SUMMARY_REL="tmp/wave_b_ci_gate_summary_${RUN_ID}.md"
SUMMARY_ABS="$FAKE_ROOT/$SUMMARY_REL"
FLAG_FILE="$WORK_DIR/module_injected.flag"
FAKE_BASH_LOG="$WORK_DIR/fake_bash.log"
MALICIOUS_MODULES="PKCS7; touch '$FLAG_FILE'; #"
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
printf '%s\n' "$@" > "${FAFAFA_FAKE_BASH_LOG:?}"
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" "$FAKE_BIN/bash"

if rg -n --quiet '(^|[^[:alnum:]_])eval[[:space:]]' "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh"; then
  fail "wave b linux gate should not rely on eval"
fi

set +e
(
  cd "$FAKE_ROOT"
  PATH="$FAKE_BIN:$PATH" \
  FAFAFA_FAKE_BASH_LOG="$FAKE_BASH_LOG" \
  /usr/bin/bash "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" \
    --skip-compile \
    --skip-examples \
    --run-id "$RUN_ID" \
    --reports-dir tmp \
    --modules "$MALICIOUS_MODULES" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b linux gate should stay green with fake nested module runner"
fi

if [[ -e "$FLAG_FILE" ]]; then
  fail "wave b linux gate should not execute shell content embedded in --modules"
fi

if [[ ! -f "$FAKE_BASH_LOG" ]]; then
  fail "fake nested bash should have been invoked for module step"
fi

if ! rg -Fx -- "--modules" "$FAKE_BASH_LOG" >/dev/null; then
  fail "wave b linux gate should still pass --modules to nested runner"
fi

if ! rg -Fx -- "$MALICIOUS_MODULES" "$FAKE_BASH_LOG" >/dev/null; then
  fail "wave b linux gate should pass the full modules payload as data"
fi

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "wave b linux gate should still emit a summary after safe modules passthrough"
fi

echo "[PASS] wave b linux gate module injection contract passed"
