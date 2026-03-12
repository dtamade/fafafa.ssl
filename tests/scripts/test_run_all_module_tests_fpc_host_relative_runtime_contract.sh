#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_all_module_tests.sh"
WORK_REL="tmp/test_run_all_module_tests_fpc_host_relative_runtime"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] run_all_module_tests relative fpc host runtime contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/bin" "$WORK_DIR/units"

WRAPPER_REL="$WORK_REL/fpc_wrapper.sh"
WRAPPER_ABS="$ROOT_DIR/$WRAPPER_REL"
INVOKE_LOG="$WORK_DIR/fpc_wrapper_invocations.log"
RUN_LOG="$WORK_DIR/run.log"

cat > "$WRAPPER_ABS" <<EOF
#!/usr/bin/env bash
set -euo pipefail
echo "\$*" >> "$INVOKE_LOG"
exec fpc "\$@"
EOF
chmod +x "$WRAPPER_ABS"

set +e
(
  cd /tmp
  FAFAFA_FPC_EXE="$WRAPPER_REL" \
  FAFAFA_TEST_BIN_DIR="$WORK_REL/bin" \
  FAFAFA_FPC_UNIT_OUTPUT_DIR="$WORK_REL/units" \
  bash "$SCRIPT" --modules PKCS7
) >"$RUN_LOG" 2>&1
STATUS=$?
set -e

if [[ "$STATUS" -eq 0 ]]; then
  :
elif [[ "$STATUS" -eq 1 ]]; then
  :
else
  sed -n '1,220p' "$RUN_LOG" || true
  fail "script exited with unexpected status for runtime contract: $STATUS"
fi

if rg -F --quiet -- 'configured FPC executable is not executable:' "$RUN_LOG"; then
  sed -n '1,220p' "$RUN_LOG" || true
  fail "relative FPC path should be resolved under project root, not rejected as non-executable"
fi

if [[ ! -f "$INVOKE_LOG" ]]; then
  sed -n '1,220p' "$RUN_LOG" || true
  fail "wrapper invocation log should be created"
fi

if ! rg -F --quiet -- '-Mobjfpc -Sh -O2' "$INVOKE_LOG"; then
  sed -n '1,220p' "$INVOKE_LOG" || true
  fail "wrapper should receive expected compile flags"
fi

if ! rg -F --quiet -- "FPC executable: $ROOT_DIR/$WRAPPER_REL" "$RUN_LOG"; then
  sed -n '1,220p' "$RUN_LOG" || true
  fail "report header should print resolved absolute FPC executable path"
fi

echo "[PASS] run_all_module_tests relative fpc host runtime contract passed"
