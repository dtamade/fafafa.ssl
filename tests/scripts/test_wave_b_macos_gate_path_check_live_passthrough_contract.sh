#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_path_check_live_passthrough_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
FAKE_OPENSSL="$WORK_DIR/fake_openssl"
RUN_ID="wave_b_macos_path_check_live_passthrough"
SUMMARY_REL="out/wave_b_macos_gate_summary_${RUN_ID}.md"
SUMMARY_ABS="$FAKE_ROOT/$SUMMARY_REL"
PATH_CHECK_LOG_ABS="$FAKE_ROOT/out/wave_b_macos_path_check_${RUN_ID}.log"
CUSTOM_MODULES="OnlyThisOne"
PATH_CHECK_ARGS_LOG="$WORK_DIR/path_check_args.log"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

mkdir -p "$FAKE_SCRIPTS" "$FAKE_OPENSSL/lib" "$FAKE_OPENSSL/include/openssl" "$FAKE_OPENSSL/bin"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_b_macos_gate.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_SCRIPTS/detect_macos_openssl_enhanced.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo '{"status":"ok"}'
EOF

cat > "$FAKE_SCRIPTS/run_macos_openssl_path_check_draft.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "${PATH_CHECK_ARGS_LOG:?}"
echo "openssl root: $2"
echo "modules: $4"
echo "[MACOS-CHECK] nested bash scripts/run_all_module_tests.sh --modules $4 --verbose"
exit 0
EOF

cat > "$FAKE_SCRIPTS/compile_all_modules.py" <<'EOF'
#!/usr/bin/env python3
print("compile ok")
EOF

cat > "$FAKE_SCRIPTS/run_all_module_tests.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
exit 0
EOF

cat > "$FAKE_SCRIPTS/verify_examples_compile.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output)
      out="$2"
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done
mkdir -p "$(dirname "$out")"
cat > "$out" <<'JSON'
{
  "summary": {
    "total": 1,
    "passed": 1,
    "failed": 0,
    "skipped": 0,
    "pass_rate": 100.0
  }
}
JSON
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/"*.sh "$FAKE_SCRIPTS/compile_all_modules.py"

set +e
(
  cd "$FAKE_ROOT"
  PATH_CHECK_ARGS_LOG="$PATH_CHECK_ARGS_LOG" \
  OSTYPE=darwin23 \
  bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir out \
    --modules "$CUSTOM_MODULES" \
    --openssl-root "$FAKE_OPENSSL" \
    --verbose \
    --path-check-live >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b macOS gate should stay green when live path-check receives the same custom openssl root and module settings"
fi

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "expected macOS gate summary"
fi

if [[ ! -f "$PATH_CHECK_LOG_ABS" ]]; then
  fail "expected path-check log"
fi

if [[ ! -f "$PATH_CHECK_ARGS_LOG" ]]; then
  fail "expected path-check args log"
fi

if ! rg -Fx -- "--openssl-root" "$PATH_CHECK_ARGS_LOG" >/dev/null; then
  fail "macOS gate should pass --openssl-root to live path-check"
fi

if ! rg -Fx -- "$FAKE_OPENSSL" "$PATH_CHECK_ARGS_LOG" >/dev/null; then
  fail "live path-check should receive the custom openssl root as a positional argument"
fi

if ! rg -Fx -- "--modules" "$PATH_CHECK_ARGS_LOG" >/dev/null; then
  fail "macOS gate should pass --modules to live path-check"
fi

if ! rg -Fx -- "$CUSTOM_MODULES" "$PATH_CHECK_ARGS_LOG" >/dev/null; then
  fail "live path-check should receive the custom module set as a positional argument"
fi

if ! rg -Fx -- "--verbose" "$PATH_CHECK_ARGS_LOG" >/dev/null; then
  fail "macOS gate should pass --verbose to live path-check when verbose mode is enabled"
fi

if ! rg -F --quiet -- "openssl root: $FAKE_OPENSSL" "$PATH_CHECK_LOG_ABS"; then
  fail "live path-check should observe the same custom openssl root provided to the macOS gate"
fi

if ! rg -F --quiet -- "modules: $CUSTOM_MODULES" "$PATH_CHECK_LOG_ABS"; then
  fail "live path-check should observe the same custom module set provided to the macOS gate"
fi

if ! rg -F --quiet -- "--modules $CUSTOM_MODULES --verbose" "$PATH_CHECK_LOG_ABS"; then
  fail "live path-check should pass the custom module set and verbose flag to its nested module runner"
fi

if ! rg -n "^\\| path-check \\| 0 \\| PASS \\|" "$SUMMARY_ABS" >/dev/null; then
  fail "macOS gate summary should keep path-check green after passthrough alignment"
fi

echo "[PASS] wave b macOS gate path-check live passthrough contract passed"
