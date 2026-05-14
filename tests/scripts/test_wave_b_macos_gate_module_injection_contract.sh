#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_module_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
OUTPUT_DIR_REL="out"
OUTPUT_DIR_ABS="$WORK_DIR/$OUTPUT_DIR_REL"
RUN_ID="wave_b_macos_module_injection"
MARKER="$WORK_DIR/module_injected.marker"
MODULE_ARGS_LOG="$WORK_DIR/module_args.log"
MALICIOUS_MODULES="PKCS7; touch '$MARKER'; #"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$WORK_DIR/scripts"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_b_macos_gate.sh" "$WORK_DIR/scripts/"

cat > "$WORK_DIR/scripts/detect_macos_openssl_enhanced.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo '{"status":"ok"}'
EOF

cat > "$WORK_DIR/scripts/run_macos_openssl_path_check_draft.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
exit 0
EOF

cat > "$WORK_DIR/scripts/compile_all_modules.py" <<'EOF'
#!/usr/bin/env python3
print("compile ok")
EOF

cat > "$WORK_DIR/scripts/run_all_module_tests.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "${MODULE_ARGS_LOG:?}"
exit 0
EOF

cat > "$WORK_DIR/scripts/verify_examples_compile.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output) out="$2"; shift 2 ;;
    *) shift ;;
  esac
done
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

chmod +x "$WORK_DIR/scripts/"*.sh "$WORK_DIR/scripts/compile_all_modules.py"

set +e
(
  cd "$WORK_DIR"
  MODULE_ARGS_LOG="$MODULE_ARGS_LOG" \
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir "$OUTPUT_DIR_REL" \
    --modules "$MALICIOUS_MODULES" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b macOS gate should treat modules payload as data and still complete with fake green runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave b macOS gate should not execute shell content embedded in --modules"
fi

if [[ ! -f "$MODULE_ARGS_LOG" ]]; then
  fail "fake module runner should receive arguments"
fi

if ! rg -Fx -- "--modules" "$MODULE_ARGS_LOG" >/dev/null; then
  fail "wave b macOS gate should still pass the --modules flag to the nested runner"
fi

if ! rg -Fx -- "$MALICIOUS_MODULES" "$MODULE_ARGS_LOG" >/dev/null; then
  fail "wave b macOS gate should pass the full modules payload as data to the nested runner"
fi

if [[ ! -f "$OUTPUT_DIR_ABS/wave_b_macos_gate_summary_${RUN_ID}.md" ]]; then
  fail "wave b macOS gate should still emit a summary after safe modules passthrough"
fi

echo "[PASS] wave b macOS gate module injection contract passed"
