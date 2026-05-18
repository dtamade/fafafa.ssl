#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_loader_symbol_probe_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
OUTPUT_DIR_REL="out"
OUTPUT_DIR_ABS="$WORK_DIR/$OUTPUT_DIR_REL"
RUN_ID="wave_b_macos_loader_symbol_probe_contract"
SUMMARY_ABS="$OUTPUT_DIR_ABS/wave_b_macos_gate_summary_${RUN_ID}.md"
PROBE_ARGS_LOG="$WORK_DIR/loader_symbol_probe_args.log"
PROBE_JSON_ABS="$OUTPUT_DIR_ABS/wave_b_macos_loader_symbol_probe_${RUN_ID}.json"

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

cat > "$WORK_DIR/scripts/run_macos_openssl_loader_symbol_probe.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$@" > "${PROBE_ARGS_LOG:?}"
out=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output)
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
  "status": "ok",
  "loader_version_string": "OpenSSL 3.0.x"
}
JSON
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
exit 0
EOF

cat > "$WORK_DIR/scripts/verify_examples_compile.sh" <<'EOF'
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
EOF

chmod +x "$WORK_DIR/scripts/"*.sh "$WORK_DIR/scripts/compile_all_modules.py"

(
  cd "$WORK_DIR"
  PROBE_ARGS_LOG="$PROBE_ARGS_LOG" \
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir "$OUTPUT_DIR_REL" >/dev/null
)

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "expected macOS gate summary to be generated"
fi

if [[ ! -f "$PROBE_JSON_ABS" ]]; then
  fail "loader symbol probe should emit its JSON artifact"
fi

if [[ ! -f "$PROBE_ARGS_LOG" ]]; then
  fail "fake loader symbol probe should receive arguments"
fi

if ! rg -Fx -- "--run-id" "$PROBE_ARGS_LOG" >/dev/null; then
  fail "macOS gate should pass --run-id to loader symbol probe"
fi

if ! rg -Fx -- "$RUN_ID" "$PROBE_ARGS_LOG" >/dev/null; then
  fail "loader symbol probe should receive the current run-id"
fi

if ! rg -Fx -- "--output" "$PROBE_ARGS_LOG" >/dev/null; then
  fail "macOS gate should pass --output to loader symbol probe"
fi

if ! rg -Fx -- "$OUTPUT_DIR_REL/wave_b_macos_loader_symbol_probe_${RUN_ID}.json" "$PROBE_ARGS_LOG" >/dev/null; then
  fail "loader symbol probe should receive the run-scoped JSON output path"
fi

if ! rg -n "^\\| loader-symbol-probe \\| 0 \\| PASS \\| $OUTPUT_DIR_REL/wave_b_macos_loader_symbol_probe_${RUN_ID}\\.json \\|" "$SUMMARY_ABS" >/dev/null; then
  fail "macOS gate summary should expose the loader symbol probe evidence row"
fi

echo "[PASS] wave b macOS gate loader symbol probe contract passed"
