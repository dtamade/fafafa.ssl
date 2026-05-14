#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_invalid_examples_json_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
RUN_ID="wave_b_macos_invalid_examples_json"
OUTPUT_DIR_REL="$WORK_REL/out"
SUMMARY_ABS="$WORK_DIR/$OUTPUT_DIR_REL/wave_b_macos_gate_summary_${RUN_ID}.md"
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
printf '{ bad json\n' > "$out"
exit 0
EOF

chmod +x "$WORK_DIR/scripts/"*.sh "$WORK_DIR/scripts/compile_all_modules.py"

set +e
(
  cd "$WORK_DIR"
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir "$OUTPUT_DIR_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -eq 0 ]]; then
  fail "wave b macOS gate should fail when examples json is invalid even if helper exits 0"
fi

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "wave b macOS gate should still emit a summary when examples json is invalid"
fi

if rg -n "JSONDecodeError|Traceback" "$STDERR_LOG" >/dev/null; then
  fail "wave b macOS gate should not leak python traceback when examples json is invalid"
fi

if ! rg -n "^\\| examples \\| 0 \\| FAIL \\|" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b macOS summary should mark examples step FAIL when json is invalid"
fi

if ! rg -n "^- overall: \\*\\*FAIL\\*\\*" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b macOS summary should become FAIL when examples json is invalid"
fi

if ! rg -n "summary: passed=n/a, failed=n/a, skipped=n/a, total=n/a, pass_rate=n/a" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b macOS summary should keep examples metrics at n/a when json is invalid"
fi

echo "[PASS] wave b macOS gate invalid examples json contract passed"
