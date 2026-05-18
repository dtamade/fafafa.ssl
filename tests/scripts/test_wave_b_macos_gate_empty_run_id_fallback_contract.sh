#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_empty_run_id_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
OUTPUT_DIR_REL="$WORK_REL/out"
OUTPUT_DIR_ABS="$WORK_DIR/$OUTPUT_DIR_REL"
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

cat > "$WORK_DIR/scripts/run_macos_openssl_loader_symbol_probe.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output) out="$2"; shift 2 ;;
    *) shift ;;
  esac
done
mkdir -p "$(dirname "$out")"
cat > "$out" <<'JSON'
{"status":"ok"}
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
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "" \
    --output-dir "$OUTPUT_DIR_REL" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b macOS gate should fall back to a generated run-id when an empty run-id is provided"
fi

if [[ -e "$OUTPUT_DIR_ABS/wave_b_macos_gate_summary_.md" ]]; then
  fail "wave b macOS gate should not emit an empty-suffix summary when run-id is empty"
fi

summary_count=$(find "$OUTPUT_DIR_ABS" -maxdepth 1 -name 'wave_b_macos_gate_summary_*.md' | wc -l | tr -d ' ')
if [[ "$summary_count" != "1" ]]; then
  fail "wave b macOS gate should emit exactly one summary after empty run-id fallback"
fi

summary_file=$(find "$OUTPUT_DIR_ABS" -maxdepth 1 -name 'wave_b_macos_gate_summary_*.md' | head -n 1)
generated_run_id="${summary_file##*/wave_b_macos_gate_summary_}"
generated_run_id="${generated_run_id%.md}"

if [[ -z "$generated_run_id" ]]; then
  fail "wave b macOS gate should generate a non-empty fallback run-id"
fi

if ! rg -n "^- run_id: ${generated_run_id}$" "$summary_file" >/dev/null; then
  fail "wave b macOS summary should record the generated fallback run-id"
fi

if [[ ! -f "$OUTPUT_DIR_ABS/examples_compile_gate_macos_${generated_run_id}.json" ]]; then
  fail "wave b macOS gate should keep report file names aligned with the generated fallback run-id"
fi

echo "[PASS] wave b macOS gate empty run-id fallback contract passed"
