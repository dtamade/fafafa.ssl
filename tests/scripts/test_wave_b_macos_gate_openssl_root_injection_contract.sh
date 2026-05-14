#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_openssl_root_injection_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
OUTPUT_DIR_REL="out"
OUTPUT_DIR_ABS="$WORK_DIR/$OUTPUT_DIR_REL"
RUN_ID="wave_b_macos_openssl_root_injection"
MARKER="$WORK_DIR/openssl_root_injected.marker"
OPENSSL_ROOT_LOG="$WORK_DIR/openssl_root.log"
MALICIOUS_OPENSSL_ROOT="/tmp/ssl'; touch '$MARKER'; echo '"
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
printf '%s' "${OPENSSL_ROOT:-}" > "${OPENSSL_ROOT_LOG:?}"
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
  OPENSSL_ROOT_LOG="$OPENSSL_ROOT_LOG" \
  OSTYPE=darwin23 bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir "$OUTPUT_DIR_REL" \
    --openssl-root "$MALICIOUS_OPENSSL_ROOT" >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b macOS gate should treat openssl-root payload as data and still complete with fake green runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave b macOS gate should not execute shell content embedded in --openssl-root"
fi

if [[ ! -f "$OPENSSL_ROOT_LOG" ]]; then
  fail "fake probe should observe OPENSSL_ROOT"
fi

if [[ "$(cat "$OPENSSL_ROOT_LOG")" != "$MALICIOUS_OPENSSL_ROOT" ]]; then
  fail "wave b macOS gate should pass the full openssl-root payload as data to step environments"
fi

if [[ ! -f "$OUTPUT_DIR_ABS/wave_b_macos_gate_summary_${RUN_ID}.md" ]]; then
  fail "wave b macOS gate should still emit a summary after safe openssl-root passthrough"
fi

echo "[PASS] wave b macOS gate openssl-root injection contract passed"
