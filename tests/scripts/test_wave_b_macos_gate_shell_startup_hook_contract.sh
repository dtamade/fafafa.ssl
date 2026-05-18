#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_macos_gate_shell_startup_hook_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
RUN_ID="wave_b_macos_shell_startup_hook"
SUMMARY_ABS="$FAKE_ROOT/out/wave_b_macos_gate_summary_${RUN_ID}.md"
MARKER="$WORK_DIR/shell_startup_hook.marker"
ZDOTDIR_DIR="$WORK_DIR/zdotdir"
STDOUT_LOG="$WORK_DIR/stdout.log"
STDERR_LOG="$WORK_DIR/stderr.log"

cleanup() {
  rm -rf "$WORK_DIR"
}

mkdir -p "$FAKE_SCRIPTS"
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

if [[ ! -x "/usr/bin/zsh" ]]; then
  echo "[PASS] skipped: /usr/bin/zsh is unavailable, so current macOS gate zsh startup-hook path is not active on this host"
  exit 0
fi

cp "$ROOT_DIR/scripts/run_wave_b_macos_gate.sh" "$FAKE_SCRIPTS/"

cat > "$FAKE_SCRIPTS/detect_macos_openssl_enhanced.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo '{"status":"ok"}'
EOF

cat > "$FAKE_SCRIPTS/run_macos_openssl_loader_symbol_probe.sh" <<'EOF'
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

cat > "$FAKE_SCRIPTS/run_macos_openssl_path_check_draft.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
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
    -o|--output) out="$2"; shift 2 ;;
    *) shift ;;
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

mkdir -p "$ZDOTDIR_DIR"
cat > "$ZDOTDIR_DIR/.zshenv" <<EOF
touch $(printf '%q' "$MARKER")
EOF

set +e
(
  cd "$FAKE_ROOT"
  ZDOTDIR="$ZDOTDIR_DIR" \
  OSTYPE=darwin23 \
  bash scripts/run_wave_b_macos_gate.sh \
    --run-id "$RUN_ID" \
    --output-dir out >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b macOS gate should still stay green with fake green runners"
fi

if [[ -e "$MARKER" ]]; then
  fail "wave b macOS gate should not source zsh startup hooks while executing steps"
fi

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "expected macOS gate summary to be generated"
fi

if ! rg -n "^- overall: \\*\\*PASS\\*\\*" "$SUMMARY_ABS" >/dev/null; then
  fail "macOS gate summary should stay PASS in the fake green scenario"
fi

echo "[PASS] wave b macOS gate shell startup-hook contract passed"
