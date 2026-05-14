#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_invalid_examples_json_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
RUN_ID="wave_b_linux_invalid_examples_json"
SUMMARY_REL="tmp/wave_b_ci_gate_summary_${RUN_ID}.md"
SUMMARY_ABS="$FAKE_ROOT/$SUMMARY_REL"
STDOUT_LOG="$FAKE_ROOT/stdout.log"
STDERR_LOG="$FAKE_ROOT/stderr.log"

mkdir -p "$FAKE_SCRIPTS"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_b_ci_gate.sh" "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh"

cat > "$FAKE_SCRIPTS/verify_examples_compile.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

OUTPUT=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o|--output)
      OUTPUT="$2"
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done

mkdir -p "$(dirname "$OUTPUT")"
printf '{ bad json\n' > "$OUTPUT"
exit 0
EOF

chmod +x "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" "$FAKE_SCRIPTS/verify_examples_compile.sh"

set +e
(
  cd "$FAKE_ROOT"
  bash "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" \
    --skip-compile \
    --skip-modules \
    --run-id "$RUN_ID" \
    --reports-dir tmp >"$STDOUT_LOG" 2>"$STDERR_LOG"
)
exit_code=$?
set -e

if [[ "$exit_code" -eq 0 ]]; then
  fail "wave b linux gate should fail when examples json is invalid even if helper exits 0"
fi

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "wave b linux gate should still emit a summary when examples json is invalid"
fi

if rg -n "JSONDecodeError|Traceback" "$STDERR_LOG" >/dev/null; then
  fail "wave b linux gate should not leak python traceback when examples json is invalid"
fi

if ! rg -n "^\\| verify_examples_compile \\| \`0\` \\| \\*\\*FAIL\\*\\* \\|" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b linux summary should mark examples step FAIL when json is invalid"
fi

if ! rg -n "^- Overall Status: \\*\\*FAIL\\*\\*$" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b linux summary should become FAIL when examples json is invalid"
fi

if ! rg -n "Summary: \`passed=n/a, failed=n/a, skipped=n/a, total=n/a, pass_rate=n/a\`" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b linux summary should keep examples metrics at n/a when json is invalid"
fi

echo "[PASS] wave b linux gate invalid examples json contract passed"
