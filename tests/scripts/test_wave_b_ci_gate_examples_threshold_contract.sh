#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_examples_threshold_$(date +%s)"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_ROOT="$WORK_DIR/fake_project"
FAKE_SCRIPTS="$FAKE_ROOT/scripts"
RUN_ID="wave_b_examples_threshold_truth"
SUMMARY_REL="tmp/wave_b_ci_gate_summary_${RUN_ID}.md"
SUMMARY_ABS="$FAKE_ROOT/$SUMMARY_REL"

mkdir -p "$FAKE_SCRIPTS"
trap 'rm -rf "$WORK_DIR"' EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

cp "$ROOT_DIR/scripts/run_wave_b_ci_gate.sh" "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh"

cat > "$FAKE_SCRIPTS/compile_all_modules.py" <<'EOF'
#!/usr/bin/env python3
print("compile ok")
EOF

cat > "$FAKE_SCRIPTS/run_all_module_tests.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo "modules ok"
EOF

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
cat > "$OUTPUT" <<'JSON'
{
  "timestamp": "fake",
  "fpc_version": "fake",
  "summary": {
    "total": 75,
    "passed": 71,
    "failed": 2,
    "skipped": 2,
    "pass_rate": 94.7
  },
  "failed_files": [
    "examples/fake_fail_1.pas",
    "examples/fake_fail_2.pas"
  ]
}
JSON

exit 1
EOF

chmod +x "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" \
  "$FAKE_SCRIPTS/compile_all_modules.py" \
  "$FAKE_SCRIPTS/run_all_module_tests.sh" \
  "$FAKE_SCRIPTS/verify_examples_compile.sh"

set +e
(cd "$FAKE_ROOT" && bash "$FAKE_SCRIPTS/run_wave_b_ci_gate.sh" \
  --run-id "$RUN_ID" \
  --examples-threshold 80.0 \
  --reports-dir tmp >/dev/null)
exit_code=$?
set -e

if [[ ! -f "$SUMMARY_ABS" ]]; then
  fail "expected wave b linux gate summary"
fi

if [[ "$exit_code" -ne 0 ]]; then
  fail "wave b linux gate should stay green when examples pass_rate meets threshold even if helper reports failed files"
fi

if ! rg -n "^- Overall Status: \\*\\*PASS\\*\\*$" "$SUMMARY_ABS" >/dev/null; then
  fail "wave b linux gate summary should stay PASS when examples pass_rate meets threshold"
fi

if ! rg -n "^\\| verify_examples_compile \\| \`1\` \\| \\*\\*PASS\\*\\* \\|" "$SUMMARY_ABS" >/dev/null; then
  fail "examples step should be judged by threshold truth instead of helper exit code alone"
fi

if ! rg -n "passed=71, failed=2, skipped=2, total=75, pass_rate=94.7" "$SUMMARY_ABS" >/dev/null; then
  fail "summary should expose the threshold-driving examples metrics"
fi

echo "[PASS] wave_b_ci_gate examples threshold contract passed"
