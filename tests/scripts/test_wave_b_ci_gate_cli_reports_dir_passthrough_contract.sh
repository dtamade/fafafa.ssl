#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_cli_reports_dir_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
LEGACY_DIR="$ROOT_DIR/test-reports"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,260p' "$file" || cat "$file" || true
    exit 1
  fi
}

echo "[TEST] wave b ci gate cli reports-dir passthrough contract"

mkdir -p "$FAKE_BIN_DIR"
cat > "$FAKE_BIN_DIR/fpc" <<'EOF_FAKE_FPC'
#!/bin/bash
set -euo pipefail
if [[ "${1:-}" == "-iV" ]]; then
  echo "3.2.2"
  exit 0
fi
exit 0
EOF_FAKE_FPC
chmod +x "$FAKE_BIN_DIR/fpc"

PATH="$FAKE_BIN_DIR:$PATH" \
/bin/bash scripts/run_wave_b_ci_gate.sh --skip-compile --skip-modules --reports-dir "$REPORTS_REL" >/dev/null

EXAMPLES_JSON="$REPORTS_DIR/examples_compile_ci_gate.json"
[[ -f "$EXAMPLES_JSON" ]] || fail "examples json should be generated under cli reports dir"

SUMMARY_FILE="$(find "$REPORTS_DIR" -maxdepth 1 -name 'wave_b_ci_gate_summary_*.md' | head -1)"
[[ -n "$SUMMARY_FILE" && -f "$SUMMARY_FILE" ]] || fail "summary should be generated under cli reports dir"
RUN_ID="$(basename "$SUMMARY_FILE" | sed -E 's/^wave_b_ci_gate_summary_(.*)\.md$/\1/')"
[[ -n "$RUN_ID" ]] || fail "run_id should be derivable from summary filename"

assert_contains "$EXAMPLES_JSON" "\"run_id\": \"$RUN_ID\""
assert_contains "$SUMMARY_FILE" "- Report: \`$REPORTS_REL/examples_compile_ci_gate.json\`"
assert_contains "$SUMMARY_FILE" "- Current Alias: \`$REPORTS_REL/examples_compile_ci_gate.json\`"
assert_contains "$SUMMARY_FILE" "- Run-Scoped Copy: \`$REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json\`"
assert_contains "$SUMMARY_FILE" "- Archive Copy: \`$REPORTS_REL/examples-compile-history/examples_compile_ci_gate_${RUN_ID}.json\`"

[[ ! -f "$LEGACY_DIR/examples_compile_ci_gate.json" ]] || fail "examples json should not leak to legacy test-reports"
[[ ! -f "$LEGACY_DIR/wave_b_ci_gate_summary_${RUN_ID}.md" ]] || fail "summary should not leak to legacy test-reports"

echo "[PASS] wave b ci gate cli reports-dir passthrough contract passed"
