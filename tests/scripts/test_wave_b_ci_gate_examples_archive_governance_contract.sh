#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_examples_archive_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"

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

echo "[TEST] wave b ci gate examples archive governance contract"

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
FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
/bin/bash scripts/run_wave_b_ci_gate.sh --skip-compile --skip-modules >/dev/null

SUMMARY_FILE="$(find "$REPORTS_DIR" -maxdepth 1 -name 'wave_b_ci_gate_summary_*.md' | head -1)"
[[ -n "$SUMMARY_FILE" && -f "$SUMMARY_FILE" ]] || fail "summary should be generated"
RUN_ID="$(basename "$SUMMARY_FILE" | sed -E 's/^wave_b_ci_gate_summary_(.*)\.md$/\1/')"
[[ -n "$RUN_ID" ]] || fail "run_id should be derivable from summary filename"

STATIC_JSON="$REPORTS_DIR/examples_compile_ci_gate.json"
RUN_SCOPED_JSON="$REPORTS_DIR/examples_compile_ci_gate_${RUN_ID}.json"
ARCHIVE_JSON="$REPORTS_DIR/examples-compile-history/examples_compile_ci_gate_${RUN_ID}.json"

[[ -f "$STATIC_JSON" ]] || fail "static current alias should be generated"
[[ -f "$RUN_SCOPED_JSON" ]] || fail "run-scoped copy should be generated"
[[ -f "$ARCHIVE_JSON" ]] || fail "archive copy should be generated"

assert_contains "$STATIC_JSON" "\"run_id\": \"$RUN_ID\""
assert_contains "$RUN_SCOPED_JSON" "\"run_id\": \"$RUN_ID\""
assert_contains "$ARCHIVE_JSON" "\"run_id\": \"$RUN_ID\""
assert_contains "$SUMMARY_FILE" "- Current Alias: \`$REPORTS_REL/examples_compile_ci_gate.json\`"
assert_contains "$SUMMARY_FILE" "- Alias Owner Run ID: \`$RUN_ID\`"
assert_contains "$SUMMARY_FILE" "- Run-Scoped Copy: \`$REPORTS_REL/examples_compile_ci_gate_${RUN_ID}.json\`"
assert_contains "$SUMMARY_FILE" "- Archive Copy: \`$REPORTS_REL/examples-compile-history/examples_compile_ci_gate_${RUN_ID}.json\`"

echo "[PASS] wave b ci gate examples archive governance contract passed"
