#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_examples_history_retention_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
RUN_ID="wave_b_history_retention_$$"
BAD_ARCHIVE_REL="$REPORTS_REL/examples-compile-history/examples_compile_latest.json"
BAD_ARCHIVE_FILE="$ROOT_DIR/$BAD_ARCHIVE_REL"
EXPECTED_ARCHIVE_REL="$REPORTS_REL/examples-compile-history/examples_compile_ci_gate_${RUN_ID}.json"
EXPECTED_ARCHIVE_FILE="$ROOT_DIR/$EXPECTED_ARCHIVE_REL"

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

echo "[TEST] wave b ci gate examples history retention naming contract"

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
FAFAFA_WAVE_B_CI_GATE_RUN_ID="$RUN_ID" \
FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
FAFAFA_WAVE_B_EXAMPLES_ARCHIVE_REPORT_REL="$BAD_ARCHIVE_REL" \
/bin/bash scripts/run_wave_b_ci_gate.sh --skip-compile --skip-modules >/dev/null

SUMMARY_FILE="$REPORTS_DIR/wave_b_ci_gate_summary_${RUN_ID}.md"
[[ -f "$SUMMARY_FILE" ]] || fail "summary should be generated"
[[ -f "$EXPECTED_ARCHIVE_FILE" ]] || fail "archive copy should be normalized to run-scoped history name"
[[ ! -f "$BAD_ARCHIVE_FILE" ]] || fail "generic archive filename should not be used"

assert_contains "$EXPECTED_ARCHIVE_FILE" "\"run_id\": \"$RUN_ID\""
assert_contains "$SUMMARY_FILE" "- Archive Copy: \`$EXPECTED_ARCHIVE_REL\`"
assert_contains "$SUMMARY_FILE" "- History Alias Path: \`$REPORTS_REL/examples-compile-history/examples_compile_ci_gate.json\`"

echo "[PASS] wave b ci gate examples history retention naming contract passed"
