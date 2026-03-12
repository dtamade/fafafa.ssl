#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_examples_cleanup_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
HISTORY_ALIAS="$REPORTS_DIR/examples-compile-history/examples_compile_ci_gate.json"

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

echo "[TEST] wave b ci gate examples history alias cleanup contract"

mkdir -p "$FAKE_BIN_DIR" "$(dirname "$HISTORY_ALIAS")"
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

cat > "$HISTORY_ALIAS" <<'EOF_STALE'
{"run_id":"stale_history","summary":{"total":1,"passed":0,"failed":1,"skipped":0,"pass_rate":0.0}}
EOF_STALE

PATH="$FAKE_BIN_DIR:$PATH" \
FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" \
/bin/bash scripts/run_wave_b_ci_gate.sh --skip-compile --skip-modules >/dev/null

SUMMARY_FILE="$(find "$REPORTS_DIR" -maxdepth 1 -name 'wave_b_ci_gate_summary_*.md' | head -1)"
[[ -n "$SUMMARY_FILE" && -f "$SUMMARY_FILE" ]] || fail "summary should be generated"

[[ ! -f "$HISTORY_ALIAS" ]] || fail "stale history alias should be cleaned up"
assert_contains "$SUMMARY_FILE" "- History Alias Path: \`$REPORTS_REL/examples-compile-history/examples_compile_ci_gate.json\`"
assert_contains "$SUMMARY_FILE" '- History Alias Cleanup: `removed`'

echo "[PASS] wave b ci gate examples history alias cleanup contract passed"
