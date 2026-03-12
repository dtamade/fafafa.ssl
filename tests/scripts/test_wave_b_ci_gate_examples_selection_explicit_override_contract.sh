#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_ci_gate_examples_selection_override_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
FAKE_BIN_DIR="$WORK_DIR/fakebin"
REPORTS_REL="$WORK_REL/reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
CUSTOM_REL="$WORK_REL/custom/examples_override.json"
CUSTOM_FILE="$ROOT_DIR/$CUSTOM_REL"
RUN_ID="wave_b_ci_gate_selection_override_$$"

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

echo "[TEST] wave b ci gate examples selection explicit override contract"

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
/bin/bash scripts/run_wave_b_ci_gate.sh \
  --skip-compile \
  --skip-modules \
  --reports-dir "$REPORTS_REL" \
  --examples-report "$CUSTOM_REL" >/dev/null

SUMMARY_FILE="$(find "$REPORTS_DIR" -maxdepth 1 -name 'wave_b_ci_gate_summary_*.md' | head -1)"
[[ -n "$SUMMARY_FILE" && -f "$SUMMARY_FILE" ]] || fail "summary should be generated"
[[ -f "$CUSTOM_FILE" ]] || fail "custom examples report should be generated"

assert_contains "$SUMMARY_FILE" "- Selection: \`explicit_override\`"

echo "[PASS] wave b ci gate examples selection explicit override contract passed"
