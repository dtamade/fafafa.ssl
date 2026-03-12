#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_b_b2_closure_readiness.sh"
WORK_REL="tmp/test_wave_b_b2_closure_runid_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
REPORTS_REL="$WORK_REL/wave_b_reports"
REPORTS_DIR="$ROOT_DIR/$REPORTS_REL"
TARGET_RUN="wave_b_closure_target_$$"
OTHER_RUN="wave_b_closure_other_$$"
OUTPUT_REL="$WORK_REL/closure_target.md"
OUTPUT_FILE="$ROOT_DIR/$OUTPUT_REL"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave-b b2 closure readiness run-id scoped default contract"

mkdir -p "$REPORTS_DIR"

cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${TARGET_RUN}.md" <<EOF_TARGET_LINUX
# Wave B Linux CI Gate Summary

- Run ID: ${TARGET_RUN}
- Overall Status: **PASS**
EOF_TARGET_LINUX

cat > "$WORK_DIR/macos_target.md" <<EOF_MAC
# Wave B macOS Gate Summary

- run_id: ${TARGET_RUN}
- overall: **PASS**
EOF_MAC

cat > "$WORK_DIR/windows_target.md" <<EOF_WIN
# Wave B Windows Gate Summary

- run_id: ${TARGET_RUN}
- overall: **PASS**
EOF_WIN

sleep 1
cat > "$REPORTS_DIR/wave_b_ci_gate_summary_${OTHER_RUN}.md" <<EOF_OTHER_LINUX
# Wave B Linux CI Gate Summary

- Run ID: ${OTHER_RUN}
- Overall Status: **FAIL**
EOF_OTHER_LINUX

(cd /tmp && FAFAFA_WAVE_B_REPORTS_DIR="$REPORTS_REL" bash "$SCRIPT" \
  --run-id "$TARGET_RUN" \
  --macos-summary "$WORK_REL/macos_target.md" \
  --windows-summary "$WORK_REL/windows_target.md" \
  --output "$OUTPUT_REL" >/dev/null)

[[ -f "$OUTPUT_FILE" ]] || fail "closure report should be generated"

if ! rg -F --quiet -- '- closure_status: **CLOSED**' "$OUTPUT_FILE"; then
  sed -n '1,220p' "$OUTPUT_FILE" || true
  fail "closure should be CLOSED when target run evidence is complete"
fi

if ! rg -F --quiet -- "| linux | PASS | summary parsed | $REPORTS_REL/wave_b_ci_gate_summary_${TARGET_RUN}.md |" "$OUTPUT_FILE"; then
  sed -n '1,220p' "$OUTPUT_FILE" || true
  fail "default linux summary should prefer the current run-id scoped file"
fi

echo "[PASS] wave-b b2 closure readiness run-id scoped default contract passed"
