#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_wave_b_platform_archive_logs_$$"
WORK_DIR="$ROOT_DIR/$WORK_REL"
WAVE_B_REL="$WORK_REL/wave_b_reports"
WAVE_B_DIR="$ROOT_DIR/$WAVE_B_REL"
ARCHIVE_OUT_REL="$WORK_REL/archive_out"

RUN_ID="wave_b_archive_logs_$$"

cleanup() {
  rm -rf "$WORK_DIR"
}
trap cleanup EXIT

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave b platform archive debug logs runtime contract"

mkdir -p "$WAVE_B_DIR"

cat > "$WAVE_B_DIR/wave_b_compile_${RUN_ID}.log" <<EOF_LOG
compile log
EOF_LOG

cat > "$WAVE_B_DIR/wave_b_modules_${RUN_ID}.log" <<EOF_LOG
modules log
EOF_LOG

cat > "$WAVE_B_DIR/wave_b_examples_${RUN_ID}.log" <<EOF_LOG
examples log
EOF_LOG

cat > "$WAVE_B_DIR/wave_b_macos_probe_${RUN_ID}.log" <<EOF_LOG
macos probe log
EOF_LOG

cat > "$WAVE_B_DIR/wave_b_windows_winssl_${RUN_ID}.log" <<EOF_LOG
windows winssl log
EOF_LOG

ARCHIVE_OUT="$({
  FAFAFA_WAVE_B_REPORTS_DIR="$WAVE_B_REL" \
  bash scripts/archive_ci_artifacts_draft.sh --profile pr --run-id "$RUN_ID" --output-root "$ARCHIVE_OUT_REL" --dry-run
} 2>&1)"

for expected in \
  "$WAVE_B_REL/wave_b_compile_${RUN_ID}.log" \
  "$WAVE_B_REL/wave_b_modules_${RUN_ID}.log" \
  "$WAVE_B_REL/wave_b_examples_${RUN_ID}.log" \
  "$WAVE_B_REL/wave_b_macos_probe_${RUN_ID}.log" \
  "$WAVE_B_REL/wave_b_windows_winssl_${RUN_ID}.log"
do
  if [[ "$ARCHIVE_OUT" != *"$expected"* ]]; then
    echo "$ARCHIVE_OUT"
    fail "archive dry-run should include Wave B debug log $expected"
  fi
done

echo "[PASS] wave b platform archive debug logs runtime contract passed"
