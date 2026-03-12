#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/review_closure_gate_weekly_trend_drift_draft.sh"
REL_WORK="tmp/test_review_closure_gate_weekly_strict"
REL_REPORT_DIR="$REL_WORK/reports"
REL_REPORT_GLOB="$REL_REPORT_DIR/*.md"
REL_OUTPUT="$REL_WORK/review_strict.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    echo "[INFO] top of output ($file):"
    sed -n '1,160p' "$file" || true
    exit 1
  fi
}

write_gate_report() {
  local file="$1"
  local total="$2"
  local failed="$3"
  cat > "$file" <<EOF
# Closure Gate Fixture

| Field | Value |
|-------|-------|
| total_checks | $total |
| checks_failed | $failed |
EOF
}

run_strict_contract() {
  rm -rf "$ROOT_DIR/$REL_WORK"
  mkdir -p "$ROOT_DIR/$REL_REPORT_DIR"
  rm -f "$ROOT_DIR/$REL_OUTPUT" "/tmp/$REL_OUTPUT"

  write_gate_report "$ROOT_DIR/$REL_REPORT_DIR/2026-01.md" 10 5
  write_gate_report "$ROOT_DIR/$REL_REPORT_DIR/2026-02.md" 10 1

  if (cd /tmp && bash "$SCRIPT" \
    --review-id strict_contract_drift_detected \
    --gate-report-glob "$REL_REPORT_GLOB" \
    --drift-threshold 10 \
    --output "$REL_OUTPUT" \
    --strict >/dev/null 2>&1); then
    fail "strict mode should fail when drift is detected"
  fi

  [[ -f "$ROOT_DIR/$REL_OUTPUT" ]] || fail "strict mode should still write report under project root"
  [[ ! -f "/tmp/$REL_OUTPUT" ]] || fail "strict mode output leaked into /tmp"

  assert_contains "$ROOT_DIR/$REL_OUTPUT" "| trend_direction | improving |"
  assert_contains "$ROOT_DIR/$REL_OUTPUT" "| drift_status | detected |"
  assert_contains "$ROOT_DIR/$REL_OUTPUT" "| review_status | warn |"

  echo "[PASS] strict mode contract passed"
}

run_strict_contract
