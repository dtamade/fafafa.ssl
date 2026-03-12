#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_docs_governance_integration"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,280p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b149 docs governance integration contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_DEFAULT="b149_docs_gov_default_$$"
OUT_DEFAULT_REL="$WORK_REL/reports/default.md"
OUT_DEFAULT="$ROOT_DIR/$OUT_DEFAULT_REL"
DEFAULT_DOCS_LOG="$ROOT_DIR/test-reports/wave_c_docs_governance_batch_${RUN_DEFAULT}.b149.log"

rm -f "$OUT_DEFAULT" "$DEFAULT_DOCS_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_DEFAULT" \
  --output "$OUT_DEFAULT_REL" >/dev/null)

[[ -f "$OUT_DEFAULT" ]] || fail "default run should generate b149 report"
[[ -f "$DEFAULT_DOCS_LOG" ]] || fail "default run should generate docs governance batch log"
assert_contains "$OUT_DEFAULT" "| B149D docs governance strict batch |"
assert_contains "$DEFAULT_DOCS_LOG" "[PASS] docs active noise + index dedup strict batch passed"

RUN_SKIP="b149_docs_gov_skip_$$"
OUT_SKIP_REL="$WORK_REL/reports/skip.md"
OUT_SKIP="$ROOT_DIR/$OUT_SKIP_REL"
SKIP_DOCS_LOG="$ROOT_DIR/test-reports/wave_c_docs_governance_batch_${RUN_SKIP}.b149.log"

rm -f "$OUT_SKIP" "$SKIP_DOCS_LOG"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_SKIP" \
  --output "$OUT_SKIP_REL" \
  --skip-docs-governance >/dev/null)

[[ -f "$OUT_SKIP" ]] || fail "skip docs run should generate b149 report"
[[ ! -f "$SKIP_DOCS_LOG" ]] || fail "skip docs run should not generate docs governance log"
assert_contains "$OUT_SKIP" "| B149D docs governance strict batch | SKIP | <none> | <none> |"

echo "[PASS] wave c b149 docs governance integration contract passed"
