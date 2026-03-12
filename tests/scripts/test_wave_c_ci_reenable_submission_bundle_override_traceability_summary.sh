#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_override_traceability_summary"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet -- "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    sed -n '1,420p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b149 override traceability summary contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

# Case 1: default (no override)
RUN_DEFAULT="b149_override_trace_default_$$"
OUT_DEFAULT_REL="$WORK_REL/reports/default.md"
OUT_DEFAULT="$ROOT_DIR/$OUT_DEFAULT_REL"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_DEFAULT" \
  --output "$OUT_DEFAULT_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance >/dev/null)

[[ -f "$OUT_DEFAULT" ]] || fail "default report should be generated"
assert_contains "$OUT_DEFAULT" "- b147_projected_audit_note_source: B147_REPORT"
assert_contains "$OUT_DEFAULT" "- b147_projected_audit_note_override_value: NONE"

# Case 2: override
RUN_OVERRIDE="b149_override_trace_override_$$"
OUT_OVERRIDE_REL="$WORK_REL/reports/override.md"
OUT_OVERRIDE="$ROOT_DIR/$OUT_OVERRIDE_REL"
OVERRIDE_VALUE="B148_ALERT_MISSING"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_OVERRIDE" \
  --output "$OUT_OVERRIDE_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance \
  --override-b147-projected-audit-note "$OVERRIDE_VALUE" >/dev/null)

[[ -f "$OUT_OVERRIDE" ]] || fail "override report should be generated"
assert_contains "$OUT_OVERRIDE" "- b147_projected_audit_note_source: OVERRIDE"
assert_contains "$OUT_OVERRIDE" "- b147_projected_audit_note_override_value: $OVERRIDE_VALUE"
assert_contains "$OUT_OVERRIDE" "- b147_projected_b149_audit_alert_note: $OVERRIDE_VALUE"

echo "[PASS] wave c b149 override traceability summary contract passed"
