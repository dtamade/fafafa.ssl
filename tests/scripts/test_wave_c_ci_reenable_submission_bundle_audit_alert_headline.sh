#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_wave_c_ci_reenable_submission_bundle.sh"
WORK_REL="tmp/test_wave_c_b149_audit_alert_headline"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

extract_marked_state() {
  local file="$1"
  local key="$2"
  rg -o "${key}:[[:space:]]*\\*\\*[A-Z_]+\\*\\*" "$file" \
    | head -1 \
    | sed -E 's/.*\*\*([A-Z_]+)\*\*/\1/' || true
}

map_audit_note() {
  local alert_state="$1"
  case "$alert_state" in
    WARN) echo "B148_ALERT_WARN_REVIEW_REQUIRED" ;;
    CLEAR) echo "B148_ALERT_CLEAR" ;;
    MISSING) echo "B148_ALERT_MISSING" ;;
    *) echo "B148_ALERT_UNKNOWN" ;;
  esac
}

echo "[TEST] wave c b149 top audit alert headline contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR/reports"

RUN_ID="b149_alert_headline_$$"
OUT_REL="$WORK_REL/reports/b149.md"
OUT_FILE="$ROOT_DIR/$OUT_REL"
B148_REPORT="$ROOT_DIR/tmp/wave_c_ci_reenable_reports/wave_c_b148_ci_reenable_approval_brief_${RUN_ID}.md"

rm -f "$OUT_FILE" "$B148_REPORT"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "$RUN_ID" \
  --output "$OUT_REL" \
  --skip-local-guard-batch \
  --skip-docs-governance >/dev/null)

[[ -f "$OUT_FILE" ]] || fail "b149 report should be generated"
[[ -f "$B148_REPORT" ]] || fail "b148 report should be generated"

alert_state="$(extract_marked_state "$B148_REPORT" "alert_state")"
[[ -n "$alert_state" ]] || fail "b148 alert_state should be present"
expected_note="$(map_audit_note "$alert_state")"

if ! head -n 20 "$OUT_FILE" | rg -F --quiet -- "- audit_alert_note: **$expected_note**"; then
  echo "[FAIL] b149 top metadata should include mapped audit alert note"
  sed -n '1,120p' "$OUT_FILE" || true
  exit 1
fi

echo "[PASS] wave c b149 top audit alert headline contract passed"
