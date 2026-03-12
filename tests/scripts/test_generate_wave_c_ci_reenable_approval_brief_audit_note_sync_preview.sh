#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_wave_c_ci_reenable_approval_brief.sh"
WORK_REL="tmp/test_wave_c_b148_audit_note_sync_preview"
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
    sed -n '1,340p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b148 audit note sync preview contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

# Case 1: MATCH (CLEAR vs CLEAR)
RUN_MATCH="b148_sync_match_$$"
PACK_MATCH_REL="$WORK_REL/pack_match.md"
CHECK_MATCH_REL="$WORK_REL/check_match.md"
OUT_MATCH_REL="$WORK_REL/brief_match.md"
OUT_MATCH="$ROOT_DIR/$OUT_MATCH_REL"

cat > "$ROOT_DIR/$PACK_MATCH_REL" <<EOF_MATCH
# Wave C B146 CI Re-enable Submission Pack

- run_id: $RUN_MATCH
- submission_state: **READY_TO_SUBMIT**
EOF_MATCH

cat > "$ROOT_DIR/$CHECK_MATCH_REL" <<EOF_MATCH_CHECK
# Wave C B147 Submission Pack Check

- run_id: $RUN_MATCH
- check_state: **PASS**
- projected_b149_audit_alert_note: **B148_ALERT_CLEAR**

## Token Checks

| token | result |
|-------|--------|
| submission_state | PASS |
| packet_state | PASS |
EOF_MATCH_CHECK

bash "$SCRIPT" --run-id "$RUN_MATCH" --input "$PACK_MATCH_REL" --check "$CHECK_MATCH_REL" --output "$OUT_MATCH_REL" >/dev/null

[[ -f "$OUT_MATCH" ]] || fail "match case should generate brief"
assert_contains "$OUT_MATCH" "## Audit Note Sync Preview"
assert_contains "$OUT_MATCH" "- projected_b149_audit_alert_note: B148_ALERT_CLEAR"
assert_contains "$OUT_MATCH" "- b149_audit_alert_note_preview: **B148_ALERT_CLEAR**"
assert_contains "$OUT_MATCH" "- b149_audit_alert_note_sync_state: **MATCH**"

# Case 2: MISMATCH (projected CLEAR vs preview WARN)
RUN_MISMATCH="b148_sync_mismatch_$$"
PACK_MISMATCH_REL="$WORK_REL/pack_mismatch.md"
CHECK_MISMATCH_REL="$WORK_REL/check_mismatch.md"
OUT_MISMATCH_REL="$WORK_REL/brief_mismatch.md"
OUT_MISMATCH="$ROOT_DIR/$OUT_MISMATCH_REL"

cat > "$ROOT_DIR/$PACK_MISMATCH_REL" <<EOF_MISMATCH
# Wave C B146 CI Re-enable Submission Pack

- run_id: $RUN_MISMATCH
- submission_state: **HOLD**
EOF_MISMATCH

cat > "$ROOT_DIR/$CHECK_MISMATCH_REL" <<EOF_MISMATCH_CHECK
# Wave C B147 Submission Pack Check

- run_id: $RUN_MISMATCH
- check_state: **FAIL**
- projected_b149_audit_alert_note: **B148_ALERT_CLEAR**

## Token Checks

| token | result |
|-------|--------|
| submission_state | PASS |
| packet_state | FAIL |
EOF_MISMATCH_CHECK

bash "$SCRIPT" --run-id "$RUN_MISMATCH" --input "$PACK_MISMATCH_REL" --check "$CHECK_MISMATCH_REL" --output "$OUT_MISMATCH_REL" >/dev/null

[[ -f "$OUT_MISMATCH" ]] || fail "mismatch case should generate brief"
assert_contains "$OUT_MISMATCH" "- projected_b149_audit_alert_note: B148_ALERT_CLEAR"
assert_contains "$OUT_MISMATCH" "- b149_audit_alert_note_preview: **B148_ALERT_WARN_REVIEW_REQUIRED**"
assert_contains "$OUT_MISMATCH" "- b149_audit_alert_note_sync_state: **MISMATCH**"

echo "[PASS] wave c b148 audit note sync preview contract passed"
