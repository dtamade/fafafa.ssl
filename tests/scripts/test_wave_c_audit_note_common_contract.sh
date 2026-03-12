#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
COMMON_LIB="$ROOT_DIR/scripts/wave_c_audit_note_common.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] wave c audit note common lib contract"

[[ -f "$COMMON_LIB" ]] || fail "common lib not found: $COMMON_LIB"

# shellcheck source=/dev/null
source "$COMMON_LIB"

assert_map() {
  local alert_state="$1"
  local expected="$2"
  local actual
  actual="$(wave_c_map_alert_state_to_audit_note "$alert_state")"
  [[ "$actual" == "$expected" ]] || fail "map mismatch for $alert_state: expected $expected got $actual"
}

assert_map "WARN" "B148_ALERT_WARN_REVIEW_REQUIRED"
assert_map "CLEAR" "B148_ALERT_CLEAR"
assert_map "MISSING" "B148_ALERT_MISSING"
assert_map "UNKNOWN" "B148_ALERT_UNKNOWN"
assert_map "ANYTHING_ELSE" "B148_ALERT_UNKNOWN"

allowed_values=(
  "B148_ALERT_WARN_REVIEW_REQUIRED"
  "B148_ALERT_CLEAR"
  "B148_ALERT_MISSING"
  "B148_ALERT_UNKNOWN"
)
for value in "${allowed_values[@]}"; do
  wave_c_is_allowed_audit_note "$value" || fail "allowed value rejected: $value"
done

if wave_c_is_allowed_audit_note "INVALID_AUDIT_NOTE"; then
  fail "invalid value should be rejected"
fi

sync_missing="$(wave_c_compute_audit_note_sync_state "MISSING" "B148_ALERT_CLEAR")"
[[ "$sync_missing" == "MISSING" ]] || fail "sync should be MISSING when projected is MISSING"

sync_unknown="$(wave_c_compute_audit_note_sync_state "UNKNOWN" "B148_ALERT_CLEAR")"
[[ "$sync_unknown" == "MISSING" ]] || fail "sync should be MISSING when projected is UNKNOWN"

sync_match="$(wave_c_compute_audit_note_sync_state "B148_ALERT_CLEAR" "B148_ALERT_CLEAR")"
[[ "$sync_match" == "MATCH" ]] || fail "sync should be MATCH"

sync_mismatch="$(wave_c_compute_audit_note_sync_state "B148_ALERT_CLEAR" "B148_ALERT_WARN_REVIEW_REQUIRED")"
[[ "$sync_mismatch" == "MISMATCH" ]] || fail "sync should be MISMATCH"

chain_missing="$(wave_c_compute_audit_note_chain_consistency "MISSING" "B148_ALERT_CLEAR" "B148_ALERT_CLEAR")"
[[ "$chain_missing" == "MISSING" ]] || fail "chain should be MISSING when any value is MISSING"

chain_unknown="$(wave_c_compute_audit_note_chain_consistency "B148_ALERT_CLEAR" "UNKNOWN" "B148_ALERT_CLEAR")"
[[ "$chain_unknown" == "MISSING" ]] || fail "chain should be MISSING when any value is UNKNOWN"

chain_match="$(wave_c_compute_audit_note_chain_consistency "B148_ALERT_CLEAR" "B148_ALERT_CLEAR" "B148_ALERT_CLEAR")"
[[ "$chain_match" == "MATCH" ]] || fail "chain should be MATCH"

chain_mismatch="$(wave_c_compute_audit_note_chain_consistency "B148_ALERT_CLEAR" "B148_ALERT_WARN_REVIEW_REQUIRED" "B148_ALERT_CLEAR")"
[[ "$chain_mismatch" == "MISMATCH" ]] || fail "chain should be MISMATCH"

echo "[PASS] wave c audit note common lib contract passed"
