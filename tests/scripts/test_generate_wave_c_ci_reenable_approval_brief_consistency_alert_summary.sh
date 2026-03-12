#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_wave_c_ci_reenable_approval_brief.sh"
WORK_REL="tmp/test_wave_c_b148_consistency_alert_summary"
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
    sed -n '1,320p' "$file" || true
    exit 1
  fi
}

echo "[TEST] wave c b148 consistency alert summary contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

# Case 1: WARN
RUN_WARN="b148_alert_warn_$$"
OUT_WARN_REL="$WORK_REL/brief_warn.md"
OUT_WARN="$ROOT_DIR/$OUT_WARN_REL"
PACK_WARN_REL="$WORK_REL/pack_warn.md"
CHECK_WARN_REL="$WORK_REL/check_warn.md"

cat > "$ROOT_DIR/$PACK_WARN_REL" <<EOF
# Wave C B146 CI Re-enable Submission Pack

- run_id: $RUN_WARN
- submission_state: **HOLD**
EOF

cat > "$ROOT_DIR/$CHECK_WARN_REL" <<EOF
# Wave C B147 Submission Pack Check

- run_id: $RUN_WARN
- check_state: **FAIL**

## Token Checks

| token | result |
|-------|--------|
| submission_state | PASS |
| packet_state | FAIL |
EOF

bash "$SCRIPT" --run-id "$RUN_WARN" --input "$PACK_WARN_REL" --check "$CHECK_WARN_REL" --output "$OUT_WARN_REL" >/dev/null

[[ -f "$OUT_WARN" ]] || fail "warn case should generate brief"
assert_contains "$OUT_WARN" "## Consistency Alert Summary"
assert_contains "$OUT_WARN" "- source_check: $CHECK_WARN_REL"
assert_contains "$OUT_WARN" "- check_state: FAIL"
assert_contains "$OUT_WARN" "- token_fail_count: 1"
assert_contains "$OUT_WARN" "- alert_state: **WARN**"
assert_contains "$OUT_WARN" "| packet_state | FAIL |"

# Case 2: CLEAR
RUN_CLEAR="b148_alert_clear_$$"
OUT_CLEAR_REL="$WORK_REL/brief_clear.md"
OUT_CLEAR="$ROOT_DIR/$OUT_CLEAR_REL"
PACK_CLEAR_REL="$WORK_REL/pack_clear.md"
CHECK_CLEAR_REL="$WORK_REL/check_clear.md"

cat > "$ROOT_DIR/$PACK_CLEAR_REL" <<EOF
# Wave C B146 CI Re-enable Submission Pack

- run_id: $RUN_CLEAR
- submission_state: **READY_TO_SUBMIT**
EOF

cat > "$ROOT_DIR/$CHECK_CLEAR_REL" <<EOF
# Wave C B147 Submission Pack Check

- run_id: $RUN_CLEAR
- check_state: **PASS**

## Token Checks

| token | result |
|-------|--------|
| submission_state | PASS |
| packet_state | PASS |
EOF

bash "$SCRIPT" --run-id "$RUN_CLEAR" --input "$PACK_CLEAR_REL" --check "$CHECK_CLEAR_REL" --output "$OUT_CLEAR_REL" >/dev/null

[[ -f "$OUT_CLEAR" ]] || fail "clear case should generate brief"
assert_contains "$OUT_CLEAR" "- check_state: PASS"
assert_contains "$OUT_CLEAR" "- token_fail_count: 0"
assert_contains "$OUT_CLEAR" "- alert_state: **CLEAR**"
assert_contains "$OUT_CLEAR" "| <none> | PASS |"

echo "[PASS] wave c b148 consistency alert summary contract passed"
