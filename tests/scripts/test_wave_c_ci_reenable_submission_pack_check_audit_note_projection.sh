#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/check_wave_c_ci_reenable_submission_pack.sh"
WORK_REL="tmp/test_wave_c_b147_audit_note_projection"
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
    sed -n '1,280p' "$file" || true
    exit 1
  fi
}

make_b146_input() {
  local file="$1"
  local submission_state="$2"
  cat > "$file" <<EOT
# Wave C B146 CI Re-enable Submission Pack

- run_id: test
- submission_state: **${submission_state}**

## Gate Checks

| check | value | expected | result |
|------|-------|----------|--------|
| workflow_state | DISABLED | DISABLED | PASS |
| packet_state | READY_FOR_APPROVAL | READY_FOR_APPROVAL | PASS |
| fullgate_state | PASS | PASS | PASS |
| status_overall | HEALTHY | HEALTHY | PASS |
| alert_level | NONE | NONE | PASS |
| ops_pack_state | PASS | PASS | PASS |
EOT
}

echo "[TEST] wave c b147 audit note projection contract"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

PASS_INPUT="$WORK_DIR/b146_pass.md"
PASS_OUTPUT="$WORK_DIR/b147_pass.md"
make_b146_input "$PASS_INPUT" "READY_TO_SUBMIT"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "b147_projection_pass_$$" \
  --input "$PASS_INPUT" \
  --output "$PASS_OUTPUT" >/dev/null)

[[ -f "$PASS_OUTPUT" ]] || fail "pass output should be generated"
assert_contains "$PASS_OUTPUT" "- projected_b148_alert_state: **CLEAR**"
assert_contains "$PASS_OUTPUT" "- projected_b149_audit_alert_note: **B148_ALERT_CLEAR**"

FAIL_INPUT="$WORK_DIR/b146_fail.md"
FAIL_OUTPUT="$WORK_DIR/b147_fail.md"
make_b146_input "$FAIL_INPUT" "HOLD"

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --run-id "b147_projection_fail_$$" \
  --input "$FAIL_INPUT" \
  --output "$FAIL_OUTPUT" >/dev/null)

[[ -f "$FAIL_OUTPUT" ]] || fail "fail output should be generated"
assert_contains "$FAIL_OUTPUT" "- projected_b148_alert_state: **WARN**"
assert_contains "$FAIL_OUTPUT" "- projected_b149_audit_alert_note: **B148_ALERT_WARN_REVIEW_REQUIRED**"

echo "[PASS] wave c b147 audit note projection contract passed"
