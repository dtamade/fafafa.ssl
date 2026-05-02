#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b147_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/b146.md" <<'EOF_INPUT'
# Wave C B146 CI Re-enable Submission Pack

- submission_state: **READY_TO_SUBMIT**

## Gate Checks

| check | value | expected | result |
|------|-------|----------|--------|
| workflow_state | DISABLED | DISABLED | PASS |
| signoff_state | READY_FOR_APPROVAL | READY_FOR_APPROVAL | PASS |
| enable_state | HOLD | HOLD | PASS |
| packet_signoff_state | READY_FOR_APPROVAL | READY_FOR_APPROVAL | PASS |
| packet_enable_state | HOLD | HOLD | PASS |
EOF_INPUT

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b147.md"

cd "$PROJECT_ROOT"

bash scripts/check_wave_c_ci_reenable_submission_pack.sh \
  --run-id 20260315_contract \
  --input "$TEST_DIR/b146.md" \
  --strict \
  --output "$TEST_DIR/b147.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B147 check report not generated"
  exit 1
fi

if ! rg -F --quiet -- '- check_state: **PASS**' "$OUTPUT_FILE"; then
  echo "[FAIL] B147 check_state should be PASS"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- submission_state: READY_TO_SUBMIT' "$OUTPUT_FILE"; then
  echo "[FAIL] decision section should echo READY_TO_SUBMIT"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] check_wave_c_ci_reenable_submission_pack unified contract passed"
