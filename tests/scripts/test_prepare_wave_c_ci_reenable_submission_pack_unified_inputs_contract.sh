#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b146_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/signoff.md" <<'EOF_SIGNOFF'
# Wave C B113 Release Signoff Record

- signoff_state: READY_FOR_APPROVAL
EOF_SIGNOFF

cat > "$PROJECT_ROOT/$TEST_DIR/prereq.md" <<'EOF_PREREQ'
# Wave C B115 Workflow Enable Prereq

- enable_state: **HOLD**
EOF_PREREQ

cat > "$PROJECT_ROOT/$TEST_DIR/packet.md" <<'EOF_PACKET'
# Wave C B116 Enablement Request Packet

- signoff_state: READY_FOR_APPROVAL
- enable_state: HOLD
EOF_PACKET

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b146.md"

cd "$PROJECT_ROOT"

bash scripts/prepare_wave_c_ci_reenable_submission_pack.sh \
  --run-id 20260315_contract \
  --signoff-record "$TEST_DIR/signoff.md" \
  --prereq-report "$TEST_DIR/prereq.md" \
  --packet-report "$TEST_DIR/packet.md" \
  --strict \
  --output "$TEST_DIR/b146.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B146 submission pack not generated"
  exit 1
fi

if ! rg -F --quiet -- '- submission_state: **READY_TO_SUBMIT**' "$OUTPUT_FILE"; then
  echo "[FAIL] submission_state should be READY_TO_SUBMIT"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| signoff_state | READY_FOR_APPROVAL | READY_FOR_APPROVAL | PASS |' "$OUTPUT_FILE"; then
  echo "[FAIL] signoff_state row should pass"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| packet_enable_state | HOLD | HOLD | PASS |' "$OUTPUT_FILE"; then
  echo "[FAIL] packet_enable_state row should pass"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] prepare_wave_c_ci_reenable_submission_pack unified input contract passed"
