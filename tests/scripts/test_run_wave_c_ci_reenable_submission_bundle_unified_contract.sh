#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b149_$(date +%s)"
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

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b149.md"

cd "$PROJECT_ROOT"

bash scripts/run_wave_c_ci_reenable_submission_bundle.sh \
  --run-id 20260316_contract \
  --signoff-record "$TEST_DIR/signoff.md" \
  --prereq-report "$TEST_DIR/prereq.md" \
  --packet-report "$TEST_DIR/packet.md" \
  --approval-input "$TEST_DIR/packet.md" \
  --strict \
  --output "$TEST_DIR/b149.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B149 bundle report not generated"
  exit 1
fi

if ! rg -F --quiet -- '- overall: **PASS**' "$OUTPUT_FILE"; then
  echo "[FAIL] B149 overall should be PASS"
  sed -n '1,240p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- submission_state: **READY_TO_SUBMIT**' "$OUTPUT_FILE"; then
  echo "[FAIL] B149 submission_state should be READY_TO_SUBMIT"
  sed -n '1,240p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- b148_submission_state: READY_FOR_APPROVAL' "$OUTPUT_FILE"; then
  echo "[FAIL] B149 should carry B148 brief_state"
  sed -n '1,240p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- "tmp/test-reports/wave_c_b146_ci_reenable_submission_pack_20260316_contract.b149.log" "$OUTPUT_FILE"; then
  echo "[FAIL] B149 should write step logs under tmp/test-reports by default"
  sed -n '1,240p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] run_wave_c_ci_reenable_submission_bundle unified contract passed"
