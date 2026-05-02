#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b116_$(date +%s)"
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

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b116.md"

cd "$PROJECT_ROOT"

bash scripts/prepare_wave_c_b116_enablement_packet.sh \
  --run-id 20260315_contract \
  --signoff-record "$TEST_DIR/signoff.md" \
  --prereq-report "$TEST_DIR/prereq.md" \
  --output "$TEST_DIR/b116.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B116 packet not generated"
  exit 1
fi

if ! rg -F --quiet -- '- signoff_state: READY_FOR_APPROVAL' "$OUTPUT_FILE"; then
  echo "[FAIL] packet should expose signoff_state"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- enable_state: HOLD' "$OUTPUT_FILE"; then
  echo "[FAIL] packet should expose enable_state HOLD"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- 建议：保持禁用，等待人工签核完成。' "$OUTPUT_FILE"; then
  echo "[FAIL] packet should recommend staying disabled"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] prepare_wave_c_b116_enablement_packet unified input contract passed"
