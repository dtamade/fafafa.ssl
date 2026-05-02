#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b148_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/b116.md" <<'EOF_INPUT'
# Wave C B116 Enablement Request Packet

- signoff_state: READY_FOR_APPROVAL
- enable_state: HOLD
EOF_INPUT

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b148.md"

cd "$PROJECT_ROOT"

bash scripts/generate_wave_c_ci_reenable_approval_brief.sh \
  --run-id 20260315_contract \
  --input "$TEST_DIR/b116.md" \
  --output "$TEST_DIR/b148.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B148 approval brief not generated"
  exit 1
fi

if ! rg -F --quiet -- '- brief_state: **READY_FOR_APPROVAL**' "$OUTPUT_FILE"; then
  echo "[FAIL] brief_state should reflect READY_FOR_APPROVAL"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '当前技术证据链已完成，但仍待人工签核；建议提交审批，保持 workflow disabled。' "$OUTPUT_FILE"; then
  echo "[FAIL] brief summary should reflect pending human approval"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] generate_wave_c_ci_reenable_approval_brief B116 contract passed"
