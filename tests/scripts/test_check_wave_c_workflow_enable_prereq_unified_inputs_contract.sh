#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_wave_c_b115_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

cat > "$PROJECT_ROOT/$TEST_DIR/signoff.md" <<'EOF_SIGNOFF'
# Wave C B113 Release Signoff Record

- signoff_state: READY_FOR_APPROVAL
EOF_SIGNOFF

cat > "$PROJECT_ROOT/$TEST_DIR/bundle.md" <<'EOF_BUNDLE'
# Wave C Quick Sprint Bundle

- overall: **PASS**
EOF_BUNDLE

touch "$PROJECT_ROOT/$TEST_DIR/workflow.yml.disabled"

OUTPUT_FILE="$PROJECT_ROOT/$TEST_DIR/b115.md"

cd "$PROJECT_ROOT"

bash scripts/check_wave_c_workflow_enable_prereq.sh \
  --run-id 20260315_contract \
  --signoff-record "$TEST_DIR/signoff.md" \
  --acceptance "$TEST_DIR/bundle.md" \
  --workflow "$TEST_DIR/workflow.yml.disabled" \
  --output "$TEST_DIR/b115.md"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B115 prereq report not generated"
  exit 1
fi

if ! rg -F --quiet -- '- enable_state: **HOLD**' "$OUTPUT_FILE"; then
  echo "[FAIL] enable_state should stay HOLD before approval"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| signoff_state | READY_FOR_APPROVAL | FAIL |' "$OUTPUT_FILE"; then
  echo "[FAIL] signoff check should fail when approval is pending"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '| acceptance_bundle | PASS | PASS |' "$OUTPUT_FILE"; then
  echo "[FAIL] bundle acceptance should pass for quick bundle raw report"
  sed -n '1,200p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] check_wave_c_workflow_enable_prereq unified input contract passed"
