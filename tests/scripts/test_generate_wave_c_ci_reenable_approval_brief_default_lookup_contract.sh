#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

RUN_ID="default_lookup_$(date +%s)_$$"
INPUT_FILE="$PROJECT_ROOT/docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_${RUN_ID}.md"
OUTPUT_FILE="$PROJECT_ROOT/docs/test_reports/WAVE_C_B148_CI_REENABLE_APPROVAL_BRIEF_${RUN_ID}.md"

mkdir -p "$PROJECT_ROOT/docs/test_reports"
trap 'rm -f "$INPUT_FILE" "$OUTPUT_FILE"' EXIT

cat > "$INPUT_FILE" <<'EOF_INPUT'
# Wave C B116 Enablement Request Packet

- signoff_state: READY_FOR_APPROVAL
- enable_state: HOLD
EOF_INPUT

cd "$PROJECT_ROOT"

bash scripts/generate_wave_c_ci_reenable_approval_brief.sh --run-id "$RUN_ID"

if [[ ! -f "$OUTPUT_FILE" ]]; then
  echo "[FAIL] B148 default lookup should generate output under docs/test_reports"
  exit 1
fi

if ! rg -F --quiet -- "- source_pack: docs/test_reports/WAVE_C_B116_ENABLEMENT_REQUEST_PACKET_${RUN_ID}.md" "$OUTPUT_FILE"; then
  echo "[FAIL] B148 default lookup should prefer latest B116 packet"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

if ! rg -F --quiet -- '- brief_state: **READY_FOR_APPROVAL**' "$OUTPUT_FILE"; then
  echo "[FAIL] B148 output should reflect packet state"
  sed -n '1,220p' "$OUTPUT_FILE" || true
  exit 1
fi

echo "[PASS] generate_wave_c_ci_reenable_approval_brief default lookup contract passed"
