#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
TEST_DIR="tmp/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_$(date +%s)"
RUN_ID="contract_windows_runtime_artifacts"
OUTPUT_FILE="$TEST_DIR/evidence_consistency.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

mkdir -p "$ROOT_DIR/$TEST_DIR"
trap 'rm -rf "$ROOT_DIR/$TEST_DIR"' EXIT

cat > "$ROOT_DIR/$TEST_DIR/linux_summary.md" <<EOF
# Wave B CI Gate Summary

- run_id: $RUN_ID
- Overall Status: PASS
EOF

cat > "$ROOT_DIR/$TEST_DIR/examples.json" <<'EOF'
{
  "summary": {
    "total": 75,
    "passed": 75,
    "failed": 0,
    "skipped": 0,
    "pass_rate": "100%"
  }
}
EOF

cat > "$ROOT_DIR/$TEST_DIR/windows_summary.md" <<EOF
# Wave B Windows Gate Summary

- run_id: $RUN_ID
- overall: PASS
EOF

cat > "$ROOT_DIR/$TEST_DIR/cross_summary.md" <<EOF
# Wave B Cross-Platform Summary

- run_id: $RUN_ID
EOF

cat > "$ROOT_DIR/$TEST_DIR/closure_report.md" <<EOF
# Wave B / B2 Closure Readiness

- run_id: $RUN_ID
- closure_status: **CLOSED**
EOF

cd "$ROOT_DIR"

set +e
bash scripts/check_wave_b_b2_evidence_consistency.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$TEST_DIR/linux_summary.md" \
  --linux-examples "$TEST_DIR/examples.json" \
  --windows-summary "$TEST_DIR/windows_summary.md" \
  --cross-summary "$TEST_DIR/cross_summary.md" \
  --closure-report "$TEST_DIR/closure_report.md" \
  --strict \
  --output "$OUTPUT_FILE"
status=$?
set -e

if [[ "$status" -eq 0 ]]; then
  fail "strict consistency should reject missing Windows quick smoke / runtime transcript when windows summary exists"
fi

if [[ ! -f "$ROOT_DIR/$OUTPUT_FILE" ]]; then
  fail "expected consistency report to be generated even on strict failure"
fi

if ! rg -n "windows_quick_log" "$ROOT_DIR/$OUTPUT_FILE" >/dev/null; then
  fail "consistency report should surface windows_quick_log in the artifact matrix"
fi

if ! rg -n "windows_runtime_transcript" "$ROOT_DIR/$OUTPUT_FILE" >/dev/null; then
  fail "consistency report should surface windows_runtime_transcript in the artifact matrix"
fi

if ! rg -n "windows_quick_log .* missing|windows_runtime_transcript .* missing" "$ROOT_DIR/$OUTPUT_FILE" >/dev/null; then
  fail "consistency report should mark missing Windows runtime artifacts explicitly"
fi

echo "[PASS] wave-b-b2 evidence consistency Windows runtime artifacts contract passed"
