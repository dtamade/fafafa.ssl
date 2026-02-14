#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_risk_response_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Archive Audit Risk Response - Path Resolution Contract"

# 使用实际的样例文件
DASHBOARD="docs/test_reports/ARCHIVE_AUDIT_STATUS_DASHBOARD_SAMPLE_B30.md"
CHECKLIST="docs/test_reports/PRE_RELEASE_ARCHIVE_AUDIT_CHECKLIST_SAMPLE_B28.md"
HOLD_REVIEW="docs/test_reports/HOLD_EXPIRY_REVIEW_SAMPLE_B25.md"
WEEKLY="docs/test_reports/ARCHIVE_AUDIT_WEEKLY_REPORT_SAMPLE_B29.md"

echo "[SCENARIO A] Execute from project root with relative paths"

cd "$PROJECT_ROOT"

bash scripts/generate_archive_audit_risk_response_draft.sh \
  --matrix-id test_root \
  --dashboard "$DASHBOARD" \
  --checklist "$CHECKLIST" \
  --hold-review "$HOLD_REVIEW" \
  --weekly-report "$WEEKLY" \
  --output "$TEST_DIR/risk_response_root.md"

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/risk_response_root.md" ]]; then
  echo "[FAIL] Scenario A: output file not generated"
  exit 1
fi

echo "[PASS] Scenario A: project root execution succeeded"

echo "[SCENARIO B] Execute from /tmp with relative paths"

cd /tmp

bash "$PROJECT_ROOT/scripts/generate_archive_audit_risk_response_draft.sh" \
  --matrix-id test_tmp \
  --dashboard "$DASHBOARD" \
  --checklist "$CHECKLIST" \
  --hold-review "$HOLD_REVIEW" \
  --weekly-report "$WEEKLY" \
  --output "$TEST_DIR/risk_response_tmp.md" 2>&1 || {
    echo "[EXPECTED FAIL] Scenario B: /tmp execution failed (RED state)"
    exit 1
  }

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/risk_response_tmp.md" ]]; then
  echo "[EXPECTED FAIL] Scenario B: output file not in expected location (RED state)"
  exit 1
fi

echo "[PASS] Scenario B: /tmp execution succeeded"
echo "[PASS] Path resolution contract passed"
exit 0
