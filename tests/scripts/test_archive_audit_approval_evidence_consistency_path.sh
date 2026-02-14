#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_approval_consistency_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Archive Audit Approval Evidence Consistency - Path Resolution Contract"

# 使用实际的样例文件（已存在于仓库中）
APPROVAL_CHAIN="docs/test_reports/ARCHIVE_AUDIT_EXECUTION_APPROVAL_CHAIN_SAMPLE_B39.md"
RETEST_GATE="docs/test_reports/ARCHIVE_AUDIT_BLOCKER_RETEST_REGRESSION_GATE_SAMPLE_B40.md"
WRITEBACK="docs/test_reports/ARCHIVE_AUDIT_EXECUTION_RECEIPT_WRITEBACK_SAMPLE_B42.md"
CONVERGENCE="docs/test_reports/ARCHIVE_AUDIT_MULTIWEEK_RISK_CONVERGENCE_DASHBOARD_SAMPLE_B41.md"

echo "[SCENARIO A] Execute from project root with relative paths"

cd "$PROJECT_ROOT"

bash scripts/check_archive_audit_approval_evidence_consistency_draft.sh \
  --audit-id test_root \
  --approval-chain "$APPROVAL_CHAIN" \
  --retest-gate "$RETEST_GATE" \
  --writeback "$WRITEBACK" \
  --convergence-dashboard "$CONVERGENCE" \
  --output "$TEST_DIR/consistency_root.md"

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/consistency_root.md" ]]; then
  echo "[FAIL] Scenario A: output file not generated"
  exit 1
fi

# 验证 audit_status（根据样例数据应该有状态）
if ! grep -q "audit_status" "$PROJECT_ROOT/$TEST_DIR/consistency_root.md"; then
  echo "[FAIL] Scenario A: expected audit_status in output"
  exit 1
fi

echo "[PASS] Scenario A: project root execution succeeded"

echo "[SCENARIO B] Execute from /tmp with relative paths"

cd /tmp

bash "$PROJECT_ROOT/scripts/check_archive_audit_approval_evidence_consistency_draft.sh" \
  --audit-id test_tmp \
  --approval-chain "$APPROVAL_CHAIN" \
  --retest-gate "$RETEST_GATE" \
  --writeback "$WRITEBACK" \
  --convergence-dashboard "$CONVERGENCE" \
  --output "$TEST_DIR/consistency_tmp.md" 2>&1 || {
    echo "[EXPECTED FAIL] Scenario B: /tmp execution failed (RED state)"
    echo "[INFO] This is expected - path resolution needs fixing"
    exit 1
  }

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/consistency_tmp.md" ]]; then
  echo "[EXPECTED FAIL] Scenario B: output file not in expected location (RED state)"
  exit 1
fi

echo "[PASS] Scenario B: /tmp execution succeeded"
echo "[PASS] Path resolution contract passed"
exit 0
