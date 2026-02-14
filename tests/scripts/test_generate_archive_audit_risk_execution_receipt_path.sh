#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_receipt_path_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Archive Audit Risk Execution Receipt - Path Resolution Contract"

# 创建 fixture risk matrix (B31 样例格式)
cat > "$PROJECT_ROOT/$TEST_DIR/risk_matrix.md" << 'RISKEOF'
# Archive Audit Risk Response

- risk_id: test_risk
- generated_at: 2026-02-14 12:00:00 +0800

## 3) Risk Matrix

| risk_code | severity | category | owner | status |
|-----------|----------|----------|-------|--------|
| RISK-001 | critical | data-loss | ops | open |
| RISK-002 | high | security | secops | open |
RISKEOF

# 创建 fixture blockers report (B32 样例格式)
cat > "$PROJECT_ROOT/$TEST_DIR/blockers.md" << 'BLOCKERSEOF'
# Pre-Release Audit Blockers

- blockers_id: test_blockers
- generated_at: 2026-02-14 12:00:00 +0800

## 4) Blocker Items

| blocker_code | source | blocker_key | severity | owner | action | evidence |
|--------------|--------|-------------|----------|-------|--------|----------|
| BLK-001 | checklist | readiness_fail | critical | release-manager | block-release | readiness=fail |
| BLK-002 | weekly | status_fail | high | qa-secops | resolve | status=fail |
BLOCKERSEOF

# 创建 fixture threshold policy (B34 样例格式)
cat > "$PROJECT_ROOT/$TEST_DIR/threshold_policy.md" << 'THRESHOLDEOF'
# Archive Audit Dashboard Threshold Policy

- policy_id: test_policy
- generated_at: 2026-02-14 12:00:00 +0800

## Threshold Status

| metric | threshold | current | status |
|--------|-----------|---------|--------|
| critical_count | 0 | 2 | exceeded |
| high_count | 5 | 3 | ok |
THRESHOLDEOF

echo "[SCENARIO A] Execute from project root with relative paths"

cd "$PROJECT_ROOT"

bash scripts/generate_archive_audit_risk_execution_receipt_draft.sh \
  --receipt-id test_root \
  --risk-matrix "$TEST_DIR/risk_matrix.md" \
  --blockers "$TEST_DIR/blockers.md" \
  --threshold-policy "$TEST_DIR/threshold_policy.md" \
  --output "$TEST_DIR/receipt_root.md"

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/receipt_root.md" ]]; then
  echo "[FAIL] Scenario A: output file not generated"
  exit 1
fi

# 验证 execution_readiness=fail（因为有 critical blockers）
if ! grep -q "execution_readiness.*fail" "$PROJECT_ROOT/$TEST_DIR/receipt_root.md"; then
  echo "[FAIL] Scenario A: expected execution_readiness=fail"
  cat "$PROJECT_ROOT/$TEST_DIR/receipt_root.md" | grep -A 5 "execution_readiness"
  exit 1
fi

echo "[PASS] Scenario A: project root execution succeeded"

echo "[SCENARIO B] Execute from /tmp with relative paths"

cd /tmp

bash "$PROJECT_ROOT/scripts/generate_archive_audit_risk_execution_receipt_draft.sh" \
  --receipt-id test_tmp \
  --risk-matrix "$TEST_DIR/risk_matrix.md" \
  --blockers "$TEST_DIR/blockers.md" \
  --threshold-policy "$TEST_DIR/threshold_policy.md" \
  --output "$TEST_DIR/receipt_tmp.md" 2>&1 || {
    echo "[EXPECTED FAIL] Scenario B: /tmp execution failed (RED state)"
    echo "[INFO] This is expected - path resolution needs fixing"
    exit 1
  }

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/receipt_tmp.md" ]]; then
  echo "[EXPECTED FAIL] Scenario B: output file not in expected location (RED state)"
  exit 1
fi

echo "[PASS] Scenario B: /tmp execution succeeded"
echo "[PASS] Path resolution contract passed"
exit 0
