#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_checklist_path_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Pre-Release Archive Audit Checklist - Path Resolution Contract"

# 创建 fixture gate summary (B20 样例，包含阻断条件)
cat > "$PROJECT_ROOT/$TEST_DIR/gate_summary.md" <<'GATE_EOF'
# Cross-Platform Gate Summary

- run_id: test_gate
- overall_status: fail

## Blockers
- Linux: 1 critical issue
- macOS: pending
- Windows: pending
GATE_EOF

# 创建 fixture hold review (B25 样例，包含到期 hold)
cat > "$PROJECT_ROOT/$TEST_DIR/hold_review.md" <<'HOLD_EOF'
# Hold Expiry Review

- review_id: test_hold
- expired_count: 2
- expiring_soon_count: 1

## Expired Holds
- artifact_001: expired 5 days ago
- artifact_002: expired 3 days ago
HOLD_EOF

# 创建 fixture linkage report (B27 样例)
cat > "$PROJECT_ROOT/$TEST_DIR/linkage_report.md" <<'LINKAGE_EOF'
# Archive Audit Hold Linkage

- linkage_id: test_linkage
- sampling_status: incomplete
- hold_status: has_expired

## Summary
- Total holds: 3
- Expired: 2
- Active: 1
LINKAGE_EOF

echo "[SCENARIO A] Execute from project root with relative paths"

cd "$PROJECT_ROOT"

bash scripts/generate_pre_release_archive_audit_checklist_draft.sh \
  --checklist-id test_root \
  --gate-summary "$TEST_DIR/gate_summary.md" \
  --hold-review "$TEST_DIR/hold_review.md" \
  --linkage-report "$TEST_DIR/linkage_report.md" \
  --output "$TEST_DIR/checklist_root.md"

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/checklist_root.md" ]]; then
  echo "[FAIL] Scenario A: output file not generated"
  exit 1
fi

# 验证 readiness=fail（因为有阻断条件）
if ! grep -q "readiness.*fail" "$PROJECT_ROOT/$TEST_DIR/checklist_root.md"; then
  echo "[FAIL] Scenario A: expected readiness=fail"
  exit 1
fi

echo "[PASS] Scenario A: project root execution succeeded"

echo "[SCENARIO B] Execute from /tmp with relative paths"

cd /tmp

bash "$PROJECT_ROOT/scripts/generate_pre_release_archive_audit_checklist_draft.sh" \
  --checklist-id test_tmp \
  --gate-summary "$TEST_DIR/gate_summary.md" \
  --hold-review "$TEST_DIR/hold_review.md" \
  --linkage-report "$TEST_DIR/linkage_report.md" \
  --output "$TEST_DIR/checklist_tmp.md" 2>&1 || {
    echo "[EXPECTED FAIL] Scenario B: /tmp execution failed (RED state)"
    echo "[INFO] This is expected - path resolution needs fixing"
    exit 1
  }

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/checklist_tmp.md" ]]; then
  echo "[EXPECTED FAIL] Scenario B: output file not in expected location (RED state)"
  exit 1
fi

echo "[PASS] Scenario B: /tmp execution succeeded"
echo "[PASS] Path resolution contract passed"
exit 0
