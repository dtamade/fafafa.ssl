#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_remediation_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Archive Audit Consistency Remediation - Path Resolution Contract"

# 使用实际的样例文件
CONSISTENCY="docs/test_reports/ARCHIVE_AUDIT_WEEKLY_CHECKLIST_CONSISTENCY_SAMPLE_B33.md"
CLOSURE="docs/test_reports/ARCHIVE_AUDIT_BLOCKER_CLOSURE_WAIVER_RECORD_SAMPLE_B36.md"
BLOCKERS="docs/test_reports/PRE_RELEASE_AUDIT_BLOCKERS_SAMPLE_B32.md"

echo "[SCENARIO A] Execute from project root with relative paths"

cd "$PROJECT_ROOT"

bash scripts/generate_archive_audit_consistency_remediation_draft.sh \
  --plan-id test_root \
  --consistency-report "$CONSISTENCY" \
  --closure-record "$CLOSURE" \
  --blockers "$BLOCKERS" \
  --output "$TEST_DIR/remediation_root.md"

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/remediation_root.md" ]]; then
  echo "[FAIL] Scenario A: output file not generated"
  exit 1
fi

echo "[PASS] Scenario A: project root execution succeeded"

echo "[SCENARIO B] Execute from /tmp with relative paths"

cd /tmp

bash "$PROJECT_ROOT/scripts/generate_archive_audit_consistency_remediation_draft.sh" \
  --plan-id test_tmp \
  --consistency-report "$CONSISTENCY" \
  --closure-record "$CLOSURE" \
  --blockers "$BLOCKERS" \
  --output "$TEST_DIR/remediation_tmp.md" 2>&1 || {
    echo "[EXPECTED FAIL] Scenario B: /tmp execution failed (RED state)"
    exit 1
  }

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/remediation_tmp.md" ]]; then
  echo "[EXPECTED FAIL] Scenario B: output file not in expected location (RED state)"
  exit 1
fi

echo "[PASS] Scenario B: /tmp execution succeeded"
echo "[PASS] Path resolution contract passed"
exit 0
