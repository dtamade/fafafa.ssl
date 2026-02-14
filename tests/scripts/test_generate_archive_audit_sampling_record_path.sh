#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

TEST_DIR="tmp/test_sampling_$(date +%s)"
mkdir -p "$PROJECT_ROOT/$TEST_DIR"
trap 'rm -rf "$PROJECT_ROOT/$TEST_DIR"' EXIT

echo "[TEST] Archive Audit Sampling Record - Path Resolution Contract"

# 创建测试 artifact 目录结构
ARTIFACT_ROOT="$TEST_DIR/artifacts"
mkdir -p "$PROJECT_ROOT/$ARTIFACT_ROOT/run_a" "$PROJECT_ROOT/$ARTIFACT_ROOT/run_b"

# 创建 fixture manifest 文件
cat > "$PROJECT_ROOT/$ARTIFACT_ROOT/run_a/manifest.json" << 'MANIFEST_EOF'
{"run_id": "run_a", "profile": "pr", "timestamp": "2026-02-14T12:00:00Z"}
MANIFEST_EOF

cat > "$PROJECT_ROOT/$ARTIFACT_ROOT/run_b/manifest.json" << 'MANIFEST_EOF'
{"run_id": "run_b", "profile": "pr", "timestamp": "2026-02-14T13:00:00Z"}
MANIFEST_EOF

echo "[SCENARIO A] Execute from project root with relative paths"

cd "$PROJECT_ROOT"

bash scripts/generate_archive_audit_sampling_record_draft.sh \
  --sample-id test_root \
  --artifact-root "$ARTIFACT_ROOT" \
  --profile pr \
  --sample-size 2 \
  --output "$TEST_DIR/sampling_root.md"

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/sampling_root.md" ]]; then
  echo "[FAIL] Scenario A: output file not generated"
  exit 1
fi

echo "[PASS] Scenario A: project root execution succeeded"

echo "[SCENARIO B] Execute from /tmp with relative paths"

cd /tmp

bash "$PROJECT_ROOT/scripts/generate_archive_audit_sampling_record_draft.sh" \
  --sample-id test_tmp \
  --artifact-root "$ARTIFACT_ROOT" \
  --profile pr \
  --sample-size 2 \
  --output "$TEST_DIR/sampling_tmp.md" 2>&1 || {
    echo "[EXPECTED FAIL] Scenario B: /tmp execution failed (RED state)"
    exit 1
  }

if [[ ! -f "$PROJECT_ROOT/$TEST_DIR/sampling_tmp.md" ]]; then
  echo "[EXPECTED FAIL] Scenario B: output file not in expected location (RED state)"
  exit 1
fi

echo "[PASS] Scenario B: /tmp execution succeeded"
echo "[PASS] Path resolution contract passed"
exit 0
