#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_cleanup_execution_record_draft.sh"

WORK_REL="tmp/test_archive_cleanup_execution_record_contract"
REL_OUTPUT="$WORK_REL/cleanup_record.md"
RECORD_ID="path_contract_cleanup_record_20260213"

assert_has_record_id() {
  local file="$1"
  if ! grep -qE "^\\| record_id \\| ${RECORD_ID} \\|" "$file"; then
    echo "[FAIL] expected record_id=${RECORD_ID} in output: $file"
    exit 1
  fi
}

mkdir -p "$ROOT_DIR/$WORK_REL"
rm -f "$ROOT_DIR/$REL_OUTPUT"
rm -f "/tmp/$REL_OUTPUT" 2>/dev/null || true

(cd "$ROOT_DIR" && bash "$SCRIPT" \
  --record-id "$RECORD_ID" \
  --output "$REL_OUTPUT" >/dev/null)

if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
  echo "[FAIL] output missing for root-dir execution"
  exit 1
fi

assert_has_record_id "$ROOT_DIR/$REL_OUTPUT"

rm -f "$ROOT_DIR/$REL_OUTPUT"

# Key contract: relative --output should still resolve under project root when invoked from /tmp.
(cd /tmp && bash "$SCRIPT" \
  --record-id "$RECORD_ID" \
  --output "$REL_OUTPUT" >/dev/null)

if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
  echo "[FAIL] output should be resolved under project root for relative --output"
  exit 1
fi

assert_has_record_id "$ROOT_DIR/$REL_OUTPUT"

echo "[PASS] path resolution contract passed"

