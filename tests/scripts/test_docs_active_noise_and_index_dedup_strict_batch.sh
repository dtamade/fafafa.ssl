#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORK_REL="tmp/test_docs_active_noise_and_index_dedup_strict_batch"
WORK_DIR="$ROOT_DIR/$WORK_REL"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] docs active noise + index dedup strict batch"

rm -rf "$WORK_DIR"
mkdir -p "$WORK_DIR"

NOISE_REPORT="$WORK_DIR/active_docs_noise.md"
DEDUP_REPORT="$WORK_DIR/docs_index_dedup.md"

(cd "$ROOT_DIR" && bash scripts/scan_active_docs_noise_draft.sh \
  --strict \
  --output "$WORK_REL/active_docs_noise.md" >/dev/null) || fail "active docs noise strict scan should pass"

(cd "$ROOT_DIR" && bash scripts/check_docs_index_dedup_draft.sh \
  --scope all \
  --strict \
  --output "$WORK_REL/docs_index_dedup.md" >/dev/null) || fail "docs index dedup strict scan should pass"

(cd "$ROOT_DIR" && bash tests/scripts/test_plans_current_index_contract.sh >/dev/null) || fail "plans current index contract should pass"

[[ -f "$NOISE_REPORT" ]] || fail "noise report should exist"
[[ -f "$DEDUP_REPORT" ]] || fail "dedup report should exist"

if ! rg -F --quiet "| total_hits | 0 |" "$NOISE_REPORT"; then
  sed -n '1,220p' "$NOISE_REPORT" || true
  fail "noise report should record zero hits"
fi

if ! rg -F --quiet "| duplicate_paths | 0 |" "$DEDUP_REPORT"; then
  sed -n '1,220p' "$DEDUP_REPORT" || true
  fail "dedup report should record zero duplicate paths"
fi

if ! rg -F --quiet "| duplicate_titles | 0 |" "$DEDUP_REPORT"; then
  sed -n '1,220p' "$DEDUP_REPORT" || true
  fail "dedup report should record zero duplicate titles"
fi

echo "[PASS] docs active noise + index dedup strict batch passed"
