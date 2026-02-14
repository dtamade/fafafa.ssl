#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/generate_archive_audit_hold_linkage_draft.sh"

WORK_REL="tmp/test_archive_audit_hold_linkage_contract"
WORK="$ROOT_DIR/$WORK_REL"
REL_OUTPUT="$WORK_REL/linkage.md"

REL_SAMPLING="$WORK_REL/sampling.md"
REL_HOLD_REVIEW="$WORK_REL/hold_review.md"

write_fixtures() {
  mkdir -p "$WORK"

  cat > "$WORK/sampling.md" <<'MD'
# Archive Audit Sampling Record

## 4) Sampled Runs

| run_id | profile | age_days | sample_hold | manifest | source_path |
|--------|---------|----------|-------------|----------|-------------|
| run_demo | pr | 3 | yes | manifest.md | artifacts/run_demo |
MD

  cat > "$WORK/hold_review.md" <<'MD'
# Hold Expiry Review

## 3) Hold Review Rows

| run_id | expires_on | days_left | review_status | owner | reason | meta_path |
|--------|------------|-----------|---------------|-------|--------|----------|
| run_demo | 2026-02-01 | -1 | overdue | qa | audit hold | meta/run_demo.yml |
MD
}

assert_status_warn() {
  local report="$1"
  if ! grep -qE "^\\| status \\| warn \\|" "$report"; then
    echo "[FAIL] expected status=warn in report: $report"
    exit 1
  fi
}

run_path_contract() {
  write_fixtures
  rm -f "$ROOT_DIR/$REL_OUTPUT"

  (cd "$ROOT_DIR" && bash "$SCRIPT" \
    --linkage-id path_contract_root \
    --sampling "$REL_SAMPLING" \
    --hold-review "$REL_HOLD_REVIEW" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output missing for root-dir execution"
    exit 1
  fi

  assert_status_warn "$ROOT_DIR/$REL_OUTPUT"

  rm -f "$ROOT_DIR/$REL_OUTPUT"
  rm -f "/tmp/$REL_OUTPUT" 2>/dev/null || true

  # Key contract: should still resolve relative inputs + relative --output under project root.
  (cd /tmp && bash "$SCRIPT" \
    --linkage-id path_contract_tmp \
    --sampling "$REL_SAMPLING" \
    --hold-review "$REL_HOLD_REVIEW" \
    --output "$REL_OUTPUT" >/dev/null)

  if [[ ! -f "$ROOT_DIR/$REL_OUTPUT" ]]; then
    echo "[FAIL] output should be resolved under project root for relative --output"
    exit 1
  fi

  assert_status_warn "$ROOT_DIR/$REL_OUTPUT"

  echo "[PASS] path resolution contract passed"
}

run_strict_contract() {
  write_fixtures
  local out="$WORK/strict.md"
  rm -f "$out"

  if bash "$SCRIPT" \
    --linkage-id strict_contract_case \
    --sampling "$REL_SAMPLING" \
    --hold-review "$REL_HOLD_REVIEW" \
    --output "$out" \
    --strict >/dev/null 2>&1; then
    echo "[FAIL] strict mode should fail when linkage status is non-pass"
    exit 1
  fi

  if [[ ! -f "$out" ]]; then
    echo "[FAIL] strict mode should still write linkage report"
    exit 1
  fi

  assert_status_warn "$out"

  echo "[PASS] strict mode contract passed"
}

case "${1:-}" in
  --strict-check)
    run_strict_contract
    ;;
  *)
    run_path_contract
    ;;
esac

