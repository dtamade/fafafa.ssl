#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
MANIFEST="$ROOT_DIR/docs/archive/reports/2026-03-test-reports-migration-manifest.md"
CURRENT_SUMMARY="$ROOT_DIR/docs/plans/2026-03-current-summary.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene historical singleton tail contract"

legacy_singletons=(
  "test-reports/mbedtls_test_suite_20260209.md"
  "test-reports/test_p4_engine_result.txt"
  "test-reports/test_provider_result.txt"
)

for rel in "${legacy_singletons[@]}"; do
  if git -C "$ROOT_DIR" ls-files --error-unmatch "$rel" >/dev/null 2>&1; then
    fail "legacy singleton artifact should not remain tracked: $rel"
  fi

  if [[ -e "$ROOT_DIR/$rel" ]]; then
    fail "legacy singleton artifact should not exist in workspace: $rel"
  fi

done

if git -C "$ROOT_DIR" ls-files -- test-reports | grep -q .; then
  echo "[INFO] remaining tracked test-reports surface:"
  git -C "$ROOT_DIR" ls-files -- test-reports | sed -n '1,80p'
  fail "tracked test-reports surface should be empty after singleton cleanup"
fi

[[ -f "$MANIFEST" ]] || fail "missing migration manifest"
[[ -f "$CURRENT_SUMMARY" ]] || fail "missing March current summary"

for rel in "${legacy_singletons[@]}"; do
  grep -Fq "$rel" "$MANIFEST" || fail "manifest should record singleton removal: $rel"
done

grep -Fq '当前 tracked `test-reports/` 已降为 `0`' "$CURRENT_SUMMARY" || fail "current summary should record tracked test-reports zero state"

if grep -Fq '当前只剩 3 个单文件尾巴' "$CURRENT_SUMMARY"; then
  fail "current summary should not describe the old singleton tail state"
fi

echo "[PASS] repo hygiene historical singleton tail contract passed"
