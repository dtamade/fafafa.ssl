#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_linux_openssl_matrix_draft.sh"
REL_REPORT="tmp/test_linux_matrix_report_output/summary.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

assert_contains() {
  local file="$1"
  local pattern="$2"
  if ! rg -F --quiet "$pattern" "$file"; then
    echo "[FAIL] missing expected pattern: $pattern"
    echo "[INFO] top of report ($file):"
    sed -n '1,160p' "$file" || true
    exit 1
  fi
}

echo "[TEST] Linux OpenSSL matrix report output contract"

rm -f "$ROOT_DIR/$REL_REPORT" "/tmp/$REL_REPORT"

if (cd /tmp && bash "$SCRIPT" \
  --dry-run \
  --skip-compile \
  --skip-phase2-dryrun \
  --modules PKCS7 \
  --report-output "$REL_REPORT" >/dev/null 2>&1); then
  :
else
  fail "linux matrix dry-run with --report-output should succeed"
fi

[[ -f "$ROOT_DIR/$REL_REPORT" ]] || fail "report should be written under project root"
[[ ! -f "/tmp/$REL_REPORT" ]] || fail "report should not leak into /tmp"

assert_contains "$ROOT_DIR/$REL_REPORT" "==== Profile: system-default ===="
assert_contains "$ROOT_DIR/$REL_REPORT" "cd '$ROOT_DIR'"
assert_contains "$ROOT_DIR/$REL_REPORT" "[PASS] linux openssl matrix draft finished"

echo "[PASS] linux matrix report output contract passed"
