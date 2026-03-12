#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"
EXPECTED_CMD="[GATE] cd '$ROOT_DIR' && bash tests/scripts/test_linux_multi_platform_path_checks_dryrun_batch.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate platform path-check dry-run integration contract"

OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun 2>&1)"
if [[ "$OUT" != *"$EXPECTED_CMD"* ]]; then
  echo "$OUT"
  fail "minimal gate should invoke multi-platform dry-run batch by default"
fi

SKIP_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun 2>&1)"
if [[ "$SKIP_OUT" == *"test_linux_multi_platform_path_checks_dryrun_batch.sh"* ]]; then
  echo "$SKIP_OUT"
  fail "skip flag should disable multi-platform dry-run batch invocation"
fi

echo "[PASS] minimal ci gate platform path-check integration contract passed"
