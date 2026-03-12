#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_minimal_ci_gate.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate openssl cache runtime option contract"

DEFAULT_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun 2>&1)"
if [[ "$DEFAULT_OUT" == *"test_openssl_cert_verify_cache_policy_runtime.pas"* ]]; then
  echo "$DEFAULT_OUT"
  fail "default dry-run should not invoke openssl cache runtime regression"
fi

set +e
ENABLED_OUT="$(cd /tmp && bash "$SCRIPT" --dry-run --skip-compile --skip-modules --skip-phase2-dryrun --skip-platform-path-checks-dryrun --with-openssl-cert-verify-cache-runtime 2>&1)"
STATUS=$?
set -e

if [[ "$STATUS" -ne 0 ]]; then
  echo "$ENABLED_OUT"
  fail "enabled option should be accepted and keep dry-run success"
fi

if [[ "$ENABLED_OUT" != *"tests/integration/test_openssl_cert_verify_cache_policy_runtime.pas"* ]]; then
  echo "$ENABLED_OUT"
  fail "enabled option should compile runtime cache policy test"
fi

if [[ "$ENABLED_OUT" != *"FAFAFA_RUN_NETWORK_TESTS=1 ./tmp/test_openssl_cert_verify_cache_policy_runtime"* ]]; then
  echo "$ENABLED_OUT"
  fail "enabled option should run runtime cache policy test with network gate env"
fi

echo "[PASS] minimal ci gate openssl cache runtime option contract passed"
