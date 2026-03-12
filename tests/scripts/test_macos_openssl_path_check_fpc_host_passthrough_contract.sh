#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_macos_openssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] macOS openssl path-check fpc host passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_FPC_EXE="contract-fpc" \
  bash "$SCRIPT" \
    --dry-run \
    --skip-phase2-dryrun \
    --modules PKCS7 \
    --openssl-root /opt/homebrew/opt/openssl@3 2>&1
)"

if [[ "$OUT" != *"[MACOS-CHECK] contract-fpc -iV"* ]]; then
  echo "$OUT"
  fail "fpc precheck command should use FAFAFA_FPC_EXE override"
fi

if [[ "$OUT" != *"FAFAFA_FPC_EXE='contract-fpc'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_FPC_EXE"
fi

echo "[PASS] macOS openssl path-check fpc host passthrough contract passed"
