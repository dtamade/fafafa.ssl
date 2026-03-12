#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_android_openssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] android openssl path-check fpc host passthrough contract"

OUT="$(
  cd /tmp
  FAFAFA_FPC_EXE="contract-fpc" \
  bash "$SCRIPT" \
    --dry-run \
    --skip-phase2-dryrun \
    --modules PKCS7 \
    --abi arm64-v8a \
    --api-level 24 \
    --ndk-root /opt/android-ndk \
    --openssl-root /opt/android-openssl 2>&1
)"

if [[ "$OUT" != *"[ANDROID-CHECK] contract-fpc -iV"* ]]; then
  echo "$OUT"
  fail "fpc precheck command should use FAFAFA_FPC_EXE override"
fi

if [[ "$OUT" != *"FAFAFA_FPC_EXE='contract-fpc'"* ]]; then
  echo "$OUT"
  fail "module step should passthrough FAFAFA_FPC_EXE"
fi

echo "[PASS] android openssl path-check fpc host passthrough contract passed"
