#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
SCRIPT="$ROOT_DIR/scripts/run_android_openssl_path_check_draft.sh"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] Android OpenSSL path check draft dry-run contract"

OUT="$(cd /tmp && bash "$SCRIPT" \
  --dry-run \
  --skip-module-tests \
  --skip-phase2-dryrun \
  --modules PKCS7 \
  --abi arm64-v8a \
  --api-level 24 \
  --ndk-root /opt/android-ndk \
  --openssl-root /opt/android-openssl 2>&1)"

if [[ "$OUT" != *"[PASS] android openssl path check draft finished"* ]]; then
  fail "android path check dry-run should finish with PASS line"
fi

if [[ "$OUT" != *"cd '$ROOT_DIR'"* ]]; then
  fail "android path check commands should execute under project root"
fi

if [[ "$OUT" != *"ANDROID_NDK_ROOT='/opt/android-ndk'"* ]]; then
  fail "android dry-run output should include NDK env prefix"
fi

echo "[PASS] android openssl path check dry-run contract passed"
