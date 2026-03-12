#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] Linux-focused multi-platform path-check dry-run batch"

LINUX_OUT="$(cd /tmp && bash "$ROOT_DIR/scripts/run_linux_openssl_matrix_draft.sh" \
  --dry-run \
  --skip-compile \
  --skip-phase2-dryrun \
  --modules PKCS7 2>&1)"
[[ "$LINUX_OUT" == *"[PASS] linux openssl matrix draft finished"* ]] || fail "linux matrix dry-run should pass"
[[ "$LINUX_OUT" == *"cd '$ROOT_DIR'"* ]] || fail "linux commands should execute under project root"
echo "[PASS] linux path check"

MACOS_OUT="$(cd /tmp && bash "$ROOT_DIR/scripts/run_macos_openssl_path_check_draft.sh" \
  --dry-run \
  --skip-module-tests \
  --skip-phase2-dryrun \
  --modules PKCS7 \
  --openssl-root /opt/homebrew/opt/openssl@3 2>&1)"
[[ "$MACOS_OUT" == *"[WARN] non-macOS environment detected"* ]] || fail "macOS dry-run should emit non-macOS warning"
[[ "$MACOS_OUT" == *"[PASS] macOS openssl path check draft finished"* ]] || fail "macOS path check dry-run should pass"
[[ "$MACOS_OUT" == *"cd '$ROOT_DIR'"* ]] || fail "macOS commands should execute under project root"
echo "[PASS] macOS path check"

ANDROID_OUT="$(cd /tmp && bash "$ROOT_DIR/scripts/run_android_openssl_path_check_draft.sh" \
  --dry-run \
  --skip-module-tests \
  --skip-phase2-dryrun \
  --modules PKCS7 \
  --abi arm64-v8a \
  --api-level 24 \
  --ndk-root /opt/android-ndk \
  --openssl-root /opt/android-openssl 2>&1)"
[[ "$ANDROID_OUT" == *"[PASS] android openssl path check draft finished"* ]] || fail "android path check dry-run should pass"
[[ "$ANDROID_OUT" == *"cd '$ROOT_DIR'"* ]] || fail "android commands should execute under project root"
echo "[PASS] android path check"

WINDOWS_OUT="$(cd /tmp && bash "$ROOT_DIR/scripts/run_windows_winssl_path_check_draft.sh" \
  --dry-run \
  --skip-module-tests \
  --skip-phase2-dryrun \
  --modules PKCS7 \
  --msys2-root C:/msys64 2>&1)"
[[ "$WINDOWS_OUT" == *"[WARN] non-Windows environment detected"* ]] || fail "windows dry-run should emit non-Windows warning"
[[ "$WINDOWS_OUT" == *"[PASS] windows winssl path check draft finished"* ]] || fail "windows path check dry-run should pass"
[[ "$WINDOWS_OUT" == *"cd '$ROOT_DIR'"* ]] || fail "windows commands should execute under project root"
echo "[PASS] windows path check"

echo "[PASS] linux-focused multi-platform dry-run batch passed"
