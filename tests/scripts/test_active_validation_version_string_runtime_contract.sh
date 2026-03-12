#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

check_not_unknown() {
  local source_file="$1"
  local output_file="$2"
  local pattern="$3"
  local log_file="$4"

  if ! fpc -Fu./src "$source_file" -o"$output_file" >/tmp/active-version-compile.log 2>&1; then
    echo '[INFO] compile output:'
    sed -n '1,220p' /tmp/active-version-compile.log || true
    fail "$source_file should compile"
  fi

  if ! "./$output_file" >"$log_file" 2>&1; then
    echo '[INFO] runtime output:'
    sed -n '1,220p' "$log_file" || true
    fail "$source_file should run"
  fi

  if rg -F --quiet -- "$pattern: Unknown" "$log_file"; then
    echo '[INFO] runtime output:'
    sed -n '1,220p' "$log_file" || true
    fail "$source_file should not print Unknown version"
  fi
}

check_not_unknown tests/test_core_modules_only.pas tmp/test_core_modules_only_version '版本' /tmp/test_core_modules_only_version.log
check_not_unknown tests/test_headers_validation.pas tmp/test_headers_validation_version 'Version' /tmp/test_headers_validation_version.log
check_not_unknown tests/test_module_headers_quick.pas tmp/test_module_headers_quick_version '版本' /tmp/test_module_headers_quick_version.log
check_not_unknown examples/hello_ssl.pas tmp/hello_ssl_version 'Version' /tmp/hello_ssl_version.log

echo '[PASS] active validation programs print resolved OpenSSL version strings'
