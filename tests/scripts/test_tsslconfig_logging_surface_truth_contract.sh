#!/usr/bin/env bash
set -euo pipefail

api_ref="docs/reference/API_REFERENCE.md"
arch_ref="docs/reference/ARCHITECTURE.md"
user_guide="docs/guides/USER_GUIDE.md"
troubleshooting="docs/guides/TROUBLESHOOTING.md"

require_fixed() {
  local needle="$1"
  local file="$2"
  local message="$3"
  if ! grep -Fq "$needle" "$file"; then
    echo "FAIL: $message" >&2
    echo "  missing: $needle" >&2
    echo "  file: $file" >&2
    exit 1
  fi
}

require_fixed '通过 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` 调整 `LogLevel`，通过 `ISSLLibrary.SetLogCallback(...)` 安装回调；fresh request config 仍会回到 `sslLogError` + `nil` baseline。' \
  "$api_ref" \
  "API reference no longer explains the library-default logging entrypoints and request-safe baseline"

require_fixed '通过 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` 调整 `LogLevel`，通过 `ISSLLibrary.SetLogCallback(...)` 安装回调；factory request path 不接受 request-local 覆盖。' \
  "$arch_ref" \
  "Architecture reference no longer states the split logging entrypoints"

require_fixed 'LLogConfig := LLib.GetDefaultConfig;' \
  "$user_guide" \
  "User guide no longer fetches library default config before raising the log level"
require_fixed 'LLogConfig.LogLevel := sslLogInfo;' \
  "$user_guide" \
  "User guide no longer shows LogLevel configuration for info-level logging"
require_fixed 'LLib.SetDefaultConfig(LLogConfig);' \
  "$user_guide" \
  "User guide no longer persists the raised logging level through SetDefaultConfig"

require_fixed 'LLogConfig := LLib.GetDefaultConfig;' \
  "$troubleshooting" \
  "Troubleshooting guide no longer fetches library default config before raising the log level"
require_fixed 'LLogConfig.LogLevel := sslLogDebug;' \
  "$troubleshooting" \
  "Troubleshooting guide no longer shows debug-level logging through library defaults"
require_fixed 'LLib.SetDefaultConfig(LLogConfig);' \
  "$troubleshooting" \
  "Troubleshooting guide no longer persists the raised logging level before installing callback"

echo "PASS: TSSLConfig logging surface truth remains aligned across active docs"
