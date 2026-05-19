#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOWS=(
  "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml"
  "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml.disabled"
)
README_FILE="$ROOT_DIR/.github/README.md"

fail() {
  echo "[FAIL] $1"
  exit 1
}

require_match() {
  local file="$1"
  local label="$2"
  local pattern="$3"
  local message="$4"
  if ! rg -n --multiline --multiline-dotall -- "$pattern" "$file" >/dev/null; then
    fail "$label: $message"
  fi
}

require_literal() {
  local file="$1"
  local label="$2"
  local fragment="$3"
  local message="$4"
  if ! rg -F -n --quiet -- "$fragment" "$file"; then
    fail "$label: $message"
  fi
}

require_no_literal() {
  local file="$1"
  local label="$2"
  local fragment="$3"
  local message="$4"
  if rg -F -n --quiet -- "$fragment" "$file"; then
    fail "$label: $message"
  fi
}

echo "[TEST] wave-b-b2 WinSSL native probe input contract"

[[ -f "$README_FILE" ]] || fail "missing README file: $README_FILE"

for workflow in "${WORKFLOWS[@]}"; do
  [[ -f "$workflow" ]] || fail "missing workflow: $workflow"

  label="$(basename "$workflow")"

  require_match "$workflow" "$label" 'winssl_enable_native_probe:\s*\n\s*description: Optional opt-in WinSSL native probe for Schannel session evidence \(may fail the broader runtime suite\)\s*\n\s*required: false\s*\n\s*default: "false"' \
    'workflow should expose an explicit false-default native-probe investigation input'

  require_match "$workflow" "$label" 'Run broader WinSSL runtime suite[\s\S]*\$nativeProbeInput = "\$\{\{ github\.event\.inputs\.winssl_enable_native_probe \}\}"[\s\S]*if \(\$nativeProbeInput -match .*true.*1.*yes.*on.*\)' \
    'workflow should resolve the manual native-probe input inside the broader WinSSL runtime step'

  require_literal "$workflow" "$label" '$env:FAFAFA_WINSSL_ENABLE_NATIVE_PROBE = "1"' \
    'workflow should only enable the native probe through the explicit opt-in env var'

  require_literal "$workflow" "$label" '[INFO] Enabling risky WinSSL native probe for Schannel session evidence' \
    'workflow should log when the risky native probe is explicitly enabled'

  require_literal "$workflow" "$label" '[INFO] Keeping WinSSL native probe disabled by default' \
    'workflow should log when the native probe remains on the safe default path'

  require_no_literal "$workflow" "$label" 'FAFAFA_WINSSL_REQUIRE_NATIVE_REUSE' \
    'workflow should not auto-promote the manual evidence lane into a strict native-reuse assertion lane'
done

require_match "$README_FILE" ".github/README.md" '可选输入 `winssl_enable_native_probe` 可开启有风险的 WinSSL native probe 调查 lane，用来补充 Schannel session evidence；默认保持关闭' \
  'README should document the explicit opt-in native-probe workflow lane and its default-off semantics'

echo "[PASS] wave-b-b2 WinSSL native probe input contract passed"
