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

echo "[TEST] wave-b-b2 WinSSL session host input contract"

[[ -f "$README_FILE" ]] || fail "missing README file: $README_FILE"

for workflow in "${WORKFLOWS[@]}"; do
  [[ -f "$workflow" ]] || fail "missing workflow: $workflow"

  label="$(basename "$workflow")"

  require_match "$workflow" "$label" 'winssl_session_host:\s*\n\s*description: Optional host override for WinSSL session resumption runtime probe \(e\.g\. www\.google\.com\)\s*\n\s*required: false\s*\n\s*default: ""' \
    'workflow should expose an optional blank-default winssl_session_host input'

  require_match "$workflow" "$label" 'Run broader WinSSL runtime suite[\s\S]*\$sessionHost = "\$\{\{ github\.event\.inputs\.winssl_session_host \}\}"[\s\S]*if \(-not \[string\]::IsNullOrWhiteSpace\(\$sessionHost\)\)' \
    'workflow should resolve the manual winssl_session_host input inside the WinSSL runtime step'

  require_literal "$workflow" "$label" '$env:FAFAFA_WINSSL_SESSION_HOST = $sessionHost' \
    'workflow should inject FAFAFA_WINSSL_SESSION_HOST from the manual input'

  require_match "$workflow" "$label" 'Run broader WinSSL runtime suite[\s\S]*Using default WinSSL session resumption host from test program' \
    'workflow should log when it falls back to the test-program default host'
done

require_match "$README_FILE" ".github/README.md" '可选输入 `winssl_session_host` 可把 Windows broader WinSSL runtime suite 的 session-resumption 调查切到指定 host' \
  'README should document the optional workflow host override investigation lane'

echo "[PASS] wave-b-b2 WinSSL session host input contract passed"
