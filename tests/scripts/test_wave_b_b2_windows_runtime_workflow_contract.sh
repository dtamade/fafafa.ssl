#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOWS=(
  "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml"
  "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml.disabled"
)

fail() {
  echo "[FAIL] $1"
  exit 1
}

require_match() {
  local file="$1"
  local pattern="$2"
  local message="$3"
  if ! rg -n --multiline --multiline-dotall "$pattern" "$file" >/dev/null; then
    fail "$message"
  fi
}

echo "[TEST] wave-b-b2 windows runtime workflow contract"

for workflow in "${WORKFLOWS[@]}"; do
  [[ -f "$workflow" ]] || fail "missing workflow file: ${workflow#$ROOT_DIR/}"

  require_match "$workflow" 'runs-on:\s*windows-latest' \
    "${workflow#$ROOT_DIR/} should keep a windows-latest gate"

  require_match "$workflow" 'choco install -y .*lazarus|choco install -y lazarus.*' \
    "${workflow#$ROOT_DIR/} should install Lazarus for lazbuild-based runtime checks"

  require_match "$workflow" 'Get-Command lazbuild' \
    "${workflow#$ROOT_DIR/} should verify lazbuild availability"

  require_match "$workflow" 'pwsh -NoProfile -ExecutionPolicy Bypass -File tests/quick_winssl_validation\.ps1' \
    "${workflow#$ROOT_DIR/} should run quick WinSSL smoke via pwsh for UTF-8-safe runtime validation"

  require_match "$workflow" 'pwsh -NoProfile -ExecutionPolicy Bypass -File scripts/run_wave_b_windows_gate\.ps1' \
    "${workflow#$ROOT_DIR/} should run the Wave B Windows gate via pwsh"

  require_match "$workflow" 'pwsh -NoProfile -ExecutionPolicy Bypass -File tests/run_winssl_tests\.ps1' \
    "${workflow#$ROOT_DIR/} should run the broader WinSSL suite via pwsh"

  require_match "$workflow" 'Tee-Object -Variable runtimeOutput' \
    "${workflow#$ROOT_DIR/} should stream broader WinSSL suite output into a reusable runtime capture variable"

  require_match "$workflow" 'Out-File -FilePath \$runtimeLog -Encoding utf8' \
    "${workflow#$ROOT_DIR/} should persist broader WinSSL suite evidence as a UTF-8 log"

  require_match "$workflow" 'wave_b_windows_gate_summary_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.md' \
    "${workflow#$ROOT_DIR/} should upload Wave B Windows gate summary artifact"

  require_match "$workflow" 'winssl_runtime_suite_\$\{\{ needs\.setup\.outputs\.run_id \}\}\.log' \
    "${workflow#$ROOT_DIR/} should upload broader WinSSL suite transcript artifact"
done

require_match "$ROOT_DIR/.github/workflows/wave-b-b2-manual.yml" 'prepare_wave_b_b2_handoff_bundle\.sh' \
  'summary workflow should route Windows runtime evidence through prepare_wave_b_b2_handoff_bundle.sh as the single handoff truth source'

echo "[PASS] wave-b-b2 windows runtime workflow contract passed"
