#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW_FILE="$ROOT_DIR/.github/workflows/winssl-tests.yml.disabled"

fail() {
  echo "[FAIL] $1"
  exit 1
}

[[ -f "$WORKFLOW_FILE" ]] || fail "missing workflow file: .github/workflows/winssl-tests.yml.disabled"

python3 - "$WORKFLOW_FILE" <<'PY'
from pathlib import Path
import sys

workflow = Path(sys.argv[1])
text = workflow.read_text(encoding="utf-8")

required_fragments = [
    "choco install -y freepascal lazarus",
    "Get-Command lazbuild",
    "pwsh -NoProfile -ExecutionPolicy Bypass -File tests/quick_winssl_validation.ps1",
    "pwsh -NoProfile -ExecutionPolicy Bypass -File tests/run_winssl_tests.ps1",
    "Tee-Object -Variable runtimeOutput",
    "Out-File -FilePath $runtimeLog -Encoding utf8",
    "test-reports/winssl_quick_smoke_${{ github.run_id }}.log",
    "test-reports/winssl_runtime_suite_${{ github.run_id }}.log",
    "This template records the observed results of the repository WinSSL scripts for the current run only.",
    "Review the uploaded runtime logs before making backend readiness or production support claims.",
]

forbidden_fragments = [
    "test_suite:",
    "lazbuild src/fafafa.ssl.winssl.lpk",
    "lazbuild tests/test_winssl_comprehensive.lpi",
    'tests\\bin\\test_winssl_comprehensive.exe',
    "**Production Ready**: ✅ YES",
    "All WinSSL tests PASSED",
    "PRODUCTION READY",
    "Zero-dependency Windows deployment SUPPORTED",
]

for fragment in required_fragments:
    if fragment not in text:
        print(f"[FAIL] missing truthful winssl-tests fragment: {fragment}")
        raise SystemExit(1)

for fragment in forbidden_fragments:
    if fragment in text:
        print(f"[FAIL] stale winssl-tests fragment still present: {fragment}")
        raise SystemExit(1)

print("[PASS] winssl-tests workflow truth contract passed")
PY
