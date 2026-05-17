#!/usr/bin/env bash

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
WORKFLOW_FILE="$ROOT_DIR/.github/workflows/test-all-platforms.yml.disabled"

fail() {
  echo "[FAIL] $1"
  exit 1
}

[[ -f "$WORKFLOW_FILE" ]] || fail "missing workflow file: .github/workflows/test-all-platforms.yml.disabled"

python3 - "$WORKFLOW_FILE" <<'PY'
from pathlib import Path
import sys

workflow = Path(sys.argv[1])
text = workflow.read_text(encoding="utf-8")

required_fragments = [
    "name: Test-Results-Windows",
    "name: Test-Results-Linux",
    "name: Test-Results-macOS",
    'WINDOWS_RESULT="${{ needs.test-windows.result }}"',
    'LINUX_RESULT="${{ needs.test-linux.result }}"',
    'MACOS_RESULT="${{ needs.test-macos.result }}"',
    'find test-results -mindepth 1 -maxdepth 1 -type d | sort',
]

forbidden_fragments = [
    "fpc-version: [ '3.2.2', '3.3.1' ]",
    "| Windows | ✅ | 3.2.2 |",
    "| Windows | ✅ | 3.3.1 |",
    "| Linux | ✅ | 3.2.2 |",
    "| Linux | ✅ | 3.3.1 |",
    "| macOS | ✅ | 3.2.2 |",
    "| macOS | ✅ | 3.3.1 |",
    "Test-Results-Windows-FPC${{ matrix.fpc-version }}",
    "Test-Results-Linux-FPC${{ matrix.fpc-version }}",
]

for fragment in required_fragments:
    if fragment not in text:
        print(f"[FAIL] missing truthful multi-platform fragment: {fragment}")
        raise SystemExit(1)

for fragment in forbidden_fragments:
    if fragment in text:
        print(f"[FAIL] stale fake-matrix or hardcoded-summary fragment still present: {fragment}")
        raise SystemExit(1)

print("[PASS] test-all-platforms workflow truth contract passed")
PY
