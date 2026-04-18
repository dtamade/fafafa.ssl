#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

require_file() {
  local file="$1"
  local message="$2"
  if [[ ! -f "$file" ]]; then
    echo "[FAIL] $message"
    echo "  missing: $file"
    exit 1
  fi
}

require_fixed() {
  local file="$1"
  local expected="$2"
  local message="$3"
  if ! grep -Fq -- "$expected" "$file"; then
    echo "[FAIL] $message"
    echo "  file: $file"
    echo "  expected: $expected"
    exit 1
  fi
}

require_file "README.md" "README.md must exist for FreePascal early-data public opt-in guidance"
require_file "docs/reference/API_REFERENCE.md" "API reference must exist for FreePascal early-data public opt-in guidance"

require_fixed "README.md" "TSSLConfig.ServerEarlyDataReplayStoreFile" \
  "README.md must mention the file-backed early-data replay-store config field"
require_fixed "README.md" "TSSLConfig.ServerEarlyDataReplayStoreDirectory" \
  "README.md must mention the directory-backed early-data replay-store config field"
require_fixed "README.md" "WithServerEarlyDataReplayStoreFile" \
  "README.md must mention the builder file-backed early-data replay-store opt-in"
require_fixed "README.md" "WithServerEarlyDataReplayStoreDirectory" \
  "README.md must mention the builder directory-backed early-data replay-store opt-in"
require_fixed "README.md" "in-memory single-process anti-replay ledger" \
  "README.md must keep the default in-memory single-process anti-replay wording"

require_fixed "docs/reference/API_REFERENCE.md" "TSSLConfig.ServerEarlyDataReplayStoreFile" \
  "API reference must mention the file-backed early-data replay-store config field"
require_fixed "docs/reference/API_REFERENCE.md" "TSSLConfig.ServerEarlyDataReplayStoreDirectory" \
  "API reference must mention the directory-backed early-data replay-store config field"
require_fixed "docs/reference/API_REFERENCE.md" "WithServerEarlyDataReplayStoreFile" \
  "API reference must mention the builder file-backed early-data replay-store opt-in"
require_fixed "docs/reference/API_REFERENCE.md" "WithServerEarlyDataReplayStoreDirectory" \
  "API reference must mention the builder directory-backed early-data replay-store opt-in"
require_fixed "docs/reference/API_REFERENCE.md" "mutually exclusive" \
  "API reference must document that file and directory replay-store opt-ins are mutually exclusive"

echo "[PASS] FreePascal early-data public opt-in docs contract passed"
