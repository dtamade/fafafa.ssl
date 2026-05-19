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
require_file "docs/INTEGRATION_GUIDE.md" "Integration guide must exist for FreePascal replay-store integration truth"
require_file "docs/guides/security-best-practices.md" "Security best practices guide must exist for FreePascal capability truth"

require_fixed "README.md" "TSSLConfig.ServerEarlyDataReplayStoreFile" \
  "README.md must mention the file-backed early-data replay-store config field"
require_fixed "README.md" "TSSLConfig.ServerEarlyDataReplayStoreDirectory" \
  "README.md must mention the directory-backed early-data replay-store config field"
require_fixed "README.md" "WithServerEarlyDataReplayStoreFile" \
  "README.md must mention the builder file-backed early-data replay-store opt-in"
require_fixed "README.md" "WithServerEarlyDataReplayStoreDirectory" \
  "README.md must mention the builder directory-backed early-data replay-store opt-in"
require_fixed "README.md" "默认 replay truth 落到本地持久化 replay-store" \
  "README.md must record the durable default replay-store truth"
require_fixed "README.md" "fail-closed reject" \
  "README.md must record the fail-closed default-path behavior"

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
require_fixed "docs/reference/API_REFERENCE.md" "默认 shipped path 已经会把 replay truth 落到本地持久化 replay-store 路径" \
  "API reference must record the durable default replay-store truth"
if grep -Fq -- "不代表默认路径已经改成持久化" "docs/reference/API_REFERENCE.md"; then
  echo "[FAIL] API reference must stop contradicting the durable default replay-store truth"
  exit 1
fi

require_fixed "docs/INTEGRATION_GUIDE.md" "默认 shipped path 仍会落到本地持久化 replay-store 目录" \
  "Integration guide must record the durable default replay-store path"
require_fixed "docs/INTEGRATION_GUIDE.md" "fail-closed reject resumed early data" \
  "Integration guide must record the fail-closed default replay-store behavior"
if grep -Fq -- "in-memory single-process anti-replay ledger" "docs/INTEGRATION_GUIDE.md"; then
  echo "[FAIL] Integration guide must stop teaching the retired in-memory default replay truth"
  exit 1
fi

require_fixed "docs/guides/security-best-practices.md" \
  "local persistent anti-replay replay-store path; if the path is unavailable or unwritable, resumed early data is rejected fail-closed." \
  "Security best practices must quote the current FreePascal KnownIssues truth"

require_fixed "src/fafafa.ssl.freepascal.context.pas" \
  "TFreePascalDefaultPersistentEarlyDataReplayLedger.Create(" \
  "FreePascal server context must still default to the persistent replay ledger"
require_fixed "src/fafafa.ssl.freepascal.lib.pas" \
  "local persistent anti-replay replay-store path; if the path is unavailable or unwritable, resumed early data is rejected fail-closed." \
  "FreePascal capability KnownIssues must keep the durable default replay-store wording"

echo "[PASS] FreePascal early-data public opt-in docs contract passed"
