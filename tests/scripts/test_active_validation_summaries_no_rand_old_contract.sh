#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

if rg -n '\brand_old\b' tests/test_core_modules_only.pas tests/test_headers_validation.pas; then
  echo '[FAIL] active validation summaries should not mention removed rand_old inventory item'
  exit 1
fi

echo '[PASS] active validation summaries do not mention rand_old'
