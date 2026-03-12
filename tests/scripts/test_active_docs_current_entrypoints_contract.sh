#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
cd "$ROOT_DIR"

fail() {
  echo "[FAIL] $1"
  exit 1
}

if rg -n 'ci_pipeline\.sh' README.md >/dev/null; then
  rg -n 'ci_pipeline\.sh' README.md
  fail 'README should not reference missing ci_pipeline.sh'
fi

if rg -n '\bsslClient\b' docs/README.md src/fafafa.ssl.factory.pas >/dev/null; then
  rg -n '\bsslClient\b' docs/README.md src/fafafa.ssl.factory.pas
  fail 'Active docs/comments should use sslCtxClient'
fi

echo '[PASS] active docs use current entry points and symbols'
