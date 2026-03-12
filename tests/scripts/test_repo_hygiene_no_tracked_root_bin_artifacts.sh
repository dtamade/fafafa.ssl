#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene no tracked root bin artifacts contract"

if git -C "$ROOT_DIR" ls-files 'bin/*' | grep -q .; then
  echo "[INFO] tracked bin sample:"
  git -C "$ROOT_DIR" ls-files 'bin/*' | sed -n '1,10p'
  fail "root bin artifacts should not be tracked by git"
fi

echo "[PASS] no tracked root bin artifacts"
