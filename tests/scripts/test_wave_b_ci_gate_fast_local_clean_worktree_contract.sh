#!/usr/bin/env bash

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"

cd "$PROJECT_ROOT"

if ! git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
  echo "[SKIP] not a git worktree"
  exit 0
fi

before_status="$(git status --porcelain)"

# Keep the contract cheap: dry-run still writes logs/summary, but skips the heavy compilation/test work.
bash scripts/run_wave_b_ci_gate.sh --fast-local --dry-run

after_status="$(git status --porcelain)"

if [[ "$before_status" != "$after_status" ]]; then
  echo "[FAIL] fast-local Wave B gate changed git status output"
  echo "[INFO] before:"
  printf '%s\n' "$before_status"
  echo "[INFO] after:"
  printf '%s\n' "$after_status"
  exit 1
fi

echo "[PASS] fast-local Wave B gate does not add workspace dirt"

