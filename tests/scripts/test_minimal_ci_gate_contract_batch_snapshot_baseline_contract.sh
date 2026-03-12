#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BATCH_SCRIPT="$ROOT_DIR/tests/scripts/test_minimal_ci_gate_contract_batch.sh"
SNAPSHOT_FILE="$ROOT_DIR/tests/fixtures/minimal_ci_gate/contract_batch_scripts.snapshot"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate contract batch snapshot baseline contract"

if [[ ! -f "$BATCH_SCRIPT" ]]; then
  fail "batch script missing: $BATCH_SCRIPT"
fi

if [[ ! -f "$SNAPSHOT_FILE" ]]; then
  fail "snapshot baseline missing: $SNAPSHOT_FILE"
fi

mapfile -t BATCH_SCRIPTS < <(
  awk '
    /SCRIPTS=\(/ {inlist=1; next}
    inlist && /^[[:space:]]*\)/ {inlist=0; exit}
    inlist {
      line=$0
      gsub(/^[[:space:]]*"/, "", line)
      gsub(/"[[:space:]]*$/, "", line)
      if (length(line) > 0) print line
    }
  ' "$BATCH_SCRIPT" | sort
)

mapfile -t SNAPSHOT_SCRIPTS < <(
  grep -v '^[[:space:]]*#' "$SNAPSHOT_FILE" | sed '/^[[:space:]]*$/d' | sort
)

if [[ "${#BATCH_SCRIPTS[@]}" -eq 0 ]]; then
  fail "unable to parse SCRIPTS list from batch script"
fi

if [[ "${#SNAPSHOT_SCRIPTS[@]}" -eq 0 ]]; then
  fail "snapshot baseline should contain at least one contract path"
fi

BATCH_JOINED="$(printf '%s\n' "${BATCH_SCRIPTS[@]}")"
SNAPSHOT_JOINED="$(printf '%s\n' "${SNAPSHOT_SCRIPTS[@]}")"

if [[ "$BATCH_JOINED" != "$SNAPSHOT_JOINED" ]]; then
  echo "[INFO] batch scripts set:"
  printf '%s\n' "${BATCH_SCRIPTS[@]}"
  echo "[INFO] snapshot scripts set:"
  printf '%s\n' "${SNAPSHOT_SCRIPTS[@]}"
  echo "[INFO] diff (snapshot -> batch):"
  diff -u <(printf '%s\n' "${SNAPSHOT_SCRIPTS[@]}") <(printf '%s\n' "${BATCH_SCRIPTS[@]}") || true
  fail "minimal gate contract batch set changed; update snapshot baseline explicitly"
fi

echo "[PASS] minimal ci gate contract batch snapshot baseline contract passed"
