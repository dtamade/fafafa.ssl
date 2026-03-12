#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
BATCH_SCRIPT="$ROOT_DIR/tests/scripts/test_minimal_ci_gate_contract_batch.sh"

REQUIRED_SCRIPTS=(
  "tests/scripts/test_minimal_ci_gate_warning_noise_governance_batch_option.sh"
  "tests/scripts/test_minimal_ci_gate_warning_noise_timing_output_contract.sh"
  "tests/scripts/test_minimal_ci_gate_fast_local_preset_contract.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_minimal_preset_contract.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_equivalence_contract.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_preset_precedence_contract.sh"
  "tests/scripts/test_minimal_ci_gate_only_platform_path_check_mode.sh"
  "tests/scripts/test_minimal_ci_gate_only_tls13_sign_bench_mode.sh"
  "tests/scripts/test_minimal_ci_gate_preset_precedence_last_flag_wins_contract.sh"
  "tests/scripts/test_minimal_ci_gate_help_preset_precedence_note_contract.sh"
  "tests/scripts/test_minimal_ci_gate_help_pre_commit_minimal_contract.sh"
  "tests/scripts/test_minimal_ci_gate_contract_batch_coverage_contract.sh"
  "tests/scripts/test_minimal_ci_gate_contract_batch_snapshot_baseline_contract.sh"
  "tests/scripts/test_minimal_ci_gate_contract_batch_recommended_docs_contract.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_docs_contract.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_triplet_contract_batch_option.sh"
  "tests/scripts/test_minimal_ci_gate_pre_commit_triplet_option_docs_help_contract.sh"
)

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate contract batch coverage completeness contract"

if [[ ! -f "$BATCH_SCRIPT" ]]; then
  fail "batch script missing: $BATCH_SCRIPT"
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
  ' "$BATCH_SCRIPT"
)

if [[ "${#BATCH_SCRIPTS[@]}" -eq 0 ]]; then
  fail "unable to parse SCRIPTS list from batch script"
fi

DUPLICATES="$(printf '%s\n' "${BATCH_SCRIPTS[@]}" | sort | uniq -d || true)"
if [[ -n "$DUPLICATES" ]]; then
  echo "$DUPLICATES"
  fail "batch script should not contain duplicate contract entries"
fi

for required in "${REQUIRED_SCRIPTS[@]}"; do
  COUNT="$(printf '%s\n' "${BATCH_SCRIPTS[@]}" | grep -Fxc "$required" || true)"
  if [[ "$COUNT" -eq 0 ]]; then
    fail "missing required contract entry: $required"
  fi
  if [[ "$COUNT" -gt 1 ]]; then
    fail "required contract entry appears multiple times: $required"
  fi
done

echo "[PASS] minimal ci gate contract batch coverage completeness contract passed"
