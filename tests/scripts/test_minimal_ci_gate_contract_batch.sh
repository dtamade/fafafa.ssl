#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] minimal ci gate contract batch"

SCRIPTS=(
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

for script in "${SCRIPTS[@]}"; do
  if [[ ! -f "$ROOT_DIR/$script" ]]; then
    fail "missing contract script: $script"
  fi

  if ! bash "$ROOT_DIR/$script"; then
    fail "contract failed: $script"
  fi
done

echo "[PASS] minimal ci gate contract batch passed"
