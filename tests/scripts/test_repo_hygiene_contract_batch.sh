#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"

fail() {
  echo "[FAIL] $1"
  exit 1
}

echo "[TEST] repo hygiene contract batch"

SCRIPTS=(
  "tests/scripts/test_repo_hygiene_no_tracked_root_bin_artifacts.sh"
  "tests/scripts/test_repo_hygiene_winssl_codepage_contract.sh"
  "tests/scripts/test_focused_style_contract_factory_and_selector.sh"
  "tests/scripts/test_focused_style_contract_openssl_context_and_backed.sh"
  "tests/scripts/test_focused_style_contract_winssl_context_and_connection.sh"
  "tests/scripts/test_focused_style_contract_openssl_certificate_and_ocsp_stapling.sh"
  "tests/scripts/test_focused_style_contract_wolfssl_and_mbedtls_contexts.sh"
  "tests/scripts/test_focused_style_contract_cert_utils.sh"
  "tests/scripts/test_focused_style_contract_x509v3.sh"
  "tests/scripts/test_focused_style_contract_capability_diff_and_sha3.sh"
  "tests/scripts/test_focused_style_contract_capability_serializer.sh"
  "tests/scripts/test_focused_style_contract_cert_rotation.sh"
  "tests/scripts/test_focused_style_contract_tls13_servercertverify.sh"
  "tests/scripts/test_focused_style_contract_openssl_api_ts.sh"
  "tests/scripts/test_focused_style_contract_cert_advanced_pinning_and_dns_ldns.sh"
  "tests/scripts/test_focused_style_contract_tail_infra_units.sh"
  "tests/scripts/test_workflow_trigger_convergence_contract.sh"
  "tests/scripts/test_main_ci_workflow_local_verified_commands_contract.sh"
  "tests/scripts/test_openssl_lib_canonical_imports_contract.sh"
  "tests/scripts/test_legacy_openssl_api_shim_coverage_contract.sh"
  "tests/scripts/test_legacy_openssl_api_canonical_imports_contract.sh"
  "tests/scripts/test_legacy_openssl_examples_compile_contract.sh"
  "tests/scripts/test_historical_snapshot_notice_contract.sh"
  "tests/scripts/test_repo_hygiene_tmp_report_defaults_contract.sh"
  "tests/scripts/test_wave_b_cross_platform_summary_default_output_contract.sh"
  "tests/scripts/test_repo_hygiene_wave_c_local_guard_tmp_defaults_contract.sh"
  "tests/scripts/test_wave_c_local_guard_default_reports_runtime_contract.sh"
  "tests/scripts/test_repo_hygiene_wave_c_ci_reenable_tmp_defaults_contract.sh"
  "tests/scripts/test_wave_c_ci_reenable_default_reports_runtime_contract.sh"
  "tests/scripts/test_repo_hygiene_wave_c_quick_enablement_tmp_defaults_contract.sh"
  "tests/scripts/test_wave_c_quick_enablement_default_reports_runtime_contract.sh"
  "tests/scripts/test_repo_hygiene_wave_b_tls13_tmp_defaults_contract.sh"
  "tests/scripts/test_wave_b_tls13_default_reports_runtime_contract.sh"
  "tests/scripts/test_repo_hygiene_wave_b_platform_surface_tmp_defaults_contract.sh"
  "tests/scripts/test_wave_b_platform_archive_default_reports_runtime_contract.sh"
  "tests/scripts/test_repo_hygiene_workflow_wave_b_tls13_tmp_report_paths_contract.sh"
  "tests/scripts/test_repo_hygiene_workflow_wave_c_quick_and_ci_matrix_tmp_report_paths_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_test_report_bucket_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_test_p2_bucket_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_wave_b_bucket_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_examples_and_tls13_buckets_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_wave_c_quick_enablement_bucket_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_wave_c_pre_ci_submission_bucket_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_wave_c_local_first_guard_bucket_contract.sh"
  "tests/scripts/test_repo_hygiene_historical_singleton_tail_contract.sh"
  "tests/scripts/test_git_status_noise_summary_contract.sh"
)

for script in "${SCRIPTS[@]}"; do
  if [[ ! -f "$ROOT_DIR/$script" ]]; then
    fail "missing contract script: $script"
  fi

  if ! bash "$ROOT_DIR/$script"; then
    fail "contract failed: $script"
  fi
done

echo "[PASS] repo hygiene contract batch passed"
