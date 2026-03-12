# 2026-03-07 Runtime Contracts Current Index

Use this page when you want the **current** runtime-contract entrypoints for active Pascal programs.

## Start here

For the broadest local sweep, run:

```bash
bash tests/scripts/test_active_program_runtime_contract_batch.sh
```

This batch is the current top-level runtime regression entrypoint for active `program` tests and examples.

## Current contract entrypoints

### Core and crypto
- `tests/scripts/test_core_openssl_validation_runtime_contract.sh`
- `tests/scripts/test_module_headers_quick_runtime_contract.sh`
- `tests/scripts/test_quick_module_validation_runtime_contract.sh`
- `tests/scripts/test_backend_and_algorithms_runtime_contract.sh`
- `tests/scripts/test_algorithm_availability_runtime_contract.sh`
- `tests/scripts/test_crypto_basics_runtime_contract.sh`
- `tests/scripts/test_crypto_family_a_runtime_contract.sh`
- `tests/scripts/test_crypto_family_b_runtime_contract.sh`
- `tests/scripts/test_benchmark_crypto_runtime_contract.sh`

### Certificate and OpenSSL smoke
- `tests/scripts/test_cert_and_diag_runtime_contract.sh`
- `tests/scripts/test_certificate_smoke_runtime_contract.sh`
- `tests/scripts/test_certificate_utilities_runtime_contract.sh`
- `tests/scripts/test_certificate_p2_core_runtime_contract.sh`
- `tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`
- `tests/scripts/test_cert_load_debug_contract.sh`
- `tests/scripts/test_ocsp_simple_runtime_contract.sh`
- `tests/scripts/test_openssl_load_program_runtime_contract.sh`
- `tests/scripts/test_active_validation_version_string_runtime_contract.sh`

### Integration
- `tests/scripts/test_integration_runtime_contract.sh`
- `tests/scripts/test_integration_simple_runtime_contract.sh`
- `tests/scripts/test_integration_pkcs11_runtime_contract.sh`

### Examples and tools
- `tests/scripts/test_self_contained_examples_runtime_contract.sh`
- `tests/scripts/test_pkcs7_examples_runtime_contract.sh`
- `tests/scripts/test_tool_examples_runtime_contract.sh`

## Fast default vs broad sweep

Use the fast default gate when you want the cheapest high-signal check:

```bash
bash scripts/run_minimal_ci_gate.sh --fast-local
```

Use the runtime batch when you want a broader sweep over active executable programs.

## Historical notes

If you want the historical plan trail for this cleanup work, check `docs/plans/2026-03-07-runtime-contracts-historical-index.md`.

Many `docs/plans/2026-03-07-*.md` files are still useful as change records, but several focused plans are now historical because their runtime checks were consolidated into fewer scripts.

When a plan starts with a superseded note, treat it as historical context rather than the current entrypoint.
