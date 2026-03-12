# PR Body: Runtime Contract Cleanup (March 7, 2026)

## Summary
- add explicit runtime or compile-only contract coverage for all active Pascal `program` entrypoints under `tests/integration` and `tests/certificate`
- add a top-level runtime regression batch at `tests/scripts/test_active_program_runtime_contract_batch.sh`
- add a coverage guard for the batch at `tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh`
- consolidate overlapping runtime-contract scripts to reduce maintenance cost without reducing coverage
- add stable ASCII completion markers for heavy smoke/workflow programs so shell contracts can assert success reliably across terminals and locales
- fix a few real drifts found during the sweep, including OpenSSL serial-number helper loading, X.509 test assumptions, certificate metadata array semantics, and outdated native-handle test usage

## Why
- the repo had many runnable Pascal `program` entrypoints, but coverage was fragmented across examples, smoke programs, and certificate/integration diagnostics
- several programs were green in practice but hard to contract safely because they only emitted localized or emoji-heavy success banners
- a few tests had drifted away from the current API surface
- the contract set itself had started to sprawl, so the cleanup also needed a consolidation pass

## What Changed

### Runtime-contract coverage
- cover all active `program` entrypoints under `tests/integration` and `tests/certificate`
- keep environment-coupled certificate probing explicit via `tests/scripts/test_cert_load_debug_contract.sh`
- preserve a fast default gate while also adding a broader runtime sweep

### Script consolidation
- consolidate overlapping integration runtime contracts into `tests/scripts/test_integration_simple_runtime_contract.sh`
- consolidate overlapping certificate non-P2 runtime contracts into `tests/scripts/test_certificate_utilities_runtime_contract.sh`
- keep focused P2 runtime contracts split by domain:
  - `tests/scripts/test_certificate_p2_core_runtime_contract.sh`
  - `tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`

### Top-level entrypoints
- broad sweep: `bash tests/scripts/test_active_program_runtime_contract_batch.sh`
- fast default gate:
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `python3 scripts/compile_all_modules.py`

### Fixes discovered during cleanup
- `src/fafafa.ssl.openssl.certificate.pas` now loads required BN / ASN.1 helpers before formatting certificate serial numbers
- `tests/integration/test_x509_basic.pas` now matches current OpenSSL X.509 name encoding and version semantics
- `tests/certificate/test_cert_store.pas` now uses the canonical native-handle helper
- `tests/certificate/test_certificate_unit.pas` now aligns with the current `TSSLStringArray` metadata API

## Documentation
- current runtime entrypoint index: `docs/plans/2026-03-07-runtime-contracts-current-index.md`
- historical cleanup trail: `docs/plans/2026-03-07-runtime-contracts-historical-index.md`
- short cleanup summary: `docs/testing/RUNTIME_CONTRACT_CLEANUP_SUMMARY_2026-03-07.md`
- PR-ready bilingual summary: `docs/testing/RUNTIME_CONTRACT_CLEANUP_PR_SUMMARY_2026-03-07.md`

## Testing
- `bash tests/scripts/test_active_program_runtime_contract_batch_coverage_contract.sh`
- `bash tests/scripts/test_active_program_runtime_contract_batch.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Results
- active Pascal `program` entrypoints left without contract coverage: `0`
- deleted runtime-contract script stale references left in docs and memory files: `0`
- `python3 scripts/compile_all_modules.py`: `231/231`

## Notes
- heavy FPC linking can still show occasional environment-level `ld.bfd` contention if too many large runtime contracts are forced in parallel; final validation here was kept serial
- `tests/scripts/test_cert_load_debug_contract.sh` intentionally remains compile-only because it is tied to environment-specific certificate paths
