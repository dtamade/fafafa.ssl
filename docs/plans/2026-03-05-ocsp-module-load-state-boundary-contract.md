# OCSP Module-Load State Boundary Contract

## Goal
Lock the semantic boundary between OCSP module loaded state and helper dependency availability:
- `IsModuleLoaded(osmOCSP)` can remain `true` while helper dependencies become unavailable.
- In that state, `TOCSPClient.CheckCertificate` must fail closed with controlled unsupported semantic.

## Architecture
- RED:
  - Add module-state boundary scenario in `tests/unit/test_ocsp_client_semantics.pas`.
  - Assert (initially) dependency availability right after module load.
  - In current environment this fails, exposing that load-state and dependency-set are intentionally different dimensions.
- GREEN:
  - Install deterministic dependency baseline before boundary assertions.
  - Assert boundary behavior deterministically:
    - module loaded stays true after one helper dependency is nil.
    - helper dependency check turns false.
    - `TOCSPClient.CheckCertificate` raises controlled unsupported (`OpenSSL API CheckCertificateStatus`).
- Test hygiene improvement:
  - Extract shared helper `InstallDeterministicDependencyBaseline` for reuse across dependency and missing-API scenarios.

## Files
- Modify: `tests/unit/test_ocsp_client_semantics.pas`

## Steps
1. Add module-state boundary contract and run RED.
2. Inject deterministic baseline and run GREEN.
3. Run OCSP/CRL focused regressions and compile gate.

## Execution Log (2026-03-05)

### RED
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added `RunOCSPModuleLoadedStateBoundaryScenario`.
  - Added initial baseline assumption check:
    - `ocsp-module-state deps available baseline`
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- RED key output:
  - `[FAIL] ocsp-module-state deps available baseline: expected dependencies available immediately after module load`
  - `Results: 73 passed, 1 failed`

### GREEN
- Modified `tests/unit/test_ocsp_client_semantics.pas`:
  - Added reusable helper: `InstallDeterministicDependencyBaseline`.
  - Reused helper in:
    - `RunCheckCertificateStatusDependencyContractScenario`
    - `RunMissingCheckCertificateStatusAPIFailClosedScenario`
    - `RunOCSPModuleLoadedStateBoundaryScenario`
  - Updated boundary scenario assertion to deterministic baseline semantics.
- Command:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- GREEN output:
  - `Results: 74 passed, 0 failed`

### Regression
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
  - PASS (`OCSP TEST COMPLETE`)
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - PASS (`CRL TEST COMPLETE`)
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
  - PASS (`Results: 26 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)

### Post-Batch Addendum (2026-03-05)
- Source anchor update: `src/fafafa.ssl.openssl.api.ocsp.pas`
  - Added explicit comments documenting why module loaded semantics remain minimal and where runtime safety is enforced.
- Verification:
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
  - PASS (`74 passed, 0 failed`)
