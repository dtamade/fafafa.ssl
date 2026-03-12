# 2026-03-07 Certificate P2 PKCS Runtime Contract

## Goal
Cover the CI-safe PKCS/workflow certificate entrypoints:
- `tests/certificate/test_p2_pkcs12.pas`
- `tests/certificate/test_p2_pkcs7.pas`
- `tests/certificate/test_pkcs12_workflow.pas`
- `tests/certificate/test_x509_enterprise.pas`

## Architecture
- Add one grouped runtime contract using `fpc -Fu./src`.
- Reuse existing summary tokens for PKCS tests.
- Add one stable ASCII completion marker to `test_x509_enterprise.pas` so the contract has a robust token.

## Files
- Modify: `tests/certificate/test_x509_enterprise.pas`
- Add: `tests/scripts/test_certificate_p2_pkcs_runtime_contract.sh`
- Add: `docs/plans/2026-03-07-certificate-p2-pkcs-runtime-contract.md`

## Steps
1. RED
- Add contract expecting `[PASS] x509 enterprise completed`.
- Run the contract and observe failure because that marker is not yet printed.
2. GREEN
- Add the marker on the success path in `test_x509_enterprise.pas`.
- Re-run `bash -n` and `bash` on the new contract.
3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- The new grouped contract passes.
- `test_x509_enterprise.pas` prints `[PASS] x509 enterprise completed` on success.
- Existing gates remain green.
