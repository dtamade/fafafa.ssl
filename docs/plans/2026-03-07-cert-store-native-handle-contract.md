# 2026-03-07 Cert Store Native Handle Contract

> Superseded on 2026-03-07 by `tests/scripts/test_certificate_utilities_runtime_contract.sh` and `docs/plans/2026-03-07-certificate-nonp2-runtime-consolidation.md`.

## Goal
Fix `tests/certificate/test_cert_store.pas` so it compiles against the current certificate-store native-handle access pattern without changing production behavior.

## Architecture
- Keep the fix test-side.
- Replace the stale direct interface member call with the canonical helper from `fafafa.ssl.native_handle`.
- Cover the program through the consolidated non-P2 certificate runtime contract using its existing success summary.

## Files
- Modify: `tests/certificate/test_cert_store.pas`
- Cover via: `tests/scripts/test_certificate_utilities_runtime_contract.sh`

## Steps
1. RED
- `fpc -Fu./src tests/certificate/test_cert_store.pas -otmp/test_cert_store_fix`
- Expected: fail with `Identifier idents no member "GetNativeHandle"`.

2. GREEN
- Import `fafafa.ssl.native_handle`.
- Replace stale direct call with `IsNativeHandleAvailable(Store)`.
- Add focused runtime contract expecting `All tests PASSED!`.
- Re-run:
  - `fpc -Fu./src tests/certificate/test_cert_store.pas -otmp/test_cert_store_fix`
  - `./tmp/test_cert_store_fix`
  - `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `test_cert_store.pas` compiles and runs.
- `tests/scripts/test_certificate_utilities_runtime_contract.sh` passes.
- Repo gates remain green.
