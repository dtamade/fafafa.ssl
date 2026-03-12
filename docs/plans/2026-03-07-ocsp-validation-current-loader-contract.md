# 2026-03-07 OCSP Validation Current Loader Contract

## Goal
Bring `tests/certificate/test_ocsp_validation.pas` onto current OpenSSL loader semantics and include it in the grouped certificate smoke runtime contract.

## Architecture
- Contract-first change:
  - extend `tests/scripts/test_certificate_smoke_runtime_contract.sh` with `test_ocsp_validation.pas`
  - expect a stable ASCII marker: `[PASS] ocsp validation completed`
- Minimal program fix:
  - use current core-load semantics (`LoadOpenSSLCore` + loader-state check)
  - explicitly load OCSP procvars via `LoadOpenSSLOCSP(GetCryptoLibHandle)`
  - keep runtime summary behavior, only add a stable completion marker on success

## Files
- Modify: `tests/scripts/test_certificate_smoke_runtime_contract.sh`
- Modify: `tests/certificate/test_ocsp_validation.pas`

## Steps
1. RED
- `bash tests/scripts/test_certificate_smoke_runtime_contract.sh`
- Expected: fail because `test_ocsp_validation.pas` still uses stale boolean-returning `LoadOpenSSLCore` semantics.

2. GREEN
- Update `tests/certificate/test_ocsp_validation.pas` to current loader semantics.
- Re-run:
  - `fpc -Fu./src tests/certificate/test_ocsp_validation.pas -otmp/test_ocsp_validation_fix && ./tmp/test_ocsp_validation_fix`
  - `bash tests/scripts/test_certificate_smoke_runtime_contract.sh`
- Expected: compile/runtime pass and emit `[PASS] ocsp validation completed`.

3. Regression
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `test_certificate_smoke_runtime_contract.sh` passes with `test_ocsp_validation.pas` included.
- `test_ocsp_validation.pas` prints `[PASS] ocsp validation completed` on success.
- `--fast-local` and `compile_all_modules.py` remain green.
