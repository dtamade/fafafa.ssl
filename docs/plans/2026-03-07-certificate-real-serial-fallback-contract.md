# 2026-03-07 Certificate Real Serial Fallback Contract

> Runtime validation from this focused plan was consolidated on 2026-03-07 into `tests/scripts/test_certificate_utilities_runtime_contract.sh`; keep this file as the historical record of the production-side serial-number fix.

## Goal
Fix `tests/certificate/test_certificate_real.pas` so real system certificates no longer fail when `TOpenSSLCertificate.GetSerialNumber` runs without BN helper bindings.

## Architecture
- RED uses the existing runtime test entrypoint.
- Minimal production fix in `src/fafafa.ssl.openssl.certificate.pas`:
  - keep the current hex path when `ASN1_INTEGER_to_BN` + `BN_bn2hex` are available
  - add a safe fallback using ASN.1 integer helpers when BN helpers are unavailable
- Cover `tests/certificate/test_certificate_real.pas` through the consolidated non-P2 certificate runtime contract.

## Files
- Modify: `src/fafafa.ssl.openssl.certificate.pas`
- Cover via: `tests/scripts/test_certificate_utilities_runtime_contract.sh`
- Add: `docs/plans/2026-03-07-certificate-real-serial-fallback-contract.md`

## Steps
1. RED
- `fpc -Fu./src tests/certificate/test_certificate_real.pas -otmp/test_certificate_real_fix && ./tmp/test_certificate_real_fix`
- Expected: fail at `Serial number retrieved`.

2. GREEN
- Update `GetSerialNumber` to gracefully fall back when BN helpers are not loaded.
- Add a focused contract expecting `✅ ALL TESTS PASSED!`.
- Re-run focused runtime and contract.

3. Regression
- `bash tests/scripts/test_certificate_utilities_runtime_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `test_certificate_real.pas` passes on Linux with system certificates.
- Consolidated non-P2 certificate runtime contract passes.
- Repo gates remain green.
