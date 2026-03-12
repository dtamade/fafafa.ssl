# OCSP EnsureOCSPModuleLoaded Pointer-Rebinding Stability Contract

## Goal
Lock stability semantics for `EnsureOCSPModuleLoaded` when OCSP module reload happens after a local test stub override:
- module loaded state must recover;
- OCSP decode pointer must be rebound by loader (not keep stale counting stub);
- status path must exit `OCSP API not available` state after successful reload.

## Architecture
- Contract-only hardening (no production behavior change expected):
  - Extend `tests/openssl/test_ocsp_connection_verification_regression.pas` with a reload scenario.
  - Force `TOpenSSLLoader.SetModuleLoaded(osmOCSP, False)` before call to trigger `EnsureOCSPModuleLoaded` reload path.
  - Pre-install `d2i_OCSP_RESPONSE := CountingD2IOCSPResponse` to verify loader rebind behavior.
  - Assert:
    - module state becomes loaded after call;
    - `d2i_OCSP_RESPONSE` pointer is no longer the counting stub;
    - status result is not `OCSP API not available`.

## Files
- Modify: `tests/openssl/test_ocsp_connection_verification_regression.pas`

## Steps
1. Add pointer-rebinding stability contract and register in test entry.
2. Run focused OCSP connection regression.
3. Run compile gate (`compile_all_modules.py`).
4. Update `task_plan.md`, `findings.md`, `progress.md` with evidence.

## Execution Log (2026-03-05)

### Contract
- Modified: `tests/openssl/test_ocsp_connection_verification_regression.pas`
  - Added scenario: `TestEnsureOCSPModuleLoadedPointerRebindingStability`.
- Command:
  - `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
- Output (key):
  - `=== EnsureOCSPModuleLoaded pointer-rebinding stability ===`
  - `[PASS] EnsureOCSPModuleLoaded reload rebinds OCSP pointers and exits API-unavailable state`
  - `Passed: 16`
  - `Failed: 0`
  - `Skipped: 0`

### Regression
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
