# OCSP EnsureOCSPModuleLoaded Reuse in Post-Handshake OCSP Entry

## Goal
Unify OCSP module-loading guard in OpenSSL connection runtime paths by reusing `EnsureOCSPModuleLoaded` inside `ValidatePostHandshake` (OCSP revocation-check branch), without expanding public interfaces and without behavior changes.

## Architecture
- Scope: refactor-only (semantic preservation).
- Replace duplicated module-load block in `ValidatePostHandshake`:
  - from direct `TOpenSSLLoader.IsModuleLoaded + LoadOpenSSLOCSP` checks
  - to single `EnsureOCSPModuleLoaded` call.
- Keep fail-closed behavior unchanged:
  - if module load still fails, set `verify_result = X509_V_ERR_OCSP_VERIFY_FAILED` and return `False`.

## Files
- Modify: `src/fafafa.ssl.openssl.connection.pas`

## Steps
1. Replace duplicated OCSP module-load logic with `EnsureOCSPModuleLoaded` in post-handshake OCSP verify path.
2. Run focused OCSP connection regression.
3. Run compile gate (`compile_all_modules.py`).
4. Update `task_plan.md`, `findings.md`, `progress.md`.

## Execution Log (2026-03-05)

### Implementation
- Modified: `src/fafafa.ssl.openssl.connection.pas`
  - In `ValidatePostHandshake` OCSP revocation-check branch:
    - replaced direct module-load logic with `if not EnsureOCSPModuleLoaded then ... fail-closed`.

### Regression
- `fpc -Fu./src tests/openssl/test_ocsp_connection_verification_regression.pas -otmp/test_ocsp_connection_verification_regression && ./tmp/test_ocsp_connection_verification_regression`
  - PASS (`Passed: 16, Failed: 0, Skipped: 0`)
- `python3 scripts/compile_all_modules.py | tail -n 18`
  - PASS (`179/179`, `0 failed`, `100.0%`)
