# CRL Parse Failure State Reset (nextUpdate stale metadata guard)

## Goal
Prevent stale `ICRLManager.GetNextUpdate` metadata from leaking across failed CRL reloads. After a parse failure, manager state must reflect "no CRL loaded" (`GetNextUpdate=0`, `IsExpired=True`).

## Architecture
- Add RED contract in `tests/unit/test_crl_revocation_semantics.pas`:
  - load valid fixture CRL
  - force an invalid CRL reload via `LoadFromPEM`
  - assert reload fails
  - assert `GetNextUpdate=0` and `IsExpired=True`
- Minimal GREEN fix in `src/fafafa.ssl.cert.advanced.pas`:
  - clear `FNextUpdate` before parse attempt in `TCRLManagerImpl.ParseCRL`.

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: new stale-state assertions fail.

2. GREEN
- same command as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-04)

### RED
- Added contracts in `tests/unit/test_crl_revocation_semantics.pas`:
  - `nextUpdate extracted from fixture`
  - `invalid crl reload must fail`
  - `nextUpdate cleared after failed crl reload`
  - `isExpired true after failed crl reload`
- RED key output:
  - `[FAIL] nextUpdate cleared after failed crl reload: expected 0 got=...`
  - `[FAIL] isExpired true after failed crl reload: failed reload should leave manager in no-CRL state`

### GREEN
- Updated `src/fafafa.ssl.cert.advanced.pas`:
  - `TCRLManagerImpl.ParseCRL` now clears `FNextUpdate` at method entry.
- GREEN result:
  - `Results: 22 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS
- `test_unit_ocsp`: PASS
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
