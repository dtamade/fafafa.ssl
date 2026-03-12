# CRL LoadFromFile Boundary Fail-Closed Semantics

## Goal
Harden `TCRLManagerImpl.LoadFromFile` for file boundary conditions:
- missing file should return controlled load error semantics
- empty file should return explicit invalid-data semantics
- avoid unsafe read patterns on empty buffers

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas` with contracts:
  - missing CRL file raises controlled load error (`Failed to load`)
  - empty CRL file raises invalid-data semantic
- GREEN: minimal implementation in `src/fafafa.ssl.cert.advanced.pas`:
  - `FileExists` guard + `RaiseLoadError`
  - `LStream.Size <= 0` guard + `RaiseInvalidData('CRL file (empty)')`
  - short-read guard after `Read`

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: missing/empty file contracts fail under current behavior.

2. GREEN
- same command as RED.
- Expected: all assertions pass.

3. Regression
- `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
- `fpc -Fu./src tests/unit/test_ocsp.pas -otmp/test_unit_ocsp && ./tmp/test_unit_ocsp`
- `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- `python3 scripts/compile_all_modules.py`

## Execution Log (2026-03-05)

### RED
- Added contracts in `tests/unit/test_crl_revocation_semantics.pas`:
  - `missing crl file returns controlled load error`
  - `empty crl file returns invalid-data semantic`
- RED key output:
  - missing file emitted raw system open error (no controlled load message)
  - empty file emitted parse error semantics instead of invalid-data semantics

### GREEN
- Updated `src/fafafa.ssl.cert.advanced.pas` (`LoadFromFile`):
  - file existence guard
  - empty file guard
  - short-read guard
- GREEN result:
  - `Results: 33 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS
- `test_unit_ocsp`: PASS
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)
