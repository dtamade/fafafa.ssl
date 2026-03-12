# CRL LoadFromPEM Empty Input Invalid-Data Semantics

## Goal
Harden `ICRLManager.LoadFromPEM` input boundary behavior:
- empty PEM payload must return explicit invalid-data semantics,
- avoid relying on low-level parse-error text for empty-input cases.

## Architecture
- RED: extend `tests/unit/test_crl_revocation_semantics.pas`:
  - call `LoadFromPEM('')`
  - assert exception message contains invalid-data semantic
- GREEN: minimal source fix in `src/fafafa.ssl.cert.advanced.pas`:
  - keep `LoadFromPEM` as `ParseCRL` delegate
  - perform `Trim(APEM)=''` guard inside `TCRLManagerImpl.ParseCRL` (after metadata reset / old-state release)
  - keep non-empty payload parse path unchanged

## Files
- Modify: `tests/unit/test_crl_revocation_semantics.pas`
- Modify: `src/fafafa.ssl.cert.advanced.pas`

## Steps
1. RED
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected: new assertion fails because empty payload currently surfaces parse-error semantic.

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
- Modified `tests/unit/test_crl_revocation_semantics.pas`:
  - Added contracts:
    - `empty crl pem returns invalid-data semantic`
    - `empty crl pem must fail closed`
- RED key output:
  - `[FAIL] empty crl pem returns invalid-data semantic: unexpected message: Failed to parse CRL data...`
  - `Results: 36 passed, 1 failed`

### GREEN
- Modified `src/fafafa.ssl.cert.advanced.pas`:
  - `LoadFromPEM` delegates to `ParseCRL`.
  - `ParseCRL` checks empty/whitespace payload and raises invalid-data semantic.
- GREEN result:
  - `Results: 37 passed, 0 failed` (`test_crl_revocation_semantics`)

### Regression
- `test_unit_crl`: PASS (`CRL TEST COMPLETE`)
- `test_unit_ocsp`: PASS (`OCSP TEST COMPLETE`)
- `test_ocsp_crl_interface`: PASS (`26 passed, 0 failed`)
- `compile_all_modules.py`: PASS (`179/179`, `0 failed`, `100.0%`)

## Follow-up Correction (2026-03-05)

### Issue
- Empty-payload preflight in `LoadFromPEM` could bypass parse-entry state reset on reload failure.
- Risk: stale CRL metadata leak (`GetNextUpdate` not cleared) when previous CRL was already loaded.

### Added Contracts
- `empty crl pem reload returns invalid-data semantic`
- `nextUpdate cleared after empty pem reload failure`
- `isExpired true after empty pem reload failure`
- `whitespace-only crl pem reload returns invalid-data semantic`
- `nextUpdate cleared after whitespace pem reload failure`
- `isExpired true after whitespace pem reload failure`

### Final Fix Shape
- Empty/whitespace guard is enforced in `ParseCRL` after state-clear path.
- `LoadFromPEM` remains a thin delegate to `ParseCRL` to keep one parse入口语义。

### Verification Checkpoint
- `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
  - PASS (`53 passed, 0 failed`)
- `python3 scripts/compile_all_modules.py`
  - PASS (`179/179`, `0 failed`, `100.0%`)
