# CRL Revocation Semantics Real Metadata Extraction

## Goal
Replace placeholder revocation metadata behavior in `src/fafafa.ssl.cert.advanced.pas` with real extraction for:
- revoked date from CRL entry
- revocation reason from CRL reason extension

Also stabilize CRL parsing path to avoid runtime access violations during `LoadFromFile/LoadFromPEM`.

## Architecture
- Add deterministic offline CRL fixtures with a known revoked certificate, pinned revocation timestamp, and keyCompromise reason.
- Add strict unit test that validates:
  - certificate is revoked
  - revoked year matches fixture timestamp (2024)
  - reason equals `KeyCompromise`
- Implement minimal source fix:
  - robust ASN.1 time decoding from raw ASN.1 string form (UTCTime/GeneralizedTime)
  - helper for revoked-entry lookup to avoid duplicated pointer flow
  - revocation reason mapping with fallback parsing

## Files
- Modify: `src/fafafa.ssl.cert.advanced.pas`
- Add: `tests/unit/test_crl_revocation_semantics.pas`
- Add: `tests/fixtures/p2/crl/revoked_cert_keycompromise_v1.txt`
- Add: `tests/fixtures/p2/crl/revoked_list_keycompromise_v1.txt`
- Modify: `tests/fixtures/p2/README.md`

## Step-by-step
1. RED (failing test)
- Command:
  - `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
- Expected:
  - failure on metadata assertions (initially also exposed access-violation in CRL load path)

2. GREEN (minimal implementation)
- Update `ASN1TimeToDateTime` to parse ASN.1 raw time bytes first.
- Add revoked-entry helper and real extraction logic in:
  - `IsRevoked`
  - `GetRevokedDate`
  - `GetRevocationReason`
- Keep backward-compatible fallback for environments lacking some OpenSSL accessors.

3. Regression
- Command:
  - `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
  - `fpc -Fu./src tests/unit/test_crl.pas -otmp/test_unit_crl && ./tmp/test_unit_crl`
  - `fpc -Fu./src tests/test_ocsp_crl_interface.pas -otmp/test_ocsp_crl_interface && ./tmp/test_ocsp_crl_interface`
- Expected:
  - all pass; no regression in CRL/OCSP interface smoke coverage

## Execution Record (2026-03-04)
- RED observed:
  - fixture test initially failed with access violation on CRL load path, then reason extraction mismatch.
- GREEN observed:
  - CRL fixture test passes with `revoked year = 2024` and `reason = KeyCompromise`.
- Regression observed:
  - `test_crl` PASS
  - `test_ocsp_crl_interface` PASS
