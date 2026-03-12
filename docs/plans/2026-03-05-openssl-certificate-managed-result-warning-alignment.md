# OpenSSL Certificate Managed-Result Warning Alignment

## Goal
Eliminate managed-result initialization warnings in `src/fafafa.ssl.openssl.certificate.pas` with minimal semantics-preserving initialization changes.

## Architecture
- Keep runtime behavior unchanged.
- Replace warning-prone empty-result initialization and managed-record zeroing patterns with explicit typed initialization:
  - `Result := nil` for `TBytes`
  - `Result := Default(TSSLCertificateInfo)` for managed record

## Scope
- Modify: `src/fafafa.ssl.openssl.certificate.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Implement warning-alignment changes:
   - `TOpenSSLCertificate.SaveToDER`
   - `TOpenSSLCertificate.GetInfo`
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Focused command passes and warning count decreases by 2 compared with current baseline (`23 -> 21`).
- `python3 scripts/compile_all_modules.py` reports all modules compiled successfully.
