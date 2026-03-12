# ICertificateEx Namespace Migration (Batch 2 Test Sweep)

## Goal
Complete remaining test-side namespace convergence for OpenSSL handle-cast test doubles by replacing deprecated `fafafa.ssl.cert.builder.ICertificateEx` usage with `fafafa.ssl.openssl.cert.builder.ICertificateEx`.

## Scope
- Modify:
  - `tests/unit/test_crl_revocation_semantics.pas`
  - `tests/unit/test_ocsp_client_semantics.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Replace interface implementation/variable types to `fafafa.ssl.openssl.cert.builder.ICertificateEx`.
2. Remove old-namespace explicit casts where base `ICertificate` cast is sufficient.
3. Focused verification:
   - `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
   - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
4. Regression gate:
   - `python3 scripts/compile_all_modules.py`
