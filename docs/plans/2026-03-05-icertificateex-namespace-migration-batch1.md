# ICertificateEx Namespace Migration (Batch 1)

## Goal
Migrate remaining test-side OpenSSL handle casts away from deprecated `fafafa.ssl.cert.builder.ICertificateEx` naming and align with `fafafa.ssl.openssl.cert.builder.ICertificateEx` without behavior changes.

## Scope
- Modify: `tests/openssl/test_openssl_chain_issuer_selection.pas`
- Add evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Update test cast type
- Replace unqualified/deprecated `ICertificateEx` references with fully-qualified `fafafa.ssl.openssl.cert.builder.ICertificateEx`.
- Remove deprecated warning suppression blocks that are no longer needed.

2. Focused verification
- `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`

3. Regression gate
- `python3 scripts/compile_all_modules.py`

4. Writeback
- Record commands/results and queue updates in:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
