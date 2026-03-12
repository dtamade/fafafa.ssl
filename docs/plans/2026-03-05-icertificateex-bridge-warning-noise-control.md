# ICertificateEx Bridge Warning Noise Control

## Goal
Suppress transitive deprecated warnings emitted inside the canonical bridge unit `fafafa.ssl.openssl.cert.builder` without changing runtime behavior or public API shape.

## Scope
- Modify: `src/fafafa.ssl.openssl.cert.builder.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Wrap bridge alias declarations with local compiler warning suppression for deprecated symbols.
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
   - Confirm no warning lines from `src/fafafa.ssl.openssl.cert.builder.pas` about deprecated `ICertificateEx/IPrivateKeyEx`.
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`
