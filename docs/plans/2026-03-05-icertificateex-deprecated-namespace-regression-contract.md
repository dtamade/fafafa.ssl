# ICertificateEx Deprecated Namespace Regression Contract

## Goal
Prevent reintroduction of deprecated namespace symbols `fafafa.ssl.cert.builder.ICertificateEx/IPrivateKeyEx` in `src/` and `tests/` outside the canonical bridge file.

## Scope
- Add: `tests/scripts/test_deprecated_icertificateex_namespace_regression_contract.sh`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Add a grep-based contract script:
   - scan `src/` and `tests/` for deprecated namespace symbols.
   - allow bridge-only hits in `src/fafafa.ssl.openssl.cert.builder.pas`.
   - fail if any other file matches.
2. Verify script quality:
   - `bash -n tests/scripts/test_deprecated_icertificateex_namespace_regression_contract.sh`
   - `bash tests/scripts/test_deprecated_icertificateex_namespace_regression_contract.sh`
