# ICertificateEx / IPrivateKeyEx Deprecation Migration Roadmap

## Goal
Remove project-side dependence on deprecated handle-cast symbols from `fafafa.ssl.cert.builder` and converge all OpenSSL-handle access to `fafafa.ssl.openssl.cert.builder` while preserving runtime behavior.

## Current State (2026-03-05)
- Canonical OpenSSL handle namespace exists:
  - `src/fafafa.ssl.openssl.cert.builder.pas`
- Deprecated aliases remain in:
  - `src/fafafa.ssl.cert.builder.pas` (`ICertificateEx`, `IPrivateKeyEx`)
- Core migration progress:
  - `src/fafafa.ssl.cert.advanced.pas` has switched to `fafafa.ssl.openssl.cert.builder.*`.
  - `tests/openssl/test_openssl_chain_issuer_selection.pas` has switched to `fafafa.ssl.openssl.cert.builder.ICertificateEx`.
  - `tests/unit/test_crl_revocation_semantics.pas` has switched to `fafafa.ssl.openssl.cert.builder.ICertificateEx`.
  - `tests/unit/test_ocsp_client_semantics.pas` has switched to `fafafa.ssl.openssl.cert.builder.ICertificateEx`.

## Inventory (Direct Consumers)
- Runtime bridge only:
  - `src/fafafa.ssl.openssl.cert.builder.pas` (alias bridge to deprecated symbols)
- No direct old-namespace consumer remains in `src/` + `tests/` (verified by `rg` scan).

## Phased Execution

### Phase 1: Test-Side Namespace Convergence (P1)
- Status: complete.
- Completed deliverables:
  - migrated `tests/openssl/test_openssl_chain_issuer_selection.pas`
  - migrated `tests/unit/test_crl_revocation_semantics.pas`
  - migrated `tests/unit/test_ocsp_client_semantics.pas`

### Phase 2: Builder Alias Safety Check (P1)
- Status: complete.
- Completed deliverable:
  - `src/fafafa.ssl.openssl.cert.builder.pas` bridge aliases are wrapped with local deprecated-warning suppression.
  - Targeted compile no longer emits bridge-local deprecated warnings for alias lines.
- Validate if `src/fafafa.ssl.openssl.cert.builder.pas` alias declarations emit deprecated warnings transitively.
- If warnings are emitted, adjust alias strategy without changing public runtime behavior.
- Exit criteria:
  - No new deprecation-noise introduced by canonical namespace wrapper.

### Phase 3: Source-Level Final Sweep (P2)
- Status: complete.
- Completed deliverable:
  - sweep command:
    - `rg -n "WARN SYMBOL_DEPRECATED OFF|ICertificateEx is deprecated|IPrivateKeyEx is deprecated|fafafa\\.ssl\\.cert\\.builder\\.ICertificateEx|fafafa\\.ssl\\.cert\\.builder\\.IPrivateKeyEx" src tests`
  - result: only bridge-local lines in `src/fafafa.ssl.openssl.cert.builder.pas`.
- Re-scan `src/` and `tests/` for old-symbol casts and deprecated suppression blocks.
- Remove unnecessary `{$WARN SYMBOL_DEPRECATED OFF}` blocks after migration.
- Exit criteria:
  - Deprecated suppression only exists where technically unavoidable.

### Phase 4: Deprecation Enforcement (P2)
- Status: complete (baseline contract established).
- Completed deliverable:
  - `tests/scripts/test_deprecated_icertificateex_namespace_regression_contract.sh`
  - Contract blocks deprecated namespace symbols outside bridge file.
- Add/extend a lightweight contract check (script or test) to prevent reintroduction of deprecated namespace usage in OpenSSL-handle paths.
- Exit criteria:
  - Contract fails when newly added old-symbol usage appears.

## Verification Commands
- Focused:
  - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
  - `fpc -Fu./src tests/unit/test_crl_revocation_semantics.pas -otmp/test_crl_revocation_semantics && ./tmp/test_crl_revocation_semantics`
  - `fpc -Fu./src tests/unit/test_ocsp_client_semantics.pas -otmp/test_ocsp_client_semantics && ./tmp/test_ocsp_client_semantics`
- Gate:
  - `python3 scripts/compile_all_modules.py`

## Risk Notes
- `ICertificateEx`-style casts are OpenSSL-specific by design; migration is namespace/contract alignment, not cross-backend abstraction.
- Must avoid broad interface refactors in this lane; keep to minimal cast-site updates plus regression checks.
