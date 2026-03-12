# Deprecated Warning Noise Control for Compatibility Shims

## Goal
Eliminate remaining deprecated warnings in focused compile by applying local warning scopes to backward-compatibility call sites, without changing runtime behavior.

## Architecture
- Keep compatibility APIs intact.
- Do not remove deprecated surfaces.
- Apply narrow `{$PUSH}{$WARN SYMBOL_DEPRECATED OFF}...{$POP}` only at intended compatibility bridge calls.
- Keep all functional logic unchanged.

## Scope
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `src/fafafa.ssl.openssl.connection.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `src/fafafa.ssl.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. In factory/config application paths, localize deprecated context-level SNI setter calls.
2. In OpenSSL connection constructors, read context default server name under local deprecated warning scope.
3. In OpenSSL library default-config application, localize deprecated context-level SNI get/set comparison.
4. In public compatibility facade (`fafafa.ssl.pas`), localize deprecated wrapper-forwarding warnings.
5. Verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Focused compile warning count reaches `0` (non-warning notes may remain).
- `python3 scripts/compile_all_modules.py` reports all modules compiled successfully.
