# TSSLContextConfig Builder Adoption

## Goal

Make the recommended `TSSLContextBuilder` path consume the new additive
`TSSLContextConfig` surface for context-scoped fields, instead of continuing to
set every context field manually after raw context creation.

## Boundary

This batch is behavior-preserving:

- no public builder API changes
- no removal of `TSSLConfig`
- no change to PEM / PKCS#11 loading order
- no change to HTTP hooks, OCSP stapled response loading, or replay-store
  installer error contracts
- no change to deprecated `WithSNI(...)` warning + ignore behavior

The builder must preserve `WithSessionCache(False)` explicitly after the
factory call, because legacy normalization can otherwise re-enable the session
cache option from default cache sizing.

## Scope

- `src/fafafa.ssl.context.builder.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `tests/config/test_context_builder_try.pas`
- `tests/test_backend_custom_cipher_capability_truth_contract.pas`
- `tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh`
- `docs/BACKEND_CAPABILITY_MATRIX.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Verification

```bash
bash tests/scripts/test_tsslcontextconfig_builder_adoption_contract.sh
bash tests/scripts/test_tsslcontextconfig_surface_contract.sh
bash tests/scripts/test_custom_cipher_capability_truth_contract.sh
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_backend_custom_cipher_capability_truth_contract \
  -FEtmp/test_backend_custom_cipher_capability_truth_contract \
  -otmp/test_backend_custom_cipher_capability_truth_contract/test_backend_custom_cipher_capability_truth_contract \
  tests/test_backend_custom_cipher_capability_truth_contract.pas
./tmp/test_backend_custom_cipher_capability_truth_contract/test_backend_custom_cipher_capability_truth_contract
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc python3 scripts/compile_all_modules.py --rebuild
git diff --check
```

## Expected Result

- `BuildClient` / `BuildServer` create contexts through
  `TSSLFactory.CreateContext(const TSSLContextConfig)`.
- Existing builder runtime behavior stays green.
- `WithSessionCache(False)` remains observable on the built context.
- FreePascal custom cipher capability truth stays aligned with its
  fail-closed runtime setter behavior.
