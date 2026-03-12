# FreePascal Connection/Context Managed-Result Warning Alignment

## Goal
Eliminate managed-result initialization warnings in `src/fafafa.ssl.freepascal.connection.pas` and `src/fafafa.ssl.freepascal.context.pas` with minimal semantics-preserving initialization changes.

## Architecture
- Keep behavior unchanged (empty chain/empty bytes semantics remain the same).
- Replace warning-prone implicit managed-result initialization with explicit `Result := nil`.
- Keep read path guard-first: allocate and read only when payload size is positive.

## Scope
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Evidence writeback:
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Steps
1. Implement warning-alignment changes:
   - `TFreePascalConnection.DoGetPeerCertificateChain`: `Result := nil`
   - `TFreePascalContext.ReadStreamToBytes`: explicit `Result := nil`, allocate/read only when `LSize > 0`
2. Focused verification:
   - `fpc -Fu./src tests/openssl/test_openssl_chain_issuer_selection.pas -otmp/test_openssl_chain_issuer_selection && ./tmp/test_openssl_chain_issuer_selection`
3. Regression gate:
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Focused command passes and warning count decreases by 2 compared with previous baseline (`25 -> 23`).
- `python3 scripts/compile_all_modules.py` reports all modules compiled successfully.
- `task_plan.md` keeps exactly one `### Active Queue ...` section.
