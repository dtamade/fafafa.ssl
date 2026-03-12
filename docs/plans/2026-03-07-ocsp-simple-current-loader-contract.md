# 2026-03-07 OCSP Simple Current Loader Contract

> Final decision on 2026-03-07: keep the runtime-safe current-loader path. Redirected stdin exits cleanly in CI, so this entrypoint can stay friendly for manual runs while also supporting a focused runtime contract.

## Goal
Bring `tests/certificate/test_ocsp_simple.pas` onto current OpenSSL loader semantics and make it CI-safe under redirected stdin, with a stable ASCII completion marker.

## Architecture
- Contract-first change:
  - add `tests/scripts/test_ocsp_simple_runtime_contract.sh`
  - expect `[PASS] ocsp simple completed`
- Minimal program fix:
  - keep the simple/manual console output shape
  - use `LoadOpenSSLCore` + `TOpenSSLLoader.IsModuleLoaded(osmCore)`
  - explicitly load OCSP procvars via `LoadOpenSSLOCSP(GetCryptoLibHandle)`
  - print a stable `[PASS]` marker when enough OCSP functions are available
  - keep the final `ReadLn` prompt for interactive/manual use; redirected stdin remains non-blocking in CI

## Files
- Add: `tests/scripts/test_ocsp_simple_runtime_contract.sh`
- Modify: `tests/certificate/test_ocsp_simple.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps
1. RED
- `bash tests/scripts/test_ocsp_simple_runtime_contract.sh`
- Expected: fail because the current program does not emit `[PASS] ocsp simple completed` and does not explicitly load OCSP procvars.

2. GREEN
- Update `tests/certificate/test_ocsp_simple.pas` to current loader semantics and stable marker.
- Re-run:
  - `fpc -Fu./src tests/certificate/test_ocsp_simple.pas -otmp/test_ocsp_simple_fix && ./tmp/test_ocsp_simple_fix >/tmp/test_ocsp_simple_fix.log 2>&1`
  - `bash tests/scripts/test_ocsp_simple_runtime_contract.sh`
- Expected: compile/runtime pass and emit `[PASS] ocsp simple completed`.

3. Regression
- `bash tests/scripts/test_certificate_smoke_runtime_contract.sh`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `python3 scripts/compile_all_modules.py`

## Expected Outputs
- `test_ocsp_simple_runtime_contract.sh` passes.
- `test_ocsp_simple.pas` prints `[PASS] ocsp simple completed` on success.
- Existing certificate runtime batch and repo gates remain green.
