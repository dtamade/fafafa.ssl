# Certificate Smoke + Production Example Compile Contracts Plan

## Goal
Add focused shell contracts for two remaining uncontracted high-signal areas:
1. self-contained certificate/OpenSSL smoke programs that are suitable for local runtime validation;
2. production-style HTTPS example programs that should be compile-validated locally but not runtime-validated because they depend on network endpoints or external certificates.

## Architecture
- Add one grouped runtime contract under `tests/scripts/` for selected `tests/certificate/*.pas` programs.
- Add one grouped compile-only contract under `tests/scripts/` for `examples/production/*.pas` programs.
- Keep the change minimal: prefer existing summary strings as pass tokens, and add a single stable ASCII completion marker only where a suitable token is missing.

## Files
- Add: `tests/scripts/test_certificate_smoke_runtime_contract.sh`
- Add: `tests/scripts/test_production_examples_compile_contract.sh`
- Modify: `tests/certificate/test_p2_pkcs7_data.pas`
- Update evidence: `task_plan.md`, `findings.md`, `progress.md`

## Steps
1. RED: add `test_certificate_smoke_runtime_contract.sh` expecting a stable ASCII completion token from `test_p2_pkcs7_data.pas`; run it and observe failure.
2. GREEN: add the minimal stable marker to `tests/certificate/test_p2_pkcs7_data.pas`.
3. Verify the new certificate runtime contract passes.
4. Add `test_production_examples_compile_contract.sh` using `-Fu./src -Fu./examples` for the five `examples/production/*.pas` programs.
5. Run `bash -n` on both new scripts.
6. Run focused regressions plus repo gates:
   - `bash tests/scripts/test_certificate_smoke_runtime_contract.sh`
   - `bash tests/scripts/test_production_examples_compile_contract.sh`
   - `bash scripts/run_minimal_ci_gate.sh --fast-local`
   - `python3 scripts/compile_all_modules.py`

## Expected Outputs
- Certificate smoke programs gain an executable grouped runtime contract.
- Production example programs gain an executable grouped compile-only contract with the correct helper-unit search path.
- Local baseline stays green.
