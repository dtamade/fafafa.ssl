# Managed Result Init Safety Wave 6

## Goal

继续把 managed-result 初始化安全从生产 TLS 1.3 单元推进到
`run_freepascal_tls13_completeness_gate` 覆盖的测试 harness。上一轮
residual discovery 证明核心源码已经 `186/186` 编译且 `0` warning，但
TLS 1.3 completeness gate 仍暴露了一批测试 helper 的同类 warning。

这批仍然不改测试语义，只把 `TBytes` result 初始化改成类型安全写法。

## Scope

- `tests/test_tls13_posthandshake.pas`
- `tests/test_tls13_clienthello_parser.pas`
- `tests/test_tls13_servercertverify.pas`
- `tests/test_freepascal_client_certificateverify_runtime.pas`
- `tests/test_freepascal_client_chain_trust_runtime.pas`
- `tests/test_freepascal_client_ocsp_stapling_runtime.pas`
- `tests/test_freepascal_server_ocsp_stapling_runtime.pas`
- `tests/test_freepascal_client_online_ocsp_runtime.pas`
- `tests/test_freepascal_client_ct_sct_surface.pas`
- `tests/test_freepascal_client_cert_verify_flags_runtime.pas`
- `tests/test_freepascal_tls13_early_data.pas`
- `tests/scripts/test_managed_result_init_safety_wave6_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Why This Batch

Residual discovery ran:

```bash
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
python3 scripts/compile_all_modules.py --rebuild \
  --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
  --unit-output-dir tmp/managed_result_residual_discovery_units \
  --timeout 120

FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
bash scripts/run_freepascal_tls13_completeness_gate.sh \
  --fast-local \
  --run-id managed_result_residual_discovery_tls13 \
  --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc
```

Results:

- `compile_all_modules.py`: `186/186` source modules compiled, `0` warnings.
- TLS 1.3 completeness gate: all runtime checks passed, but the compile logs
  contained managed-result warnings in the 11 test files listed above.

## Expected Result

- All targeted helper functions initialize `TBytes Result` with `Result := nil`
  before appending or resizing.
- The wave6 target files no longer use `SetLength(Result, 0)` as the initial
  empty result expression.
- Focused TLS 1.3 completeness gate still passes.
- Compile-log grep for
  `Warning: Function result variable of a managed type does not seem to be initialized`
  is clean for the wave6 target files.

## Verification

```bash
bash -n tests/scripts/test_managed_result_init_safety_wave6_contract.sh
bash tests/scripts/test_managed_result_init_safety_wave6_contract.sh
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
bash scripts/run_freepascal_tls13_completeness_gate.sh \
  --fast-local \
  --run-id managed_result_wave6_tls13 \
  --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
  2>&1 | tee tmp/managed_result_wave6_tls13_completeness.log
rg -n "Warning: Function result variable of a managed type does not seem to be initialized" tmp/managed_result_wave6_tls13_completeness.log || true
git diff --check
```

## Execution Result

- Updated the 11 TLS 1.3 completeness harness files in this batch so empty
  `TBytes` result initialization uses `Result := nil` instead of
  `SetLength(Result, 0)`.
- Added explicit `Result := nil` before direct result resizing helpers where
  the residual gate showed the same managed-result warning class.
- Focused contract passed:
  - `bash -n tests/scripts/test_managed_result_init_safety_wave6_contract.sh`
  - `bash tests/scripts/test_managed_result_init_safety_wave6_contract.sh`
- TLS 1.3 completeness gate passed:
  - run id: `managed_result_wave6_tls13`
  - report: `tmp/test-reports/freepascal_tls13_completeness_managed_result_wave6_tls13.md`
  - result: `18` passed, `0` failed
- Target warning grep is clean:
  - `rg -n "Warning: Function result variable of a managed type does not seem to be initialized" tmp/managed_result_wave6_tls13_completeness.log`
  - result: no matches
- Non-target warnings remain in the gate logs, including case-coverage,
  range-check constant, string-conversion, and generic
  `Function result does not seem to be set` warnings. Those are intentionally
  outside the wave6 scope.

## Next

Post-wave6 residual discovery ran:

```bash
FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
FAFAFA_FAST_LOCAL=1 \
FAFAFA_FPC_UNIT_OUTPUT_DIR=tmp/managed_result_post_wave6_module_units \
bash scripts/run_all_module_tests.sh --fast-local

rg -n "Warning: Function result variable of a managed type does not seem to be initialized" \
  tmp/managed_result_post_wave6_run_all_module_tests.log \
  tmp/test-reports/*20260524_234622_1649209*

FAFAFA_FPC_EXE=/opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
python3 scripts/compile_all_modules.py --rebuild \
  --fpc-exe /opt/fpcupdeluxe/fpc/bin/x86_64-linux/fpc \
  --unit-output-dir tmp/managed_result_post_wave6_compile_all_units \
  --timeout 120
```

Results:

- `run_all_module_tests.sh --fast-local`: `22` passed, `0` failed, `0` skipped.
- Broad module-test compile-log grep found no managed-result warnings.
- `compile_all_modules.py --rebuild`: `186/186` source modules compiled,
  `0` warnings.

No wave7 is justified for managed-result warnings on current evidence. The next
round should switch to a separately named warning family; current broad module
logs point at test `Unreachable code` warnings as the next concrete candidate.
