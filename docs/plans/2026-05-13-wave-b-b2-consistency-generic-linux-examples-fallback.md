# Wave B/B2 Consistency Generic Linux Examples Fallback Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 的 direct 调用缺口，让它在未显式传 `--linux-examples` 且只有旧 generic `test-reports/examples_compile_ci_gate.json` 存在时，也能与 `generate_wave_b_cross_platform_summary.sh` / `prepare_wave_b_b2_handoff_bundle.sh` 保持一致，不再误判为缺失。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 cross summary 已经会消费 generic Linux examples JSON，但 direct consistency 仍只认 run-specific 默认路径；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内补齐 `run-specific 优先、generic fallback` 的默认解析；
- 最后用 focused 合同、现有 run-specific contract 与 handoff/workflow 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-generic-linux-examples-fallback.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for generic Linux examples fallback drift

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because cross summary already picks `test-reports/examples_compile_ci_gate.json`
- but `check_wave_b_b2_evidence_consistency.sh` still requires `test-reports/examples_compile_ci_gate_<run_id>.json`

## Task 2: Minimal direct consistency fallback hardening

Patch:

- add the same `default_linux_examples_json_path(...)` rule already used by `generate` and `prepare`
- keep precedence as:
  - explicit `--linux-examples`
  - run-specific `test-reports/examples_compile_ci_gate_<run_id>.json`
  - generic `test-reports/examples_compile_ci_gate.json`
- do not expand this batch into producer-side artifact naming changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
git diff --check
```

Expected after fix:

- direct consistency no longer misreports generic Linux examples JSON as missing
- run-specific Linux examples path still takes precedence when it exists
- handoff/run-id regressions remain GREEN
