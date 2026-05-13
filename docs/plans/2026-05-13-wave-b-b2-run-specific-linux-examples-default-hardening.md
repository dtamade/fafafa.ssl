# Wave B/B2 Run-Specific Linux Examples Default Hardening Plan

**Goal:** 收口 `generate_wave_b_cross_platform_summary.sh` 与 `prepare_wave_b_b2_handoff_bundle.sh` 在 Linux examples JSON 默认路径上的漂移，让它们优先消费 `test-reports/examples_compile_ci_gate_<run_id>.json`，只在缺失时回退到旧 generic 路径。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 handoff bundle 仍会因为旧 generic 默认值吃到陈旧 examples JSON；
- 再把 `generate` 与 `prepare` 的默认路径解析统一成“run-specific 优先，generic fallback”；
- 最后用 focused contract 和现有 cross-summary / handoff 回归合同一起验证，不扩到 `run_wave_b_ci_gate.sh` 的产物策略。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-run-specific-linux-examples-default-hardening.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`
- Update: `scripts/generate_wave_b_cross_platform_summary.sh`
- Update: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for run-specific Linux examples preference

Run:

```bash
bash -n tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because `prepare_wave_b_b2_handoff_bundle.sh` still forwards `test-reports/examples_compile_ci_gate.json`
- generated cross summary and consistency report therefore do not point at the run-specific JSON even when `test-reports/examples_compile_ci_gate_<run_id>.json` exists

## Task 2: Minimal default-path unification

Patch:

- keep `run_wave_b_ci_gate.sh` untouched for this batch because it still emits generic JSON by default outside workflow-passed `--examples-report`
- add the same `default_linux_examples_json_path()` helper to `prepare_wave_b_b2_handoff_bundle.sh`
- keep `generate_wave_b_cross_platform_summary.sh` on the same preference order
- avoid widening scope into new CLI options or mandatory run-specific-only behavior

## Task 3: Regression verification

Run:

```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash -n tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
git diff --check
```

Expected after fix:

- handoff bundle prefers the run-specific Linux examples JSON when it exists
- cross summary metrics come from the run-specific JSON rather than stale generic residue
- older cross-summary and handoff contracts stay GREEN

## Execution Result

- RED was captured as expected:
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh` failed because the generated reports still pointed to the generic Linux examples JSON
- Minimal default-path hardening landed in:
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- GREEN verification passed:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - `git diff --check`
