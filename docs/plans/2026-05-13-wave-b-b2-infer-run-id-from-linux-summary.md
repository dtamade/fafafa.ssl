# Wave B/B2 Infer Run ID From Linux Summary Plan

**Goal:** 收口 `Wave B/B2` 脚本在未显式传 `--run-id` 时的默认 run_id 漂移，让 `prepare` / `generate` / `closure` / `consistency` 都优先继承 Linux summary 的 run_id，而不是各自用当前时间戳制造新的证据批次。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 `prepare_wave_b_b2_handoff_bundle.sh` 在只给 Linux summary 的情况下会把输出文件命名成新的时间戳批次，导致一致性错误；
- 再把四个共享脚本统一为“显式 `--run-id` 优先，否则从 Linux summary 推导，最后才回退时间戳”；
- 最后用 focused contract 与现有 cross-summary / handoff / evidence / run-id passthrough 合同共同回归。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-infer-run-id-from-linux-summary.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`
- Update: `scripts/generate_wave_b_cross_platform_summary.sh`
- Update: `scripts/check_wave_b_b2_closure_readiness.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for inherited run_id

Run:

```bash
bash -n tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because `prepare_wave_b_b2_handoff_bundle.sh` still uses a fresh timestamp run_id
- derived artifacts therefore do not appear as `..._<linux_summary_run_id>.md`
- consistency report also drifts to `INCONSISTENT` due to linux_summary run_id mismatch

## Task 2: Minimal run_id inference unification

Patch:

- keep `--run-id` as the highest-priority explicit source
- when `--run-id` is omitted and Linux summary exists, parse `- run_id:` from that markdown and adopt it
- only if inference fails, fall back to `date +%Y%m%d_%H%M%S`
- apply the same rule to:
  - `generate_wave_b_cross_platform_summary.sh`
  - `check_wave_b_b2_closure_readiness.sh`
  - `check_wave_b_b2_evidence_consistency.sh`
  - `prepare_wave_b_b2_handoff_bundle.sh`

## Task 3: Regression verification

Run:

```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash -n scripts/check_wave_b_b2_closure_readiness.sh
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash -n tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh
bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

Expected after fix:

- default bundle mode inherits the Linux summary run_id
- generated cross/closure/consistency/bundle artifacts use the same run_id
- consistency stays `CONSISTENT` when Linux evidence is internally aligned
- older absolute-path, Windows-evidence, run-specific-examples, and run-id passthrough contracts remain GREEN

## Execution Result

- RED was captured as expected:
  - before the fix, omitted `--run-id` caused bundle outputs to be named from a fresh timestamp instead of the Linux summary run_id
- Minimal run_id inference hardening landed in:
  - `scripts/generate_wave_b_cross_platform_summary.sh`
  - `scripts/check_wave_b_b2_closure_readiness.sh`
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- GREEN verification passed:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
  - `bash -n scripts/check_wave_b_b2_closure_readiness.sh`
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_linux_checklist.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_absolute_input_contract.sh`
  - `bash tests/scripts/test_wave_b_ci_gate_run_id_passthrough_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
  - `git diff --check`
