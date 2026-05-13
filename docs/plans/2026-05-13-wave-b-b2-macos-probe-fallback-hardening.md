# Wave B/B2 macOS Probe Fallback Hardening Plan

**Goal:** 收口 `prepare_wave_b_b2_handoff_bundle.sh` 与 `wave-b-b2-manual.yml` 在 macOS probe-only 场景下丢失证据的问题，确保没有 macOS summary 时仍能把 `wave_b_macos_gate_probe_<run_id>.json` 传入 cross summary。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 handoff bundle 在只有 macOS probe 时仍会把 macOS 误报成 `PENDING/no evidence`；
- 再给 `prepare` 增加 `--macos-probe` 支持与默认 fallback；
- 同步补齐 workflow 汇总阶段的 probe fallback，并用文本 contract 锁住；
- 最后跑 focused 合同与现有 handoff/cross-summary/workflow 回归。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-macos-probe-fallback-hardening.md`
- Add: `tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`
- Add: `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- Update: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Update: `.github/workflows/wave-b-b2-manual.yml`
- Update: `.github/workflows/wave-b-b2-manual.yml.disabled`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for macOS probe-only fallback

Run:

```bash
bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because `prepare_wave_b_b2_handoff_bundle.sh` still ignores `wave_b_macos_gate_probe_<run_id>.json`
- generated cross summary therefore shows `macos = PENDING / no evidence` instead of `PROBE_ONLY`

## Task 2: Minimal macOS probe fallback

Patch:

- add `--macos-probe FILE` support to `prepare_wave_b_b2_handoff_bundle.sh`
- default it to `test-reports/wave_b_macos_gate_probe_<run_id>.json`
- when macOS summary is absent but the probe exists, forward `--macos-probe` into `generate_wave_b_cross_platform_summary.sh`
- include the macOS probe in the handoff artifact list for visibility
- update live/disabled workflow summary stage to use the same summary-first, probe-fallback rule

## Task 3: Regression verification

Run:

```bash
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled
git diff --check
```

Expected after fix:

- handoff bundle preserves `PROBE_ONLY` macOS evidence in the generated cross summary
- workflow summary stage also falls back to macOS probe evidence when summary is absent
- existing handoff/workflow contracts remain GREEN

## Execution Result

- RED was captured as expected:
  - `prepare_wave_b_b2_handoff_bundle.sh` ignored the macOS probe and the resulting cross summary downgraded macOS to `PENDING`
- Minimal probe fallback hardening landed in:
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
- GREEN verification passed:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_infer_run_id_from_linux_summary_contract.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary.sh`
  - `bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - `diff -u .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled`
  - `git diff --check`
