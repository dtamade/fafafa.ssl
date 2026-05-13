# Wave B/B2 Handoff Bundle Windows Companion Path Hardening Plan

**Goal:** 修复 `prepare_wave_b_b2_handoff_bundle.sh` 的默认 Windows companion artifact 路径推导，让自定义 `--windows-summary` 路径不再和硬编码 `test-reports/...` 日志路径发生冲突。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明自定义 summary 路径下会出现假性 `INCONSISTENT`；
- 再只修 handoff bundle 自己的 companion-path 推导；
- 最后用 handoff 相关旧 contract 回归，确认没有把前两批路径修法带坏。

**Files:**

- Add: `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- Update: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for custom windows summary companion paths

Run:

```bash
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because the script still forwards `test-reports/winssl_quick_smoke_<run_id>.log` and `test-reports/winssl_runtime_suite_<run_id>.log` even when the Windows summary lives in a custom directory with sibling logs already present

## Task 2: Minimal companion-path derivation fix

Patch:

- add a tiny helper that derives sibling artifact paths from `WINDOWS_SUMMARY`
- keep current `test-reports/...` behavior unchanged when the summary itself still lives under `test-reports/`
- do not expand this batch into new CLI options

## Task 3: Regression verification

Run:

```bash
bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh
bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

Expected after fix:

- custom summary + sibling runtime logs no longer produce fake inconsistency
- older absolute-output / evidence-consistency contracts remain green

## Execution Result

- RED was captured as expected:
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh` failed because the resulting consistency report did not stay `CONSISTENT`
- Minimal companion-path hardening landed only in `scripts/prepare_wave_b_b2_handoff_bundle.sh`.
- GREEN verification passed:
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `bash -n tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_absolute_output_path_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
  - `git diff --check`
