# Wave B/B2 Windows Runtime Evidence Consistency Hardening Plan

**Goal:** 修复 `Wave B/B2` 最终 evidence consistency 的 Windows 证据缺口，避免 `windows_summary` 已存在时，`quick smoke` 日志与 `broader runtime suite transcript` 缺失却仍被判成 `CONSISTENT`。

**Architecture:** 这批保持 repo-side、脚本级、小范围修复，不碰 `src/` 生产 Pascal 代码。顺序是：
- 先把缺口写成 focused RED contract；
- 再最小扩展 `scripts/check_wave_b_b2_evidence_consistency.sh` 的 Windows artifact 口径；
- 最后把 workflow / handoff bundle 的参数链补齐为显式传递，确保 summary 阶段不会再只依赖 `windows_summary`。

**Files:**

- Add: `tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `.github/workflows/wave-b-b2-manual.yml`
- Update: `.github/workflows/wave-b-b2-manual.yml.disabled`
- Update: `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- Update: `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Prove the current false-closure window with a focused RED contract

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - fixture includes Linux summary / examples / Windows summary / cross summary / closure report
  - fixture deliberately omits `winssl_quick_smoke_<run_id>.log` and `winssl_runtime_suite_<run_id>.log`
  - current `scripts/check_wave_b_b2_evidence_consistency.sh --strict` still exits `0`

## Task 2: Tighten the consistency checker to require Windows runtime artifacts

Implement:

- Add explicit CLI inputs for:
  - `--windows-quick-log`
  - `--windows-runtime-transcript`
- Default them to:
  - `test-reports/winssl_quick_smoke_<run_id>.log`
  - `test-reports/winssl_runtime_suite_<run_id>.log`
- When `windows_summary` exists, treat the two runtime artifacts as required evidence.
- Surface both artifacts in the report matrix so missing runtime evidence is visible in the generated markdown.

Expected after fix:

- strict mode returns non-zero when Windows summary exists but either runtime artifact is missing
- non-Windows / no-Windows-summary runs remain backward compatible

## Task 3: Make the caller chain explicit

Patch:

- `.github/workflows/wave-b-b2-manual.yml`
  - extend `WINDOWS_ARGS` to include the quick smoke log and runtime transcript paths when Windows summary exists
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - use the same explicit `WINDOWS_ARGS` shape for local/handoff bundle generation
- `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - lock the new workflow expectation so future edits cannot quietly drop the runtime-artifact arguments again

Expected after fix:

- summary phase no longer communicates Windows evidence as only `--windows-summary`
- the workflow contract proves the runtime-artifact parameter chain is explicit

## Task 4: Verify and close the batch

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh
git diff --check
git status --short
```

Expected outputs:

- new RED contract turns GREEN
- existing Windows workflow/bundle contracts stay green
- diff hygiene passes
- working-memory files reflect the actual fix and verification evidence

## Execution Result

- RED was captured exactly as expected:
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh` failed before the fix because strict consistency still returned success without `winssl_quick_smoke` / `winssl_runtime_suite`
- The minimal repo-side fix landed in the intended places:
  - `scripts/check_wave_b_b2_evidence_consistency.sh`
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
  - `scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- GREEN verification passed:
  - `bash -n scripts/check_wave_b_b2_evidence_consistency.sh`
  - `bash -n scripts/prepare_wave_b_b2_handoff_bundle.sh`
  - `bash -n tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh`
  - `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
  - `bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
  - `bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh`
  - `git diff --check`
- Extra smoke proof:
  - local `prepare_wave_b_b2_handoff_bundle.sh` fixture run now emits `consistency_status: INCONSISTENT` plus missing `windows_quick_log` / `windows_runtime_transcript` rows when those artifacts are absent
- Template parity stayed intact:
  - `.github/workflows/wave-b-b2-manual.yml.disabled` remains textually aligned with the live workflow
