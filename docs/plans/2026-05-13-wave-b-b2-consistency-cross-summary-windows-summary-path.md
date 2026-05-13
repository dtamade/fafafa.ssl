# Wave B/B2 Consistency Cross Summary Windows Summary Path Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 的 Windows active-path 漂移，让它在未显式传 `--windows-summary` 时，也能从 `cross summary` 继承实际使用的 custom `windows_summary` 路径，并因此正确触发 Windows runtime artifact 严格校验。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 cross summary 已经声明 active custom `windows_summary`，但 direct consistency 仍会忽略它并给出假绿灯；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `windows_summary` 路径的解析与继承；
- 最后用 focused 合同、Windows companion-path、linux examples active-path、macOS probe 与 Windows strict 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-cross-summary-windows-summary-path.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for active custom windows summary truth drift

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - cross summary already records an active custom `windows_summary`
  - that active Windows evidence implies runtime artifact strictness should apply
  - but direct consistency still ignores the custom summary path and returns green

## Task 2: Minimal active windows summary inheritance hardening

Patch:

- add a parser for the Windows `summary: <path> (overall=...)` row in cross summary
- add explicit flags for:
  - `--windows-summary`
  - `--windows-quick-log`
  - `--windows-runtime-transcript`
- when those flags are omitted:
  - inherit active `windows_summary` from cross summary first
  - then derive sibling runtime artifact paths from that resolved summary path
- do not expand this batch into workflow or producer-side changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

Expected after fix:

- direct consistency no longer gives a false-green when cross summary is already using a custom Windows summary path
- Windows runtime artifact strictness now follows the active custom summary path
- companion-path, linux examples active-path, macOS probe, and Windows missing-artifact regressions remain GREEN
