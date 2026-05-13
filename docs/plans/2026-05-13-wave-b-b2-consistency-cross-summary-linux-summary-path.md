# Wave B/B2 Consistency Cross Summary Linux Summary Path Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 的 Linux active-path 漂移，让它在未显式传 `--linux-summary` 时，也能从 `cross summary` 继承实际使用的 custom `linux_summary` 路径，并在 active summary 缺失/漂移时不再给出假绿灯。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 cross summary 已经声明 active custom `linux_summary`，但 direct consistency 仍会忽略它并保持 green；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `linux_summary` 路径的解析与继承；
- 最后用 focused 合同、linux examples active-path、active macOS summary、active Windows summary、inactive macOS probe 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-cross-summary-linux-summary-path.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for active custom Linux summary truth drift

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - cross summary already records an active custom `linux_summary`
  - that active summary is later removed
  - but direct consistency still ignores it and stays green by tracking the default run-specific summary

## Task 2: Minimal active Linux summary inheritance hardening

Patch:

- add a parser for `- linux_summary:` from cross summary
- add an explicit flag for `--linux-summary`
- when `--linux-summary` is omitted:
  - inherit active Linux summary from cross summary first
  - keep it as required evidence
- do not expand this batch into run-id inference redesign or producer-side changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
git diff --check
```

Expected after fix:

- direct consistency no longer gives a false-green when cross summary is already using a custom Linux summary path
- linux examples/macOS summary/Windows summary active-path regressions remain GREEN
- inactive macOS probe boundary remains GREEN
