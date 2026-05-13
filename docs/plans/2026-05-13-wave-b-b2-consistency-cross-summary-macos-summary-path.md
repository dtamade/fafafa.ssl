# Wave B/B2 Consistency Cross Summary macOS Summary Path Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 的 macOS active-path 漂移，让它在未显式传 `--macos-summary` 时，也能从 `cross summary` 继承实际使用的 custom `macOS summary` 路径，并在 active summary 缺失/漂移时不再给出假绿灯。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 cross summary 已经声明 active custom `macOS summary`，但 direct consistency 仍会忽略它并保持 green；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `macOS summary` 路径的解析与继承；
- 最后用 focused 合同、inactive macOS probe、probe-only consistency、Windows active-summary strict 与 linux examples active-path 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-cross-summary-macos-summary-path.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for active custom macOS summary truth drift

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - cross summary already records an active custom `macOS summary`
  - that active summary is later removed
  - but direct consistency still ignores it and stays green

## Task 2: Minimal active macOS summary inheritance hardening

Patch:

- add a parser for the macOS `summary: <path> (overall=...)` row in cross summary
- add an explicit flag for `--macos-summary`
- when `--macos-summary` is omitted:
  - inherit active macOS summary from cross summary first
  - treat that inherited active summary as required evidence
- keep probe-only behavior unchanged:
  - `PROBE_ONLY/PROBE_OK` still route through `macos_probe`
  - inactive default probe files still stay ignored

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_macos_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_consistency_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_windows_summary_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
git diff --check
```

Expected after fix:

- direct consistency no longer gives a false-green when cross summary is already using a custom macOS summary path
- macOS probe-only and inactive-probe boundaries remain GREEN
- Windows and linux examples active-path regressions remain GREEN
