# Wave B/B2 Consistency Cross Summary Linux Examples Path Plan

**Goal:** 收口 `check_wave_b_b2_evidence_consistency.sh` 的 active-path 漂移，让它在未显式传 `--linux-examples` 时，能够从 `cross summary` 继承实际使用的 `linux_examples_json` 路径，而不是继续盯着默认 generic JSON。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 cross summary 已经声明了 custom `linux_examples_json`，但 consistency 仍可能因为 generic JSON 存在而给出假绿灯；
- 再只在 `check_wave_b_b2_evidence_consistency.sh` 内增加对 cross summary 中 active `linux_examples_json` 路径的解析与继承；
- 最后用 focused 合同、generic fallback / run-specific / macOS probe / Windows strict 回归共同验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-b2-consistency-cross-summary-linux-examples-path.md`
- Add: `tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh`
- Update: `scripts/check_wave_b_b2_evidence_consistency.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for false-green active-path drift

Run:

```bash
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because:
  - cross summary already records a custom `linux_examples_json` path
  - that active JSON is later corrupted
  - but consistency still tracks `test-reports/examples_compile_ci_gate.json` and stays green

## Task 2: Minimal active-path inheritance hardening

Patch:

- add a parser for `- linux_examples_json:` from cross summary
- when `--linux-examples` is not explicitly passed:
  - prefer the active path declared by cross summary
  - otherwise keep the current run-specific > generic fallback
- do not expand this batch into producer-side output changes

## Task 3: Regression verification

Run:

```bash
bash -n scripts/check_wave_b_b2_evidence_consistency.sh
bash -n tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_cross_summary_linux_examples_path_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_generic_linux_examples_fallback_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_run_specific_linux_examples_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_ignores_inactive_macos_probe_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

Expected after fix:

- consistency inherits the active `linux_examples_json` path from cross summary
- corrupted/missing active custom JSON no longer yields a false-green `CONSISTENT`
- generic fallback, run-specific priority, macOS probe, and Windows runtime strict rules remain GREEN
