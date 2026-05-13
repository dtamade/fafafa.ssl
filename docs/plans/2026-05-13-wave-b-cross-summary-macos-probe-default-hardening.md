# Wave B Cross Summary macOS Probe Default Hardening Plan

**Goal:** 收口 `generate_wave_b_cross_platform_summary.sh` 的 direct 调用缺口，让它在未显式传 `--macos-probe` 且没有 macOS summary 时，也能自动拾取 `test-reports/wave_b_macos_gate_probe_<run_id>.json`。

**Architecture:** 这批继续保持纯脚本层、静态、最小修法：
- 先用 focused contract 证明 direct cross-summary 入口仍会忽略默认 macOS probe；
- 再只给 `generate_wave_b_cross_platform_summary.sh` 增加 run-specific probe 默认检测；
- 最后用 focused 合同和现有 cross-summary / handoff probe 合同回归。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-cross-summary-macos-probe-default-hardening.md`
- Add: `tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh`
- Update: `scripts/generate_wave_b_cross_platform_summary.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED contract for default macOS probe pickup

Run:

```bash
bash -n tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh
```

Expected before fix:

- `bash -n` passes
- runtime contract fails because the direct script still ignores `test-reports/wave_b_macos_gate_probe_<run_id>.json` unless `--macos-probe` is provided explicitly
- resulting cross summary therefore shows `macos = PENDING / no evidence`

## Task 2: Minimal default probe detection

Patch:

- after run_id is known, default `MACOS_PROBE` to `test-reports/wave_b_macos_gate_probe_<run_id>.json` when the caller omits it
- keep macOS summary precedence unchanged
- do not expand this batch into closure/evidence changes, since those scripts intentionally remain summary-only

## Task 3: Regression verification

Run:

```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash -n tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary_macos_probe_default_contract.sh
bash tests/scripts/test_wave_b_cross_platform_summary.sh
bash tests/scripts/test_wave_b_cross_platform_summary_no_todo_pending_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_macos_probe_fallback_contract.sh
git diff --check
```

Expected after fix:

- direct cross-summary generation preserves `PROBE_ONLY` evidence without requiring explicit `--macos-probe`
- existing handoff-level probe fallback contract stays GREEN
- no older cross-summary contracts regress
