# Wave B Cross Summary Run ID Help Sync Plan

**Goal:** 收口 `generate_wave_b_cross_platform_summary.sh` 的帮助文本漂移，让 `--run-id` 文案与刚落地的“优先从 Linux summary 推导 run_id”实际行为保持一致。

**Architecture:** 这批只修 help/usage 文案，不动逻辑：
- 静态检查四个脚本的 `--run-id` 说明；
- 只改 `generate_wave_b_cross_platform_summary.sh` 唯一残留的“默认时间戳”旧描述；
- 用语法检查和 `--help` 输出匹配做轻量验证。

**Files:**

- Add: `docs/plans/2026-05-13-wave-b-cross-summary-run-id-help-sync.md`
- Update: `scripts/generate_wave_b_cross_platform_summary.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Static doc-sync fix

Patch:

- update `generate_wave_b_cross_platform_summary.sh` usage text for `--run-id`
- keep wording aligned with:
  - `check_wave_b_b2_closure_readiness.sh`
  - `check_wave_b_b2_evidence_consistency.sh`
  - `prepare_wave_b_b2_handoff_bundle.sh`

## Task 2: Lightweight verification

Run:

```bash
bash -n scripts/generate_wave_b_cross_platform_summary.sh
bash scripts/generate_wave_b_cross_platform_summary.sh --help | rg "默认优先从 Linux summary 推导，否则时间戳"
git diff --check
```

Expected after fix:

- syntax remains valid
- help output matches the new run_id inference behavior
- no whitespace or conflict issues

## Execution Result

- Static doc drift found only in `scripts/generate_wave_b_cross_platform_summary.sh`.
- Minimal help-text sync landed there only.
- Verification passed:
  - `bash -n scripts/generate_wave_b_cross_platform_summary.sh`
  - `bash scripts/generate_wave_b_cross_platform_summary.sh --help | rg "默认优先从 Linux summary 推导，否则时间戳"`
  - `git diff --check`
