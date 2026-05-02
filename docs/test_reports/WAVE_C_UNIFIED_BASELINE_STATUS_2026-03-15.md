# Wave C Unified Baseline Status（2026-03-15）

## Goal

记录当前 Wave C 的真实本地执行入口，避免继续依赖 2026-02-08 的历史旁路命令与旧输出路径。

## Current Entry Commands

```bash
# Phase 2 baseline（推荐，本地 clean-worktree）
bash scripts/run_phase2_performance_baseline.sh --fast-local --iterations 1 --tls-iterations 1 --skip-tls

# B101 validation playbook（推荐，本地 clean-worktree，全门禁）
bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate
```

## Latest Verified Outputs

| Flow | Command | Result | Output |
|------|---------|--------|--------|
| Phase 2 baseline | `bash scripts/run_phase2_performance_baseline.sh --fast-local --iterations 1 --tls-iterations 1 --skip-tls` | `PASS (3/3)` | `tmp/test-reports/PHASE2_BASELINE_DRAFT_20260315_165859.md` |
| B101 validation | `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate` | `PASS` | `tmp/test-reports/wave_c_b101_validation_20260315_172046.md` |

## What Changed

1. `benchmark_cert_verify_cache` 已进入统一 benchmark runner，不再是旁路 probe。
2. Phase 2 baseline 在 `--fast-local` 下会同时隔离：
   - benchmark results
   - benchmark bin
   - generated draft report
3. B101 validation playbook 在 `--fast-local` 下会同时隔离：
   - report/logs
   - benchmark bin
   - full-gate compile/module 临时目录

## Current Acceptance Snapshot

| Check | Status |
|-------|--------|
| baseline runner includes cert verify cache | PASS |
| baseline fast-local stays clean | PASS |
| B101 playbook fast-local dry-run stays clean | PASS |
| B101 playbook fast-local execution stays clean | PASS |
| Wave B summary no longer emits `TODO` placeholders | PASS |

## Recommended Next Step

基于当前统一入口，补一份新的 Wave C comparison/readiness 文档，替代旧的单独 probe 结论，后续阈值/默认开启评估都以该入口为准。
