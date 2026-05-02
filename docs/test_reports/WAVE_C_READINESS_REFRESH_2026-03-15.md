# Wave C Readiness Refresh（2026-03-15）

## Purpose

基于 2026-03-15 的最新本地执行结果，刷新 Wave C 的“流程 readiness”判断，并明确新的阈值结论必须来自统一后的 fast-local 入口。

## Current Truth

1. 流程入口已经统一：
   - `scripts/run_phase2_performance_baseline.sh --fast-local`
   - `scripts/run_wave_c_b101_validation_playbook.sh --fast-local`
2. `benchmark_cert_verify_cache` 已经进入统一 benchmark runner，不再是旁路 probe。
3. baseline 与 B101 两条链路都已具备 clean-worktree 语义，适合继续做本地连续验证。

## Fresh Evidence

| Flow | Command | Result | Evidence |
|------|---------|--------|----------|
| Phase 2 baseline | `bash scripts/run_phase2_performance_baseline.sh --fast-local --iterations 1 --tls-iterations 1 --skip-tls` | `PASS (3/3)` | `tmp/test-reports/PHASE2_BASELINE_DRAFT_20260315_165859.md` |
| B101 validation #1 | `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate` | `PASS` | `tmp/test-reports/wave_c_b101_validation_20260315_172046.md` |
| B101 validation #2 | `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate` | `PASS` | `tmp/test-reports/wave_c_b101_validation_20260315_180632.md` |
| B101 validation #3 | `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate` | `PASS` | `tmp/test-reports/wave_c_b101_validation_20260315_180735.md` |

## Readiness Decision

### Ready now

- 可以继续推进 Wave C 的本地连续验证与对比采样。
- 可以把后续所有 cert verify cache 证据都统一到当前两条入口命令上。
- 基于连续 3 次新的 B101 full-gate 样本，旧 B106 阈值已经在新入口下被重新证明。

### Still a manual decision

- 现在已经具备重新讨论 readiness/default-on 的证据。
- 但默认策略是否变化，仍应由你显式决定；当前不自动改成默认开启。

## Next Required Evidence

1. 以 `docs/test_reports/WAVE_C_UNIFIED_THRESHOLD_REFRESH_2026-03-15.md` 作为新的阈值观察入口。
2. 如继续推进，下一步应生成新的 B107/B108 风格 readiness / canary 文档。
3. 所有后续阈值讨论都应只引用 2026-03-15 之后的新入口证据。

## Recommendation

后续 Wave C 的所有 readiness/comparison 讨论，应以 2026-03-15 之后的新 fast-local 入口为准，不再以旧的单独 probe 或历史路径作为主证据。新的阈值观察表见 `docs/test_reports/WAVE_C_UNIFIED_THRESHOLD_REFRESH_2026-03-15.md`。
