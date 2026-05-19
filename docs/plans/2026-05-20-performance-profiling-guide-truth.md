# Performance Profiling Guide Truth（2026-05-20）

## Goal
- 把 `docs/guides/PERFORMANCE_PROFILING_GUIDE.md` 收口到当前 profiling / runtime truth：
  - 不再把 session public surface 当成默认已命中的性能优化路径
  - 不再把固定 `70-90%` / `< 10ms` / 本地网络目标表写成 current truth
  - 补上 direct `CreateConnection(...)` 的 profiling 场景使用原因说明

## Why now
- WinSSL quickstart / user guide / best-practices 已经收回当前 conservative session truth。
- `PERFORMANCE_PROFILING_GUIDE` 仍保留：
  - `**预期提升**: 70-90% 握手时间减少`
  - `- [ ] 启用 Session 复用`
  - `| Session 复用握手 | < 10ms | 本地网络 |`
  - 握手 benchmark 直接使用 `CreateConnection(...)` 但没有解释 profiling 场景下为什么要走 direct path
- 这会把性能剖析页误读成“当前已验证的长期性能真相”，而不是“如何测、去哪里拿最新基线”。

## Scope
- `docs/guides/PERFORMANCE_PROFILING_GUIDE.md`
- `tests/scripts/test_performance_profiling_guide_truth_contract.sh`
- `docs/plans/2026-05-20-performance-profiling-guide-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不重开 WinSSL native resumed-handshake / benchmark 实现调查。
- 不重做既有 benchmark 脚本与 baseline artifacts。

## Approach
1. 新增 focused shell contract，冻结：
   - `PERFORMANCE_PROFILING_GUIDE`
     必须明确：
       - direct `CreateConnection(...)` 是为了 profiling 当前 caller-owned socket /
         handshake path，而不是 generic facade 主入口
       - WinSSL session public surface 当前仍只能按
         `observed_reuse=false` / `session_configured=true`
         的实验性 public truth 理解
       - 固定性能目标表不是 current truth，最新 baseline 应回到
         `scripts/run_phase2_performance_baseline.sh` /
         `tests/benchmarks/run_all_benchmarks.sh` /
         `docs/test_reports/PHASE2_PERFORMANCE_METRICS_TEMPLATE.md`
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_performance_profiling_guide_truth_contract.sh
bash tests/scripts/test_performance_profiling_guide_truth_contract.sh
bash tests/scripts/test_active_owner_path_docs_alignment_contract.sh
bash tests/scripts/test_secondary_guides_connection_level_sni_api_drift_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- `PERFORMANCE_PROFILING_GUIDE` 不再把固定性能数字和 WinSSL session public surface 误教成 current truth
- 读者可以清楚知道为什么 profiling 样例会直接落到 `CreateConnection(...)`
- 将来如果这页又回漂，focused contract 会立即报警
