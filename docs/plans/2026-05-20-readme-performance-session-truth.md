# README Performance + Session Truth（2026-05-20）

## Goal
- 把根 `README.md` 里的高入口性能/会话口径收回当前 truth，避免仓库首页继续把固定性能数字和 session public surface 写成长期结论。
- 当前需要锁住的 truth：
  - 能力矩阵缓存 / 性能相关结论应回到 fresh benchmark 与 baseline truth：
    - `docs/guides/PERFORMANCE_GUIDE.md`
    - `scripts/run_phase2_performance_baseline.sh`
    - `tests/benchmarks/run_all_benchmarks.sh`
  - `会话复用 / Session Ticket` 不能在首页直接写成固定 `70-90%` 收益
  - 尤其 WinSSL 当前仍应按 backend-specific conservative truth 理解：
    - `observed_reuse=false`
    - `session_configured=true`

## Why now
- `PERFORMANCE_PROFILING_GUIDE`、`WINSSL_BEST_PRACTICES`、`WINSSL_USER_GUIDE`
  等高可见页面已经收回当前性能/session truth。
- 根 `README.md` 仍保留：
  - `能力矩阵缓存，10,000x+ 性能提升（>10M ops/s）`
  - `会话复用: 70-90% 握手性能提升`
- 这些写法会直接改写用户对当前项目性能真相和 backend-specific session truth 的第一印象。

## Scope
- `README.md`
- `tests/scripts/test_readme_performance_session_truth_contract.sh`
- `docs/plans/2026-05-20-readme-performance-session-truth.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals
- 不修改 Pascal source。
- 不重做 benchmark runner / baseline artifacts。
- 不改版本历史里的历史叙事，只收当前高入口 active README 口径。

## Approach
1. 新增 focused shell contract，冻结：
   - `README.md`
     必须明确：
       - 性能相关结论应回到 `PERFORMANCE_GUIDE` 与 benchmark/baseline 入口
       - `会话复用 / Session Ticket` 是 backend-specific truth，
         不能首页直接承诺固定收益
       - WinSSL 当前仍应按 experimental public surface 理解
2. 先跑合同拿到 RED。
3. 做最小文档修复。
4. 跑 focused 合同与相关旧合同。
5. 更新 planning files 后提交推送。

## Commands
```bash
bash -n tests/scripts/test_readme_performance_session_truth_contract.sh
bash tests/scripts/test_readme_performance_session_truth_contract.sh
bash tests/scripts/test_landing_quickstarts_direct_path_classification_contract.sh
bash tests/scripts/test_performance_guides_benchmark_truth_contract.sh
bash tests/scripts/test_winssl_session_resumption_docs_truth_contract.sh
git diff --check
git status --short
```

## Expected Outputs
- 根 `README.md` 不再把固定性能数字和 session public surface 误教成 current truth
- 仓库首页会把性能 truth 重新导向 benchmark/baseline 入口
- 将来如果首页又回漂，focused contract 会立即报警
