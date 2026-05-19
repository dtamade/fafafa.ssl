# Backend Capability Matrix Performance And Selection Truth

## Goal

收紧 `docs/BACKEND_CAPABILITY_MATRIX.md` 后半段的性能与选型口径，消除“固定性能数字 +
一刀切推荐”这两类已经脱离当前 benchmark 真相源和 backend-specific capability truth
的漂移。

## Architecture

这批保持 docs-only：

- 新增 focused shell contract，冻结根入口能力矩阵的性能/选型 truth
- 只修改 `docs/BACKEND_CAPABILITY_MATRIX.md`
- 不改 benchmark 脚本
- 不改任何 backend 实现
- 不扩大到旧版 archive/历史报告

## Files

- Add: `docs/plans/2026-05-19-backend-capability-matrix-performance-selection-truth.md`
- Add: `tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前 `docs/BACKEND_CAPABILITY_MATRIX.md` 后半段还有两类高风险漂移：

- 固定性能数字
  - 仍然保留 `1.0x / 1.2x / 0.8x` 这类相对值表
  - 但 repo 现行真相源已经切到：
    - `scripts/run_phase2_performance_baseline.sh`
    - `tests/benchmarks/run_all_benchmarks.sh`
    - `docs/guides/PERFORMANCE_GUIDE.md`
    - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
- 一刀切选型建议
  - 比如“Windows 应用推荐 WinSSL”本身不算错
  - 但如果不同时写清：
    - `Early Data`
    - caller-provided server OCSP stapling
    - session resumption / tickets runtime truth
    - custom cipher / PKCS#11 / 完整 PKCS#12 helper
    等 capability 边界，就会把“推荐入口”误读成“功能已完整且无 caveat”

这类问题比普通文案更危险，因为：

- 读者往往会直接根据这一页决定 backend 方向
- 一旦根入口给出的是“旧 benchmark 数字”或“无条件推荐”，后面再细读专项文档也很难完全纠正第一印象

## Verification

```bash
bash -n tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh
bash tests/scripts/test_backend_capability_matrix_performance_selection_truth_contract.sh
npx prettier --write docs/BACKEND_CAPABILITY_MATRIX.md
git diff --check
```

## Expected Outcome

- 根入口能力矩阵不再维护固定性能相对值表
- 性能部分改成当前 benchmark 入口与解读边界
- 选型建议改成 capability-aware recommendation，而不是无条件好坏排序
- Windows / FreePascal / embedded 路线的 caveat 会在根入口就被说清楚
