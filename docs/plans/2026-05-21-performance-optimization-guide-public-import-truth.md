# Performance Optimization Guide Public Import Truth

## Goal

收口 `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
里一个仍残留的 active public import drift：

- 文档已经明确把
  `ISSLSessionResumption`
  /
  `ISSLDiagnostics`
  讲成 owner path
- 但示例 `uses`
  仍然退回
  `fafafa.ssl.base`

让这份活跃性能指南重新回到当前主门面 truth：

- 普通调用方可以直接
  `uses fafafa.ssl`
- owner-path 语义继续保持不变

## Scope

- Update:
  - `docs/guides/PERFORMANCE_OPTIMIZATION_GUIDE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Add:
  - `docs/plans/2026-05-21-performance-optimization-guide-public-import-truth.md`
  - `tests/contract/test_performance_optimization_guide_public_owner_surface_probe.pas`
  - `tests/scripts/test_performance_optimization_guide_public_import_truth_contract.sh`

不做：

- 不改性能语义
- 不改 MbedTLS / WinSSL session caveat 说明
- 不扩到 `PERFORMANCE_PROFILING_GUIDE.md`

## Why This Batch

当前 `PERFORMANCE_OPTIMIZATION_GUIDE`
已经正确强调：

- `GetSession / SetSession / IsSessionReused`
  优先走
  `ISSLSessionResumption`
- `GetPerformanceMetrics`
  优先走
  `ISSLDiagnostics`

但两段示例的 `uses`
仍写着：

- `fafafa.ssl.base`

而当前主门面
`fafafa.ssl`
已经 re-export：

- `ISSLConnection`
- `ISSLSession`
- `ISSLSessionResumption`
- `ISSLDiagnostics`
- `TSSLPerformanceMetrics`
- `TSSLStream`

所以这不是缺类型，
而是 active owner-path guide
仍在继续发布旧导入入口。

## Minimal Fix

1. 新增 focused contract，
   冻结这份指南的当前 public-import truth
2. 把两段示例 `uses`
   收回到
   `fafafa.ssl`
3. 增加 compile probe，
   证明这些 owner-surface 类型
   现在确实可由门面直接提供
4. 跑 focused + neighboring verification

## Verification

```bash
bash -n tests/scripts/test_performance_optimization_guide_public_import_truth_contract.sh
bash tests/scripts/test_performance_optimization_guide_public_import_truth_contract.sh
bash tests/scripts/test_mbedtls_session_resumption_doc_truth_contract.sh
git diff --check
```

## Expected Outcome

- 性能优化指南不再继续教学
  `fafafa.ssl.base`
- owner-path 语义保持原样，
  但普通入口重新回到主门面
- 这份高可见 active guide
  与前面收掉的 active examples / Linux quickstart
  在 public entry truth 上保持一致

## Execution Result

- PASS
- focused RED 首轮证明的是
  真实 active guide import drift，
  不是 compile probe 自己设计偏了：
  - `HEAD` 快照下
    新 contract
    第一条就因
    性能指南示例
    仍未使用
    `fafafa.ssl`
    而失败
- 最小修复后：
  - `PERFORMANCE_OPTIMIZATION_GUIDE`
    两段 owner-path 示例
    现已统一回到：
    - `uses fafafa.ssl;`
  - owner-path 语义
    保持不变：
    - `ISSLSessionResumption`
    - `ISSLDiagnostics`
- focused verification：
  - `bash -n tests/scripts/test_performance_optimization_guide_public_import_truth_contract.sh`
    - PASS
  - `HEAD` snapshot contract
    - FAIL
  - `bash tests/scripts/test_performance_optimization_guide_public_import_truth_contract.sh`
    - PASS
  - `bash tests/scripts/test_mbedtls_session_resumption_doc_truth_contract.sh`
    - PASS
  - `git diff --check`
    - PASS
