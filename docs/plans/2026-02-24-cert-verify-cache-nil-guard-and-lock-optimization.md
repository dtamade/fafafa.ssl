# 2026-02-24 Cert Verify Cache nil-guard + 锁粒度优化

## Goal
- 修复 `TCertVerifyCache.ComputeFingerprint` 失败路径返回未初始化 32-byte 结果导致 `Put(nil, ...)` 污染缓存的问题。
- 在不改变外部行为前提下缩短 `TryGet/Put` 锁持有时间（指纹计算移出临界区），降低并发热点。

## Architecture / Scope
- 新增合同测试：`tests/test_cert_verify_cache_nil_guard.pas`
- 修改实现：`src/fafafa.ssl.cert.verify.cache.pas`
  - `ComputeFingerprint` 增加 nil/失败保护，返回空字节数组
  - DER 获取改为动态长度（移除固定 4096 buffer 截断风险）
  - `TryGet/Put` 先计算指纹，再进入锁

## Files
- `tests/test_cert_verify_cache_nil_guard.pas`
- `src/fafafa.ssl.cert.verify.cache.pas`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps (RED -> GREEN -> Regression)
1. RED：新增 nil-guard 合同，复现 `Put(nil, ...)` 导致缓存 size 非 0。
2. GREEN：最小修复 `ComputeFingerprint` 与 `TryGet/Put` 锁粒度。
3. Regression：运行新合同 + cert verify cache benchmark + 编译门禁。

## Expected Outputs
- `Put(nil, ...)` 不再写入缓存；`TryGet(nil, ...)` 稳定 miss。
- `ComputeFingerprint` 不再有 managed-result 未初始化 warning。
- 主干编译与关键回归保持全绿。
