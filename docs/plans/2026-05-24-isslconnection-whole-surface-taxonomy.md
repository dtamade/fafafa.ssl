# 2026-05-24 ISSLConnection Whole-Surface Taxonomy

## Goal

把当前 `v1.5.0` shipped 的 `ISSLConnection` surface 精确分桶成一张稳定、可验证、可继续演进的 taxonomy 图，避免后续再用“当前 core 只有 17 个方法”的目标叙事去覆盖真实 shipped surface。

## Why Now

`ISSLConnection` 现在已经不是“只有连接生命周期 + 读写”的小接口了。
它的当前 shipped surface 已经清晰分成：

- `17` core methods
- `6` convenience mirrors
- `18` compatibility-core mirrors

这张图如果不写进设计文档和契约里，后续很容易继续在局部 getter / owner path 上反复拉扯。

## Architecture

- 不改 public signature。
- 不做 runtime 行为变更。
- 只把当前 source truth 的全表面分类写清楚，并用 shell contract 锁住。
- `INTERFACE_DESIGN_V2` 继续保留 v2 目标 core，但必须同时承认当前 shipped truth 的完整 partition。

## Files

- `docs/reference/INTERFACE_DESIGN_V2.md`
- `tests/scripts/test_isslconnection_whole_surface_taxonomy_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md`

## Steps

1. 在 `INTERFACE_DESIGN_V2` 增加 current v1.5 whole-surface taxonomy section。
2. 新建 focused contract，检查：
   - source 里 `ISSLConnection` 仍精确是 41 个方法
   - 41 = 17 + 6 + 18 的分桶被写进 design doc
   - text / control / info / diagnostics / session / cert-verification / OCSP owner buckets 都落在文档里
3. 更新计划 / 发现 / 进度记录。
4. 必要时同步 roadmap 里的下一批推荐，避免 taxonomy batch 完成后路线文档过时。

## Verification

- `bash tests/scripts/test_isslconnection_whole_surface_taxonomy_contract.sh`
- `bash tests/scripts/test_isslconnection_convenience_surface_classification_contract.sh`
- `bash tests/scripts/test_isslconnection_control_owner_path_contract.sh`
- `bash tests/scripts/test_isslconnection_text_owner_path_contract.sh`
- `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
- `git diff --check`

## Risks

- 不要把当前 shipped taxonomy 误写成 v2 final shape。
- 不要把 `ISSLClientConnection` / `ISSLNativeHandleAccess` 误算进这 41 个方法的 partition。
- 不要把已经收口的 convenience / owner route 再退回成“未分类 getter 堆”。

## Execution Result

- PASS.
- Revalidated `tests/scripts/test_isslconnection_whole_surface_taxonomy_contract.sh` with `bash -n` and `bash`.
- The 41-method shipped taxonomy still holds as `17 core + 6 convenience mirrors + 18 compatibility-core mirrors`.
