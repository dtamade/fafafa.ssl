# Migration Guide Raw Connection Truth Alignment

## Goal

把 `docs/guides/MIGRATION_GUIDE.md`
在 raw `ISSLConnection` 客户端示例上的当前真相重新压实：

- 不再要求 guide 直接点名 frozen 的 context-level SNI surface literal 名称
- 但如果 guide 继续保留 raw `ISSLConnection` 示例，
  就必须明确展示：
  - `Supports(..., ISSLClientConnection, ...)`
  - per-connection `SetServerName(...)`

同时把
`tests/scripts/test_migration_guide_active_truth_contract.sh`
从上一阶段的旧真相
同步到当前规则，
避免 contract 自己反过来要求已经被禁止的 literal-name 写法。

## Scope

- `docs/guides/MIGRATION_GUIDE.md`
- `tests/scripts/test_migration_guide_active_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Non-Goals

- 不改其它 guides / references
- 不重开 broader `ServerName` frozen-surface 文档范围
- 不改生产代码

## TDD Steps

1. 跑 `test_migration_guide_active_truth_contract.sh`，
   确认当前红灯是真实存在的 stale contract / guide snippet mismatch
2. 更新 `MIGRATION_GUIDE`：
   - raw `ISSLConnection` 示例改成 `Supports(..., ISSLClientConnection, ...)`
   - 保持 generic compatibility wording，不再点名 frozen surface literal 名称
3. 更新 contract：
   - 改成要求 generic compatibility wording
   - 保留对 raw connection + per-connection SNI 的正向要求
   - 显式禁止旧 literal-name 列表回流
4. Focused verification：
   - `bash -n tests/scripts/test_migration_guide_active_truth_contract.sh`
   - `bash tests/scripts/test_migration_guide_active_truth_contract.sh`
   - `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
   - `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
   - `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
   - `bash tests/scripts/test_withsni_surface_truth_contract.sh`
   - `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
   - `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
   - `git diff --check`

## Expected Outputs

- `MIGRATION_GUIDE` 的 raw connection 示例重新对齐当前 per-connection SNI truth
- contract 不再要求已经被禁止的 frozen-surface literal-name 列表
- migration guide 与三条 `ServerName` frozen-surface contract 可以同时保持绿色
