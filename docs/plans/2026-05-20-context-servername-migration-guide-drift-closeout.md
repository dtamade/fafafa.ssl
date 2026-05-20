# Context ServerName Migration Guide Drift Closeout

## Goal

把 `context-level ServerName` 主线已经冻结的
compatibility-only surface
继续限制在
`docs/reference/API_REFERENCE.md`
这一处权威说明里，
收掉
`docs/guides/MIGRATION_GUIDE.md`
重新点名
`TSSLConfig.ServerName`
/ `ISSLContext.SetServerName(...)`
/ `TSSLContextBuilder.WithSNI(...)`
的活跃文档漂移。

## Architecture

- 不改 runtime 行为
- 不改 public source declaration
- 只修 active guide wording 和 focused source/doc contract
- 让规则更对称：
  - `TSSLConfig.ServerName`
    只能在 `API_REFERENCE` 以 literal 名称出现
  - `WithSNI(...)`
    只能在 `API_REFERENCE` 以 literal 名称出现
  - direct `ISSLContext.SetServerName/GetServerName`
    也同步收紧到同一条 literal-name rule

## Files

- Add:
  - `docs/plans/2026-05-20-context-servername-migration-guide-drift-closeout.md`
- Modify:
  - `docs/guides/MIGRATION_GUIDE.md`
  - `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## TDD Steps

1. 先跑现有 focused contract，确认：
   - `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
   - `tests/scripts/test_withsni_surface_truth_contract.sh`
   已经因为 `MIGRATION_GUIDE` 漂移而变红
2. 把 `MIGRATION_GUIDE` 改成 generic wording，
   不再点名 frozen surface 的 literal API 名称
3. 顺手补强
   `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`，
   让 direct context API 也显式守
   “literal 名称只允许留在 API_REFERENCE”
4. Focused verification：
   - `bash -n tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
   - `bash tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
   - `bash -n tests/scripts/test_withsni_surface_truth_contract.sh`
   - `bash tests/scripts/test_withsni_surface_truth_contract.sh`
   - `bash -n tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
   - `bash tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
   - `git diff --check`

## Expected Outputs

- `MIGRATION_GUIDE` 不再把 frozen compatibility surface 重新抬回活跃指南层
- 三组 ServerName surface-truth contract 重新同时变绿
- direct context contract 不再只拦“调用示例”，也能拦 literal-name drift
