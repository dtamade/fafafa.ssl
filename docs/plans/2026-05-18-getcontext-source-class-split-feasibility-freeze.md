# `GetContext` Source/Class Split Feasibility Freeze

## Goal

把 `GetContext` 当前剩余的 source/class split 依赖面直接冻结成 allowlist：源码注释明确 `ISSLConnectionInfo.GetContext` 是当前 owner，活跃文档继续走 `ConnInfo.GetContext`，而 direct core `LConn.GetContext` 只允许保留在一处 mirror-equality contract proof 中。

## Scope

本批只处理 source comments、focused source contract 与台账：

- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.connection.base.pas`
- `tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 public signature
- 不改 backend connection 实现
- 不重跑重型 Pascal contract suite

## Why This Batch

`GetContext` 经过 active guidance cut 与 contract owner primacy 之后，live surface 已经非常小：

- 生产源码里只剩接口声明与 `TBaseSSLConnection.GetContext` 共享实现
- 活跃文档只剩 `ConnInfo.GetContext`
- direct core `LConn.GetContext` 只剩 `tests/contract/test_backend_contract.pas` 的 mirror proof

这说明它已经具备 source/class split feasibility，但如果不把这层真相锁成 allowlist，下一批很容易又回到“还有没有别的活跃依赖面”这种重复考古。

## Planned Changes

1. 在 `src/fafafa.ssl.base.pas` 的 `GetContext` 注释里补出更明确的 owner / preferred-access 说明。
2. 在 `src/fafafa.ssl.connection.base.pas` 的基类注释里补出 `GetContext` 当前只剩共享实现与 mirror-proof 语义。
3. 新增 focused source contract，守住：
   - active docs 继续只教 `ConnInfo.GetContext`
   - 生产源码没有新的 direct call dependency
   - direct core `LConn.GetContext` 只允许保留在 backend contract 的 mirror proof 里

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh
bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh
git diff --check
```

## Expected Outcome

- `GetContext` 的 remaining live surface 被 freeze 成稳定 allowlist
- 下一批可以更自信地决定是否进入 public deprecation wording 或切到下一条 mirror

## Execution Result

- PASS
- `tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh` 已通过，说明 source/class split allowlist 已冻结到当前 shipped truth。
