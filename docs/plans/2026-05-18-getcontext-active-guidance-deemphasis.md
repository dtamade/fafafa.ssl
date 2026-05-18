# `GetContext` Active Guidance De-emphasis

## Goal

把活跃文档里最后一处把 `ISSLConnection.GetContext` 当推荐路径的示例收掉，并把 `GetContext` 明确纳入 `ISSLConnectionInfo` 的优先 owner 说明，让 `GetContext` 成为下一条真正实现切片前最干净的 mirror。

## Scope

本批只处理 active docs、focused contract 与台账：

- `docs/CAPABILITY_MATRIX_GUIDE.md`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不修改 `src/` 下 public signature
- 不改 backend connection 实现
- 不重跑重型 compile-all / minimal-ci gates

## Why This Batch

`ISSLConnectionInfo` mirror group 的 migration map、active guidance、source classification 已基本冻结，但 `GetContext` 还留着两个残余问题：

- `docs/CAPABILITY_MATRIX_GUIDE.md` 仍直接示例 `Conn.GetContext.GetLibrary.GetCapabilities`
- `API_REFERENCE.md` 的优先路径说明还没有把 `GetContext` 明确纳入 `ISSLConnectionInfo` first guidance

当前 live evidence 也说明它适合作为第一优先 mirror：

- 生产源码里没有额外活跃调用点，除基类实现外只剩 contract mirror-equality 验证
- 活跃文档漂移面最小，最适合先收 owner 教学路径

## Planned Changes

1. 把 `CAPABILITY_MATRIX_GUIDE.md` 的 capability 示例改成先 `Supports(..., ISSLConnectionInfo, ...)`，再从 `ConnInfo.GetContext` 取 library capabilities。
2. 把 `API_REFERENCE.md` 的优先路径说明扩展到 `GetContext`。
3. 新增 focused contract，防止活跃文档重新教回 `Conn.GetContext`。
4. 在路线图里把 `GetContext` 明确记成当前第一优先 mirror。

## Verification

```bash
bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh
bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh
git diff --check
```

## Expected Outcome

- 活跃文档不再把 `ISSLConnection.GetContext` 当推荐路径
- `GetContext` 明确并入 `ISSLConnectionInfo` first guidance
- 下一批可以直接进入 `GetContext` 的 source/class split feasibility 或 deprecation 路线，而不是继续补文档残面
