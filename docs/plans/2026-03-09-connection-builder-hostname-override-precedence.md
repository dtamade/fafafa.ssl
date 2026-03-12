# Connection Builder Hostname Override Precedence Plan

**Goal**
- 收口 `TSSLConnectionBuilder.WithHostname(...)` 对 context fallback 的 precedence：默认继承、显式覆盖、显式清空三种路径都要有明确合同。

**Architecture**
- 当前 builder 在 `TryBuildClient(...)` 里只在 `FHostname <> ''` 时才调用 `ISSLClientConnection.SetServerName(...)`。
- 这意味着 non-empty override 可以覆盖 context fallback，但 empty override 无法表达；如果 context 默认 `ServerName` 已经在 connection 构造时注入，builder 就无法显式清空它。
- 这波先用离线 fake context/fake connection 把 precedence 合同锁住，再对 builder 增加最小 `has-override` 语义，不扩散到 connector 或更大 DSL。

**Files**
- Add: `docs/plans/2026-03-09-connection-builder-hostname-override-precedence.md`
- Add: `tests/test_connection_builder_hostname_override_precedence.pas`
- Modify: `src/fafafa.ssl.connection.builder.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 builder precedence focused RED。
2. 跑 RED，确认 empty override 不能清掉 context fallback。
3. 在 builder 内部增加最小 `has hostname override` 语义。
4. 跑 focused suites + compile-all。
5. 回写 working memory 与下一队列。

**Expected Outputs**
- 不调用 `WithHostname(...)` 时，builder 保留 connection 继承来的 context fallback。
- `WithHostname('override.example')` 覆盖 context fallback。
- `WithHostname('')` 显式清空 context fallback，而不是静默保留旧值。
