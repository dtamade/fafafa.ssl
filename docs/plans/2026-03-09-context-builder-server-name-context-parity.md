# Context Builder ServerName Context Parity Plan

**Goal**
- 修复 `TSSLContextBuilderImpl.BuildServer` 与现有 context/config 语义之间关于 `ServerName` 的遗漏分叉。

**Architecture**
- 当前 `BuildClient` 会把 builder 中的 `ServerName` 写入 context，`TSSLFactory.CreateContext(const AConfig)` 也会通过 `ApplyConfigToContext(...)` 写入 `ServerName`。
- 但 `BuildServer` 只应用 `ALPNProtocols`，遗漏 `ServerName`，导致同一 builder/config surface 在 client/server 路径上行为不一致。
- 这波先用 `tests/config/test_config_validation.pas` 增加 focused RED，再在 `BuildServer` 做最小补齐，不扩散到其他 scope 规则。

**Files**
- Add: `docs/plans/2026-03-09-context-builder-server-name-context-parity.md`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `tests/config/test_config_validation.pas`
- Update: `docs/plans/2026-03-current-summary.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

**Steps**
1. 增加 server build `ServerName` parity RED。
2. 跑 focused RED，确认 `BuildServer` 未把 `ServerName` 写入 context。
3. 在 `BuildServer` 做最小修复。
4. 跑 focused suites + compile-all。
5. 回写 working memory 与下一队列。

**Expected Outputs**
- `BuildServer` 与 `BuildClient` / factory request path 对 `ServerName` 的 context-level行为一致。
- `BuildServerWithValidation(...)` 成功用例不再静默丢失 `WithSNI(...)` 输入。
