# 2026-05-19 Interface Audit Current Truth Refresh

## Goal

把 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md` 里已经落后于当前源码/活跃文档真相的几条结论收回到当前状态，避免后续路线判断继续被旧审计带偏：

- context-level SNI 当前不再是 “factory/builder 仍主动写回 context” 的 live drift
- `ISSLServerConnection` 当前不再是 “活跃文档还承诺它存在” 的 docs drift
- `TSSLConfig.BufferSize` / `HandshakeTimeout` 当前不再是 “只是看起来像 inert 字段”

## Scope

- 只刷新当前静态审计报告与执行台账
- 用 focused shell contract 钉住：
  - 当前 source 对 `TSSLConfig.ServerName` / `WithSNI(...)` 的 `warning + ignore` 真相
  - 当前活跃架构/设计文档对 `ISSLServerConnection` 缺位的显式说明
  - 审计报告不再保留旧的 live-drift 结论
- 不修改 runtime 实现
- 不重写历史验证报告

## Files

- `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
- `tests/scripts/test_interface_audit_current_truth_contract.sh`
- `docs/plans/2026-05-19-interface-audit-current-truth-refresh.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Truth

- `TSSLFactory.CreateContext(...)` 当前对 `TSSLConfig.ServerName` 的 truth 是：
  - warning + ignore
  - 不再把它写回新建 context
- `TSSLContextBuilder.WithSNI(...)` 当前是：
  - compile-time deprecated compatibility-only surface
  - `BuildClient` warning + ignore
  - `BuildServer` warning + ignore
- 各 direct-library `CreateContext(...)` 当前也已对 `TSSLConfig.ServerName` 统一为：
  - server-side reject
  - client-side warning + ignore
- 活跃 `ARCHITECTURE` / `INTERFACE_DESIGN_V2` 当前都已显式说明：
  - public Pascal source 尚未声明 `ISSLServerConnection`
- `TSSLConfig.BufferSize` / `HandshakeTimeout` 当前在 factory / direct-library create-path 上是：
  - 显式 reject
  - 不是 silent inert，也不是普通 context-scoped option

## Steps

1. 增加 focused contract，让旧审计说法先 RED。
2. 刷新 `INTERFACE_DESIGN_AUDIT_V1.5.0.md` 的 summary、SNI、`ISSLServerConnection`、`TSSLConfig` 三段。
3. 同步台账，避免后续再按旧审计重开已收口问题。
4. 跑轻量验证并提交。

## Commands

```bash
bash -n tests/scripts/test_interface_audit_current_truth_contract.sh
bash tests/scripts/test_interface_audit_current_truth_contract.sh
git diff --check
```

## Expected Result

- 审计报告不再把已经收口的 SNI / `ISSLServerConnection` drift 写成当前 live blocker
- `TSSLConfig` 的描述回到“mixed-scope 仍是设计问题，但部分边界已显式 reject/warn”这个更准确的层次
- 后续路线判断重新回到当前源码真相

## Result

- 已完成。
- `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md` 现已重新对齐当前 source / active-doc truth：
  - 高层 `ServerName` / `WithSNI(...)` = warning/reject/ignore 的 frozen compatibility surface
  - 活跃 docs 已明确说明当前没有 `ISSLServerConnection`
  - `BufferSize` / `HandshakeTimeout` 在 create-path 上是显式 reject
- 这批没有改 runtime，只修正了静态审计控制面。

## Verification

```bash
bash -n tests/scripts/test_interface_audit_current_truth_contract.sh
bash tests/scripts/test_interface_audit_current_truth_contract.sh
git diff --check
```

- 结果：全部通过
