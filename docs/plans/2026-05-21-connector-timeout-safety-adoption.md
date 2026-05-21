# Connector Timeout Safety Adoption

## Goal

把
`TTimeoutDuration`
从
“主门面已 re-export、迁移文档已讲过”
继续推进到
当前最常见的 TLS 高入口：

- `TSSLConnector`
- `TSSLAcceptor`
- `ISSLConnectionBuilder`

让这些 public fluent path
不再只暴露裸毫秒整数，
而是同时支持明确单位的
`TTimeoutDuration`
overload。

本批 focused 目标：

- 为 timeout fluent API
  增加 type-safe overload
- 保留旧
  `Integer`
  overload
  维持兼容
- 把活跃文档 / compileable examples
  切到 type-safe 写法
- 用 focused compile/runtime contract
  证明：
  - typed timeout
    真能落到现有毫秒存储路径
  - legacy integer overload
    仍未丢

## Why This Batch

当前 repo
已经 shipped：

- `TTimeoutDuration`
- `docs/guides/MIGRATION_GUIDE_PHASE_2.4.md`
  里对“毫秒/秒混淆”的解释

但静态审查说明，
真实高入口仍有明显 adoption gap：

- `src/fafafa.ssl.tls.pas`
  仍只公开：
  - `TSSLConnector.WithTimeout(AMs: Integer)`
  - `TSSLAcceptor.WithTimeout(AMs: Integer)`
- `src/fafafa.ssl.connection.builder.pas`
  仍只公开：
  - `ISSLConnectionBuilder.WithTimeout(AMs: Integer)`
- 活跃示例仍在写：
  - `.WithTimeout(15000)`

这会让：

- 门面已经 re-export 的 type-safety truth
- 高入口实际教学面

继续分叉。

## Current Truth

- 连接级 timeout
  当前底层真相仍然是：
  `ISSLConnection.SetTimeout(Integer)`
- `TSSLConnector`
  /
  `TSSLAcceptor`
  /
  `TSSLConnectionBuilder`
  内部也都用
  `Integer`
  字段保存 timeout
- 所以这批不重构底层存储，
  只做高入口 bridge：
  `TTimeoutDuration -> Integer milliseconds`

## Scope

- Add:
  - `docs/plans/2026-05-21-connector-timeout-safety-adoption.md`
  - `tests/contract/test_connector_timeout_safety_entry.pas`
  - `tests/scripts/test_connector_timeout_safety_contract.sh`
- Update:
  - `src/fafafa.ssl.tls.pas`
  - `src/fafafa.ssl.connection.builder.pas`
  - `docs/INTEGRATION_GUIDE.md`
  - `docs/guides/MIGRATION_GUIDE.md`
  - `tests/examples/test_real_websites.pas`
  - `tests/examples/test_real_websites_enhanced.pas`
  - `tests/examples/test_real_websites_comprehensive.pas`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Minimal Fix

1. 在
   `TSSLConnector`
   /
   `TSSLAcceptor`
   /
   `ISSLConnectionBuilder`
   增加 overload：
   - `WithTimeout(const ATimeout: TTimeoutDuration)`
2. 保留旧：
   - `WithTimeout(AMs: Integer)`
3. bridge 规则：
   - 普通 duration：
     `ATimeout.ToMilliseconds`
   - `Infinite`：
     保持
     `-1`
     语义
   - 超出当前
     `Integer`
     毫秒范围时，
     明确抛
     `ESSLInvalidArgument`
4. 更新活跃文档 / compileable examples：
   - `TTimeoutDuration.Seconds(15)`

## Verification

```bash
bash -n tests/scripts/test_connector_timeout_safety_contract.sh
bash tests/scripts/test_connector_timeout_safety_contract.sh
git diff --check
```

contract 脚本内部会完成：

- public timeout overload 静态检查
- docs/examples adoption 静态检查
- compile/run focused probe：
  - connector typed timeout -> `15000`
  - acceptor typed timeout -> `20000`
  - builder typed timeout -> `12000`
  - connector legacy integer timeout -> `2500`

## Expected Result

- `TTimeoutDuration`
  不再只停留在 facade/doc truth
  与孤立迁移说明里
- TLS 高入口的 timeout fluent path
  开始真实采用 type-safe overload
- 活跃文档与 compileable examples
  不再继续教学裸
  `15000`
  这种单位不透明写法
