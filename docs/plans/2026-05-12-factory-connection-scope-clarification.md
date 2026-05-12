# Factory Connection-Scope Clarification

## Goal

修复 `TSSLConfig.BufferSize` / `TSSLConfig.HandshakeTimeout` 在 factory/context 创建路径上的“假可用配置”问题：当前 public config、默认值和调试输出会让调用方以为这两个字段会被 `TSSLFactory.CreateContext(...)` 消费，但真实运行时并不会应用它们。

## Architecture

- 保持现有 runtime 行为不变，不临时扩 `ISSLContext` 或后端连接实现去“补消费”这两个字段。
- 先用 focused RED 合同证明：
  - one-shot `TSSLFactory.CreateContext(const AConfig)` 会静默接受自定义 `BufferSize` / `HandshakeTimeout`
  - library-default path `TSSLFactory.CreateContext(AContextType, ALibType)` 也会静默接受通过 `SetDefaultConfig(...)` 注入的自定义值
- 最小修法是在 factory 入口 fail-fast：
  - `HandshakeTimeout` 明确引导到 `TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)` / `ISSLConnection.SetTimeout(...)`
  - `BufferSize` 明确说明它不是 context-scoped factory 选项，应在 transport/IO 层配置
- 不重开 builder、connector、backend runtime 行为；这批只收口配置作用域真相。

## Files

- Add: `docs/plans/2026-05-12-factory-connection-scope-clarification.md`
- Add: `tests/test_factory_connection_scope_clarification.pas`
- Modify: `src/fafafa.ssl.factory.pas`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Steps

1. 更新 working-memory，锁定这批只处理 config-scope drift。
2. 写 focused RED：
   - one-shot request path 自定义 `HandshakeTimeout`
   - one-shot request path 自定义 `BufferSize`
   - library-default path 注入自定义 `HandshakeTimeout`
   - library-default path 注入自定义 `BufferSize`
3. 最小实现 factory fail-fast 校验。
4. 跑 focused test、必要 compile proof、diff hygiene。
5. 回填 findings/progress/task plan，review 后提交。

## Verification

1. `fpc -Fu./src -Fu./tests tests/test_factory_connection_scope_clarification.pas -otmp/test_factory_connection_scope_clarification && ./tmp/test_factory_connection_scope_clarification`
2. `git diff --check`
3. `git status --short`

## Risks

- 不要把未生效字段硬塞进 `ISSLContext` 或后端连接默认值系统，避免半套语义。
- 不要误伤合法的 logging-scope / early-data / server-name 既有 contracts。
- 错误消息要给出真实替代路径，不能只说“不支持”。
