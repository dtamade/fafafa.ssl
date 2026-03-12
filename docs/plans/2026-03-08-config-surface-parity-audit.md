# Config Surface Parity Audit (2026-03-08)

## Goal
- 审查 `TSSLConfig`、`ISSLLibrary.SetDefaultConfig/GetDefaultConfig`、`TSSLFactory.CreateContext(const AConfig)`、以及 `TSSLContextBuilder` 之间的字段覆盖与作用域是否一致。
- 把“看起来能配、实际不生效”与“builder-only / library-only / context-only”边界说清楚。
- 为下一波最小整改提供优先级，而不是在接口边界不清时直接改行为。

## Scope
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.context.builder.pas`
- `src/fafafa.ssl.connection.base.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `docs/plans/2026-03-current-summary.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Architecture Snapshot
### 当前存在三类配置面
1. **Context/runtime config**
   - 通过 `ISSLContext` 可直接表达：protocols、preferred version、verify mode/depth、cipher list/suites、session cache/timeout、server name、ALPN、cert/key/CA file。
2. **Library-scope config**
   - 通过 `ISSLLibrary.SetDefaultConfig` 落到 `FDefaultConfig`，并且还会顺手更新 library 自身日志状态：`LogLevel` / `LogCallback`。
3. **Builder-only material/config**
   - `certificate PEM`、`private key PEM`、`PKCS#11 URI/PIN`、`session_cache_enabled` 等保存在 `TSSLContextBuilder`，不属于 `TSSLConfig` round-trip surface。

### 这三层目前并不是同构集合
- `TSSLConfig` 中有字段没有 runtime apply 面。
- `TSSLContextBuilder` 中有字段不属于 `TSSLConfig`。
- `SetDefaultConfig` 既保存 defaults，又修改 library 运行时状态，导致同一个 `TSSLConfig` record 同时承载不同作用域。

## Findings
### P1: `BufferSize` / `HandshakeTimeout` 是高风险“死字段”
- `TSSLConfig` 声明了 `BufferSize` 与 `HandshakeTimeout`，并且默认值会被填充到公共默认配置里。
- 但当前 `ISSLContext` 没有对应 setter/getter；`TSSLFactory.ApplyConfigToContext(...)` 也没有这些字段的应用点。
- `HandshakeTimeout` 的真实 runtime 面在 `ISSLConnection.SetTimeout(...)`，属于连接级，而不是 context 创建级。
- `BufferSize` 目前只存在于配置记录、默认值和 debug dump 中，没有看到 context / connection 创建链消费它。
- 结论：这两个字段现在会制造“配置成功”的错觉，是下一波最值得先收口的死字段。

### P1: `LogLevel` / `LogCallback` 是 library-scope 字段，但混在 request config 里
- 各 backend 的 `SetDefaultConfig(...)` 都会把 `LogLevel` / `LogCallback` 写入 library 状态。
- 但 `TSSLFactory.CreateContext(const AConfig)` 出于 P0 隔离要求，不会把请求配置写回 library。
- 这意味着：把 `LogLevel` / `LogCallback` 填进 request-scoped `TSSLConfig`，在 factory path 上是静默不生效的；只有 library-default path 才会生效。
- 结论：同一个 `TSSLConfig` record 现在同时装了 context-scope 与 library-scope 字段，语义容易误用。

### P2: `TSSLConfig` 与 `TSSLContextBuilder` 已经形成两套 DSL
- Builder 持有 `CertificatePEM`、`PrivateKeyPEM`、`PKCS#11 URI/PIN`、`session_cache_enabled` 等字段，并有完整 validation/build/apply 路径。
- `TSSLConfig` 没有这些 builder-only 字段，但有 `BufferSize` / `HandshakeTimeout` / `LogLevel` / `LogCallback` 这类 builder 不消费、runtime 也未完全消费的字段。
- 这解释了为什么前一波会误把 PEM 字段加进 factory config path：两个配置面看起来相似，但并不是同一个抽象。

## Evidence
- `TSSLConfig` 字段：`src/fafafa.ssl.base.pas`
- request apply 面：`src/fafafa.ssl.factory.pas`
- builder apply / validation：`src/fafafa.ssl.context.builder.pas`
- 连接级 timeout：`src/fafafa.ssl.connection.base.pas`
- library-level logging sink：
  - `src/fafafa.ssl.openssl.backed.pas`
  - `src/fafafa.ssl.winssl.lib.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.mbedtls.lib.pas`
  - `src/fafafa.ssl.wolfssl.lib.pas`

## Recommended Next Waves
### Wave D1 (P1): dead-field visibleization
- 目标：让 `BufferSize` / `HandshakeTimeout` 不再静默伪生效。
- 推荐最小方案（二选一）：
  1. 在 factory/config validation 层对这些字段给出明确 warning / error；
  2. 若确认短期不支持，则在 API/注释/文档中显式标记为未接线。
- 不建议在未统一 context/connection ownership 前，直接把它们硬接到某一个 backend。

### Wave D2 (P1): logging scope split
- 目标：把 `LogLevel` / `LogCallback` 从 request-scoped `CreateContext(AConfig)` 语义里剥离出来。
- 推荐方向：
  1. 保留它们仅用于 `ISSLLibrary.SetDefaultConfig`；或
  2. 引入单独的 library config surface，避免与 context request config 混用。

### Wave D3 (P2): config DSL boundary cleanup
- 目标：明确 `TSSLConfig` 与 `TSSLContextBuilder` 的职责边界。
- 推荐方向：
  1. `TSSLConfig` 只保留可 round-trip 到 runtime context 的字段；
  2. PEM / PKCS#11 / validation-only / builder ergonomics 继续留在 builder；
  3. 为两者建立一份 parity/ownership contract，防止再次跨面串字段。

## Verification Commands
- `rg -n "BufferSize|HandshakeTimeout|LogLevel|LogCallback" src tests docs`
- `rg -n "ApplyConfigToContext|CreateContext\(const AConfig|SetDefaultConfig\(|GetDefaultConfig\(" src`
- `rg -n "ValidateClient|ValidateServer|BuildClient|BuildServer" src/fafafa.ssl.context.builder.pas`
