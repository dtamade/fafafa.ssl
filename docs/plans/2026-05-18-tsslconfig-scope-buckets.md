# TSSLConfig Scope Buckets

## Goal

把 `TSSLConfig` 中最容易跨层混淆的 mixed-scope 字段收成一份稳定 truth：

- 不改 runtime 行为
- 不在这一批直接删字段或重构 backend
- 只把 source / docs / contract 对齐到当前已经证实的 scope buckets

## Architecture

- 保留 `TSSLConfig` 作为当前 `v1.x` public record
- 把 mixed-scope 字段明确分成 5 类：
  - `library-scoped defaults`
  - `context-scoped`
  - `connection-scoped`
  - `compatibility-only`
  - `option-bridge`
- 用 focused contract 固定这些 bucket，不再让后续审查重复考古 factory / backend 调用点

## Scope Buckets

### Ordinary Context/Request Fields

这一批不争议、仍按普通 context/config 输入处理的字段：

- `LibraryType`
- `ContextType`
- `ProtocolVersions`
- `PreferredVersion`
- `CertificateFile`
- `PrivateKeyFile`
- `PrivateKeyPassword`
- `CAFile`
- `CAPath`
- `VerifyMode`
- `VerifyDepth`
- `CipherList`
- `CipherSuites`
- `Options`

### Library-Scoped Defaults

- `LogLevel`
- `LogCallback`

Truth:

- 通过 `ISSLLibrary.GetDefaultConfig / SetDefaultConfig` 承载
- `TSSLFactory.CreateContext(...)` request path 显式拒绝它们

### Context-Scoped

- `SessionCacheSize`
- `SessionTimeout`
- `ALPNProtocols`
- `ClientEarlyDataEnabled`
- `ServerEarlyDataPolicy`
- `ServerMaxEarlyDataSize`
- `ServerEarlyDataReplayStoreFile`
- `ServerEarlyDataReplayStoreDirectory`

Truth:

- `TSSLFactory.CreateContext(...)` 等 context 创建路径会消费这些字段
- replay-store 两个字段还额外带有 server-only 约束

### Connection-Scoped

- `HandshakeTimeout`
- `BufferSize`

Truth:

- `TSSLFactory.CreateContext(...)` request path 显式拒绝自定义值
- timeout 应改走 `TSSLConnector.WithTimeout` / `TSSLAcceptor.WithTimeout` / `ISSLConnection.SetTimeout`
- buffer sizing 应放在外围 transport / IO layer

### Compatibility-Only

- `ServerName`

Truth:

- 当前只保留为 deprecated context-level SNI compatibility
- client context 创建路径是 warning + ignore
- server context 创建路径会 reject
- 新代码应走 per-connection SNI

### Option-Bridge Compatibility Flags

- `EnableCompression`
- `EnableSessionTickets`
- `EnableOCSPStapling`

Truth:

- 仍保留在 `v1.x` public record 中
- factory 会把它们归一化进 `Options`
- 这批不再把它们扩成新的 backend-private 配置槽

## Files

- Add: `docs/plans/2026-05-18-tsslconfig-scope-buckets.md`
- Add: `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
- Update: `src/fafafa.ssl.base.pas`
- Update: `docs/reference/API_REFERENCE.md`
- Update: `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Commands

1. `bash -n tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
2. `bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
3. `mkdir -p tmp/test_factory_connection_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_connection_scope_clarification -FEtmp/test_factory_connection_scope_clarification -otmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification tests/test_factory_connection_scope_clarification.pas && ./tmp/test_factory_connection_scope_clarification/test_factory_connection_scope_clarification`
4. `mkdir -p tmp/test_factory_logging_scope_clarification && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_factory_logging_scope_clarification -FEtmp/test_factory_logging_scope_clarification -otmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification tests/test_factory_logging_scope_clarification.pas && ./tmp/test_factory_logging_scope_clarification/test_factory_logging_scope_clarification`
5. `git diff --check`

## Expected Outputs

- `TSSLConfig` mixed-scope truth 可直接从源码注释和 API 参考读出
- focused contract 证明 5 个 buckets 仍与 factory / backend 真实行为一致
- 既有 logging / connection scope Pascal tests 继续为 green
- patch 保持 whitespace-clean
