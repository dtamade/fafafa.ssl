# TSSLConfig Public-Surface Slimming Roadmap

## Goal

把 `TSSLConfig` 从“已经知道有 mixed-scope / compatibility debt，但还没有字段级迁移决策”的状态，推进成一份可执行的 public-surface slimming roadmap。

## Why This Batch

当前 `TSSLConfig` 这条主线已经收口了这些前置真相：

- scope buckets truth
- fresh default-config truth parity
- option-bridge precedence freeze
- option-bridge surface truth freeze
- active guidance cleanup

但还缺一份真正能指导后续实现批次的字段级迁移矩阵：

- 哪些字段继续留在 `v1.x` record 主路径
- 哪些字段只是 library defaults
- 哪些字段应该明确迁走到 connection / transport surface
- 哪些字段是 compatibility-only，需要在 `v2` 彻底脱挂

## Deliverables

1. 在 `docs/reference/API_REFERENCE.md` 增加 `TSSLConfig Migration Targets` 段落
2. 给出字段级替代入口
3. 明确 `v1.x` 状态与 `v2` 方向
4. 增加 focused contract，防止 migration map 回退

## Field-Level Migration Decisions

### 继续留在 context-safe `TSSLConfig` 主路径

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
- `SessionCacheSize`
- `SessionTimeout`
- `ALPNProtocols`
- `ClientEarlyDataEnabled`
- `ServerEarlyDataPolicy`
- `ServerMaxEarlyDataSize`
- `ServerEarlyDataReplayStoreFile`
- `ServerEarlyDataReplayStoreDirectory`

### 迁移到 library defaults surface

- `LogLevel`
  - 当前推荐：`ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)`
- `LogCallback`
  - 当前推荐：`ISSLLibrary.SetLogCallback(...)`
- `v2` 方向：
  - 不再把 library defaults 混在 context/request config record 中

### 迁移到 connection / transport surface

- `HandshakeTimeout`
  - 当前推荐：`TSSLConnector.WithTimeout(...)` / `TSSLAcceptor.WithTimeout(...)` / `ISSLConnection.SetTimeout(...)`
- `BufferSize`
  - 当前推荐：外围 socket / stream / transport / app-level buffer policy
- `v2` 方向：
  - 从 context factory record 中移出 connection-adjacent 字段

### 迁移到 per-connection SNI surface

- `ServerName`
  - 当前推荐：`TSSLConnectionBuilder.WithHostname(...)` / `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `v1.x` 状态：deprecated compatibility-only field
  - `v2` 方向：不再作为 context-level config field 主挂载

### 迁移到 option-set surface

- `EnableCompression`
- `EnableSessionTickets`
- `EnableOCSPStapling`
  - 当前推荐：直接写 `Options`，或 builder `WithOption(...)`
  - `v1.x` 状态：compatibility-only option-bridge booleans
  - `v2` 方向：不再作为正常首选写入口

## Execution Order

1. 冻结字段级 migration map
2. 维持 `v1.x` compatibility，不急着删字段
3. 后续实现批次再决定：
   - `TSSLLibraryDefaults` 一类独立 surface
   - 更窄的 `TSSLContextConfig`
   - builder / connector / transport 侧补面
4. 只有在替代入口稳定后，才进入真实 removal / remount

## Files

- `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
- `docs/reference/API_REFERENCE.md`
- `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`
- `docs/test_reports/INTERFACE_AND_BACKEND_VERIFICATION_2026-05-18.md`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`

## Verification

```bash
bash -n tests/scripts/test_tsslconfig_migration_targets_contract.sh
bash tests/scripts/test_tsslconfig_migration_targets_contract.sh
git diff --check
```

## Expected Outcome

- 后续不再只停留在“`TSSLConfig` 有 mixed-scope 设计债”的抽象结论
- 字段级迁移方向和当前替代入口有统一权威文档
- 下一批可以直接从 migration matrix 进入实现/拆分，而不是先重做分析
