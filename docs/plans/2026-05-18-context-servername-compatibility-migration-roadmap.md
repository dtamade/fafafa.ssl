# Context ServerName Compatibility Migration Roadmap

## Goal

把 `context-level ServerName` 从“到处残留、反复把我们拉回旧兼容语义”的历史包袱，收成一条可执行、可验证、可分批推进的迁移主线；在不误伤当前兼容用户的前提下，最终把客户端 SNI/hostname 真正收敛到 per-connection 路径。

## Why This Is The Main Remaining Interface Track

当前 `fafafa.ssl` 的接口/实现大盘里，已经阶段性收口的包括：

- 文档不再承诺不存在的 `ISSLServerConnection`
- capability runtime truth
- deserializer precedence
- capability diff
- capability serializer output truth
- context-level ServerName 的内部 deprecated warning 噪音

真正仍跨越 public interface、factory/builder、高层 connector、五个 backend connection constructor、以及多份 focused tests 的剩余主线，就是 `context-level ServerName` 兼容迁移。

## Current Truth Map

### 1. Public / high-level write paths

- `TSSLFactory.CreateContext(...)`
  - client path 仍会把 `TSSLConfig.ServerName` 写回 context
  - server path 已 fail-fast 拒绝 `ServerName`
- `TSSLContextBuilderImpl.WithSNI(...)`
  - `BuildClient` 仍把 `FServerName` 写回 context
  - `BuildServer` 也仍保留这个兼容写入，但 validation 已明确 warning “server-side connections ignore it”
- `TSSLConnector`
  - 已经走正确方向：把 hostname 设置到 `ISSLClientConnection.SetServerName(...)`
  - 是目标语义的现成参考实现

### 2. Backend fallback read paths

以下 backend connection constructor 仍会把 `AContext.GetServerName` 复制到连接实例：

- `src/fafafa.ssl.openssl.connection.pas`
- `src/fafafa.ssl.freepascal.connection.pas`
- `src/fafafa.ssl.wolfssl.connection.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `src/fafafa.ssl.winssl.connection.pas`

这说明当前 compatibility truth 不是单一入口残留，而是 5 backend 共同实现的历史语义。

### 3. Tests that intentionally lock the compatibility boundary

#### Compatibility should remain observable for now

- `tests/test_connection_builder_hostname_precedence.pas`
- `tests/test_tls_connector_hostname_override_precedence.pas`
- `tests/test_freepascal_context_server_name_inheritance.pas`
- `tests/test_context_builder_server_servername_runtime_consistency.pas`
- `tests/test_sslctxboth_client_capability_clarification.pas`
- `tests/integration/test_cross_backend_consistency_contract.pas`
- `tests/integration/test_cross_backend_errors_contract.pas`

#### Scope / warning semantics already tightened

- `tests/test_factory_server_name_scope_clarification.pas`
- `tests/config/test_config_validation.pas`

## Roadmap

### Phase A: Freeze The Compatibility Boundary

**Target:** stop losing time on rediscovery.

Deliverables:

- unified migration roadmap on disk
- intentional-compatibility tests explicitly labeled and guarded by shell contract
- planning files updated with source/test map

Exit:

- everyone can answer “哪些地方是暂时保留的兼容语义，哪些地方已经禁止新增漂移” without fresh archaeology

### Phase B: Narrow High-Level Write Surfaces

**Target:** reduce new writes into deprecated context-level SNI without touching backend fallback yet.

Planned direction:

- keep `TSSLConnector` / `TSSLConnectionBuilder.WithHostname` as preferred client path
- evaluate whether builder needs a split between:
  - compatibility `WithSNI(...)`
  - modern per-connection hostname-oriented path
- avoid changing server builder/runtime behavior until a dedicated RED is chosen

Suggested first implementation candidates:

1. isolate builder import/export/config surfaces that still serialize `server_name`
2. decide whether builder clone/snapshot/import/export should keep or de-emphasize that field
3. add focused contracts before any behavioral deletion

Delivered first cut:

- builder JSON/INI import/export now keeps `server_name` for compatibility but also emits `server_name_mode=deprecated_context_sni`
- legacy JSON/INI payloads that only carry `server_name` still import, and re-export with the new compatibility marker
- focused regressions proved clone/reset/merge/import-export behavior stayed green

Delivered second cut:

- factory default-config path and one-shot config path still preserve `TSSLConfig.ServerName` compatibility, but they now emit an explicit runtime warning through `TSecurityLog.Warning('Factory', ...)`
- warning text directly identifies `TSSLConfig.ServerName` as deprecated context-level SNI compatibility and redirects callers to `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
- `src/fafafa.ssl.base.pas` and `docs/reference/API_REFERENCE.md` now describe the field as compatibility-only instead of a normal recommended path
- focused regressions proved scope, isolation, and logging-scope behavior stayed green

### Phase C: Replace Backend Inherited Fallback With Explicit Compatibility Shim

**Target:** stop each backend constructor from silently reading deprecated context state directly.

Planned direction:

- move fallback behavior behind one shared compatibility seam
- then deprecate/shrink that seam in one place rather than five backend constructors

Precondition:

- Phase B has already made “new recommended usage” clear and tested

### Phase D: Final Surface Cleanup

**Target:** finish interface shape cleanup once migration risk is low enough.

Candidates:

- shrink `TSSLConfig.ServerName` responsibility
- revisit builder `WithSNI(...)` naming/scope
- update docs/reference after runtime truth actually changes

## Progress Report

### Workstream status

- capability truth line: closed for current scope
- document/interface drift line: largely closed for current scope
- context-level ServerName migration:
  - Phase A discovery/lockpoint mapping complete
  - Phase B builder surface first cut complete
  - Phase B factory/config write-surface narrowing complete
  - next mainline is Phase C shared compatibility shim extraction
- `TSSLConfig` cross-layer slimming: intentionally deferred until SNI migration stabilizes

### What This Means Operationally

- we are no longer blocked on “what is true now”
- we are now blocked on “which compatibility cut to implement first”

## Next Recommended Batch

Choose one bounded implementation family only:

1. **Shared compatibility shim extraction**
   - prepare one shared helper for context-to-connection fallback
   - leave public behavior unchanged in the first patch
2. **Final surface cleanup prep**
   - re-evaluate whether `TSSLConfig.ServerName` and builder `WithSNI(...)` still need their current naming/placement once the shared shim exists

Recommended first pick: **Shared compatibility shim extraction**.

Phase B is now closed for the current additive-compatibility scope; the next highest-value work is consolidating the backend fallback seam before any real behavioral deletion.

## Verification

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- `git diff --check`
