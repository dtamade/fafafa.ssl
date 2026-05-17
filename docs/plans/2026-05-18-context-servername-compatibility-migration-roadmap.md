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
  - client path 继续接受 `TSSLConfig.ServerName`
  - 但现在只发 compatibility warning，并忽略这份输入，不再把它写回新建 context
  - server path 已 fail-fast 拒绝 `ServerName`
- `TSSLContextBuilderImpl.WithSNI(...)`
  - `BuildClient` 现在只保留 compatibility warning，并忽略这份输入
  - `BuildServer` 现在只保留 compatibility warning，并忽略这份 client-only state
- `ISSLLibrary.SetDefaultConfig + TOpenSSLLibrary.CreateContext(...)`
  - OpenSSL backend-specific direct library path 现在也已与上面两条高层入口对齐
  - client path 会发出 library warning，并忽略 deprecated `ServerName`
  - server path 会 fail-fast 拒绝 `ServerName`
- `TSSLConnector`
  - 已经走正确方向：把 hostname 设置到 `ISSLClientConnection.SetServerName(...)`
  - 是目标语义的现成参考实现
- direct `ISSLContext.SetServerName/GetServerName`
  - 当前仍保留为最后仍可观察的 context-level compatibility surface

Current caveat discovered by live focused retest:

- all client backends now follow the same no-inheritance rule
- builder / factory high-level write paths no longer preserve deprecated context-level `ServerName` state on newly built contexts
- this moves the remaining migration question forward to the final public surface cleanup:
  - whether `TSSLConfig.ServerName` should keep its current naming / placement
  - whether `WithSNI(...)` should keep its current naming / placement
  - how long the direct context compatibility API should remain

### 2. Backend fallback read paths

以下 constructor path 仍保留 shared seam 痕迹，但不再把 context-level `ServerName` 兼容继承到连接实例：

- `src/fafafa.ssl.openssl.connection.pas`
- `src/fafafa.ssl.wolfssl.connection.pas`
- `src/fafafa.ssl.mbedtls.connection.pas`
- `src/fafafa.ssl.winssl.connection.pas`

当前最新真相是：

- direct deprecated `AContext.GetServerName` / `FContext.GetServerName` 读取已经从这五个 backend 的构造路径移除
- OpenSSL / WolfSSL / MbedTLS / WinSSL 仍统一经由 `src/fafafa.ssl.context.compat.pas`
- `src/fafafa.ssl.freepascal.connection.pas` 已经不再调用 shared helper
- shared helper 现在对任意非空 context 返回 `''`，因此它保留的是 no-inheritance seam，而不是 inherited fallback
- compatibility truth 仍然保留在 context API surface 本身，但它已经不再自动流入新的 client connection

### 3. Tests that intentionally lock the compatibility boundary

#### High-level write surfaces are now warning + ignore

- `tests/test_context_builder_server_servername_runtime_consistency.pas`
- `tests/test_factory_server_name_scope_clarification.pas`
- `tests/test_factory_config_server_name_isolation.pas`
- `tests/test_context_builder_server_name_compatibility_warning.pas`
- `tests/test_factory_server_name_compatibility_warning.pas`
- `tests/config/test_config_validation.pas`

#### Compatibility remains observable only through direct context API / explicit API-surface coverage

- `tests/test_cross_backend_client_context_server_name_clarification.pas`
- `tests/mbedtls/test_mbedtls_context_contract.pas`
- `tests/wolfssl/test_wolfssl_context_contract.pas`
- `tests/winssl/test_winssl_library_basic.pas`

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

Delivered third cut:

- `TSSLContextBuilderImpl.BuildClient` / `BuildServer` no longer silently apply `WithSNI(...)`
- builder runtime path now emits explicit compatibility warnings through `TSecurityLog.Warning('ContextBuilder', ...)`
- `docs/reference/API_REFERENCE.md` now explicitly classifies `TSSLContextBuilder.WithSNI(...)` as compatibility-only
- focused builder warning regressions and adjacent validation/runtime consistency tests stayed green

Delivered fourth cut:

- `TSSLContextBuilderImpl.BuildServer` no longer writes `WithSNI(...)` into the built server context
- server-side builder warning and validation wording now explicitly say `BuildServer ignores it and server-side connections ignore it`
- focused RED -> GREEN proved the built server context no longer retains the client-only `ServerName`, while adjacent warning/validation coverage stayed green

Delivered fifth cut:

- `TSSLContextBuilderImpl.BuildClient` no longer writes `WithSNI(...)` into the built client context
- factory default-config path and one-shot config path no longer write `TSSLConfig.ServerName` into newly built client contexts
- runtime / validation / API wording now consistently says the high-level builder/factory paths ignore deprecated context-level `ServerName` inputs for new contexts
- focused RED -> GREEN proved:
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
  - `tests/test_factory_server_name_compatibility_warning.pas`
  - `tests/config/test_config_validation.pas`
  all align to the new `warning + ignore` truth

Delivered sixth cut:

- `TOpenSSLLibrary.CreateContext(...)` no longer preserves `FDefaultConfig.ServerName` on newly built client contexts
- the OpenSSL backend-specific direct library path now emits a warning through the library log callback when client default-config still carries deprecated `ServerName`
- `TOpenSSLLibrary.CreateContext(sslCtxServer)` now fail-fast rejects default-config `ServerName` before creating the context
- focused RED -> GREEN proved:
  - `tests/test_openssl_library_default_config_server_name_clarification.pas`
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  so the remaining high-level write-surface drift is no longer hiding inside the OpenSSL direct library entry

### Phase C: Replace Backend Inherited Fallback With Explicit Compatibility Shim

**Target:** stop each backend constructor from silently reading deprecated context state directly.

Planned direction:

- move fallback behavior behind one shared compatibility seam
- then deprecate/shrink that seam in one place rather than five backend constructors

Precondition:

- Phase B has already made “new recommended usage” clear and tested

Delivered first cut:

- add `src/fafafa.ssl.context.compat.pas`
- OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL constructor fallback now all route through `GetContextLevelServerNameCompatibilityValue(...)`
- backend-local direct deprecated reads were removed from the targeted constructor paths
- focused source contract and runtime regressions proved behavior stayed intact

Delivered second cut:

- `GetContextLevelServerNameCompatibilityValue(...)` now returns empty for `sslCtxBoth`
- dual-role contexts still expose client-capable connections where appropriate, but deprecated context-level `ServerName` no longer auto-flows into that ambiguous role
- focused RED -> GREEN proved the `sslCtxBoth` fallback cut landed without regressing the existing roleless-handshake fail-fast boundary

Delivered third cut:

- `src/fafafa.ssl.freepascal.connection.pas` socket / stream client constructors no longer read `GetContextLevelServerNameCompatibilityValue(AContext)`
- FreePascal client connections now start with empty `ServerName` unless callers explicitly set per-connection hostname/SNI
- `tests/test_freepascal_context_server_name_inheritance.pas` was flipped from intentional compatibility coverage to negative regression coverage
- new focused source contract `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh` now guards the FreePascal runtime cut
- adjacent mock precedence contracts stayed green, so the remaining client-side intentional fallback surface is now concentrated in builder/connector mock precedence tests

Delivered third-cut follow-up sync:

- focused retest later proved three FreePascal-focused contracts still described the pre-cut behavior:
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
- those contracts were updated so they keep asserting deprecated context state preservation, but stop teaching connection inheritance that FreePascal runtime no longer performs
- this reclassified the next real implementation question: the remaining shared client fallback in OpenSSL / WolfSSL / MbedTLS / WinSSL is now the main unresolved cross-backend consistency seam

Delivered fourth cut:

- `src/fafafa.ssl.connection.builder.pas` no longer preserves inherited context fallback on the client builder path
- if a built client connection supports `ISSLClientConnection` and callers did not provide `WithHostname(...)`, `TryBuildClient` now explicitly clears `ServerName` to `''`
- `tests/test_connection_builder_hostname_precedence.pas` was flipped from intentional compatibility coverage to no-fallback precedence coverage
- explicit override and explicit empty clear semantics remained intact
- adjacent connector precedence contract stayed green, so the remaining higher-level intentional fallback surface is now concentrated in connector-side input contracts plus the server builder compatibility test

Delivered fifth cut:

- `src/fafafa.ssl.context.compat.pas` now returns `''` for any non-nil context
- OpenSSL / WolfSSL / MbedTLS / WinSSL still route through the shared helper, but the helper no longer forwards deprecated context-level `ServerName`
- `src/fafafa.ssl.freepascal.connection.pas` remains off the helper, so all current client-capable backends now follow the same no-inheritance rule
- new focused cross-backend contract `tests/test_cross_backend_client_context_server_name_clarification.pas` proved the previous divergence and then turned green after the helper cut
- `tests/scripts/test_context_server_name_compat_shim_contract.sh` was updated to the new truth:
  - helper required in OpenSSL / WolfSSL / MbedTLS / WinSSL
  - helper forbidden in FreePascal
  - direct context getter fallback forbidden in helper and backend sources

### Phase D: Final Surface Cleanup

**Target:** finish interface shape cleanup once migration risk is low enough.

Candidates:

- shrink `TSSLConfig.ServerName` responsibility
- revisit builder `WithSNI(...)` naming/scope
- update docs/reference after runtime truth actually changes

### Phase E: Residual Test-Surface Classification And Migration

**Target:** make the remaining active `context-level SetServerName(...)` hits obviously intentional or migrate them out of normal client-flow guidance.

Delivered first cut:

- four ordinary WinSSL client-flow tests moved from context-level SNI to per-connection SNI:
  - `test_winssl_error_mapping_online`
  - `test_winssl_https_client`
  - `test_winssl_revocation_online`
  - `test_winssl_mtls_e2e_local`
- focused shell contract now proves those files no longer teach context-level SNI
- Win64 cross-compile proof succeeded for the selected files

Delivered second cut:

- residual ambiguous files are now explicitly classified:
  - `test_tls_connector_early_data_contract` -> `INTENTIONAL_COMPAT`
  - `test_mbedtls_context_contract` -> `INTENTIONAL_API_SURFACE`
  - `test_wolfssl_context_contract` -> `INTENTIONAL_API_SURFACE`
  - `test_winssl_library_basic` -> `INTENTIONAL_API_SURFACE`
  - `test_winssl_mtls_skeleton` config smoke -> `INTENTIONAL_API_SURFACE`
- the real handshake path inside `test_winssl_mtls_skeleton` moved from `Ctx.SetServerName(ServerHost)` to per-connection `ISSLClientConnection.SetServerName(ServerHost)`
- focused residual contract is green, Linux-safe focused compiles are green, and Win64 cross-compiles for the two WinSSL files are green

Delivered third cut:

- `tests/integration/test_cross_backend_consistency_contract.pas`
  and `tests/integration/test_cross_backend_errors_contract.pas`
  no longer use deprecated context-level SNI guidance
- both contracts now require `ISSLClientConnection` and set hostname via `ClientConn.SetServerName(...)` before `Connect`
- the `www.google.com:80` handshake-failure branch in the error contract was migrated to the same per-connection path
- they were removed from `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- new focused source contract `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh` now guards that these files do not regress back to `Ctx.SetServerName(...)`
- compile/run shape stayed green; live network execution remained env-gated by `FAFAFA_RUN_NETWORK_TESTS!=1`

Delivered fourth cut:

- `tests/test_tls_connector_hostname_override_precedence.pas` no longer uses inherited context fallback as an intentional input
- the mock context-level `SetServerName('ctx.example.com')` setup was removed
- new focused source contract `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh` now guards that this test does not regress back to context-level SNI guidance
- connector override precedence behavior stayed green without the old compatibility setup, proving the contract only needs explicit per-connection override semantics

Delivered fifth cut:

- `tests/test_tls_connector_early_data_contract.pas` no longer uses inherited context fallback as an intentional input
- the mock context-level `SetServerName('ctx.example.com')` setup was removed
- new focused source contract `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh` now guards that this test does not regress back to context-level SNI guidance
- connector early-data ordering behavior stayed green without the old compatibility setup, proving the contract only needs explicit per-connection hostname plus early-data sequencing

## Progress Report

### Workstream status

- capability truth line: closed for current scope
- document/interface drift line: largely closed for current scope
- context-level ServerName migration:
  - Phase A discovery/lockpoint mapping complete
  - Phase B builder surface first cut complete
  - Phase B factory/config write-surface narrowing complete
  - builder runtime warning alignment complete
  - Phase B server-side BuildServer ignore cut complete
  - Phase B client-side high-level ignore cut complete
  - Phase B OpenSSL direct-library default-config alignment complete
  - Phase C shared compatibility shim first cut complete
  - Phase C `sslCtxBoth` ambiguity cut complete
  - Phase C FreePascal client runtime fallback cut complete
  - Phase C client connection-builder explicit-hostname cut complete
  - Phase C shared client fallback cut complete
  - Phase E first WinSSL client-flow migration cut complete
  - Phase E residual ambiguous test-surface classification cut complete
  - Phase E cross-backend network contract migration cut complete
- `TSSLConfig` cross-layer slimming: intentionally deferred until SNI migration stabilizes

### What This Means Operationally

- we are no longer blocked on “what is true now”
- we are no longer blocked on “which high-level path still preserves context state”
- we are now blocked on “how to clean up the remaining public compatibility surface”

## Next Recommended Batch

Choose one bounded implementation family only:

1. **Final surface cleanup prep**
   - re-evaluate whether `TSSLConfig.ServerName` and builder `WithSNI(...)` still need their current naming/placement now that builder/factory/runtime paths are all `warning + ignore`
   - define the next focused contracts/docs that distinguish compatibility-only public surface from recommended client-flow surface
2. **Direct context compatibility API cleanup staging**
   - decide how long direct `ISSLContext.SetServerName/GetServerName` should remain
   - decide whether it needs a clearer replacement path, narrower docs, or dedicated source contracts before any further shrink
3. **Wider public-surface cleanup**
   - stage follow-up work only after the compatibility-only public surface has a stable prep plan
Recommended first pick: **final public surface cleanup prep around `TSSLConfig.ServerName`, `WithSNI(...)`, and the remaining direct context compatibility API**.

Builder/factory/shared-shim warning work, residual test-surface classification, connector-side contract cleanup, the `sslCtxBoth` ambiguity cut, the shared client fallback divergence, and the final high-level direct-state control cut are no longer the blocker; the next highest-value work is shaping the final public compatibility surface.

## Verification

- `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- `git diff --check`
