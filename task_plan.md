# Task Plan - Interface Design And Backend Implementation Verification

## Goal

全面验证 `fafafa.ssl` 的公共接口设计、门面/工厂/builder/config 语义、以及各 backend 实现与 capability 发布是否一致；把发现写成可复用记录，并在边界清晰时直接修复高价值问题，避免后续反复从旧 release / old roadmap 入口重新拉起。

> note:
> - 本轮用户要求“执行一个 goal 全面的验证并记录”。
> - 线程内 goal 工具当前保留了一条已完成 goal，无法再次新建；因此这份 `task_plan.md` 与新增 `docs/plans/...` 将作为本轮新的权威 goal 记录。

## Current Status

- [completed] `v1.5.0` release / workflow / cross-platform runtime closeout 已经不再是当前主线：
  - 当前默认控制面应保持在 `post-release route selection`
  - 不再围绕 release lane 或旧的 Windows runtime blocker 重复开工
- [completed] 已存在一份较强的静态接口审查基线：
  - `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`
  - 但它主要聚焦 public interface 设计，不等于“接口设计 + 各 backend 实现对齐”已被全面验证
- [in_progress] 当前批次已切换到新的 repo-level goal：
  - 先建立新的计划/记录入口
  - 再按“公共接口 -> facade/factory/builder/config -> capability matrix -> backend implementation truth -> focused fix”顺序推进
- [completed] 第一轮接口/后端真相交叉验证已经完成：
  - 已确认 `ISSLServerConnection` 只存在于活跃文档承诺，不存在于 public source
  - 已确认 context-level `ServerName` 仍由 factory / builder / connection constructors / tests 一起固化
  - 已确认 `BufferSize` / `HandshakeTimeout` 是显式拒绝的 connection-scoped config，不是 silent no-op
  - 已确认 capability dual-truth 仍是系统性结构，不是单 backend 漏洞
- [completed] 当前批次已落一条边界清晰的最小修复：
  - 修正文档中不存在的 `ISSLServerConnection` 承诺
  - 新增 `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
- [completed] 第二条边界清晰的 capability 真相修复已经落地：
  - 在 `src/fafafa.ssl.base.pas` 新增 `NormalizeLegacyCapabilityBooleans(...)`
  - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 的 `GetCapabilities` 统一在返回前用 `*Support` 字段回填 legacy boolean 兼容视图
  - capability focused contracts 已切到 “runtime truth 以 support-level 为准，legacy boolean 只是 compatibility projection”
- [completed] serializer / deserializer / diff 线上的两处具体真 bug 已完成收口：
  - 反序列化现在在检测到 v1.2 `*Support` 字段时，会用 support-level truth 覆盖冲突的 legacy boolean
  - capability diff 不再忽略 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport` 以及 support-only 的 v1.2 字段
  - 新增 focused regression 证明红灯已转绿，且旧 round-trip 兼容仍保持
- [completed] `context-level ServerName` 内部 warning quarantine 已按 live 证据收窄：
  - `tests/contract/test_capabilities_contract.pas` 已固定为当前 deprecated warning compile probe
  - `wolfssl` / `mbedtls` / `winssl` 的兼容 fallback 读取点已加局部 warning quarantine
  - 没有改动 factory / builder / runtime compatibility 语义
- [completed] serializer 输出面的 truth projection 已对齐到 v1.2 support-level 真相：
  - 新增 `tests/test_capability_serialization_truth_projection.pas`，直接检查 JSON/XML 输出字符串
  - serializer 现在会在 record 已携带 support-level truth 时，先回填 legacy boolean 再输出
  - 既有 JSON/XML round-trip 兼容保持绿色
- [completed] `context-level ServerName` 迁移路线图与兼容锁点地图已固化：
  - 新增 `docs/plans/2026-05-18-context-servername-compatibility-migration-roadmap.md`
  - intentional compatibility tests 已统一纳入 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - 当前已明确下一批应优先做 builder surface narrowing，而不是直接硬删 backend fallback
- [completed] `context-level ServerName` Phase B 的第一刀 builder surface narrowing 已收口：
  - `TSSLContextBuilderImpl.ExportToJSON/INI` 在保留 `server_name` 兼容载荷时，会显式导出 `server_name_mode=deprecated_context_sni`
  - `ImportFromJSON/INI` 继续接受 legacy-only `server_name` 输入，并在回导出时自动补上兼容 marker
  - focused config regressions 证明这是 additive compatibility de-emphasis，不是 runtime 行为删改
- [completed] `context-level ServerName` Phase B 的第二刀 factory/config surface narrowing 已收口：
  - `TSSLFactory.CreateContext(AContextType, ALibType)` 与 `TSSLFactory.CreateContext(const AConfig)` 在 client-side 兼容写入 `TSSLConfig.ServerName` 时，都会发出显式 warning
  - warning 直接点名 `TSSLConfig.ServerName` 是 deprecated context-level SNI compatibility，并把调用方导向 `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `src/fafafa.ssl.base.pas` 与 `docs/reference/API_REFERENCE.md` 已把该字段降格成 compatibility-only 入口
  - focused factory regressions 证明当时这次收口没有直接改掉现有兼容写入行为；后续 FreePascal runtime cut 已让该 backend 的 client connection 不再继承
- [completed] `context-level ServerName` Phase C 的第一刀 shared compatibility shim 已收口：
  - 新增 `src/fafafa.ssl.context.compat.pas`
  - OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 的 constructor fallback 已统一改走 `GetContextLevelServerNameCompatibilityValue(...)`
  - direct deprecated `AContext.GetServerName` / `FContext.GetServerName` 读取已从五个 backend 本地构造路径移除
  - focused source contract 与当时的跨 backend fallback runtime regressions 均保持绿色；后续 FreePascal 已先行切到 no-inheritance
- [completed] `context-level ServerName` 的 builder runtime warning 已与 validation / factory 对齐：
  - `TSSLContextBuilderImpl.BuildClient` 会在应用 `WithSNI(...)` 兼容写入前发出显式 warning
  - `TSSLContextBuilderImpl.BuildServer` 会发出显式 warning；当前后续批次已进一步收口为 warning + ignore
  - `docs/reference/API_REFERENCE.md` 已把 `WithSNI(...)` 也降格成 compatibility-only 入口
  - focused builder warning regressions、validation regressions 与 runtime consistency regressions 均保持绿色
- [completed] 第一批明确属于普通 WinSSL 客户端连接流的测试已迁到 per-connection SNI：
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
  - 这些文件不再通过 context-level `SetServerName(...)` 教客户端连接流
  - focused source contract 绿灯，Win64 交叉编译也已通过
- [completed] 残余 `context-level SetServerName(...)` 模糊测试面已完成分类/收口：
  - `tests/test_tls_connector_early_data_contract.pas` 已显式标记为 `INTENTIONAL_COMPAT`
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
    已显式标记为 `INTENTIONAL_API_SURFACE`
  - `tests/winssl/test_winssl_mtls_skeleton.pas` 的真实握手路径已迁到 per-connection SNI
  - focused residual contract 绿灯，Linux-safe / Win64 focused 编译验证已通过
- [completed] 第一条真正的 behavior migration 已经以 server-side builder dead-compat cut 落地：
  - `TSSLContextBuilderImpl.BuildServer` 保留 `WithSNI(...)` compatibility warning，但不再把它写回 built context
  - `ValidateServer` / runtime warning / API note 已同步改成 `BuildServer ignores it and server-side connections ignore it`
  - focused RED -> GREEN：
    - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - `tests/config/test_config_validation.pas`
- [completed] 第一条 client-side fallback behavior migration 已经以 `sslCtxBoth` ambiguity cut 落地：
  - shared compatibility shim 不再把 dual-role `sslCtxBoth` 的 deprecated context-level `ServerName` 继承进新连接
  - `sslCtxBoth` 仍 exposes `ISSLClientConnection`，但调用方若选择 client role，必须显式在 connection 上设置 `ServerName`
  - `tests/test_sslctxboth_client_capability_clarification.pas` 已不再属于 intentional-compat label 集合
  - focused RED -> GREEN：
    - `tests/test_sslctxboth_client_capability_clarification.pas`
    - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- [completed] 跨 backend 网络合同已不再把 deprecated context-level SNI 当成普通指导路径：
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
    已统一迁到 `CreateConnection(...) -> ISSLClientConnection.SetServerName(...)`
  - 它们已从 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 的 intentional-compat 集合中移除
  - 新增 `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`，直接守住“不再教 `Ctx.SetServerName(...)`”
  - focused compile/runtime shape 保持绿色；本机 live network path 仍因 `FAFAFA_RUN_NETWORK_TESTS!=1` 保持 gate skip
- [completed] FreePascal 客户端连接已不再继承 deprecated context-level `ServerName` fallback：
  - `src/fafafa.ssl.freepascal.connection.pas` 的 socket / stream 两个 client 构造器都已移除 shared compat shim 读取
  - `tests/test_freepascal_context_server_name_inheritance.pas` 已翻成 negative regression：builder/direct context path 都不再把 `ServerName` 自动带进新连接
  - 新增 `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
  - `tests/test_freepascal_context_server_name_inheritance.pas` 已从 intentional-compat label 集合中移除
  - focused RED -> GREEN：
    - `bash tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
    - `tests/test_freepascal_context_server_name_inheritance.pas`
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
- [completed] `TSSLConnectionBuilder` 客户端路径已不再保留 inherited context fallback：
  - `src/fafafa.ssl.connection.builder.pas` 的 `TryBuildClient` 现在在连接支持 `ISSLClientConnection` 且未调用 `WithHostname(...)` 时，会显式 `SetServerName('')`
  - `tests/test_connection_builder_hostname_precedence.pas` 已翻成 no-fallback precedence contract：
    - 未调用 `WithHostname(...)` -> 不再保留 context fallback
    - `WithHostname('conn.example.com')` -> 继续显式覆盖
    - `WithHostname('')` -> 继续显式清空
  - `tests/test_connection_builder_hostname_precedence.pas` 已从 intentional-compat label 集合中移除
  - focused RED -> GREEN：
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- [completed] `TSSLConnector` override precedence 契约已不再依赖 inherited context fallback 输入：
  - `tests/test_tls_connector_hostname_override_precedence.pas` 已移除 mock `Ctx.SetServerName('ctx.example.com')`
  - 新增 `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
  - `tests/test_tls_connector_hostname_override_precedence.pas` 已从 intentional-compat label 集合中移除
  - focused 验证：
    - `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
- [completed] `TSSLConnector` early-data 合同已不再依赖 inherited context fallback 输入：
  - `tests/test_tls_connector_early_data_contract.pas` 已移除 mock `Ctx.SetServerName('ctx.example.com')`
  - 新增 `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - focused 验证：
    - `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - `tests/test_tls_connector_early_data_contract.pas`
- [completed] FreePascal-focused client context-ServerName contracts 已与 live runtime truth 重新对齐：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
    不再错误宣称 FreePascal 新连接会继承 deprecated context-level `ServerName`
  - 它们现在继续覆盖 context state 仍被保留，但 client connection 已明确不再自动继承
  - focused 验证：
    - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - `tests/test_factory_server_name_scope_clarification.pas`
    - `tests/test_factory_config_server_name_isolation.pas`
    - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
- [completed] Shared client fallback divergence 已完成跨 backend 对齐：
  - `src/fafafa.ssl.context.compat.pas` 现在对任意非空 context 都返回 `''`
  - OpenSSL / WolfSSL / MbedTLS / WinSSL 虽然仍走 shared seam，但新 client connection 不再继承 deprecated context-level `ServerName`
  - FreePascal 继续保持早先的 no-inheritance 规则，且不再依赖 shared helper
  - dedicated cross-backend contract:
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - source contract 已同步到当前真相：
    - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
      现在要求 shared helper 只出现在 OpenSSL / WolfSSL / MbedTLS / WinSSL
      并禁止 FreePascal/helper/backend source 重新引入 direct context getter fallback
- [completed] High-level context `ServerName` write surfaces 已完成 `warning + ignore` 收口：
  - `src/fafafa.ssl.context.builder.pas`
    的 `BuildClient` 不再把 `WithSNI(...)` 写回 built client context
  - `src/fafafa.ssl.factory.pas`
    的 client default-config / one-shot `CreateContext(...)` 路径
    不再把 `TSSLConfig.ServerName` 写回新建 context
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
    已翻成 built context `GetServerName = ''` 的新真相
  - focused 验证：
    - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - `tests/test_factory_server_name_compatibility_warning.pas`
    - `tests/config/test_config_validation.pas`
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
- [completed] OpenSSL backend-specific direct library default-config path 已与当前高层真相对齐：
  - `src/fafafa.ssl.openssl.backed.pas`
    的 `TOpenSSLLibrary.CreateContext(...)`
    不再把 `FDefaultConfig.ServerName` 写回新建 client context
  - 同一路径在 server context 下若 default-config 带 `ServerName`，现在会 fail-fast reject
  - direct OpenSSL library path 若配置了 log callback，也会发出 compatibility warning
  - focused 验证：
    - `tests/test_openssl_library_default_config_server_name_clarification.pas`
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
- [completed] final public surface cleanup prep 的第一刀 static classification cleanup 已收口：
  - `tests/test_quick.pas` 不再把 `.WithSNI('example.com')` 当普通 builder smoke 用法
  - `tests/winssl/test_winssl_connection_edge_cases.pas` 不再顺手写无行为意义的 `LConfig.ServerName := ...`
  - 剩余 builder/config compatibility surface 测试现在全部显式带 `INTENTIONAL_COMPAT`
  - 新增 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    守住 deprecated `WithSNI(...)` / `TSSLConfig.ServerName` 只存在于 allowlist compatibility tests
- [completed] final public surface cleanup prep 的第二刀 active direct-context classification cleanup 已收口：
  - active tests 中剩余 real `Ctx.SetServerName(...)` 命中已经全部显式分类
  - 新增 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
    守住 direct-context `SetServerName(...)` 只存在于 allowlist compatibility / API-surface tests
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
    现在都显式带 `INTENTIONAL_COMPAT`
- [completed] intentional direct-context compatibility tests 的 local warning quarantine 已补齐：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    现在对刻意保留的 deprecated context getter/setter 做局部 warning suppression
  - focused compile outputs 已不再额外夹带这些 direct-context deprecation 噪音
- [completed] `WithSNI(...)` compiler-level deprecation alignment 已收口：
  - `src/fafafa.ssl.context.builder.pas`
    的 public `ISSLContextBuilder.WithSNI(...)` 与内部 `TSSLContextBuilderImpl.WithSNI(...)`
    declaration 现在都已经是编译期 `deprecated`
  - 新增 `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    守住源码层 truth，不允许 `WithSNI(...)` 重新退回“只有注释/运行时 warning”的状态
  - 刻意保留 `.WithSNI(...)` 的 compatibility tests 现在都做了局部 warning quarantine，
    避免 focused compile 输出被这条已知 deprecated surface 反复刷屏
- [completed] `TSSLConfig.ServerName` 的 `v1.x` surface truth freeze 已收口：
  - 当前设计决定是不在 `v1.x` 直接移除或改名这个字段，避免破坏现有源码兼容
  - 但 active source/doc truth 现在已经被锁成 compatibility-only：
    - `src/fafafa.ssl.base.pas` 字段注释明确指向 `ISSLClientConnection.SetServerName`
    - generic factory / OpenSSL direct-library warning 明确点名 `TSSLConfig.ServerName`
    - active docs 只允许 `docs/reference/API_REFERENCE.md` 以 compatibility note 形式提及它
  - 新增 `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    守住这条 `v1.x freeze` truth，不允许它重新漂回普通主路径
- [completed] direct `ISSLContext.SetServerName/GetServerName` 的 `v1.x` surface truth freeze 已收口：
  - 当前设计决定是不在 `v1.x` 直接移除这组 deprecated context API，避免破坏现有源码兼容
  - 但它们现在已经被锁成 deprecated compatibility-only surface：
    - `src/fafafa.ssl.base.pas` 的 deprecation message 统一指向 `ISSLClientConnection.Set/GetServerName`
    - production `src/` 已不再存在真实 direct context caller
    - active docs 不再把 `Ctx.SetServerName(...)` 当普通 client 指导路径
  - 新增 `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    守住这条 `v1.x freeze` truth，不允许 direct context guidance 或 production caller 回流
- [completed] `WithSNI(...)` 的 `v1.x` surface truth freeze 已收口：
  - 当前设计决定是不在 `v1.x` 直接移除或改挂这个 fluent method，避免破坏现有源码兼容
  - 但它现在已经被锁成 deprecated compatibility-only fluent surface：
    - `src/fafafa.ssl.context.builder.pas` 保持 compatibility-only comment
    - compiler `deprecated` declaration 已由 dedicated contract 守住
    - active docs 只允许 `docs/reference/API_REFERENCE.md` 提及 `WithSNI(...)`
    - active tests 继续只允许 allowlist compatibility coverage
  - 新增 `tests/scripts/test_withsni_surface_truth_contract.sh`
    守住这条 `v1.x freeze` truth，不允许 `.WithSNI(...)` 重新漂回普通 fluent builder 示例

## Scope

1. 公共 Pascal surface：
   - `src/fafafa.ssl.base.pas`
   - `src/fafafa.ssl.pas`
2. 高层创建/配置路径：
   - `src/fafafa.ssl.factory.pas`
   - `src/fafafa.ssl.context.builder.pas`
3. capability truth：
   - `docs/BACKEND_CAPABILITY_MATRIX.md`
   - `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
   - `src/fafafa.ssl.backend.selector.pas`
   - `src/fafafa.ssl.capability.*`
4. backend 实现：
   - `src/fafafa.ssl.openssl.lib.pas`
   - `src/fafafa.ssl.winssl.lib.pas`
   - `src/fafafa.ssl.freepascal.lib.pas`
   - `src/fafafa.ssl.mbedtls.lib.pas`
   - `src/fafafa.ssl.wolfssl.lib.pas`
5. 验证与合同：
   - `tests/test_capability_matrix_v12.pas`
   - `tests/contract/test_backend_contract.pas`
   - 需要时新增 focused source contract

## Current Queue

1. 进入 final public surface cleanup prep：
   - `TSSLConfig.ServerName` 已冻结为 `v1.x` compatibility-only field
   - direct `ISSLContext.SetServerName/GetServerName` 已冻结为 `v1.x` deprecated compatibility API
   - `WithSNI(...)` 已冻结为 `v1.x` deprecated compatibility-only fluent surface
   - 当前 `context-level SNI` 兼容家族在 `v1.x` 已无新的即时 surface 收口项
2. `TSSLConfig` post-SNI 第一批已经落成 `scope buckets` truth：
   - `docs/plans/2026-05-18-tsslconfig-scope-buckets.md`
   - `src/fafafa.ssl.base.pas` 和 `docs/reference/API_REFERENCE.md` 现在直接写明 mixed-scope buckets：
     - `library-scoped defaults`
     - `context-scoped`
     - `connection-scoped`
     - `compatibility-only`
     - `option-bridge`
   - 新增 `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
     守住 source/doc/factory/OpenSSL direct-path 的 bucket truth
3. `ISSLLibrary.CreateContext(AType)` 的 direct-library default-config parity 已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-direct-library-default-config-parity.md`
   - 新验证：
     - `tests/test_direct_library_default_config_parity.pas`
     - `tests/scripts/test_direct_library_default_config_parity_contract.sh`
   - 当前已对齐的 context-safe 默认字段：
     - `ProtocolVersions`
     - `PreferredVersion`
     - `VerifyMode`
     - `VerifyDepth`
     - `CipherList`
     - `CipherSuites`
     - `Options`
     - `SessionCacheSize`
     - `SessionTimeout`
     - `SessionCacheMode`
     - `ALPNProtocols`
   - `SetDefaultConfig(...)` 也已在 `freepascal` / `winssl` / `mbedtls` / `wolfssl` library units 中补齐 normalization
4. direct-library path 的下一优先级不再是“默认配置应用缺口”，而是尚未统一的剩余专门语义：
   - `ServerName` compatibility warning/reject parity
   - early-data / replay-store direct-library parity
   - 这两类问题仍不适合和 `ISSLConnection` 大手术混成一批
5. 在 direct-library special-case parity 收口后，再决定 broader interface debt 的后续路线：
   - 是否继续推进 `TSSLConfig` option-bridge freeze / slimming
   - 还是进入 `ISSLConnection` 核心 surface slimming roadmap
6. 若未来要让 serializer 对“纯 legacy-only in-memory record”也具备完全无歧义的 projection，需要先为 capability model 补 presence/truth 元信息；当前批次不在无信号状态下瞎猜。

## Verification Discipline

- 默认先做静态审查与 focused contract，不重跑整条重型门禁。
- 只有当修复影响行为语义时，才补最小 Pascal/脚本合同验证。
- 每完成一个可闭环小批次，都要同步：
  - `docs/plans/...`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Risks

- 接口设计问题很多是“结构性债务”，不一定适合一批次全部动生产代码。
- capability truth 问题容易横跨文档、selector、serializer、backend source，多处同修但必须保持最小改动。
- 旧 release/runtime 历史记录很多，必须防止这轮再次被历史 closeout 信息带偏。

## Exit Criteria

- 至少形成一份新的综合审查 plan，明确记录范围、证据源、发现与后续队列。
- 至少完成一轮“公共接口 + 各 backend capability/实现”的横向验证。
- 若发现高价值且边界清晰的问题，则完成最小修复与 focused 验证。
- 给出可复用结论：哪些是已确认问题，哪些是设计债，哪些是下一批应继续推进的最优路径。
