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
- [completed] 两份顶层 core test 也已完成非交互收口：
  - `tests/test_exceptions.pas`
  - `tests/test_base_interface_contract.pas`
  - 新增 `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - 新增 `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
  - 当前这两份测试已不再输出“按回车键退出...”或依赖 `ReadLn`
  - repo-wide `ReadLn` 扫描表明剩余命中主要位于 examples / diagnostic / benchmark / WinSSL 专项程序，不属于这批顶层 core automation 收口范围
- [completed] WinSSL 活跃测试程序也已完成非交互收口：
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_errors_comprehensive.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/winssl/test_winssl_session_management.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_certificate_loading.pas`
  - 新增 `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - 新增 `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
  - `run_winssl_tests.ps1` 的 non-interactive 意图已经与源码重新对齐
  - 剩余 `ReadLn` 命中已主要收缩到 examples / diagnostics / benchmark，而不再是活跃 core/WinSSL 测试主面
- [completed] backend optional public surface 的 focused completion-audit revalidation 已补齐：
  - `tests/contract/test_backend_contract.pas` 当前已实际覆盖：
    - Contract 12: context optional interface alignment
    - Contract 13: context native-handle interface alignment
    - Contract 14: context HTTP hooks interface alignment
    - Contract 15: session native-handle interface alignment
    - Contract 17: certificate-store native-handle interface alignment
    - Contract 18: diagnostics interface alignment
  - 新增 `docs/plans/2026-05-18-backend-optional-surface-completion-audit-revalidation.md`
  - 6 份旧 plan 中原本缺失的 execution result 现已补成 focused revalidation result
  - focused 合同当前结果：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 的上述 optional surface 当前都已有 live contract 证据
  - WinSSL 继续按当前 Linux 主机的既有平台边界保持 skip truth，不误写成已本机证实
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
4. direct-library `ServerName` compatibility parity 也已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-direct-library-servername-compatibility-parity.md`
   - 新验证：
     - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
     - `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
   - 当前 direct-library path 已对齐：
     - client default-config = warning + ignore
     - server default-config = reject
   - 这条规则现在已在 `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl` library units 上保持同一条 source truth
5. direct-library `early-data / replay-store` parity 也已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-direct-library-early-data-replay-store-parity.md`
   - 新验证：
     - `tests/test_direct_library_early_data_replay_store_parity.pas`
     - `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
   - 当前 direct-library path 已对齐：
     - `ClientEarlyDataEnabled`
     - `ServerEarlyDataPolicy`
     - `ServerMaxEarlyDataSize`
     - `ServerEarlyDataReplayStoreFile`
     - `ServerEarlyDataReplayStoreDirectory`
   - replay-store 语义现在也与 factory/context path 同步：
     - client path = reject
     - server file/directory = mutually exclusive
     - backend 不实现 installer seam = fail-fast
   - 这条规则现在已通过 shared helper 固定在
     `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
     的 library `CreateContext(AType)` 路径上
6. direct-library special-case parity 当前已全部收口，下一条不该再回到这条线：
   - 这类问题已经不需要和 `ISSLConnection` 大手术混成一批
7. 在 direct-library special-case parity 收口后，再决定 broader interface debt 的后续路线：
   - 是否继续推进 `TSSLConfig` option-bridge freeze / slimming
   - 还是进入 `ISSLConnection` 核心 surface slimming roadmap
8. 若未来要让 serializer 对“纯 legacy-only in-memory record”也具备完全无歧义的 projection，需要先为 capability model 补 presence/truth 元信息；当前批次不在无信号状态下瞎猜。
9. `TSSLConfig option-bridge default truth parity` 当前也已完成第一轮收口：
   - 新 plan：
     - `docs/plans/2026-05-18-tsslconfig-option-bridge-default-truth-parity.md`
   - 新验证：
     - `tests/test_tsslconfig_option_bridge_default_truth.pas`
     - `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
   - 当前已对齐的 fresh default-config surfaces：
     - factory-held `ISSLLibrary.GetDefaultConfig(...)`
     - `CreateDefaultConfig(...)`
     - `Lib.SetDefaultConfig(Lib.GetDefaultConfig)` round-trip
   - 当前已确认的真实根因：
     - `factory` 对真实 backend 仍走 raw registered-class instantiation
     - 这条路径会丢失 backend constructor 内部建立的 `FDefaultConfig` 真相
     - 因而问题不只是 “constructor normalization 不够”，而是 “生产实例化路径本身不保真”
   - 当前修法：
     - `TSSLFactory` 增加 explicit creator-function registration path
     - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
       真实 backend 注册统一改走 `Create*SSLLibrary(...)`
   - 下一条相关路线不该再回到这个 fresh default-config surface：
     - 若继续推进，应讨论 `Options vs legacy booleans` 的 broader precedence/slimming 规则
     - 而不是重新怀疑 `CreateDefaultConfig(...)` 单点
10. `TSSLConfig option-bridge precedence freeze` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-option-bridge-precedence-freeze.md`
    - 新验证：
      - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
      - `tests/scripts/test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
    - 当前已冻结的 `v1.x` truth：
      - legacy booleans 仍是 compatibility write surface
      - 当 `Options` 与 legacy booleans 冲突时，legacy booleans 赢
      - normalization 会先把 legacy booleans 写进 `Options`
      - 再把最终 `Options` truth 回投到 legacy booleans
    - 当前 production proof 已覆盖：
      - `TSSLFactory.NormalizeConfig(...)`
      - `TSSLFactory.CreateContext(const AConfig)`
      - `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`
    - 下一条相关路线不该再回到“冲突输入到底谁赢”的讨论：
      - 若继续推进，应进入真正的 `TSSLConfig` public-surface slimming / migration 设计
      - 而不是再把 precedence 当成未定规则
11. `TSSLConfig option-bridge surface truth freeze` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-option-bridge-surface-truth-freeze.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_option_bridge_surface_truth_contract.sh`
      - `tests/test_tsslconfig_option_bridge_default_truth.pas`
      - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
      - `tests/security/test_session_security.pas`
    - 当前已冻结的 `v1.x` public truth：
      - `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling`
        是 compatibility-only option-bridge booleans
      - 新代码应优先直接写 `Options`
      - 仍需覆盖这些字段的测试必须显式标记为 compatibility coverage
      - 非 compatibility 活跃测试不应再把它们当主写入口
    - 当前 focused proof 已覆盖：
      - source comment / API reference wording
      - dedicated compatibility tests label truth
      - active session-security coverage 改走 context `SetOptions(...)` / `GetOptions(...)`
    - 下一条相关路线不该再回到“这些字段是不是普通主路径”的讨论：
      - 若继续推进，应进入真正的 `TSSLConfig` slimming / migration design
      - 而不是重复补 public wording 或兼容测试标签
12. `TSSLConfig active guidance cleanup` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-active-guidance-cleanup.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
      - `examples/example_factory_usage.pas` focused compile
    - 当前已收口的 active guidance 漂移：
      - 活跃 example 不再把 `BufferSize` / `HandshakeTimeout` 教成 factory/config 主路径
      - `docs/reference/ARCHITECTURE.md` 不再描述过时的伪 `TSSLConfig` 结构
      - `tests/examples/test_lib_core_functionality.pas` 的 direct context `SetServerName(...)` example-surface coverage 继续显式带 `INTENTIONAL_API_SURFACE`
    - 下一条相关路线不该再回到高可见度 guidance cleanup：
      - 若继续推进，应进入真正的 `TSSLConfig` public-surface slimming / migration design
      - 而不是继续修 example/reference 漂移
13. `TSSLConfig public-surface slimming roadmap` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
    - 当前已固定的字段级迁移决策：
      - `LogLevel` / `LogCallback` -> library defaults surface
      - `HandshakeTimeout` / `BufferSize` -> connection / transport surface
      - `ServerName` -> per-connection SNI surface
      - `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling` -> `Options` / `WithOption(...)`
      - context-safe 字段继续留在 `TSSLConfig` 主路径
    - 下一条相关路线不该再回到“先补一份 migration map”：
      - 若继续推进，应在上述 buckets 中挑第一条最小实现切片
      - 当前最优先候选是 `LogLevel` / `LogCallback` 的 library-default detachment
14. `TSSLConfig logging surface truth freeze` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-tsslconfig-logging-surface-truth-freeze.md`
    - 新验证：
      - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
      - `tests/test_factory_logging_scope_clarification.pas`
      - `tests/config/test_default_config.pas`
    - 当前已收口的真实 drift：
      - `docs/guides/USER_GUIDE.md`
      - `docs/guides/TROUBLESHOOTING.md`
        不再把“只调用 `ISSLLibrary.SetLogCallback(...)`”教成足以看到 `sslLogInfo` / `sslLogDebug` 输出的完整配置
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/ARCHITECTURE.md`
        现在明确拆开：
        - `LogLevel` 走 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)`
        - `LogCallback` 走 `ISSLLibrary.SetLogCallback(...)`
        - fresh/request config 仍回到 `sslLogError` + `nil` baseline
    - 当前 focused proof 已覆盖：
      - 新 docs contract 先 RED 后 GREEN，直接证明活跃 guidance 曾经和 runtime truth 冲突
      - 既有 Pascal logging 回归继续保持绿色，说明这次收口只修 guidance truth，没有扰动 runtime/source contract
    - 下一条相关路线不该再回到 logging guidance 漂移：
      - 若继续沿 `TSSLConfig` buckets 推进，应优先寻找新的 live bug 信号
      - 不要再把 `LogLevel` / `LogCallback` 的 active docs truth 当成未收口问题反复拉起
15. `direct-library connection-scope clarification` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-direct-library-connection-scope-clarification.md`
    - 新验证：
      - `tests/scripts/test_direct_library_connection_scope_clarification_contract.sh`
      - `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
      - `tests/test_factory_connection_scope_clarification.pas`
    - 当前已收口的真实 drift：
      - `ISSLLibrary.SetDefaultConfig(...)` 之前可以保存自定义 `HandshakeTimeout` / `BufferSize`
      - 五个 backend 的 `CreateContext(AType)` 又不会消费这两个 connection-scoped 字段
      - 因而 direct-library path 曾经留下了“default-config 可写、CreateContext 静默忽略”的假可用入口
    - 当前修法：
      - 在 `src/fafafa.ssl.context.config.pas` 新增 shared `ValidateDirectLibraryConnectionScope(...)`
      - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
        的 library `CreateContext(AType)` 统一接入这条 helper
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/ARCHITECTURE.md`
        也同步改成 direct-library path reject 这两个字段
    - 当前 focused proof 已覆盖：
      - 新 contract 先 RED 后 GREEN，直接证明 docs/source 曾经没有把 direct-library connection-scope truth 说清楚
      - 新 FreePascal direct-library runtime test 先 RED 后 GREEN，直接证明生产路径从 silent accept 变成 fail-fast reject
      - 既有 factory connection-scope 回归继续绿色，说明 shared helper 没扰动原有 factory truth
    - 下一条相关路线不该再回到 direct-library `HandshakeTimeout` / `BufferSize` 漂移：
      - 后续应继续找新的 live interface/implementation gap
      - 不要再把 direct-library connection-scope 静默忽略当成未收口问题反复拉起
16. `library-default LogCallback detachment` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-library-default-logcallback-detachment.md`
    - 新验证：
      - `tests/scripts/test_library_default_logcallback_detachment_contract.sh`
      - `tests/test_factory_logging_scope_clarification.pas`
      - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
      - `tests/test_openssl_library_default_config_server_name_clarification.pas`
      - `tests/config/test_default_config.pas`
      - `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
      - `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
      - `tests/scripts/test_tsslconfig_migration_targets_contract.sh`
    - 当前已收口的真实 drift：
      - public truth 已经把 callback owner 收到 `ISSLLibrary.SetLogCallback(...)`
      - 但五个 backend 的 `SetDefaultConfig(...)` 之前仍会直接把 `LConfig.LogCallback` 装进 runtime `FLogCallback`
      - 结果就是 `LogCallback` 同时挂在 default-config path 和 dedicated setter path 上，owner 不单一
    - 当前修法：
      - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
        的 `SetDefaultConfig(...)` 现在只继续更新 `LogLevel` 和其他 default-config 字段
      - runtime callback 改为只由 `SetLogCallback(...)` 维护
      - `GetDefaultConfig(...)` 仍然镜像当前 callback 真相，但 `SetDefaultConfig(...)` 不再安装或替换它
      - `docs/reference/API_REFERENCE.md`
      - `docs/reference/ARCHITECTURE.md`
      - `src/fafafa.ssl.base.pas`
        也同步写明这条 detachment truth
    - 当前 focused proof 已覆盖：
      - 新 source contract 先 RED 后 GREEN，直接证明 5 个 backend 曾经都还让 `SetDefaultConfig(...)` 安装 callback
      - 强化后的 logging runtime 回归先 RED 后 GREEN，直接证明：
        - `SetDefaultConfig(LogCallback)` 不再安装 callback
        - `SetLogCallback(...)` 仍是唯一 owner
        - 后续 `SetDefaultConfig(LogLevel)` 不会顺手清掉已安装 callback
      - 受影响的 direct-library `ServerName` warning 测试继续绿色，说明这次 detachment 没把已有 warning/reject 路线带歪
      - default-config / docs / scope-bucket / migration-targets focused contracts 继续绿色
    - 下一条相关路线不该再回到 `LogCallback` owner 模糊地带：
      - `LogLevel` / `LogCallback` 这条线当前已从 docs freeze 进入 runtime/source truth
      - 后续应继续找新的 live interface/implementation gap，而不是再把 callback default-config owner 当成未收口问题反复拉起
17. `noninteractive core compat tests` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-noninteractive-core-compat-tests.md`
    - 新验证：
      - `tests/test_factory_logic.pas`
      - `tests/test_data_structures.pas`
    - 当前已收口的真实问题：
      - 这两份核心 `TSSLConfig` record-shape / compatibility 测试此前虽然能跑通，
        但末尾仍保留：
        - `WriteLn('按回车键退出...')`
        - `ReadLn`
      - 结果就是它们继续表现得像“手工演示程序”，而不是直接适合自动化执行的测试
    - 当前修法：
      - 移除两份文件末尾的交互式退出逻辑
      - 头部 `INTENTIONAL_COMPAT` 注释同步补清：
        - deprecated `ServerName`
        - option-bridge booleans
        - mixed-scope record-shape fields（`BufferSize` / `HandshakeTimeout`）
    - 当前 focused proof 已覆盖：
      - 修复前 direct run 输出会以“按回车键退出...”收尾
      - 修复后两份测试都可直接 `timeout 2 ./...` 跑完，且输出不再留下交互式退出尾巴
    - 下一条相关路线不该再回到这两份 core test 的交互尾巴：
      - 它们当前已可作为自动化测试程序直接执行
      - 后续应继续找新的 live interface/implementation gap，而不是再把这两份文件的手工退出逻辑当成未收口问题反复拉起
18. `top-level core tests noninteractive` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-noninteractive-top-level-core-tests.md`
    - 新验证：
      - `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
      - `tests/test_exceptions.pas`
      - `tests/test_base_interface_contract.pas`
    - 当前已收口的真实问题：
      - 这两份顶层 core test 在当前 headless shell 下虽然会因 stdin EOF 直接退出，
        但源码末尾仍保留：
        - `WriteLn('按回车键退出...')`
        - `ReadLn`
      - 结果就是自动化输出会持续带着手工演示尾巴，且退出行为依赖运行方式
    - 当前修法：
      - 移除两份文件末尾的交互式退出逻辑
      - 新增 focused shell contract，禁止这两份文件重新带回交互尾巴
    - 当前 focused proof 已覆盖：
      - 新合同先 RED，直接命中 `tests/test_exceptions.pas` 的残余 `ReadLn`
      - 修复后新合同 GREEN
      - 两份测试都可直接 `timeout 2 ./...` 跑完，且输出尾部只保留测试总结
    - 下一条相关路线不该再回到这两份顶层 core test 的交互尾巴：
      - 这条线现在已经有 source contract 护栏
      - 若继续清理 `ReadLn` 残留，应优先按 `top-level test -> WinSSL specialized test -> examples/diagnostics` 分层，而不是重新混做一批
19. `WinSSL active tests noninteractive` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-noninteractive-winssl-active-tests.md`
    - 新验证：
      - `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
      - `run_winssl_tests.ps1`
      - `tests/unit/test_winssl_comprehensive.pas`
      - `tests/winssl/test_winssl_context_comprehensive.pas`
      - `tests/winssl/test_winssl_errors_comprehensive.pas`
      - `tests/winssl/test_winssl_monitoring.pas`
      - `tests/winssl/test_winssl_connection_edge_cases.pas`
      - `tests/winssl/test_winssl_certstore.pas`
      - `tests/winssl/test_winssl_session_management.pas`
      - `tests/winssl/test_winssl_library_basic.pas`
      - `tests/winssl/test_winssl_certificate_loading.pas`
    - 当前已收口的真实问题：
      - 这批文件虽然属于活跃 WinSSL 测试程序，并且仍被脚本/验证清单引用，
        但源码末尾仍保留：
        - `WriteLn('按回车键退出...')`
        - `WriteLn('Press Enter to exit...')`
        - `ReadLn`
      - 其中 `run_winssl_tests.ps1` 甚至明确把 `tests/unit/test_winssl_comprehensive.pas`
        归类为 `Minimal, non-network, non-interactive tests`
    - 当前修法：
      - 移除这批 WinSSL 活跃测试程序的交互式退出逻辑
      - 新增 focused source contract，禁止这些文件重新带回交互尾巴
      - 不混入 examples / diagnostics / benchmark
    - 当前 focused proof 已覆盖：
      - 新合同先 RED，直接命中 `tests/unit/test_winssl_comprehensive.pas`
      - 修复后新合同 GREEN
      - `tests/unit/test_winssl_comprehensive.pas` 的 Linux 非 Windows 分支可直接编译运行，输出不再带手工退出提示
      - `tests/unit/test_winssl_comprehensive.pas`
      - `tests/winssl/test_winssl_session_management.pas`
        的 Win64 交叉编译都已通过，说明这次尾部清理没有破坏 Windows 语法面
    - 下一条相关路线不该再回到 WinSSL 活跃测试程序的交互尾巴：
      - 这条线现在已有 focused contract 护栏
      - 若继续清理 `ReadLn` 残留，只应处理 examples / diagnostics / benchmark 等明确非活跃测试面
      - 更高优先级则应回到 broader interface debt，而不是继续沉在已收口的 active test prompt cleanup
20. `backend optional-surface completion-audit revalidation` 当前也已完成第一轮收口：
    - 新 plan：
      - `docs/plans/2026-05-18-backend-optional-surface-completion-audit-revalidation.md`
    - 新验证：
      - `tests/contract/test_backend_contract.pas`
      - `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
      - `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
      - `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
      - `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
      - `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
      - `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`
    - 当前已收口的真实问题：
      - 上述 6 份 plan 文档虽然对应的 contract 已经实际存在于 `tests/contract/test_backend_contract.pas`
      - 但文档本身仍缺 execution result，容易让后续会话误判这些 optional public surface 还没真的验证过
    - 当前修法：
      - focused 重新编译并运行 `tests/contract/test_backend_contract.pas`
      - 把 contracts 12-18 的现状证据回写到缺结果的 plan 文档
      - 明确标成 `Focused Revalidation Result (2026-05-18)`，不虚报未重跑的重门禁
    - 当前 focused proof 已覆盖：
      - `tests/contract/test_backend_contract.pas` 当前结果：
        - `Total Tests: 135`
        - `Passed: 111`
        - `Failed: 0`
        - `Skipped: 24`
      - OpenSSL / WolfSSL / MbedTLS / FreePascal 的 context optional/native-handle、HTTP hooks、session native-handle、certificate-store native-handle、diagnostics surface 全部 PASS
      - WinSSL 继续按 Linux 主机平台边界 SKIP；`Contract 15` 也继续明确 session truth 需要 dedicated Windows batch
    - 下一条相关路线不该再回到“这些 optional surface 可能还没验证过”的怀疑：
      - 当前缺口已经从“缺 contract/缺结果”收成“已有 focused live proof”
    - 更高优先级应回到 broader interface debt：
        - `TSSLConfig` public-surface slimming 后续
        - `ISSLConnection` 核心 surface slimming / completion audit
21. `ISSLConnection surface truth freeze` 现在应作为当前默认主线：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnection-surface-truth-freeze.md`
    - 当前已确认的工作流偏差：
      - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md` 仍把 `TSSLConfig` 写成默认 immediate next step
      - 但仓库当前更急的误导源其实是 `docs/reference/API_REFERENCE.md`
        中 `ISSLConnection` / `ISSLSession` active docs 与源码真相漂移
    - 当前批的目标：
      - 先冻结活跃文档真相，不直接修改 public signature
      - 把 `ISSLConnection` 的 compatibility-core mirrors 与 optional owner 说明写清楚
      - 新增 focused contract，阻止旧方法名再次回流到 active docs
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnection_surface_truth_contract.sh`
      - `bash tests/scripts/test_isslconnection_surface_truth_contract.sh`
      - `git diff --check`
      - 当前结果均为 PASS，说明这批已经在文档/contract 层完成收口
    - 当前批收口后，下一步才适合从稳定真相上选择第一条真正的 slimming slice：
      - `ISSLConnection` compatibility-core slimming
      - 或回到 `TSSLConfig` 的更小实现切片
22. `backend connection-surface completion-audit revalidation` 当前也应补齐：
    - 新 plan：
      - `docs/plans/2026-05-18-backend-connection-surface-completion-audit-revalidation.md`
    - 当前重新核对后确认的事实：
      - `ISSLConnectionInfo` / `ISSLSessionResumption` / `ISSLCertificateVerification`
        这些连接层 optional surface 已经有 execution result
      - 真正缺当前 execution receipt 的，是另外 3 份仍直接落在 `ISSLConnection` 主面上的旧计划：
        - `docs/plans/2026-05-04-backend-client-connection-sni-interface-alignment.md`
        - `docs/plans/2026-05-04-backend-connection-native-handle-interface-alignment.md`
        - `docs/plans/2026-05-04-backend-ocsp-connection-interface-alignment.md`
    - 当前修法：
      - focused 重新编译并运行 `tests/contract/test_backend_contract.pas`
      - 仅把 Contracts 8 / 10 / 11 的当前 live 结果回写到上述 3 份 plan
      - 不混入新的生产代码变更，也不虚报未重跑的重门禁
    - 当前 focused proof：
      - `tests/contract/test_backend_contract.pas` 当前结果仍为：
        - `Total Tests: 135`
        - `Passed: 111`
        - `Failed: 0`
        - `Skipped: 24`
      - `Contract 8`：
        - OpenSSL / WolfSSL / MbedTLS / FreePascal PASS
        - WinSSL SKIP
      - `Contract 10`：
        - OpenSSL / WolfSSL / FreePascal non-stub PASS
        - MbedTLS absent PASS
        - WinSSL SKIP
      - `Contract 11`：
        - OpenSSL / WolfSSL / MbedTLS native-handle PASS
        - FreePascal absent PASS
        - WinSSL SKIP
    - 当前批收口后，连接层历史 execution receipt 的主要缺口将被清空
    - 下一条应优先进入真正的 `ISSLConnection` slimming，而不是继续补旧计划结果
23. `ISSLConnectionInfo mirror demotion / migration-map` 现在应作为下一条 design 主线：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnectioninfo-mirror-demotion-migration-map.md`
    - 当前重新核对后确认的设计 drift：
      - `docs/reference/INTERFACE_DESIGN_V2.md` 仍漏掉 `ISSLConnectionInfo`
      - 仍保留 `ISSLAdvanced` 这个当前无实际落点的空壳名
      - `TBaseSSLConnection` 示例没列出 `ISSLConnectionInfo`
      - 迁移对照表把 `GetConnectionInfo` 错归给 `ISSLDiagnostics`
      - 还过早把 `GetStateString` / `GetContext` / `GetSelectedALPNProtocol` 直接写死到其它路线
    - 当前修法：
      - 在 `INTERFACE_DESIGN_V2.md` 中补出 `ISSLConnectionInfo`
      - 把 `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString`
        的 Stage-A demotion target 统一写成 `ISSLConnectionInfo`
      - 新增 focused contract，禁止错误 owner / `ISSLAdvanced` 回流
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`ISSLConnection` 真正剩下的问题会更聚焦到 source-facing slimming prep
24. `ISSLConnectionInfo active guidance de-emphasis` 现在应作为紧随其后的用户面收口：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnectioninfo-active-guidance-deemphasis.md`
    - 当前 active-doc drift：
      - `API_REFERENCE.md` 仍直接示例 `LConn.GetConnectionInfo` / `LConn.GetSelectedALPNProtocol` / `LConn.GetStateString`
      - `INTEGRATION_GUIDE.md` 也仍把 `Conn.GetSelectedALPNProtocol` / `Conn.GetStateString` 当推荐排错路径
    - 当前修法：
      - 把这组用户可见示例改成先 `Supports(..., ISSLConnectionInfo, ...)`
      - 新增 focused contract，防止 active guidance 回流到 direct core mirror teaching
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，用户可见路径会开始和 `ISSLConnectionInfo` 的 Stage-A demotion map 真正同向
25. `ISSLConnectionInfo source classification freeze` 现在应作为 source-facing slimming prep：
    - 新 plan：
      - `docs/plans/2026-05-18-isslconnectioninfo-source-classification-freeze.md`
    - 当前 source-facing 缺口：
      - 设计文档和 active docs 已经写明 Stage-A demotion map
      - 但 `src/fafafa.ssl.base.pas` / `src/fafafa.ssl.connection.base.pas` 还没明确写出
        这 4 个 mirrors 当前是 `compatibility-core duplicates`
    - 当前修法：
      - 在 source comments 中补出 `GetConnectionInfo` / `GetContext` /
        `GetSelectedALPNProtocol` / `GetStateString` 的 Stage-A classification note
      - 新增 focused source contract，防止 source-facing truth 再次回流丢失
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`ISSLConnection` 主线会更接近第一条真正的实现切片
26. `GetContext active guidance de-emphasis` 现在应作为第一条 mirror-specific route selection prep：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-active-guidance-deemphasis.md`
    - 当前 residual drift：
      - `docs/CAPABILITY_MATRIX_GUIDE.md` 仍直接示例 `Conn.GetContext.GetLibrary.GetCapabilities`
      - `API_REFERENCE.md` 的优先路径说明还没把 `GetContext` 明确并入 `ISSLConnectionInfo` first guidance
    - 当前修法：
      - 把 capability 示例改成先 `Supports(..., ISSLConnectionInfo, ConnInfo)` 再用 `ConnInfo.GetContext`
      - 新增 focused contract，防止活跃文档把 core `GetContext` 教回推荐路径
      - 在路线图中把 `GetContext` 固定成当前第一优先 mirror
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，下一刀就可以直接进入 `GetContext` 的 source/class split feasibility
27. `GetContext contract owner primacy` 现在应作为第一条测试层真实收窄：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-contract-owner-primacy.md`
    - 当前 residual coupling：
      - `tests/contract/test_backend_contract.pas` 仍把 `ISSLConnection.GetContext` 和
        `ISSLConnectionInfo.GetContext` 写成并列 owner
      - 失败文案也仍然是双 owner 叙事，不利于后续真正讨论 `GetContext` 离开 core 的路线
    - 当前修法：
      - 先验证 `ISSLConnectionInfo.GetContext` 与创建 context type 一致
      - 再把 `ISSLConnection.GetContext` 降为 mirror-equality proof
      - 新增 focused source guard，防止 contract 语义回流
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
      - focused contract 当前 PASS
    - 当前批收口后，下一刀就可以直接进入 `GetContext` 的更强 feasibility / deprecation 讨论
28. `GetContext source/class split feasibility freeze` 现在应作为第一条实现切片前的 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-source-class-split-feasibility-freeze.md`
    - 当前 remaining surface：
      - 生产源码里只剩接口声明与 `TBaseSSLConnection.GetContext` 共享实现
      - 活跃文档只剩 `ConnInfo.GetContext`
      - direct core `LConn.GetContext` 只剩 `tests/contract/test_backend_contract.pas` 的 mirror proof
    - 当前修法：
      - 在 source comments 中补 `GetContext` 的 preferred-access / owner / mirror 语义
      - 新增 focused allowlist contract，守住当前 remaining live surface
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetContext` 就不再需要继续做 evidence cleanup，可以决定是进入 public deprecation wording 还是切到下一条 mirror
29. `GetStateString active test de-emphasis` 现在应作为下一条 mirror 的第一刀：
    - 新 plan：
      - `docs/plans/2026-05-18-getstatestring-active-test-deemphasis.md`
    - 当前 high-value residual：
      - `tests/connection/test_connection_basic.pas` 仍直接调用 `LConnection.GetStateString`
      - `tests/integration/test_real_https_connection.pas` 仍把 `Conn.GetStateString` 用作普通握手失败输出
    - 当前修法：
      - 把 generic/integration 测试切到 `ISSLConnectionInfo.GetStateString`
      - 新增 focused contract，防止普通测试路径把 direct core `GetStateString` 教回去
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
      - `mkdir -p tmp/test_connection_basic && fpc -B -Fu./src -Fu./tests -FUtmp/test_connection_basic -FEtmp/test_connection_basic -otmp/test_connection_basic/test_connection_basic tests/connection/test_connection_basic.pas && ./tmp/test_connection_basic/test_connection_basic`
      - `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
      - `git diff --check`
      - focused contract 当前 PASS
    - 当前批收口后，下一刀就可以决定是收 residual runtime uses，还是切到 `GetSelectedALPNProtocol`
30. `GetStateString residual classification freeze` 现在应作为 active-test 之后的 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getstatestring-residual-classification-freeze.md`
    - 当前 remaining surface：
      - direct core `GetStateString` 已从 ordinary docs/tests 退出
      - 当前 residual 只剩 backend contract mirror proof 与 OpenSSL / WolfSSL backend-specific runtime files
    - 当前修法：
      - 在 source comments 中补 `GetStateString` 的 preferred-access / owner / residual-surface 说明
      - 新增 focused allowlist contract，守住 direct core residual file set
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetStateString` 就不再需要继续做 evidence cleanup，可以决定是进入更强 deprecation wording 还是切到 `GetSelectedALPNProtocol`
31. `GetSelectedALPNProtocol active test de-emphasis` 现在应作为下一条 mirror 的第一刀：
    - 新 plan：
      - `docs/plans/2026-05-18-getselectedalpn-active-test-deemphasis.md`
    - 当前 high-value residual：
      - `tests/integration/test_real_https_connection.pas` 仍直接调用 `Conn.GetSelectedALPNProtocol`
      - `tests/integration/test_cross_backend_consistency_contract.pas` 仍把 `Conn.GetSelectedALPNProtocol` 当归一化 ALPN 探测输出
    - 当前修法：
      - 在这两个 ordinary integration/contract 文件里补 `ISSLConnectionInfo`-first helper
      - 新增 focused contract，防止普通测试路径把 direct core `GetSelectedALPNProtocol` 教回去
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
      - `mkdir -p tmp/test_real_https_connection && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_real_https_connection -FEtmp/test_real_https_connection -otmp/test_real_https_connection/test_real_https_connection tests/integration/test_real_https_connection.pas && ./tmp/test_real_https_connection/test_real_https_connection`
      - `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
      - `git diff --check`
      - focused contract 当前 PASS
    - 当前批收口后，下一刀就可以决定是收 residual runtime uses，还是进入更强 client-owner / deprecation wording 讨论
32. `GetSelectedALPNProtocol residual classification freeze` 现在应作为 active-test 之后的 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getselectedalpn-residual-classification-freeze.md`
    - 当前 remaining surface：
      - direct core `GetSelectedALPNProtocol` 已从 ordinary docs/tests 退出
      - 当前 residual 只剩 backend contract mirror proof、MbedTLS backend-specific runtime test 与 WinSSL backend-specific runtime tests
    - 当前修法：
      - 在 source comments 中补 `GetSelectedALPNProtocol` 的 preferred-access / owner / residual-surface 说明
      - 新增 focused allowlist contract，守住 direct core residual file set
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetSelectedALPNProtocol` 就不再需要继续做 evidence cleanup，可以决定是进入更强 client-owner / deprecation wording，还是切到 `GetConnectionInfo`
33. `GetConnectionInfo residual classification freeze` 现在应作为这组 mirrors 的最后一条 allowlist 固化：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-residual-classification-freeze.md`
    - 当前 remaining surface：
      - direct core `GetConnectionInfo` 已从 active docs 与 ordinary tests 退出
      - 当前 residual 只剩 backend contract mirror proof、OpenSSL backend-specific connection-info contract test 与 WinSSL backend-specific runtime/edge-case tests
    - 当前修法：
      - 在 source comments 中补 `GetConnectionInfo` 的 preferred-access / owner / residual-surface 说明
      - 新增 focused allowlist contract，守住 direct core residual file set
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
      - 新 contract 当前 PASS
    - 当前批收口后，`GetConnectionInfo` 也不再需要继续做 evidence cleanup，`ISSLConnectionInfo` 这 4 条 Stage-A mirror 路线将全部进入 post-freeze 决策阶段
34. `GetConnectionInfo base enrichment from residual audit` 已完成并应作为当前默认下一步的完成记录保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-base-enrichment-from-residual-audit.md`
    - 当前已确认的共享层 completeness 修复：
      - `TBaseSSLConnection.GetConnectionInfo` 现在会统一补齐 `ServerName`
      - `SessionId` 现在会在 `FConnected or FHandshakeComplete` 且后端可返回当前 session 时补齐
      - OpenSSL / FreePascal / MbedTLS / WolfSSL / WinSSL 已通过 `DoGetConnectionInfoServerName` hook 暴露各自连接对象持有的 `FServerName`
    - 当前根因与实现约束：
      - 不应在 `TBaseSSLConnection.GetConnectionInfo` 对 `Self` 走 `Supports(Self, ISSLClientConnection, ...)`
      - 具体类直接以 object ref 使用时，这种临时 interface ref 在 `TInterfacedObject` 路径上可能触发错误的自释放
      - 因此本批使用 protected virtual hook，而不是 shared base 里的 interface cast
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` 线上的高优先级下一步不再是 residual archaeology，而是剩余 completeness debt：
      - `PeerCertificate`
      - `CipherSuiteId` / `KeyExchange` / `Cipher` / `Hash` / `KeySize` / `MacSize`
      - 更强 owner / deprecation wording route
35. `GetConnectionInfo` shared `PeerCertificate` enrichment 已完成并应作为当前 implementation-completeness 主线的继续收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-peercertificate-base-enrichment.md`
    - 当前已确认的共享层 completeness 修复：
      - `TBaseSSLConnection.GetConnectionInfo` 现在会在连接可暴露当前对端证书时统一补齐 `PeerCertificate`
      - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 的既有 `DoGetPeerCertificate` / `ISSLCertificate.GetInfo` 能力现在都能被共享层折进 `TSSLConnectionInfo`
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` 线上真正剩下的 completeness debt 已进一步收缩到：
      - `CipherSuiteId`
      - `KeyExchange`
      - `Cipher`
      - `Hash`
      - `KeySize`
      - `MacSize`
      - 更强 owner / deprecation wording route
36. `GetConnectionInfo` crypto detail name-derived first slice 已完成并应作为当前 shared/detail 分层路线的完成记录保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-crypto-detail-name-derived-first-slice.md`
    - 当前已确认的共享层 completeness 修复：
      - shared `GetConnectionInfo` 现在会基于 negotiated `CipherSuite` 名称 best-effort 推导：
        - `Cipher`
        - `Hash`
        - `KeySize`
      - 当 cipher-suite name 显式携带 legacy key-exchange 前缀时，也会 best-effort 推导：
        - `KeyExchange`
    - 当前 static audit 结论：
      - `CipherSuiteId` / `MacSize` 仍主要属于 backend/platform-specific detail
      - `Cipher` / `Hash` / `KeySize` 更适合先走 shared name-derived normalization
      - WinSSL 继续保留自己的 override，不依赖 shared parser
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - `CipherSuiteId`
      - `MacSize`
      - 无法只靠名字稳定推导的更细平台差异
      - 更强 owner / deprecation wording route
37. `GetConnectionInfo` `CipherSuiteId` first slice 已完成并应作为当前 implementation-completeness 主线的继续收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-ciphersuiteid-first-slice.md`
    - 当前已确认的 shared + backend truth：
      - shared `GetConnectionInfo` 现在会对标准 TLS 1.3 cipher-suite name best-effort 推导：
        - `CipherSuiteId`
      - OpenSSL `GetConnectionInfo` 现在会优先走：
        - `SSL_CIPHER_get_protocol_id`
      - 若该 helper 不可用，则会回退：
        - `SSL_CIPHER_get_id and $FFFF`
    - 当前 focused proof 已覆盖：
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - `MacSize`
      - 无法只靠名字或统一 low-level helper 稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
38. WinSSL `GetConnectionInfo` cipher truth correction 已完成并应作为当前 WinSSL-specific 审查纠偏记录保留：
    - 新 plan：
      - `docs/plans/2026-05-18-winssl-connectioninfo-cipher-truth-correction.md`
    - 当前已确认的 WinSSL truth:
      - `SecPkgContext_ConnectionInfo.aiCipher`
        - 只是算法级字段
        - 不应直接写入 `CipherSuiteId`
      - WinSSL `CipherSuiteId` 现在会优先走：
        - `SECPKG_ATTR_CIPHER_INFO`
        - `dwCipherSuite`
      - 当 Schannel 可返回真实 suite name 时：
        - `DoGetCipherName` / `GetConnectionInfo.CipherSuite` 会优先对齐该 truth
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - GitHub Actions `Wave B B2 Manual Gate (Template)` run `26019296095`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线更准确地收缩到：
      - `MacSize`
      - 无法只靠名字或统一 low-level helper 稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
39. `GetConnectionInfo` `MacSize` semantics matrix 已完成并应作为当前 implementation-completeness 主线的下一条 bounded 收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-macsize-semantics-matrix.md`
    - 当前已确认的 shared + backend truth：
      - shared `GetConnectionInfo` 现在会对可识别 AEAD suite name best-effort 推导：
        - `...GCM` / `...POLY1305` / `...OCB` / `...CCM` -> `MacSize = 16`
        - `...CCM_8` -> `MacSize = 8`
      - OpenSSL / FreePascal / MbedTLS / WolfSSL 当前都已通过 shared path 吃到这组统一 truth
      - WinSSL `GetConnectionInfo` 现在会先走 inherited shared path
      - WinSSL 只有在 shared path 仍未给出稳定值时，才回退：
        - `ConnInfo.dwHashStrength div 8`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_winssl_connectioninfo_cipher_truth_contract.sh`
      - `bash tests/scripts/test_winssl_connectioninfo_macsize_semantics_contract.sh`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - legacy non-AEAD `MacSize` 是否值得补更强 low-level truth
      - 无法只靠 shared suite-name 路径稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
40. `OpenSSL GetConnectionInfo legacy MacSize truth` 已完成并应作为当前 implementation-completeness 主线的进一步收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-openssl-connectioninfo-macsize-legacy-truth-feasibility.md`
    - 当前已确认的 OpenSSL truth：
      - `TOpenSSLConnection.GetConnectionInfo` 现在在 shared path 已无 `MacSize` 且 cipher 明确 non-AEAD 时，会使用：
        - `SSL_CIPHER_get_digest_nid`
        - `EVP_get_digestbynid`
        - `EVP_MD_size`
      - AEAD cipher 继续保持 shared `MacSize` owner truth，不会被 digest size 覆盖
      - `api.ssl` 与 `api.evp` 的 active export/binding chain 现在已经补齐：
        - `SSL_CIPHER_is_aead`
        - `SSL_CIPHER_get_digest_nid`
        - `EVP_get_digestbynid`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_openssl_connectioninfo_macsize_truth_contract.sh`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - WinSSL / MbedTLS / WolfSSL 是否存在值得接入的更强 legacy `MacSize` truth
      - 无法只靠 shared or current low-level helpers 稳定归一的更细平台差异
      - 更强 owner / deprecation wording route
41. `WolfSSL GetConnectionInfo legacy MacSize truth` 已完成并应作为当前 implementation-completeness 主线的进一步收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-wolfssl-connectioninfo-macsize-legacy-truth-feasibility.md`
    - 当前已确认的 WolfSSL truth：
      - `TWolfSSLConnection.GetConnectionInfo` 现在会先走 inherited shared path
      - 仅当 shared path 仍未给出 `MacSize` 时，才回退：
        - `wolfSSL_GetHmacSize(FWolfSSL)`
      - shared AEAD `MacSize` 继续保持 owner truth，不会被 backend helper 覆盖
      - `wolfssl.api` 的 active export/binding chain 现在已经补齐：
        - `wolfSSL_GetHmacSize`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_wolfssl_connectioninfo_macsize_truth_contract.sh`
      - `tests/test_wolfssl_connection_info_macsize_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - MbedTLS 是否存在值得接入的更强 legacy `MacSize` truth
      - 若收益不高，是否切回更强 owner / deprecation wording route
42. `MbedTLS GetConnectionInfo ciphersuite truth` 已完成并应作为当前 implementation-completeness 主线的进一步收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-mbedtls-connectioninfo-ciphersuite-truth-feasibility.md`
    - 当前已确认的 MbedTLS truth：
      - `TMbedTLSConnection.GetConnectionInfo` 现在会优先走：
        - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
      - direct helper 不可用时，会回退到：
        - `mbedtls_ssl_get_ciphersuite`
        - `mbedtls_ssl_get_ciphersuite_id`
      - ciphersuite info 现在会补齐：
        - `CipherSuiteId`
        - `KeySize`
        - legacy/non-AEAD `MacSize`
      - shared AEAD `MacSize` 继续保持 owner truth，不会被 digest size 覆盖
      - shared parser 现在也额外接受：
        - `TLS-RSA-...`
        - `AES-128[-GCM]`
        - `AES-256[-GCM]`
      - `mbedtls.base` 的 `MBEDTLS_MD_SHA1` / `MBEDTLS_MD_RIPEMD160` 常量真相也已修正
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_mbedtls_connectioninfo_ciphersuite_truth_contract.sh`
      - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已进一步收缩到：
      - 是否需要对这条 route 做一次 completion audit
      - FreePascal 是否还有必须单独补的 low-level truth
      - 若没有新的高价值实现缺口，是否切回更强 owner / deprecation wording route
43. `FreePascal GetConnectionInfo completion audit` 已完成并应作为当前 implementation-completeness 主线的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-freepascal-getconnectioninfo-completion-audit.md`
    - 当前已确认的 FreePascal truth：
      - `TFreePascalConnection` 没有 dedicated `GetConnectionInfo` override
      - 当前 backend 只额外提供：
        - `DoGetConnectionInfoServerName`
      - client / server TLS 1.3 runtime path 都会把 negotiated suite truth 写成：
        - `FCipherName := TLS13CipherSuiteToString(...)`
      - session / resumption path 继续保留：
        - `FCipherSuite: Word`
      - shared `GetConnectionInfo` 已能对这组标准 suite-name truth 补齐：
        - `CipherSuiteId`
        - `Hash`
        - `KeySize`
        - `MacSize`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_freepascal_connectioninfo_completion_contract.sh`
      - `tests/test_freepascal_server_accept_skeleton.pas`
      - `tests/test_freepascal_client_session_resumption.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` implementation-completeness 主线已可视为基本完成：
      - 不再默认继续往 backend 里盲补 low-level helper
      - 下一步应先切回 route-level completion audit / next-route selection
      - 默认主线回到更强 owner / deprecation wording route
44. `GetConnectionInfo contract owner primacy` 已完成并应作为当前 owner/mirror route 的正式收紧保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-contract-owner-primacy.md`
    - 当前已确认的 route truth：
      - `Contract 19` 现在先验证：
        - `ISSLConnectionInfo.GetConnectionInfo`
      - 再验证：
        - `ISSLConnection.GetConnectionInfo`
          只是 v1.x compatibility-core mirror
      - 新 completeness / proof tests 已不再默认走 direct core getter：
        - FreePascal server / session-resumption proof
        - OpenSSL cipher contract
        - WolfSSL MacSize contract
        - MbedTLS ciphersuite contract
        - shared builder proof
      - residual direct-core `GetConnectionInfo` surface 现在只剩 5 个命中：
        - `tests/contract/test_backend_contract.pas`
        - `tests/winssl/test_winssl_connection_info.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_contract_owner_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `tests/contract/test_backend_contract.pas`
      - `tests/test_connection_builder_hostname_precedence.pas`
      - `tests/test_freepascal_server_accept_skeleton.pas`
      - `tests/test_freepascal_client_session_resumption.pas`
      - `tests/test_mbedtls_connection_info_ciphersuite_contract.pas`
      - `tests/test_openssl_connection_info_cipher_contract.pas`
      - `tests/test_wolfssl_connection_info_macsize_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 更强 owner / deprecation wording route
      - 或判定剩余 WinSSL direct-core tests 是否属于 intentional core-surface proof
      - 不再继续把普通 completeness proof 留在 direct core getter 上
45. `GetConnectionInfo` WinSSL direct-core classification 已完成并应作为当前 residual route 的最终定性保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-winssl-direct-core-classification.md`
    - 当前已确认的 route truth：
      - WinSSL residual direct-core `GetConnectionInfo` file set 已稳定收缩到：
        - `tests/winssl/test_winssl_connection_info.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
      - 它们当前都已显式标记为：
        - `INTENTIONAL_CORE_SURFACE`
      - 这说明剩余 WinSSL direct-core 面属于 intentional core-surface proof，
        不是遗漏迁移的普通 completeness test
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 更强 owner / deprecation wording route
      - 不再继续停留在 residual classification 清扫
46. `GetConnectionInfo` public wording de-emphasis 已完成并应作为当前 source/doc owner truth 对齐的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-public-wording-deemphasis.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在明确写出：
        - 默认 owner 为 `ISSLConnectionInfo.GetConnectionInfo`
        - `ISSLConnection.GetConnectionInfo` 仅兼容保留，不再作为新代码 primary entry
      - `docs/reference/API_REFERENCE.md`
        现在在声明、示例、结构说明三处统一同一叙事
      - `docs/reference/INTERFACE_DESIGN_V2.md`
        不再只写“仍然存在”，而是明确把 `GetConnectionInfo` 视为 compatibility mirror
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 第一条真正的 public slimming slice feasibility selection
      - 不再重复做 wording / residual classification 清扫
47. `GetConnectionInfo` compiler deprecation alignment 已完成并应作为当前第一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getconnectioninfo-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetConnectionInfo` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetConnectionInfo'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual intentional direct-core tests 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
        - `tests/winssl/test_winssl_connection_info.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getconnectioninfo_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_getconnectioninfo_public_wording_deemphasis_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
      - `tests/contract/test_backend_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetConnectionInfo` route 的默认下一步应为：
      - 切到下一条 mirror 的 feasibility / slimming 选择
      - 不再重复做这条 getter 的 wording / deprecation 清扫
48. `GetContext` compiler deprecation alignment 已完成并应作为当前第一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getcontext-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetContext` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetContext'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual direct-core mirror proof 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
    - 当前 focused proof 已覆盖：
      - `bash tests/scripts/test_getcontext_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`
      - `tests/contract/test_backend_contract.pas`
      - `git diff --check`
    - 当前批收口后，`GetContext` route 的默认下一步应为：
      - 切到下一条 mirror 的 feasibility / slimming 选择
      - 不再重复做这条 getter 的 wording / deprecation 清扫
49. `GetStateString` compiler deprecation alignment 已完成并应作为当前下一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getstatestring-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetStateString` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetStateString'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual direct-core proofs 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
        - `tests/openssl/test_openssl_server_ocsp_stapling_runtime.pas`
        - `tests/wolfssl/test_wolfssl_server_ocsp_stapling_runtime.pas`
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_getstatestring_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_getstatestring_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后，`GetStateString` route 的默认下一步应为：
      - 切到下一条 mirror 的 feasibility / slimming 选择
      - 不再重复做这条 getter 的 wording / deprecation 清扫
50. `GetSelectedALPNProtocol` compiler deprecation alignment 已完成并应作为当前下一条 public slimming slice 的正式收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-getselectedalpn-compiler-deprecation-alignment.md`
    - 当前已确认的 route truth：
      - `src/fafafa.ssl.base.pas`
        现在把 `ISSLConnection.GetSelectedALPNProtocol` 声明成：
        - `deprecated 'Use ISSLConnectionInfo.GetSelectedALPNProtocol'`
      - `docs/reference/API_REFERENCE.md`
        与 `docs/reference/INTERFACE_DESIGN_V2.md`
        现在都明确记录：
        - core getter 仅兼容保留
        - 当前源码声明已经是编译期 deprecated
      - residual direct-core proofs 已带局部 warning quarantine：
        - `tests/contract/test_backend_contract.pas`
        - `tests/mbedtls/test_mbedtls_alpn.pas`
        - `tests/winssl/test_winssl_alpn_sni.pas`
        - `tests/winssl/test_winssl_connection_edge_cases.pas`
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_getselectedalpn_compiler_deprecated_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
      - `mkdir -p tmp/backend_contract_units && fpc -B -Fu./src -Fu./tests -FUtmp/backend_contract_units -FEtmp/backend_contract_units -otmp/backend_contract_units/test_backend_contract tests/contract/test_backend_contract.pas && ./tmp/backend_contract_units/test_backend_contract`
      - `git diff --check`
    - 当前批收口后，`GetSelectedALPNProtocol` route 的默认下一步应为：
      - 从 mirrors wording/compiler 治理线切回 interface-design completeness / implementation-completeness 主线
      - 不再重复做这条 getter 的 wording / deprecation 清扫
51. `ISSLDiagnostics` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-issldiagnostics-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/reference/API_REFERENCE.md`
        的普通 diagnostics examples 现在统一优先走：
        - `ISSLDiagnostics.IsHealthy`
        - `ISSLDiagnostics.GetHealthStatus`
        - `ISSLDiagnostics.GetPerformanceMetrics`
        - `ISSLDiagnostics.GetDiagnosticInfo`
      - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
        现在先验证 `Supports(LConn, ISSLDiagnostics, LDiag)`，再读取 diagnostics owner path
      - WinSSL diagnostics runtime tests 继续保留为 backend-specific residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
      - `bash tests/scripts/test_issldiagnostics_active_guidance_contract.sh`
      - `mkdir -p tmp/test_sslctxboth_roleless_handshake_clarification && fpc -B -Fu./src -Fu./tests -FUtmp/test_sslctxboth_roleless_handshake_clarification -FEtmp/test_sslctxboth_roleless_handshake_clarification -otmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification tests/test_sslctxboth_roleless_handshake_clarification.pas && ./tmp/test_sslctxboth_roleless_handshake_clarification/test_sslctxboth_roleless_handshake_clarification`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 继续盘点下一个 ordinary guidance 仍偏 core 的 optional-owner surface
      - 或切回更大的 interface-design completeness 选择
52. `ISSLCertificateVerification` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-isslcertificateverification-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/INTEGRATION_GUIDE.md`
        的握手失败示例与排错条目现在统一优先走：
        - `ISSLCertificateVerification.GetVerifyResult`
        - `ISSLCertificateVerification.GetVerifyResultString`
      - `docs/reference/API_DOCUMENTATION.md`
        的 CT 示例失败路径现在也统一优先走：
        - `ISSLCertificateVerification.GetVerifyResultString`
      - `tests/integration/test_cross_backend_consistency_contract.pas`
        与 `tests/integration/test_cross_backend_errors_contract.pas`
        现在都通过 helper 改走 `ISSLCertificateVerification` owner path
      - backend-specific certificate-verification runtime tests 继续保留为 residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslcertificateverification_active_guidance_contract.sh`
      - `mkdir -p tmp/test_cross_backend_consistency_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_consistency_contract -FEtmp/test_cross_backend_consistency_contract -otmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract tests/integration/test_cross_backend_consistency_contract.pas && ./tmp/test_cross_backend_consistency_contract/test_cross_backend_consistency_contract`
      - `mkdir -p tmp/test_cross_backend_errors_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_cross_backend_errors_contract -FEtmp/test_cross_backend_errors_contract -otmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract tests/integration/test_cross_backend_errors_contract.pas && ./tmp/test_cross_backend_errors_contract/test_cross_backend_errors_contract`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 继续盘点下一个 ordinary guidance 仍偏 core 的 optional-owner surface
      - 或切回更大的 interface-design completeness 选择
53. `ISSLSessionResumption` active-guidance de-emphasis 已完成并应作为当前 optional-owner surface 的下一条普通路径收口保留：
    - 新 plan：
      - `docs/plans/2026-05-18-isslsessionresumption-active-guidance-deemphasis.md`
    - 当前已确认的 route truth：
      - `docs/reference/API_REFERENCE.md`
        的 session-resumption / WinSSL session 示例现在统一优先走：
        - `ISSLSessionResumption.GetSession`
        - `ISSLSessionResumption.SetSession`
        - `ISSLSessionResumption.IsSessionReused`
      - `docs/reference/API_DOCUMENTATION.md`
        的会话缓存 / 性能问题示例现在先 capability-gate：
        - `Supports(Connection, ISSLSessionResumption, SessionResumption)`
      - `docs/INTEGRATION_GUIDE.md`
        的 resumed-session + early-data 例子现在先验证：
        - `Supports(InitialStream.Connection, ISSLSessionResumption, Resumption)`
      - `tests/integration/test_e2e_scenarios.pas`
        不再把 `Conn1.GetSession / Conn2.SetSession / Conn2.IsSessionReused`
        当普通读取/写入路径
      - backend-specific session runtime / benchmark proof 继续保留为 residual proof，不属于这批收口范围
    - 当前 focused proof 已覆盖：
      - `bash -n tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
      - `bash tests/scripts/test_isslsessionresumption_active_guidance_contract.sh`
      - `mkdir -p tmp/test_e2e_scenarios && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/test_e2e_scenarios -FEtmp/test_e2e_scenarios -otmp/test_e2e_scenarios/test_e2e_scenarios tests/integration/test_e2e_scenarios.pas && ./tmp/test_e2e_scenarios/test_e2e_scenarios`
      - `git diff --check`
    - 当前批收口后默认下一步应为：
      - 优先盘点 `ISSLOCSPStapling` ordinary guidance 是否仍在 direct core `GetOCSP*` 路径上漂移
      - 不再重复拉起 session-resumption active-guidance 清扫

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
