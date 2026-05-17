# Findings - Interface Design And Backend Implementation Verification

## 2026-05-18

- 本轮新的审查目标不是 release / workflow / runtime closeout，而是：
  - 公共接口设计是否已经失真
  - facade / factory / builder / config 是否把旧语义继续传播进新入口
  - 各 backend implementation 与 capability truth 是否仍然一致

- 当前已知静态审查基线来自 `docs/test_reports/INTERFACE_DESIGN_AUDIT_V1.5.0.md`：
  - `ISSLConnection` 过胖
  - context-level SNI 已 deprecated，但高层入口仍在写入
  - 文档承诺 `ISSLServerConnection`，源码中没有
  - `TSSLConfig` 混合跨层配置
  - capability matrix 仍有布尔字段与 support-level 双真相
  - `fafafa.ssl` 门面仍同时导出多条历史路径

- 但这份旧 audit 还没有回答两个更关键的问题：
  - 上述设计问题是否已经在 backend 实现、selector、serializer、capability contracts 中演变成真实行为漂移
  - 是否存在能用 focused contract 直接钉住并修掉的“接口设计影响实现真相”的问题

- 本轮因此采用“两段式审查”：
  - 第一段先做横向验证，确认 public surface / docs / backend truth 是否一致
  - 第二段只修高价值且边界清晰的问题，避免把结构性设计债误当成一批次即可完成的大重构

- 第一批 live 代码交叉验证当时确认：context-level `ServerName` 不只是“deprecated 但没人用了”的旧接口，而是仍然被实现主动传播：
  - `TSSLFactory.CreateContext(...)` 与 `TSSLContextBuilderImpl.Build*` 仍在对 context 调 `SetServerName(...)`
  - 当时 OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 的 connection 构造器都会把 context-level `ServerName` 带进连接实例
  - 现有测试 `tests/test_factory_server_name_scope_clarification.pas` 与 `tests/test_sslctxboth_client_capability_clarification.pas` 当时也把这种 fallback 继承锁成预期
  - 这说明 SNI 问题已经从“接口设计异味”演变成了“实现层 + 合同层一起固化的历史语义”

- `TSSLConfig.BufferSize` / `HandshakeTimeout` 当前更像“跨层暴露但带显式路障”的设计债，而不是隐藏 bug：
  - `src/fafafa.ssl.factory.pas` 已把它们判定为 connection-scoped / non-context-scoped，并在 request/default 路径上拒绝自定义值
  - 现有 `tests/test_factory_connection_scope_clarification.pas` 已把这条语义锁住
  - 所以问题不在“字段 silently ignored”，而在“公共 record 继续挂着跨层字段，增加理解负担”

- capability 体系的“双真相”问题仍然存在，而且已经扩散到 serializer / diff / selector / contracts：
  - backend `GetCapabilities` 仍同时发布 `SupportsSNI` / `SupportsOCSPStapling` / `SupportsSessionTickets` 与对应的 `*Support`
  - `src/fafafa.ssl.backend.selector.pas` 选特性时优先信 `*Support`
  - `src/fafafa.ssl.capability.serializer.pas` 与 `src/fafafa.ssl.capability.diff.pas` 又同时序列化/反序列化/比较两套字段
  - `tests/contract/test_backend_contract.pas` 对 optional interface 对齐仍主要依赖旧布尔字段（例如 `SupportsSNI`）
  - 也就是说，当前不是某一个 backend 的单点错误，而是 capability model 还没有完成“旧布尔兼容字段从属化”

- 当前最适合本批修的，不是直接拔掉 context-level SNI 旧语义：
  - 因为这会同时打到 factory、builder、各 backend connection 构造器和多份既有合同
  - 这是明确的下一阶段设计迁移，不是“单批次最小修复”
  - 本批更适合先修文档/合同真相，避免源码没有的接口继续被公开文档承诺

- 文档层的一个真实错误已经在本批收口：
  - `docs/ARCHITECTURE.md` 与 `docs/reference/INTERFACE_DESIGN_V2.md` 之前把 `ISSLServerConnection` 画进了活跃接口层次
  - live source 并没有任何 `ISSLServerConnection` 声明
  - 当前修法是把活跃文档改回“当前只公开 `ISSLClientConnection`，服务端特性主要走 optional context interfaces”
  - 并新增 `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`，防止这类承诺漂移再次回流

- focused 验证结果在当时进一步支持了当前路线判断：
  - `tests/test_factory_connection_scope_clarification.pas` PASS，证明 `BufferSize` / `HandshakeTimeout` 是显式 scope gate，而不是静默失效
  - `tests/test_factory_server_name_scope_clarification.pas` PASS，证明 client-side context `ServerName` 当时仍被正式支持为兼容路径
  - `tests/test_sslctxboth_client_capability_clarification.pas` PASS，证明多 backend 连接构造器当时仍主动继承 context-level `ServerName` fallback
  - 因此“删除 context-level SNI fallback”必须被当作一次兼容性迁移，而不是局部 bugfix

- capability 双真相的 runtime 半边已经可以安全收口，而且应该先收 runtime、后碰 serializer：
  - `src/fafafa.ssl.base.pas` 新增 `NormalizeLegacyCapabilityBooleans(...)`
  - 它统一把 `SupportsSNI` / `SupportsALPN` / `SupportsOCSPStapling` / `SupportsCertificateTransparency` / `SupportsSessionTickets` 视为对应 `*Support <> sslSupportNone` 的兼容投影
  - OpenSSL / FreePascal / WinSSL / MbedTLS / WolfSSL 的 `GetCapabilities` 现在都在返回前走同一条归一化路径
  - 这样 runtime live truth 不再分散在各 backend 自己手填的 legacy boolean 上

- capability focused contracts 也已经切换到 support-level truth：
  - `tests/contract/test_capabilities_contract.pas` 对 major backend 的 SNI / ALPN 改为检查 `SNISupport` / `ALPNSupport <> None`
  - 同时新增 bool/support-level 一致性断言，直接钉住兼容投影必须同步
  - `tests/contract/test_backend_contract.pas` 对 SNI / CT / OCSP optional interface alignment 也改为信 `*Support <> None`
  - 这说明“runtime truth 以 support-level 为准”已经不只是设计意见，而是被合同固定下来的行为规范

- 但 capability 双真相还没有全系统收完：
  - serializer / deserializer / diff 仍然同时 round-trip 和比较两套字段
  - 下一批应该设计“旧输入兼容、内部真相单一”的规则，而不是现在就删除 legacy boolean 字段

- serializer / deserializer / diff 线上的 live 问题已经被精确缩小到两处，而且都不是假问题：
  - `JSONToCapabilities(...)` / `XMLToCapabilities(...)` 原先会并列接受 legacy boolean 与 `*Support`，但对冲突输入没有裁决规则
  - `CompareCapabilities(...)` 原先几乎完全忽略 v1.2 support-level 差异，只看 legacy boolean，因此会漏掉 `experimental -> stable` 这类真实 capability 变化
  - 这两个问题都已经由新的 focused regression 先打出红灯

- 当前修法明确了 capability compatibility boundary：
  - 对反序列化输入：
    - 如果 payload 里出现了某个 `*Support` 字段，就以它为真相，并回填对应 legacy boolean
    - 如果 payload 只有旧 boolean，没有 `*Support`，则继续保留旧输入兼容，不擅自猜测 support-level
  - 对 capability diff：
    - paired feature 先比较 `*Support`
    - support-level-only 的 `SessionCacheSupport` / `ZeroRTTSupport` / `EarlyDataSupport` / `RenegotiationSupport` / `PostHandshakeAuthSupport` 也开始进入 diff
    - 只有在没有 support-level truth 可用时，legacy boolean 才作为 diff fallback

- 这也带来一个清晰的剩余边界：
  - 对“手工构造但内部已经不一致”的 `TSSLBackendCapabilities`，`CapabilitiesToJSON/XML` 目前仍偏向原样输出
  - 这不是 runtime live backend 的当前缺口，也不是反序列化/比对链路的 blocker
  - 但如果后续要把 capability model 彻底收成单真相，还需要决定 serializer 输出面是否做额外的受控归一化

- 继续沿着 SNI 主线深挖后，warning 治理这条线也已经有了新的 live 结论：
  - 旧计划 `docs/plans/2026-05-13-internal-context-servername-warning-quarantine.md` 点名的 `factory` / `builder` / `openssl.connection` / `openssl.backed` 不再是当前 compile 噪音主来源
  - `tests/test_builder_integration.pas` 也不再适合作为 warning contract 入口，因为它已经不能稳定暴露 `ISSLContext.Get/SetServerName` deprecated warning
  - 当前真正能稳定打出 warning 的 live probe 是 `tests/contract/test_capabilities_contract.pas`

- 这次 live probe 精确表明：
  - `wolfssl.connection` 两个构造器里的 context fallback 读取仍会触发 `ISSLContext.GetServerName` deprecated warning
  - `mbedtls.connection` 的 SNI 回填路径也会触发同类 warning
  - `WinSSL` 在当前 Linux compile path 里不直接暴露 warning，但源码里同样存在两处 direct `AContext.GetServerName` 兼容读取

- 因此 warning 治理的最小安全修法已经明确：
  - 不改兼容行为
  - 不碰 factory / builder
  - 只在 `wolfssl` / `mbedtls` / `winssl` 这些内部兼容读取点加局部 `{$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}` / `{$POP}` quarantine
  - focused shell contract 也改成盯 `test_capabilities_contract` 的 compile log，并对 `WinSSL` 补静态 source guard

- 继续往下验证后，serializer 输出面也被证明确实存在 live truth 漂移：
  - `CapabilitiesToJSON(...)` / `CapabilitiesToXML(...)` 原先直接输出 `ACaps.Supports*`
  - 这会让一个已经携带 v1.2 support-level truth 的 record 仍然导出自相矛盾的 payload，例如：
    - `supportsSNI=false`
    - `sniSupport="stable"`
  - 这说明“support-level 为真相”的收口如果不延伸到 serializer，外部数据面仍会重新泄漏历史双真相

- 当前最小安全修法也已经明确并落地：
  - serializer 先复制一份本地 record
  - 仅当该 record 已携带任意 support-level truth 时，才用 `NormalizeLegacyCapabilityBooleans(...)` 回填 legacy boolean 输出视图
  - 这样可以修掉 v1.2-aware record 的外部输出漂移，同时不去瞎猜纯 legacy-only in-memory record 的 `none` 是否只是默认值

- 这也把剩余边界说得更清楚了：
  - 现在已经解决的是“v1.2-aware record 导出不应自相矛盾”
  - 尚未、也不能在本批假装解决的是“纯 legacy-only record 在缺少 presence bit 时，是否应该把 `none` 当作显式不支持”
  - 如果将来想彻底消灭这类歧义，需要 capability model 自身增加 presence/truth 元信息，而不是继续在 serializer 里猜

- 在继续推进前，`context-level ServerName` 这条主线的路线图也已经被压实成当前可执行真相：
  - 高层写入面：
    - factory client path 仍会把 `TSSLConfig.ServerName` 写回 context
    - factory server path 已经禁止 `ServerName`
    - builder `BuildClient` 仍会保留 `WithSNI(...) -> context.SetServerName(...)` 的 client-side 兼容写入
    - builder `BuildServer` 现在只保留 warning / compatibility metadata，不再把 `WithSNI(...)` 写回 built context
    - connector 已经是正确方向，直接把 hostname 写到 `ISSLClientConnection.SetServerName(...)`
  - backend 继承面：
    - OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 五个 connection constructor 仍会从 context fallback 读取 `GetServerName`
  - 合同锁点：
    - precedence / inheritance / cross-backend consistency / error normalization 这些测试都仍在有意保留旧兼容语义

- 这意味着最合理的迁移顺序不是“先删 backend fallback”，而是：
  - 先收高层 surface，减少继续写入 deprecated context-level SNI 的入口
  - 再把 backend constructor 里的 fallback 提取成共享 compatibility shim
  - 最后才考虑真正删除历史继承语义

- 兼容测试的“显式标签”也需要当成一等资产维护：
  - 这次映射证明不只最初那几份测试在锁兼容语义，`test_context_builder_server_servername_runtime_consistency.pas` 与 `test_sslctxboth_client_capability_clarification.pas` 也同样在锁住旧 fallback
  - 已经把这些文件纳入 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
  - 以后做迁移时，谁要改掉这些行为，就必须先面对这些标签与合同，而不是“顺手改了再说”

- `context-level ServerName` 的 builder surface 第一刀已经证明可以安全缩窄：
  - `ExportToJSON(...)` / `ExportToINI(...)` 现在在保留 `server_name` 的同时，额外导出 `server_name_mode=deprecated_context_sni`
  - `ImportFromJSON(...)` / `ImportFromINI(...)` 显式接受这个 marker 但不让它改变 runtime state
  - legacy-only `server_name` JSON/INI 载荷在 re-export 时会被自动升级成“带兼容 marker 的旧语义”

- 这条线也顺手暴露了一个测试工作流细节：
  - builder JSON 导出使用 `FormatJSON`
  - 对这种输出做 substring 硬匹配会把空格/换行格式误判成行为回归
  - 新 focused test 已改为解析 JSON 字段值本身，避免后续在格式噪音上反复红灯

- `context-level ServerName` 的 factory/config 第二刀也已经证明可以安全收口：
  - `TSSLFactory.CreateContext(AContextType, ALibType)` 与 `TSSLFactory.CreateContext(const AConfig)` 在 client-side 兼容写入 `TSSLConfig.ServerName` 时，不再静默
  - 当前会通过 `TSecurityLog.Warning('Factory', ...)` 显式提示：
    - `TSSLConfig.ServerName` 只是 deprecated context-level SNI compatibility
    - 推荐主路径是 `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - default-config path 与 one-shot config path 都已被 focused tests 钉住

- 这一步也顺手把 public-facing truth 对齐到了一个更一致的状态：
  - `src/fafafa.ssl.base.pas` 的 `TSSLConfig.ServerName` 字段注释已经明确写成 compatibility-only
  - `docs/reference/API_REFERENCE.md` 也新增了 client SNI compatibility note
  - active docs contract 继续绿色，说明这次文档补充没有把旧 context-level SNI 路线重新教回去

- 因此 `context-level ServerName` 的高层写入面已经不再是“静默主路径”：
  - builder import/export 会加 marker
  - factory/config runtime path 会发 warning
  - 剩余的真正主问题已经转移到 backend constructor fallback 仍分散在五个实现里

- `context-level ServerName` Phase C 的第一刀已经把“分散在五个 backend 的 direct deprecated read”收成了共享 seam：
  - 新增 `src/fafafa.ssl.context.compat.pas`
  - helper 统一封装：
    - client-role gate
    - deprecated `ISSLContext.GetServerName` 读取
    - local warning suppression
  - 这使得 deprecated context fallback 的控制面重新回到一处，而不是继续散落在 OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 五份构造器里

- 这次 shared shim 提取刻意保持 backend side effect 不变：
  - OpenSSL / MbedTLS 继续走 `SetServerName(...)`
  - FreePascal / WolfSSL / WinSSL 继续走字段赋值路径
  - 因此这批是 seam consolidation，不是 behavior migration

- focused source contract 证明 Phase C 第一刀已经真正落地，而不是只在文档里说说：
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - 要求 shared helper 存在
    - 要求五个 backend 都调用 shared helper
    - 要求五个 backend 不再直接读取 `AContext.GetServerName` / `FContext.GetServerName`

- runtime regression 在那一批也证明 shared shim 没有误伤当时的兼容真相：
  - `tests/test_sslctxboth_client_capability_clarification.pas` 继续绿色
  - `tests/test_factory_server_name_scope_clarification.pas` 继续绿色
  - 这说明在那一时点，“context -> connection 的 fallback 仍存在” 与 “deprecated read 已被集中治理” 这两件事可以同时成立

- 因而 SNI 主线的剩余问题已经再次前移：
  - backend constructor 不再是散点收口对象
  - 下一批应该讨论的是 public/high-level surface cleanup，而不是重新逐个 backend 找 direct fallback read

- 继续往前收后，builder surface 的一个真实缺口也已经被证实并修掉：
  - `ValidateClient` / `ValidateServer` 早就会对 `WithSNI(...)` 发 deprecated compatibility warning
  - 但真实 `BuildClient` / `BuildServer` 路径此前仍然会静默应用它
  - 这造成 builder 的 validation truth 与 runtime truth 不对齐，也让 `WithSNI(...)` 看起来仍像正常主路径

- 当前修法把 builder 的 public/runtime/validation 三层重新对齐到了同一套术语：
  - `BuildClient` 现在会显式 warning：
    - `WithSNI` 是 deprecated context-level SNI compatibility
    - 推荐迁移到 `TSSLConnectionBuilder.WithHostname(...)` / `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `BuildServer` 现在会显式 warning：
    - `WithSNI` 只是 deprecated context-level ServerName compatibility
    - server-side connections ignore it
  - `ValidateClient` / `ValidateServer` 也同步沿用这条术语线，不再只在 validation 里单独说一套

- 这使得 `context-level ServerName` 的高层 surface 已经基本不再“静默”：
  - builder import/export 会打 compatibility marker
  - builder runtime path 会发 warning
  - factory/config runtime path 会发 warning
  - backend constructor fallback 已收成 shared shim

- 因而真正剩下的已经不是“哪里还在默默保留旧语义”，而是“哪一组 intentional compatibility tests 先改，才能开始第一条真实 behavior migration RED”

- 第一批普通 WinSSL 客户端连接流的分类与迁移也已经有了实锤：
  - `tests/winssl/test_winssl_error_mapping_online.pas`
  - `tests/winssl/test_winssl_https_client.pas`
  - `tests/winssl/test_winssl_revocation_online.pas`
  - `tests/winssl/test_winssl_mtls_e2e_local.pas`
  - 这些文件都属于真实客户端连接/握手/验证流，不是 intentional compatibility，也不是 context API-surface coverage

- 这四个文件当前已经统一改成：
  - 先 `CreateConnection(...)`
  - 再拿 `ISSLClientConnection`
  - 然后在 `Connect` / `DoHandshake` 前设置 `ServerName`
  - 因此这些文件不再继续把 deprecated context-level SNI 当成正常客户端流的指导方式

- 验证证据也说明这不是纸面改动：
  - focused shell contract 已经证明这四个文件不再含 `Ctx/Context/LCtx/LContext.SetServerName(...)`
  - 本地 Linux 直接编 `test_winssl_https_client.pas` 仍会因为 `fafafa.ssl.winssl.lib` 依赖 `Windows` 单元而失败，这不是本批引入的新问题，而是该测试本身的平台边界
  - 改走 `fpc -Twin64` 后，这四个文件的 Win64 交叉编译都成功完成

- 这轮收口之后，剩余活跃 context-level `SetServerName(...)` 命中已经更接近“故意保留”的集合：
  - connector / precedence / cross-backend compatibility tests
  - backend context contracts / framework tests
  - WinSSL comprehensive / library-basic / skeleton 这类更偏 API-surface 或未完成分类的文件
  - 剩下真正还像普通客户端流的主要残留，已经缩到 `test_winssl_mtls_skeleton.pas` 的握手路径这类更小的面

- 这批 residual 分类/收口之后，上述“更小的面”也已经被真正消化掉：
  - `tests/test_tls_connector_early_data_contract.pas`
    - 已补 `INTENTIONAL_COMPAT`，明确它故意从 inherited context fallback 起步
  - `tests/mbedtls/test_mbedtls_context_contract.pas`
  - `tests/wolfssl/test_wolfssl_context_contract.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
    - 已补 `INTENTIONAL_API_SURFACE`，明确它们是在覆盖 deprecated context setter/getter surface
  - `tests/winssl/test_winssl_mtls_skeleton.pas`
    - 配置段 `SetServerName('test.example.com')` 已补 `INTENTIONAL_API_SURFACE`
    - 真实 `TestMTLSHandshake` 路径已改成 `CreateConnection(...) -> ISSLClientConnection.SetServerName(ServerHost) -> DoHandshake`

- focused contract 与编译证据共同说明：当前剩余活跃 context-level `SetServerName(...)` 命中已经基本不再混着普通客户端流指导语义，而主要是 intentional compatibility / API-surface coverage

- 因而 SNI 主线的下一步已经可以正式前移到：
  - 选择第一组要改写的 client-side intentional-compat tests
  - 定义第一条 client-side fallback behavior migration RED
  - 而不是继续做 residual 分类考古或重复处理 server-only dead compatibility

- 第一条真正的 behavior migration 已经不再停留在路线图上：
  - `BuildServer.WithSNI(...)` 现在只会发 warning，并明确说明 `BuildServer ignores it and server-side connections ignore it`
  - built server context 不再保留这份 client-only `ServerName`
  - 这说明迁移主线已经可以从“先清 dead compatibility”继续推进到 client-side fallback 真正收缩

- 第一条 client-side fallback behavior migration 也已经有了一个很小但真实的落点：
  - `sslCtxBoth` 既然已经在握手层要求显式选择 role，就不该再静默继承 deprecated context-level client SNI fallback
  - shared compatibility shim 现在会对 `sslCtxBoth` 直接返回空字符串
  - 因而 dual-role context 仍保持 client-capable connection surface，但 inherited `ServerName` fallback 已经不再自动生效

- 这让剩余 client-side 迁移面再次收窄：
  - `sslCtxBoth` 不再需要挂在 intentional compatibility label 集合里
  - 下一步真正要碰的就是 `sslCtxClient` direct / builder / factory 这组还在显式锁 inherited fallback 的测试与路径

- cross-backend consistency / errors 这两份网络合同此前也被错误混进了 intentional compatibility 视角：
  - 它们真正要证明的是跨 backend 的结果一致性 / 错误归一化
  - 不是真正要保护 deprecated context-level SNI fallback
  - 因而把它们继续留在 intentional-compat label 集合里，只会让后续 `sslCtxClient` 迁移继续被假锁点拖慢

- 这两份合同现在已经统一迁到 per-connection SNI：
  - `tests/integration/test_cross_backend_consistency_contract.pas`
  - `tests/integration/test_cross_backend_errors_contract.pas`
  - 路径都是 `CreateConnection(...) -> ISSLClientConnection.SetServerName(...) -> Connect`
  - 连 `HTTP:80` 的握手失败分支也同步改掉了 context-level setter

- 这也让 intentional compatibility label 集合再次缩小到真正还在锁 inherited fallback 或兼容语义的文件：
  - `tests/test_connection_builder_hostname_precedence.pas`
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/test_freepascal_context_server_name_inheritance.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`

- focused source contract 与 integration compile/runtime shape 共同说明：
  - cross-backend 网络合同已经不再教 deprecated context-level SNI
  - 但它们在当前 host 上的 live network execution 仍受 `FAFAFA_RUN_NETWORK_TESTS!=1` gate 保护
  - 因此这批证明的是“合同语义与编译/runtime shape 已对齐”，不是重新做一次外网联机证明

- 在 cross-backend 网络合同收口完成的那个时点，下一条最自然的 `sslCtxClient` behavior migration RED 已经更清楚：
  - 首选应转向 `tests/test_freepascal_context_server_name_inheritance.pas`
  - 因为它比 precedence/override 类测试更直接地锁住 inherited context fallback 本体

- 这条 dedicated FreePascal runtime fallback 现在已经被真正切掉：
  - `src/fafafa.ssl.freepascal.connection.pas` 的 socket / stream client 构造器都不再读取 `GetContextLevelServerNameCompatibilityValue(AContext)`
  - 新建 FreePascal client connection 的 `ServerName` 默认回到空字符串
  - 调用方如果要走 FreePascal client path，必须显式在 connection 上 `SetServerName(...)`

- 这批证明了“builder/factory 仍写 deprecated context-level state”与“某个 backend 已不再消费这份 state”可以同时成立：
  - `TSSLContextBuilder.BuildClient.WithSNI(...)` 仍会发 compatibility warning
  - direct context `SetServerName(...)` 也仍然是 deprecated surface
  - 但 FreePascal runtime 已不再把这份 state 静默带进新连接

- 因而 intentional compatibility label 集合再次收窄：
  - 现在真正还在锁 inherited fallback / compatibility precedence 的只剩：
    - `tests/test_connection_builder_hostname_precedence.pas`
    - `tests/test_tls_connector_hostname_override_precedence.pas`
    - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_freepascal_context_server_name_inheritance.pas` 已经从“保留兼容”翻成“禁止再继承”

- 邻接 focused evidence 也说明这刀没有误伤下一层计划中的 mock precedence contracts：
  - `tests/test_connection_builder_hostname_precedence.pas` 继续绿色
  - `tests/test_tls_connector_hostname_override_precedence.pas` 继续绿色
  - 这意味着下一刀可以更直接地瞄准 `tests/test_connection_builder_hostname_precedence.pas`

- 所以当前最合理的下一条 `sslCtxClient` behavior migration RED 已经前移：
  - 不再是 `tests/test_freepascal_context_server_name_inheritance.pas`
  - 而是 `tests/test_connection_builder_hostname_precedence.pas`
  - 因为它现在成了剩余 intentional client-side inherited fallback 中最直接的下层契约

- 这条 `TSSLConnectionBuilder` mock precedence 契约现在也已经从“保留 fallback”翻成了“默认清空 fallback”：
  - `src/fafafa.ssl.connection.builder.pas` 的 `TryBuildClient` 在连接支持 `ISSLClientConnection` 时，会始终接管 per-connection hostname state
  - 若调用方没有 `WithHostname(...)`，builder 会显式写入空字符串，而不是继续保留 inherited context fallback
  - 若调用方显式 `WithHostname('conn.example.com')` 或 `WithHostname('')`，仍分别保留 override / clear 语义

- 这说明 `TSSLConnectionBuilder` 现在已经和更早之前收紧的 FreePascal runtime 一样，站到了“explicit per-connection hostname”这边：
  - builder 不再是 context-level SNI fallback 的隐式透传通道
  - connector mock precedence 成了剩余更靠上的 intentional 输入面

- focused evidence 也说明这刀是纯 builder 语义收口，而不是误伤 connector：
  - `tests/test_connection_builder_hostname_precedence.pas` RED -> GREEN
  - `tests/test_tls_connector_hostname_override_precedence.pas` 继续绿色
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 继续绿色

- 因而当前剩余最直接的 client-side intentional compatibility surface 再次收窄：
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 以及单独分类管理的 `tests/test_tls_connector_early_data_contract.pas`

- 所以下一条最合理的 `sslCtxClient` behavior migration RED 已再次前移：
  - 首选应转向 `tests/test_tls_connector_hostname_override_precedence.pas`
  - 然后再评估 `tests/test_tls_connector_early_data_contract.pas` 是否还需要继续以 inherited context fallback 作为 intentional 输入

- 这条 connector override precedence 契约现在也已经脱离了 inherited context fallback 输入：
  - `tests/test_tls_connector_hostname_override_precedence.pas` 不再需要先做 `Ctx.SetServerName('ctx.example.com')`
  - 它真正锁住的只是：
    - 非空 override 仍然胜出
    - 空 override 仍然保持空字符串
  - 这说明 connector 本身作为高层门面，已经可以完全独立地证明自己的 per-connection hostname override 语义

- focused evidence 也说明这批只是测试/合同真相同步，没有新的生产实现变更：
  - `bash tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh` PASS
  - `tests/test_tls_connector_hostname_override_precedence.pas` PASS
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` PASS

- 因而当前剩余最直接的 connector-side intentional compatibility 输入再次收窄：
  - `tests/test_tls_connector_early_data_contract.pas`
  - 以及服务端兼容语义的 `tests/test_context_builder_server_servername_runtime_consistency.pas`

- 所以下一条最合理的 `sslCtxClient` behavior migration RED 已再次前移：
  - 首选应转向 `tests/test_tls_connector_early_data_contract.pas`
  - 再决定 server-side compatibility control case 何时从当前 intentional 集合中拆开

- 这条 connector early-data contract 现在也已经脱离了 inherited context fallback 输入：
  - `tests/test_tls_connector_early_data_contract.pas` 不再需要先做 `Ctx.SetServerName('ctx.example.com')`
  - 它真正锁住的是：
    - session 先被应用
    - 显式 server name 被写到连接
    - early data 在 connect 前排队
    - unsupported early-data 路径继续返回既有错误语义
  - 这说明 connector 的 early-data convenience surface 和 override precedence 一样，都已经可以完全独立地证明自己的 per-connection hostname 语义

- focused evidence 同样说明这批只是测试/合同真相同步，没有新的生产实现变更：
  - `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh` PASS
  - `tests/test_tls_connector_early_data_contract.pas` PASS
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` PASS

- 因而当前剩余显式 intentional compatibility label 集合已经收窄到最后一个服务端控制案例：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`

- 所以下一条最合理的 bounded review 已再次前移：
  - 首选应转向 `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 再决定 public compatibility surface 还能保留到什么边界

- 但对这条 server-side control case 做 live focused retest 后，先暴露出来的反而是三份 FreePascal-focused contracts 已经失真：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - 它们都还在教 “deprecated context-level ServerName 会被 FreePascal 新连接继承”

- 这与当前 live runtime truth 冲突：
  - `src/fafafa.ssl.freepascal.connection.pas` 之前已经切掉 socket / stream client constructor 的 inherited context fallback
  - 所以当前 FreePascal 真相是：
    - context state 仍会保留
    - client connection 不再自动继承

- 这说明当前最先需要修的不是 direct server-context control case 本身，而是把这些 focused contracts 拉回真相：
  - 让它们继续覆盖 deprecated context state 是否还存在
  - 但停止错误宣称 FreePascal connection 仍会继承旧 fallback

- 这也重新排序了下一批主线：
  - 真正剩下的高价值实现问题已经前移到 shared shim 的其余四个 backend：
    - OpenSSL
    - WolfSSL
    - MbedTLS
    - WinSSL
  - 需要决定它们是否也应统一切到 no-inheritance

- dedicated cross-backend RED 已经把这条 shared shim 分歧真正钉死：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    初始明确证明：
    - FreePascal 新 client connection 已经是空 `ServerName`
    - OpenSSL / WolfSSL / MbedTLS 仍会继承 `"client.example.com"`
    - WinSSL 在当前 Linux host 上因为 backend unavailable 被跳过，但源码仍走 shared helper
  - 所以这不是文档误判，而是当时真实存在的跨 backend runtime 分歧

- 当前这条分歧已经被最小实现改动收掉：
  - `src/fafafa.ssl.context.compat.pas`
    不再读取 deprecated context-level `GetServerName`
  - helper 现在保留为 shared seam，但对任意非空 context 一律返回 `''`
  - 这让 OpenSSL / WolfSSL / MbedTLS / WinSSL 与 FreePascal 统一进入 no-inheritance 规则

- 这也暴露出一个工作流层面的真实教训：
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
    曾经正确地守住 “五个 backend 都走 shared helper”
  - 但在 FreePascal 先行切到 no-inheritance 之后，它变成了过时契约，开始错误阻塞当前批次
  - 当前已把它改回当前真相：
    - shared helper 只要求出现在 OpenSSL / WolfSSL / MbedTLS / WinSSL
    - FreePascal 明确禁止再走 helper
    - helper 与所有 backend 都禁止直接读 `(AContext|FContext).GetServerName`

- 因而 `context-level ServerName` 主线的阻塞点再次前移：
  - “shared client fallback divergence” 已不再是未决问题
  - 当前剩下的更尖锐问题回到了最后一个 direct server-context legacy-state control case：
    - 高层 builder / factory 是否还要继续保留 context state 可见性
    - 即便这份 state 已经不再对任何新 client connection 产生 inherited fallback

- 这条最后一个 direct server-context legacy-state control case 现在也已经完成收口：
  - `src/fafafa.ssl.context.builder.pas`
    的 `BuildClient` 不再把 `WithSNI(...)` 写回 built client context
  - `src/fafafa.ssl.factory.pas`
    的 client default-config / one-shot `CreateContext(...)` 路径
    不再把 `TSSLConfig.ServerName` 写回新建 context
  - warning 文案也同步切成：
    - `BuildClient ignores it...`
    - `CreateContext ignores it for new contexts...`

- 这意味着 deprecated context-level `ServerName` 已不再通过任何高层新建入口流入新的 context state：
  - builder 高层入口不会再保留它
  - factory 高层入口不会再保留它
  - 所有 client backend 也早已不再把它继承进新 client connection
  - 当前剩下的最后 compatibility surface，已经只剩 direct `ISSLContext.SetServerName/GetServerName` 本身和显式 API-surface coverage

- focused evidence 也说明这不是文案改动，而是真正的行为收口：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    PASS (`6 passed, 0 failed`)
  - `tests/test_factory_server_name_scope_clarification.pas`
    PASS (`6 passed, 0 failed`)
  - `tests/test_factory_config_server_name_isolation.pas`
    PASS (`6 passed, 0 failed`)
  - `tests/test_factory_server_name_compatibility_warning.pas`
    PASS (`16 passed, 0 failed`)
  - `tests/config/test_config_validation.pas`
    PASS (`53 passed, 0 failed`)
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    PASS (`20 passed, 0 failed, 1 skipped`)

- 因而当前下一条最有价值的路线已经不再是“builder / factory 还要不要继续保留 context state”：
  - 这个问题已经被代码和 focused regressions 一起回答为“不再保留”
  - 现在真正该进入的是 final public surface cleanup prep：
    - `TSSLConfig.ServerName` 是否继续保留当前字段位置
    - `WithSNI(...)` 是否继续保留当前命名/入口
    - direct `ISSLContext.SetServerName/GetServerName` 这条最后 compatibility surface 未来如何降级/替代

- 继续往下摸 public surface 时，又暴露出一个此前没被 focused 合同覆盖的漏口：
  - `src/fafafa.ssl.openssl.backed.pas`
    的 `TOpenSSLLibrary.CreateContext(...)`
    仍会把 `FDefaultConfig.ServerName` 写回新建 context
  - 这不是 generic factory 残留，而是 backend-specific direct library 入口自己的漂移
  - 其余 `freepascal` / `mbedtls` / `wolfssl` / `winssl` library `CreateContext(...)` 当前并没有这条 `ServerName` 注入逻辑

- 这意味着当时的“high-level write surfaces 已全部收口”还差最后一块：
  - generic factory 已经是 `warning + ignore`
  - builder 已经是 `warning + ignore`
  - 但 direct OpenSSL library default-config path 还停留在 “默默写回 context”

- 当前这条 OpenSSL direct-library 漏口也已经被收掉：
  - `TOpenSSLLibrary.CreateContext(sslCtxClient)`
    - 不再把 `FDefaultConfig.ServerName` 写回 built context
    - 若配置了 library log callback，会发出明确 compatibility warning
  - `TOpenSSLLibrary.CreateContext(sslCtxServer)`
    - 若 default-config 带 `ServerName`，现在会 fail-fast 抛 `ESSLConfigurationException`
  - server misuse 的 reject 也已经前移到真正的 fail-fast：
    - 不再先创建 context 再抛错

- focused evidence 说明这次不是只改了 OpenSSL 文案，而是补上了一个真实未覆盖实现面：
  - 新增 `tests/test_openssl_library_default_config_server_name_clarification.pas`
    - RED (`3 passed, 8 failed`) -> GREEN (`13 passed, 0 failed`)
    - 直接钉住 client ignore+warning、server reject、no-ServerName quiet
  - 邻接 retest：
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      PASS (`20 passed, 0 failed, 1 skipped`)
    - 说明这次 direct library 对齐没有碰坏当前 cross-backend no-inheritance 真相

- 因而当前路线图又收紧了一层：
  - “high-level write surfaces” 现在不仅包括 builder / generic factory
  - 也包括 direct OpenSSL library default-config path，且这几条都已经不再把 deprecated `ServerName` 流入新 context
  - 剩下的 public-surface 主问题，确实只剩最后的 compatibility API 形状，而不是还有某个 backend-specific 高层入口继续偷写旧 state
