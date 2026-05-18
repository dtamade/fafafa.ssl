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

- 当主线进入 final public surface cleanup prep 后，新的高价值问题已经不是 backend 行为，而是工作流漂移：
  - `tests/test_quick.pas` 这种普通 smoke 还在顺手示范 `.WithSNI('example.com')`
  - `tests/winssl/test_winssl_connection_edge_cases.pas` 这种普通 edge-case 也还在顺手写 `LConfig.ServerName := ...`
  - 这些命中不再承担 compatibility 行为断言，却会持续把 deprecated builder/config surface 伪装成正常主路径

- 因而 public-surface prep 的第一刀应该是“先做静态分类，再谈 API 形状”：
  - 普通测试里的旧入口示范要迁掉
  - 真正还需要保留 `WithSNI(...)` / `TSSLConfig.ServerName` 的文件，要显式标成 `INTENTIONAL_COMPAT`
  - 否则每次重新审查时，都会被这些普通测试文本重新拉回“是不是还有 runtime 主路径”的旧问题

- 新增的 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh` 已经把这条工作流护栏固化：
  - allowlist compatibility tests 必须带 `INTENTIONAL_COMPAT`
  - active `tests/*.pas` 里若重新出现 `.WithSNI(...)` 或 builder-config `ServerName :=`，会直接红灯

- 这也把“下一步该做什么”压缩得更清楚：
  - 现在已经不需要继续排 ordinary test guidance
  - 下一步可以直接讨论最终 API 形状：
    - `TSSLConfig.ServerName`
    - `WithSNI(...)`
    - direct `ISSLContext.SetServerName/GetServerName`

- 继续把 active direct-context `SetServerName(...)` 命中全盘出来后，又确认了一个此前缺少 repo-level 合同的空档：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
    这些文件实际上都在故意保留 direct-context legacy input
  - 但在本批之前，它们还没有像 WinSSL comprehensive / backend framework / diagnostic/security files 那样，被统一纳入 active-surface 分类合同

- 这意味着当时 direct context surface 还不算真正“收口”：
  - 旧命中虽然多数已经带局部注释
  - 但 repo 还没有一个 focused truth 能回答：
    - “active tests 里到底哪些 direct-context `SetServerName(...)` 是允许的？”
    - “它们是 compatibility 还是 API-surface coverage？”

- 新增的 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh` 已经把这个空档补上：
  - 它枚举 active tests 里所有 real direct-context `SetServerName(...)` 文件
  - 每个文件都必须带正确标签：
    - `INTENTIONAL_COMPAT`
    - 或 `INTENTIONAL_API_SURFACE`
  - allowlist 外若重新出现 direct context setter，会直接红灯

- 因而到当前为止，public compatibility surface 的测试面已经分成两层稳定护栏：
  - builder/config compatibility-only surface：
    - `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - direct-context compatibility/API-surface：
    - `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`

- 再往前一步后，还确认了一条很实用的工作流细节：
  - 即便 active direct-context hits 已被分类，如果 intentional compatibility tests 不做局部 warning quarantine
  - focused compile 时仍会冒出我们明知故意保留的 deprecated getter/setter warning
  - 这会让后续验证输出继续混着“已知旧 API 噪音”和“真正新的实现 warning”

- 当前已经把这批 intentional compatibility tests 的 direct-context getter/setter 都包进局部 suppression：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 结果是 focused compile 里不再反复提示这几处已知 intentional deprecated surface

- 这也让后续审查信号更干净：
  - 剩下的 compile warnings 更接近真正值得继续治理的实现/类型问题
  - 而不是被我们有意保留的 compatibility API 使用反复刷屏

- 这进一步确认了路线已经真正前移：
  - 现在不再需要继续做测试面排污或分类普查
  - 下一步的最高价值工作已经纯粹是最终 API 形状决策，而不是再找“还有没有哪个文件偷偷示范旧入口”

- `WithSNI(...)` 在这之前还留着最后一个源码层 truth 漏口：
  - runtime warning、validation wording、API 文档都已经把它降格成 compatibility-only
  - 但 public declaration 自身还不是 compiler `deprecated`
  - 这会让源码使用者在编译期看不到和文档/runtime 一致的信号

- 当前这条漏口也已经被最小收口：
  - `ISSLContextBuilder.WithSNI(...)`
  - `TSSLContextBuilderImpl.WithSNI(...)`
    都已挂上同一条 compiler `deprecated` message：
    `Use per-connection hostname via TSSLConnectionBuilder.WithHostname or ISSLClientConnection.SetServerName`
  - 新增 `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    直接守住 declaration-level truth

- 这次修法也顺手确认了一条工作流事实：
  - intentional compatibility tests 继续保留 `.WithSNI(...)` 是合理的
  - 但如果不做局部 warning quarantine，focused compile 输出会继续混入我们已知接受的 compiler deprecation 噪音
  - 因而当前对 `.WithSNI(...)` 的剩余测试使用，应一律视为“故意覆盖 compatibility surface”，不是普通 fluent builder 示例

- 因此 `WithSNI(...)` 这半边已经不再属于“语义还没收实”的范围：
  - 它现在同时具备：
    - compatibility-only 文档定位
    - runtime warning + ignore
    - compiler-level deprecated truth
    - explicit compatibility-test classification
  - 剩下真正未决的，只是最终 public surface 是否继续保留它当前的命名/挂载位置

- 对 `TSSLConfig.ServerName` 的最新静态审查进一步说明：
  - 当前已经不存在“还有某条高层 runtime path 会偷偷消费它”的实现漏口
  - generic factory、OpenSSL direct-library、ordinary tests、active docs guidance 都已经被收干净
  - 真正剩下的问题不是行为真相，而是要不要在 `v1.x` 直接改掉它的字段位置/命名

- 当前最稳妥的 `v1.x` 设计决定已经明确：
  - 不在当前版本线直接移除或改名 `TSSLConfig.ServerName`
  - 保持 source compatibility
  - 但把它冻结成一个“仅剩 compatibility truth 的 record field”

- 这条 `v1.x freeze` 现在也不再只是口头结论：
  - `src/fafafa.ssl.base.pas` 字段注释明确指向 per-connection `ISSLClientConnection.SetServerName`
  - `src/fafafa.ssl.factory.pas` 与 `src/fafafa.ssl.openssl.backed.pas` 的 warning wording 都继续点名 `TSSLConfig.ServerName`
  - `docs/reference/API_REFERENCE.md` 现在不只在顶层 compatibility note 说明它，还在 `Use TSSLConfig with TSSLFactory.CreateContext(...)` 段落旁边明确写出 client-side warning + ignore truth
  - 新增 `tests/scripts/test_tsslconfig_servername_surface_truth_contract.sh`
    把 source comment、warning wording、以及 active docs confinement 一起钉住

- 因而 `TSSLConfig.ServerName` 这条线对当前主路线的意义已经变化：
  - 它不再是“下一刀要不要删/改”的首要候选
  - 它已经被降成 `v1.x` compatibility-only frozen surface
  - 下一步真正该继续收口的，已经前移到 direct `ISSLContext.SetServerName/GetServerName` 这组最后的 context-level compatibility API

- 对 direct `ISSLContext.SetServerName/GetServerName` 的最新静态审查也已经给出同样清晰的结论：
  - 这组 API 仍然存在于 public interface，并由各 backend context 实现
  - 但 production `src/` 已经不再存在真实 direct context caller
  - active docs 也不再把 `Ctx.SetServerName(...)` 当普通 client 流指导路径

- 因而当前最稳妥的 `v1.x` 设计决定也已经明确：
  - 不在当前版本线直接移除这组 deprecated context API
  - 保持 source compatibility
  - 但把它们冻结成“deprecated but still present”的 compatibility-only context surface

- 这条 direct-context `v1.x freeze` 现在已经被 source/doc contract 固化：
  - `src/fafafa.ssl.base.pas` 的 deprecation message 继续统一指向 `ISSLClientConnection.Set/GetServerName`
  - 新增 `tests/scripts/test_direct_context_servername_surface_truth_contract.sh`
    钉住：
    - deprecated declaration message
    - production source 无 direct context caller
    - active docs 无 direct context setter guidance
  - 既有 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
    与 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    则继续守住 active tests 的 intentional coverage 边界

- 这也让主路线再次前移：
  - `TSSLConfig.ServerName` 已 frozen
  - direct context API 已 frozen
  - 当前最后仍值得继续讨论 public shape 的，只剩 `WithSNI(...)` 的命名/挂载位置

- `WithSNI(...)` 的最终静态审查结果也已经足够清楚：
  - source 中只剩两处 declaration + 一处 implementation 命中
  - active docs 现在也只剩 `docs/reference/API_REFERENCE.md` 以 compatibility note 形式提及它
  - active tests 里的 `.WithSNI(...)` 命中已经被 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh` 限制在 allowlist compatibility coverage

- 因而当前最稳妥的 `v1.x` 设计决定同样已经明确：
  - 不在当前版本线直接移除或改挂 `WithSNI(...)`
  - 保持 source compatibility
  - 但把它冻结成“deprecated but still present”的 compatibility-only fluent surface

- 这条 `WithSNI` `v1.x freeze` 现在已经被 source/doc/test contract 固化：
  - `tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    守住 compiler deprecation truth
  - 新增 `tests/scripts/test_withsni_surface_truth_contract.sh`
    守住：
    - source compatibility comment
    - active docs confinement
    - source hits 不得扩散出当前 declaration/implementation 边界
  - 既有 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    继续守住 active tests allowlist

- 这意味着当前版本线里的整个 `context-level SNI` compatibility family 已经全部 frozen：
  - `TSSLConfig.ServerName`
  - direct `ISSLContext.SetServerName/GetServerName`
  - `TSSLContextBuilder.WithSNI(...)`

- 因而下一批最值得做的事不再是继续绕着 SNI 旧兼容语义打转，而是回到更大的 interface-design debt：
  - `TSSLConfig` 跨层字段拆分 / slimming
  - `ISSLConnection` 核心 surface slimming

- 对 post-SNI 路线的最新筛选也已经有了更清楚的优先级：
  - `TSSLConfig` 路线已经积累了足够多的 scope truth：
    - `BufferSize` / `HandshakeTimeout` = connection-scoped
    - `LogLevel` / `LogCallback` = library-scoped
    - 多个 option-style 字段仍承担 compatibility bridge
  - 这意味着它更适合先做“field buckets + slimming roadmap”的 bounded batch

- 相比之下，`ISSLConnection` 核心 surface slimming 现在仍然更像下一阶段的大手术：
  - public interface 影响面更广
  - 会直接打到各 backend connection 实现与大量 tests/helpers
  - 如果马上动手，风险明显高于先做 `TSSLConfig` 路线

- 因而当前最值得执行的 post-SNI 第一条主线是：
  - 先把 `TSSLConfig` 做成明确分桶的跨层字段 roadmap
  - 再决定是否以及如何进入 `ISSLConnection` core surface slimming

- `TSSLConfig` 的 mixed-scope truth 现在已经可以稳定分成 5 个 buckets：
  - `library-scoped defaults`
    - `LogLevel`
    - `LogCallback`
  - `context-scoped`
    - `SessionCacheSize`
    - `SessionTimeout`
    - `ALPNProtocols`
    - `ClientEarlyDataEnabled`
    - `ServerEarlyDataPolicy`
    - `ServerMaxEarlyDataSize`
    - `ServerEarlyDataReplayStoreFile`
    - `ServerEarlyDataReplayStoreDirectory`
  - `connection-scoped`
    - `HandshakeTimeout`
    - `BufferSize`
  - `compatibility-only`
    - `ServerName`
  - `option-bridge`
    - `EnableCompression`
    - `EnableSessionTickets`
    - `EnableOCSPStapling`

- 这份 bucket truth 现在已经落进 durable source/doc surface：
  - `src/fafafa.ssl.base.pas`
    关键 mixed-scope 字段注释不再是泛泛中文描述，而是直接带 scope truth
  - `docs/reference/API_REFERENCE.md`
    新增 `TSSLConfig Scope Buckets` 段，避免后续审查再反复翻 factory/backend source
  - `tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh`
    用 focused contract 固定 source/doc/factory/OpenSSL direct-path 的 truth

- 当前这一轮静态横查还额外暴露出一个更实质的 backend parity risk：
  - `ISSLLibrary.CreateContext(AType)` 的 default-config 套用在各 backend 间并不一致
  - OpenSSL direct-library path：
    - 在 `TOpenSSLLibrary.CreateContext` 中显式应用：
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
    - 也显式处理 deprecated `ServerName` 的 warning/reject truth
  - WinSSL direct-library path：
    - 当前只看到 `Options` 被显式套用
  - FreePascal / MbedTLS / WolfSSL direct-library path：
    - 当前静态上只看到直接创建 context
    - 没看到 parallel 的 default-config replay/apply block

- 由于这些 backend 同时都满足下面两点，这个差异目前更像真实实现缺口，而不是“字段本来没打算支持”：
  - library side 都持有 `FDefaultConfig`
  - context side 都公开并维护：
    - `SessionCacheSize`
    - `SessionTimeout`
    - `ALPNProtocols`

- 因而当前最优先的下一批不该直接跳去 `ISSLConnection` slimming，而应先做：
  - `direct-library default-config parity audit/fix`
  - 先把 `ISSLLibrary.CreateContext(AType)` 在各 backend 的 default-config 套用 truth 守住
  - 再继续 broader `TSSLConfig` slimming 或 `ISSLConnection` surgery

- `direct-library default-config parity audit/fix` 现在已经完成第一轮收口：
  - runtime RED 已真实出现：
    - `tests/test_direct_library_default_config_parity.pas`
      在修复前证明 FreePascal direct-library `CreateContext(sslCtxClient)` 没有套用：
      - `ProtocolVersions`
      - `VerifyMode`
      - `VerifyDepth`
      - `CipherList`
      - `CipherSuites`
      - `SessionCacheSize`
      - `SessionTimeout`
      - `ALPNProtocols`
      - option-bridge normalized `Options`
  - source RED 也真实出现：
    - `tests/scripts/test_direct_library_default_config_parity_contract.sh`
      在修复前证明 `freepascal` library unit 甚至还没有先 normalize `SetDefaultConfig(...)`

- 当前已修正的实现 truth：
  - `TFreePascalSSLLibrary`
  - `TWinSSLLibrary`
  - `TMbedTLSLibrary`
  - `TWolfSSLLibrary`
  - 以上 4 个 backend library units 现在都已：
    - 在 `SetDefaultConfig(...)` 里先调用 `TSSLFactory.NormalizeConfig(...)`
    - 在 direct-library `CreateContext(AType)` 里显式套用 context-safe 默认字段

- 这轮 direct-library parity 现在覆盖的字段是：
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

- 这轮没有混入的未收口点，也因此成为下一优先级：
  - `ServerName` compatibility warning/reject parity
    - 目前 OpenSSL direct-library path 仍然更完整
  - early-data / replay-store direct-library parity
    - 当前仍主要以 factory/context path 为真相源

- 因而当前更准确的 next queue 是：
  - 先继续做 direct-library special-case parity
  - 再决定是否进入 broader `TSSLConfig` slimming 或 `ISSLConnection` surgery

- `direct-library ServerName compatibility parity` 现在也已经完成第一轮收口：
  - RED 证据：
    - `tests/scripts/test_direct_library_servername_compatibility_contract.sh`
      初次运行即证明 `freepascal` library unit 还没有：
      - server reject
      - client warning + ignore
    - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
      初次运行即证明 FreePascal direct-library path 当前只是静默忽略 client `ServerName`，server 也不 reject

- 当前已修正的实现 truth：
  - `TFreePascalSSLLibrary`
  - `TWinSSLLibrary`
  - `TMbedTLSLibrary`
  - `TWolfSSLLibrary`
  - 以上 4 个 backend library units 现在都已对齐 OpenSSL 的这条专门兼容语义：
    - client default-config = warning + ignore
    - server default-config = reject

- 这一轮 direct-library `ServerName` parity 没有再回退去“恢复 context-level SNI 正常主路径”：
  - context 上的 `GetServerName = ''` 仍保持 compatibility-only truth
  - warning message 仍明确要求迁移到：
    - `ISSLClientConnection.SetServerName`
    - `TSSLConnector.Connect*(..., ServerName)`

- 因而当前 direct-library special-case parity 的剩余重点已经进一步缩窄为：
  - early-data / replay-store direct-library parity
  - 这应当是下一条高价值、边界依然清楚的小批次

- `direct-library early-data / replay-store parity` 现在也已经完成第一轮收口：
  - RED 证据：
    - `tests/scripts/test_direct_library_early_data_replay_store_parity_contract.sh`
      初次运行即证明 5 个 backend library path 还没统一接 replay-store scope 校验与 early-data/replay-store apply helper
    - `tests/test_direct_library_early_data_replay_store_parity.pas`
      初次运行即证明 FreePascal direct-library path 当前还没有：
      - 应用 `ClientEarlyDataEnabled`
      - 应用 `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
      - 安装 replay-store file / directory
      - 拒绝 client replay-store config
      - 拒绝 conflicting replay-store file + directory

- 这轮 production fix 刻意没有把逻辑再复制进 5 份 backend：
  - 新增 `src/fafafa.ssl.context.config.pas`
  - 先把 replay-store client/server scope 校验、early-data context apply、replay-store installer apply 收成 shared internal helper
  - 再让 `TOpenSSLLibrary` / `TFreePascalSSLLibrary` / `TWinSSLLibrary` / `TMbedTLSLibrary` / `TWolfSSLLibrary`
    的 `CreateContext(AType)` 一起接回这条 helper

- 这也把 direct-library path 和 factory/context path 的关系重新说清楚了：
  - client path：
    - `ClientEarlyDataEnabled` 若 backend 暴露 `ISSLEarlyDataContext`，就会应用
    - replay-store file / directory 继续 fail-fast reject
  - server path：
    - `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize` 若 backend 暴露 `ISSLEarlyDataContext`，就会应用
    - replay-store file / directory 保持 mutually exclusive
    - 若 backend 不实现 installer seam，则保持 fail-fast，而不是静默忽略

- 当前 FreePascal runtime 已给出完整的 live proof：
  - direct-library client context 会正确反映 `ClientEarlyDataEnabled`
  - direct-library server context 会正确反映 `ServerEarlyDataPolicy` / `ServerMaxEarlyDataSize`
  - replay-store file / directory 都会真实 materialize 到配置路径
  - cross-context replay rejection 继续成立
  - client replay-store config 与 conflicting file+directory 都会抛出 `ESSLConfigurationException`

- 因而 direct-library special-case parity 当前已经全部收口：
  - `default-config`
  - deprecated `ServerName`
  - `early-data / replay-store`
  - 下一条路线不该再回到 “继续补 direct-library 小口子”，而应回到 broader interface debt 的选择：
    - `TSSLConfig` option-bridge freeze / slimming
    - 或 `ISSLConnection` 核心 surface slimming roadmap

- `TSSLConfig option-bridge default truth parity` 这轮也已经被进一步缩到一个更准确的根因：
  - 初看像是：
    - `CreateDefaultConfig(...)` 单点丢了 `EnableSessionTickets`
  - 但继续把测试缩细后确认：
    - direct `CreateFreePascalSSLLibrary` 路径是对的
    - `Lib.SetDefaultConfig(Lib.GetDefaultConfig)` direct-library round-trip 也是对的
    - 真正错误的是 factory-held backend instance 的 `GetDefaultConfig(...)`
  - 这说明问题不只在 `NormalizeConfig(...)` 或某个 public helper，而在“生产实例化路径是否真的保留了 backend constructor truth”

- 新增的 runtime narrowing 已经把这条根因钉实：
  - `TSSLFactory.GetLibrary(sslFreePascal).GetDefaultConfig`
    在修复前就已经丢了 `EnableSessionTickets = True`
  - `TSSLFactory.GetLibrary(sslAutoDetect).GetDefaultConfig`
    在 `SetDefaultLibrary(sslFreePascal)` 后同样丢失
  - 因而 `CreateDefaultConfig(...)` 的失败只是 downstream symptom，不是 upstream source

- 本批最终确认的实现问题是：
  - `factory` 对真实 backend 仍主要依赖 raw registered-class instantiation
  - 这条 path 不足以保住 backend constructor 内建立的 `FDefaultConfig` 真相
  - 所以即便 source 里各 backend constructor 已经补了：
    - `TSSLFactory.NormalizeConfig(FDefaultConfig)`
    - FreePascal `EnableSessionTickets := True`
    - 生产实例化得到的 library defaults 仍可能失真

- 当前修法没有去赌 Pascal metaclass semantics，也没有再把默认配置逻辑复制一份到 factory：
  - `TSSLFactory` 新增 explicit creator-function registration path
  - `TSSLLibraryRegistration` 现在允许直接存 `CreateFunc`
  - `CreateLibraryInstance(...)` 优先走 `CreateFunc`
  - `openssl` / `freepascal` / `winssl` / `mbedtls` / `wolfssl`
    的真实 backend 注册统一改成 `@Create*SSLLibrary`
  - 这样 factory-held instance 与 direct-library instance 回到了同一条 backend-owned constructor truth

- 这也纠正了原本 plan 里的一个误导点：
  - “多个 backend library constructor 仍是未归一化 mixed truth” 只说对了一半
  - 更准确的说法应是：
    - constructor normalization 的确必要
    - 但如果生产实例化路径不走 backend creator truth，fresh default-config surface 依然会漂移

- 当前 focused evidence 已经闭环：
  - `tests/test_tsslconfig_option_bridge_default_truth.pas`
    - PASS
    - 直接覆盖：
      - direct library default-config truth
      - factory-held `GetDefaultConfig(...)`
      - auto-detect `GetDefaultConfig(...)`
      - `CreateDefaultConfig(...)`
  - `tests/config/test_default_config.pas`
    - PASS
    - 说明既有 `CreateDefaultConfig(...)` baseline 没被这次 creator-path fix 打穿
  - `tests/scripts/test_tsslconfig_option_bridge_default_truth_contract.sh`
    - PASS
    - 现在同时守住：
      - constructor normalization
      - backend 注册必须走 explicit creator function

- 因而这条线的 next queue 也变得更清楚：
  - 不需要再反复怀疑 factory-held default-config 是否 stale
  - 之后若继续推进，应讨论：
    - `Options vs legacy booleans` 冲突优先级是否要进一步单真相化
    - `TSSLConfig` option-bridge surface 是否要继续 freeze/slim

- `TSSLConfig option-bridge precedence` 这轮也已经从“源码隐含行为”升级成了明确 contract：
  - 当前 `v1.x` truth 不是 “`Options` 总是权威输入”
  - 也不是 “legacy booleans 已经只剩只读投影”
  - 更准确的 truth 是：
    - legacy booleans 仍是兼容写入口
    - 当调用方传入冲突的 `Options` 和 legacy booleans 时，legacy booleans 赢
    - normalization 先把 legacy booleans 写入相关 option bit
    - 再把最终 `Options` truth 回投到这三个 compatibility booleans

- 这条结论不是纸面推理，而是现在已经有 focused production evidence：
  - `TSSLFactory.NormalizeConfig(...)`
    - 直接覆盖 conflict input
  - `TSSLFactory.CreateContext(const AConfig)`
    - 证明 one-shot factory path 跟随同一条 precedence truth
  - `ISSLLibrary.SetDefaultConfig(...)` / `ISSLLibrary.CreateContext(AType)`
    - 证明 direct-library path 也跟随同一条 precedence truth

- 继续做 source search 后，这条线还多确认了一个关键背景：
  - production code 里，真正会写这三个 legacy booleans 的地方已经非常集中：
    - backend default-config constructors
    - `CreateDefaultConfig(...)`
    - `TSSLFactory.NormalizeConfig(...)`
  - builder/import-export/config snapshot 等活跃高层 surface 实际上主要围绕 `Options`
  - 这意味着当前“legacy booleans 仍是 compatibility write surface”这件事，边界已经足够清晰，不再是散落在各处的隐藏入口

- 因而这轮之后，`Options vs legacy booleans` 不再是一个“未定义设计问题”，而是一个“已冻结的 `v1.x` compatibility contract”：
  - 现在该问的已经不是：
    - 冲突时到底谁赢？
  - 而是：
    - 未来要不要把这组 legacy booleans 继续缩成更窄的 compatibility-only surface
    - 若要缩，应该走什么非破坏性的 migration path

- 这也让总体路线图更清楚了一步：
  - `TSSLConfig` 这条主线当前已完成：
    - scope buckets
    - fresh default-config truth parity
    - conflict precedence freeze
  - 下一条更值得开的批次，不再是继续补 “option-bridge 真相”
  - 而是：
    - `TSSLConfig` public-surface slimming / migration roadmap
    - 或再往后才考虑 `ISSLConnection` 核心 surface slimming

- `TSSLConfig option-bridge surface` 这轮继续往前收后，public truth 也终于不再停留在“行为已经冻结，但表达还松”的状态：
  - `src/fafafa.ssl.base.pas` 现在明确把
    - `EnableCompression`
    - `EnableSessionTickets`
    - `EnableOCSPStapling`
    定义为 compatibility-only option-bridge flags，并直接提示新代码优先写 `Options`
  - `docs/reference/API_REFERENCE.md` 也同步改成同一套 public-facing truth：
    - 这三个字段是历史 compatibility 写入口
    - factory / direct-library default-config path 会先把它们折叠进 `Options`
    - fresh default-config surfaces 返回时也必须保持 boolean 与最终 `Options` 真相一致

- 这轮同时暴露了一个比“文案松”更具体的问题：
  - `tests/security/test_session_security.pas` 原本不是 compatibility coverage，却还在通过 `EnableSessionTickets := ...` 驱动语义
  - 这会继续把 legacy boolean 教成普通主路径，也会和已经冻结的 “legacy boolean 优先于冲突 `Options`” 规则纠缠在一起
  - 当前已经把这条活跃安全测试改成直接覆盖 context `SetOptions(...)` / `GetOptions(...)` 主路径

- 与此对应，仍然故意覆盖 compatibility surface 的测试也已经显式化：
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
  - `tests/test_tsslconfig_option_bridge_default_truth.pas`
  - `tests/test_tsslconfig_option_bridge_precedence_freeze.pas`
  - `tests/test_direct_library_default_config_parity.pas`
  - 它们现在都明确说明自己是在保留 option-bridge compatibility coverage，而不是继续把这组字段当普通推荐 API

- 这轮还有一个工作流层面的 live 教训值得保留：
  - 当 public wording 被收紧后，旧 contract 脚本会先因为盯旧文案而报假红灯
  - 这次 `test_tsslconfig_scope_bucket_truth_contract.sh`
    / `test_tsslconfig_option_bridge_default_truth_contract.sh`
    / `test_tsslconfig_option_bridge_precedence_freeze_contract.sh`
    都一起对齐到了新的 wording truth
  - 这比“重新跑更多重型验证”更有价值，因为它直接消掉了后续重复拉起的噪音源

- 因而当前 `TSSLConfig` 这条线的 next queue 已经进一步收敛：
  - 不需要再反复补
    - option-bridge precedence wording
    - fresh default-config wording
    - compatibility labels
  - 真正值得开的下一批，应是：
    - `TSSLConfig` public-surface slimming / migration design
    - 明确哪些 compatibility-only 字段在 `v2` 继续保留、改挂、还是迁到更窄入口

- 在 option-bridge surface 收紧之后，活跃指导面里又暴露出两条更直接的漂移：
  - `examples/example_factory_usage.pas`
    - 还在通过 `Config.BufferSize := ...` / `Config.HandshakeTimeout := ...`
      演示 `TSSLFactory.CreateContext(...)` 的配置写法
    - 但这两个字段当前早已被 factory 明确判定为 connection-scoped / transport-adjacent，不属于 context/factory 主路径
  - `docs/reference/ARCHITECTURE.md`
    - 还保留一段过时的伪 `TSSLConfig` 结构
    - 字段名例如 `DefaultLibraryType` / `ProtocolVersion` / `CertificatePath` / `ReadTimeout` / `WriteTimeout`
      已与当前 public source 明显脱节

- 这说明 `TSSLConfig` 当前不只存在“内部设计债”，还存在“高可见度用户入口仍在教旧模型”的问题：
  - 如果不先收掉这些 example/reference 漂移，后续即使开始做 slimming design，用户也还会继续从活跃入口学到 mixed-scope 旧写法

- 当前修法刻意保持在 guidance 层，不去碰 runtime：
  - `examples/example_factory_usage.pas`
    - 移除 `BufferSize` / `HandshakeTimeout` 的错误示例
    - 明确把 timeout 导向 `TSSLConnector.WithTimeout` / `ISSLConnection.SetTimeout`
    - 明确把 buffering 导向外围 socket / stream / transport 配置
  - `docs/reference/ARCHITECTURE.md`
    - 把“伪 record 结构”改成当前真实 scope buckets：
      - library-scoped defaults
      - context-scoped
      - connection-scoped
      - compatibility-only

- 与此同时，example-surface 上故意保留的 direct context API coverage 仍然保持显式分类：
  - `tests/examples/test_lib_core_functionality.pas`
    - 继续保留 `INTENTIONAL_API_SURFACE`
    - 这说明我们这轮修的是“活跃用户指导面”，不是把所有 direct context API 命中都误当成 bug

- 这轮的 focused evidence 也足够干净：
  - `tests/scripts/test_tsslconfig_active_guidance_truth_contract.sh`
    - PASS
    - 守住：
      - 活跃 example 不得再教 `BufferSize` / `HandshakeTimeout` factory/config 写法
      - 活跃 architecture reference 不得再回到过时伪结构
      - example-surface 的 direct context API coverage 继续显式带标签
  - `examples/example_factory_usage.pas`
    - focused compile PASS
    - 说明这次 guidance cleanup 没把示例代码本身改坏

- 因而现在 `TSSLConfig` 这条线又进一步少掉了一个常见重复入口：
  - 后续不该再回到
    - “example 里还在教错字段”
    - “architecture reference 里还是旧 record”
  - 真正值得开的下一批，已经更明确地只剩：
    - `TSSLConfig` public-surface slimming / migration design

- 这轮已经把 `TSSLConfig` 的 slimming / migration design 从“抽象建议”推进成了字段级决策：
  - `docs/reference/API_REFERENCE.md`
    - 新增 `TSSLConfig Migration Targets`
    - 把 mixed-scope / compatibility 字段逐一映射到当前推荐入口与 `v2` 方向
  - `docs/plans/2026-05-18-tsslconfig-public-surface-slimming-roadmap.md`
    - 把这份 map 落成了可执行 roadmap，而不是只留一句“以后再 slimming”

- 这份 migration matrix 当前已经明确了 4 条后续实现主线：
  - `LogLevel` / `LogCallback`
    - library defaults surface
  - `HandshakeTimeout` / `BufferSize`
    - connection / transport surface
  - `ServerName`
    - per-connection SNI surface
  - `EnableCompression` / `EnableSessionTickets` / `EnableOCSPStapling`
    - `Options` / builder `WithOption(...)`

- 这一步的价值在于：
  - 后续不需要每次先重做“这些字段该迁去哪”的分析
  - 真正的实现批次可以直接从这 4 条线里挑最小切片

- 当前最适合率先进入实现的，不再是 `ServerName` 或 option-bridge：
  - 这两条线虽然已经冻结 truth，但兼容历史更重
  - 更稳的第一刀应是 `LogLevel` / `LogCallback`：
    - 它们已经被 factory request path 明确拒绝
    - 替代入口也已经稳定存在于 `ISSLLibrary` defaults surface
    - 因而最适合作为 `TSSLConfig` slimming 的第一条真正实现切片

- 顺着这条 logging detachment 继续往下看后，这轮又挖出了一个真正会误导调用方的 active-doc bug：
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
    都曾经只演示 `ISSLLibrary.SetLogCallback(...)`
    然后立刻调用 `LLib.Log(sslLogInfo, ...)`
  - 但当前 runtime truth 早已固定为：
    - default `LogLevel = sslLogError`
    - backend `Log(...)` 只有在 `ALevel <= configured LogLevel` 时才会 dispatch
  - 因而这些 snippet 不是“讲得不完整”，而是“照着写也看不到示例里的 info/debug 输出”

- 这也进一步澄清了 `LogLevel` / `LogCallback` 这两个字段在 public surface 上最容易失真的点：
  - `LogCallback` 的 owner 不等于 `LogLevel` 的 owner
  - 更准确的说法是：
    - `LogLevel` 通过 `ISSLLibrary.GetDefaultConfig(...)` / `SetDefaultConfig(...)` 调整
    - `LogCallback` 通过 `ISSLLibrary.SetLogCallback(...)` 安装
    - `CreateDefaultConfig(...)` / factory request path 继续回到 request-safe baseline
  - 之前 reference/guides 把这两个动作混成一句“设置日志回调”，正是 drift 的来源

- 当前修法刻意保持在 truth freeze 层，不动 runtime：
  - `docs/reference/API_REFERENCE.md`
  - `docs/reference/ARCHITECTURE.md`
    现在都明确拆开了 logging level 与 callback 的入口
  - `docs/guides/USER_GUIDE.md`
  - `docs/guides/TROUBLESHOOTING.md`
    在演示 `sslLogInfo` / `sslLogDebug` 前，都会先通过 default-config path 抬高 library default `LogLevel`
  - 新增 `tests/scripts/test_tsslconfig_logging_surface_truth_contract.sh`
    防止 active docs 再退回“只设 callback 就应该看到 info/debug”这条假指导

- 这轮的 focused evidence 也证明这不是一次“为了文档而猜实现”的收口：
  - 新 docs contract 首次运行 RED，直接暴露 API/reference/guides 还没把 logging level 入口说清楚
  - 修正后同一条 contract GREEN
  - `tests/test_factory_logging_scope_clarification.pas`
    继续证明：
    - request path 拒绝 `LogLevel` / `LogCallback`
    - library default snapshot / dispatch truth 保持不变
  - `tests/config/test_default_config.pas`
    继续证明：
    - `CreateDefaultConfig(...)` 仍然强制返回 `sslLogError` + `nil`

- 因而 `LogLevel` / `LogCallback` 这条线当前在 `v1.x` 下已经足够清晰：
  - runtime/source truth 已稳
  - active docs truth 已稳
  - 后续不该再把 logging guidance 漂移当成未验证区域反复拉起

- 沿着 `TSSLConfig` mixed-scope buckets 继续往下查后，这轮又确认了一个真正属于“实现没收干净”的 direct-library 缺口：
  - `TSSLFactory.CreateContext(...)` 路径早已 reject 自定义 `HandshakeTimeout` / `BufferSize`
  - 但 `ISSLLibrary.SetDefaultConfig(...)` + `CreateContext(AType)` 之前还会留下另一套行为：
    - default-config 可写入自定义 `HandshakeTimeout` / `BufferSize`
    - backend `CreateContext(AType)` 又完全不消费这两个字段
  - 这不是单纯 wording 问题，而是一条真实的 silent-ignore drift

- 这条 drift 的根因也很清楚：
  - five backend library units 都会：
    - `SetDefaultConfig(...)` 存下归一化后的 `TSSLConfig`
    - `CreateContext(AType)` 手工把 context-safe 字段套到新 context
  - 但在本批修复前，这条 handoff 缺少和 factory 对齐的 connection-scope validator
  - 结果就是 direct-library path 比 factory 多留了一块“看起来能配、实际上不生效”的历史面

- 当前修法刻意走 shared seam，而不是五份散改：
  - `src/fafafa.ssl.context.config.pas`
    新增 `ValidateDirectLibraryConnectionScope(...)`
  - 统一在五个 backend 的 `CreateContext(AType)` 入口 fail-fast：
    - `TOpenSSLLibrary.CreateContext`
    - `TFreePascalSSLLibrary.CreateContext`
    - `TWinSSLLibrary.CreateContext`
    - `TMbedTLSLibrary.CreateContext`
    - `TWolfSSLLibrary.CreateContext`
  - 这样后续若继续做 `TSSLConfig` slimming，不需要再分别担心 backend library path 会不会偷偷回退

- 这也让 public truth 更完整了一步：
  - `docs/reference/API_REFERENCE.md`
    现在明确：
    - `HandshakeTimeout` / `BufferSize`
      在 factory request path 和 direct-library context path 都 reject 自定义值
  - `docs/reference/ARCHITECTURE.md`
    也同步改成：
    - 这两个字段不属于 context/factory/direct-library config 主路径

- focused evidence 说明这批不是“为了统一而统一”：
  - 新 shell contract 首次运行 RED，直接暴露 docs/source 还没把 direct-library connection-scope truth 固定下来
  - 新 `tests/test_freepascal_library_default_config_connection_scope_clarification.pas`
    首次运行 RED，直接暴露 FreePascal direct-library path 还在 silent accept
  - 修复后两者 GREEN
  - `tests/test_factory_connection_scope_clarification.pas`
    继续 GREEN，说明 shared helper 没有把已冻结的 factory 行为拉歪

- 因而 `HandshakeTimeout` / `BufferSize` 这两个字段现在已经把三条高层入口都收到了同一套 truth：
  - one-shot factory request path：reject
  - factory-held library default path：reject
  - direct-library default-config path：reject
  - 后续不该再把这条 direct-library silent-ignore drift 当成未验证区域重新拉起

- 继续沿 `TSSLConfig` 的第一条真正实现切片往下做后，这轮又确认了一条 live runtime drift：
  - public docs / migration map / active guides 已经把 logging owner 说成：
    - `LogLevel` 走 `GetDefaultConfig(...)` / `SetDefaultConfig(...)`
    - `LogCallback` 走 `ISSLLibrary.SetLogCallback(...)`
  - 但 5 个 backend 的 `SetDefaultConfig(...)` 在修复前仍会直接执行：
    - `FLogCallback := LConfig.LogCallback`
  - 这意味着 callback owner 其实还是双挂：
    - default-config path 可以安装/替换 callback
    - dedicated setter path 也可以安装/替换 callback

- 这条 drift 和前面的 logging docs truth 不同，它已经不是 wording 问题，而是 runtime/source 仍未兑现“owner 单一”的接口语义：
  - 旧的 `tests/test_factory_logging_scope_clarification.pas`
    原本只证明 request path reject + dedicated setter dispatch
  - 当它被增强成：
    - `SetDefaultConfig(LogCallback)` 不应再安装 callback
    - 已安装 callback 后续不应被 `SetDefaultConfig(...)` 顺手清掉
    就立即 RED

- 当前修法继续遵循前面 `ServerName` / option-bridge 那条“先收高层 owner，再保留 source compatibility”的路线：
  - 不移除 `TSSLConfig.LogCallback` 字段
  - 但把 active owner 收到唯一入口：
    - `SetDefaultConfig(...)` 只继续更新 `LogLevel` 和其他 default-config 字段
    - `SetLogCallback(...)` 独占 runtime callback state
  - `GetDefaultConfig(...)` 仍然反映当前 callback 真相，因此 snapshot 读面没被掐掉

- 这也让 callback 的 runtime 规则终于变得可预测：
  - 如果没有显式 `SetLogCallback(...)`
    - 即使有人把 method pointer 塞进 `DefaultConfig.LogCallback`
    - 后续 `Log(...)` 也不会开始 dispatch
  - 如果 callback 已通过 `SetLogCallback(...)` 安装
    - 后续再用 `SetDefaultConfig(...)` 调 `LogLevel`
    - callback 会继续保持，不会被顺手清掉

- 这轮顺手也修掉了两份 focused test 自己的旧混合入口：
  - `tests/test_freepascal_library_default_config_server_name_clarification.pas`
  - `tests/test_openssl_library_default_config_server_name_clarification.pas`
    之前都还借 `DefaultConfig.LogCallback := ...` 来抓 warning
  - 现在已经改成：
    - `DefaultConfig.LogLevel := ...`
    - `Lib.SetLogCallback(...)`
  - 因而这些 direct-library warning 测试本身也不再反向把旧 owner 教回去

- focused evidence 说明这批不是“为了设计洁癖而收口”：
  - 新 source contract 首次运行 RED，直接点出 OpenSSL 仍在让 `SetDefaultConfig(...)` 装 callback；其余 backend 同样存在
  - 强化后的 `tests/test_factory_logging_scope_clarification.pas`
    首次运行 RED，直接暴露：
    - `GetDefaultConfig(...)` 仍回显了来自 `SetDefaultConfig(LogCallback)` 的 callback
    - `Log(...)` 也会立刻开始 dispatch
  - 修复后同一批 focused runtime/source/docs contracts 全部 GREEN

- 因而 `LogLevel` / `LogCallback` 这条线现在不只 docs truth 已稳，runtime/source truth 也已真正对齐：
  - callback owner 已单一
  - dedicated setter 与 default-config 的职责已分开
  - 后续不该再把“`SetDefaultConfig(...)` 还能不能安装 callback”当成未验证区域重新拉起

- 继续顺着 mixed-scope / compatibility 测试面往下看后，这轮确认了一个更偏“测试完整性”但依然真实的问题：
  - `tests/test_factory_logic.pas`
  - `tests/test_data_structures.pas`
    已经承担了核心 `TSSLConfig` record-shape coverage：
    - deprecated `ServerName`
    - option-bridge booleans
    - `BufferSize` / `HandshakeTimeout` 这类 mixed-scope field visibility
  - 但它们在测试结束后仍保留：
    - `WriteLn('按回车键退出...')`
    - `ReadLn`

- 这条问题没有造成当前 headless run 卡死，但它会留下两个负面信号：
  - 自动化输出始终带着“手工程序尾巴”
  - 核心测试继续看起来像 demo，而不是 CI-friendly test binary

- 这轮修法保持得很克制：
  - 不改任何断言和 coverage 目标
  - 只移除交互式退出逻辑
  - 并把 `INTENTIONAL_COMPAT` 注释补完整：
    - 不只说明 `ServerName` / option-bridge booleans
    - 也明确说明 `BufferSize` / `HandshakeTimeout` 这类 mixed-scope record-shape 字段仍是故意保留的可见面

- focused evidence 也足够直接：
  - 修复前 direct run 的最终输出都以“按回车键退出...”收尾
  - 修复后同样两份测试都可直接在 `timeout 2 ./...` 下跑完
  - 输出尾部只剩测试总结，不再要求或暗示手工输入

- 因而这两份 core compat tests 当前已经重新回到更合理的位置：
  - 仍然保留需要的 `v1.x` record-shape / compatibility coverage
  - 但不再把自己伪装成必须手工退出的演示程序
  - 后续不该再把这两份文件的交互尾巴当成未验证区域重新拉起

- 继续往下扫后，这轮又确认了第二组同类但更“顶层核心”的非交互残留：
  - `tests/test_exceptions.pas`
  - `tests/test_base_interface_contract.pas`
  - 它们分别锁住异常层级/构造语义，以及 `fafafa.ssl.base` 的接口/常量/record-shape 契约

- 这两份文件的细节和前一批 core compat tests 不完全一样：
  - 在当前 headless shell 里，末尾 `ReadLn` 会因为 stdin EOF 直接返回，因此 `timeout 2 ./...` 并不会稳定超时
  - 但源码仍保留：
    - `WriteLn('按回车键退出...')`
    - `ReadLn`
  - 结果就是自动化输出仍然带着“按回车键退出...”尾巴，而且退出行为继续依赖运行方式

- 因而这条问题的真实边界不是“必卡死”，而是：
  - 顶层 core tests 仍残留手工演示语义
  - 自动化输出被无意义提示污染
  - 是否需要人工输入不应由 shell/pipe/tty 形态来决定

- 这轮最合适的护栏不是再假装造一个 runtime hang，而是直接把自动化要求写成 focused source contract：
  - 新增 `tests/scripts/test_top_level_core_tests_noninteractive_contract.sh`
  - 专门禁止：
    - `tests/test_exceptions.pas`
    - `tests/test_base_interface_contract.pas`
    重新出现 `ReadLn` 或“按回车键退出...”

- focused evidence 也说明这条合同是有价值的，不是形式主义：
  - 新脚本首次运行立即 RED，直接命中 `tests/test_exceptions.pas`
  - 移除两份文件末尾交互逻辑后，脚本转 GREEN
  - 重新编译并 direct run 后，输出尾部只剩测试总结，不再留下交互提示

- repo-wide 扫描也顺手给出了范围真相：
  - `ReadLn` 残留并不只这两处
  - 但其余命中主要落在：
    - `tests/examples/*`
    - `tests/diagnostic/*`
    - benchmark / file-read helpers
    - 多份 WinSSL 专项程序
  - 因而当前这批保持在“顶层 core tests 自动化面”是正确收口，不应把 examples/diagnostics/Windows-specialized 程序混进同一批

- 在顶层 core tests 收口之后，repo-wide 剩余交互尾巴的下一层主面也已经被压实：
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_context_comprehensive.pas`
  - `tests/winssl/test_winssl_errors_comprehensive.pas`
  - `tests/winssl/test_winssl_monitoring.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - `tests/winssl/test_winssl_certstore.pas`
  - `tests/winssl/test_winssl_session_management.pas`
  - `tests/winssl/test_winssl_library_basic.pas`
  - `tests/winssl/test_winssl_certificate_loading.pas`

- 这批文件不是 examples / benchmark，而是真正仍有自动化入口和验证角色的 WinSSL 测试程序：
  - `run_winssl_tests.ps1` 明确把 `tests/unit/test_winssl_comprehensive.pas`
    归类成 `Minimal, non-network, non-interactive tests`
  - `scripts/run_tests_windows.ps1` 仍试图自动编译运行 WinSSL unit-level tests
  - 多个文件仍有 `.lpi`、validation bundle、Windows checklist 引用

- 因而它们保留 `Press Enter to exit...` / `按回车键退出...` / `ReadLn` 的问题，已经不只是“输出不太好看”：
  - 这会直接让 active Windows test path 的 non-interactive 承诺失真
  - 也会让真正的自动化 runner 与源码语义对不上

- 当前最合适的修法仍然是 focused source contract，而不是假装在 Linux 本地能完整跑 WinSSL runtime：
  - 新增 `tests/scripts/test_winssl_active_tests_noninteractive_contract.sh`
  - 只守住这批活跃 WinSSL 测试程序
  - 不把 examples / diagnostics / benchmark 混成同一批

- 这条合同首次运行立即 RED，直接命中 `tests/unit/test_winssl_comprehensive.pas`：
  - 证明问题不是历史残影，而是当前源码仍在主动保留交互式退出逻辑

- 修复后，这条线的验证比前两批还更完整了一层：
  - source contract 转 GREEN
  - `tests/unit/test_winssl_comprehensive.pas` 在 Linux 下可编译运行其非 Windows 分支，并且退出不再依赖 `ReadLn`
  - `tests/unit/test_winssl_comprehensive.pas`
  - `tests/winssl/test_winssl_session_management.pas`
    的 Win64 交叉编译也通过，说明这次尾部清理没有破坏 Windows 语法面

- 这也让测试完整性路线图更清楚了一步：
  - 活跃自动化测试面的交互尾巴，当前已经从
    - 顶层 core tests
    - WinSSL 活跃测试程序
    这两层基本收干净
  - 剩余 `ReadLn` 命中主要收缩到：
    - examples
    - diagnostics
    - benchmark
    - 少量非自动化/手工验证型程序

- 因而如果继续沿“测试完整性”推进，下一层已经不该再和主测试面混做：
  - 那会是 examples / diagnostics / benchmark 的单独分类清理
  - 而不是继续把 active test automation 问题当成未收口区域

- 反过来讲，这也意味着总体主路线可以重新抬回更高层：
  - `TSSLConfig` broader slimming / freeze 后续
  - 或 `ISSLConnection` 核心 surface slimming roadmap

- 在把主测试面的交互尾巴收干净之后，这轮又确认了另一个“看起来像未完成、其实主要缺 execution receipt”的文档缺口：
  - `docs/plans/2026-05-04-backend-context-optional-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-context-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-http-hooks-interface-completion-audit.md`
  - `docs/plans/2026-05-04-backend-session-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-certificate-store-native-handle-completion-audit.md`
  - `docs/plans/2026-05-04-backend-diagnostics-interface-completion-audit.md`
  - 这些 plan 都已经对应到 `tests/contract/test_backend_contract.pas` 里的真实 contract，
    但文档本身还缺 execution result

- live 代码检查把这一点说得很明确：
  - `tests/contract/test_backend_contract.pas` 当前已包含：
    - Contract 12: Context optional interface alignment
    - Contract 13: Context native-handle interface alignment
    - Contract 14: Context HTTP hooks interface alignment
    - Contract 15: Session native-handle interface alignment
    - Contract 17: Certificate-store native-handle interface alignment
    - Contract 18: Diagnostics interface alignment
  - 换句话说，问题不在“contract 还没写”，而在“文档还没留下当前验证结果”

- 这条缺口如果不收，会继续误导总路线图判断：
  - 后续新会话很容易把这些 interface surface 当成“也许还没真正审过”
  - 结果就是重复拉起同一批 optional-surface 审计，而不是继续推进更高层的 design debt

- 当前最合适的修法因此不是重开大设计，而是做一次 focused completion-audit revalidation：
  - 重新编译并运行 `tests/contract/test_backend_contract.pas`
  - 直接用 live 结果给这 6 份 plan 补 `Focused Revalidation Result (2026-05-18)`
  - 同时明确说明：本批没有重跑 `compile_all_modules.py` / `run_minimal_ci_gate.sh --fast-local`，因为没有生产代码改动，且当前工作流明确避免重复拉重门禁

- focused evidence 进一步确认这批不是在补“纸面文档”：
  - 6 份 plan 全部被 source scan 证实 `MISSING_RESULT`
  - `tests/contract/test_backend_contract.pas` 当前 focused run 结果：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 上述 optional public surface 全部 PASS
  - WinSSL 继续保持已有平台边界 truth：
    - 当前 Linux 主机不把 WinSSL backend 当作 live runtime truth
    - session native-handle 仍明确需要 dedicated Windows batch

- 这让“接口设计/实现完整性”的证据盘又收紧了一层：
  - context optional interfaces
  - context native-handle
  - context HTTP hooks
  - session native-handle
  - certificate-store native-handle
  - diagnostics
  这些 surface 现在不只是“有计划”，而是已经有 focused live contract 结果

- 因而总体路线图可以更安心地继续前移：
  - 不需要再怀疑这些 optional surface 是否缺 backend contract
  - 下一条高优先级应重新回到 broader interface debt：
    - `TSSLConfig` public-surface slimming 后续
    - 或 `ISSLConnection` 核心 surface slimming / completion audit
  - 这些现在比继续清手工示例程序的 `ReadLn` 更接近“接口设计完整”这个总目标

- 继续对照最新提交与活跃文档后，一个新的 workflow drift 也已经很明确：
  - `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md` 仍把 `TSSLConfig` 写成默认 immediate next step
  - 但 `docs/reference/API_REFERENCE.md` 的 `ISSLConnection` 区块其实还停留在旧接口世界
  - 这会直接误导调用方，也会把后续会话拉回错误的路线优先级

- `docs/reference/API_REFERENCE.md` 当前在 `ISSLConnection` / `WinSSL Session 管理` 上的 active-doc drift 是真实且高价值的：
  - 文档仍承诺：
    - `GetCipherBits`
    - `VerifyPeerCertificate`
    - `GetSessionID`
    - `IsSessionResumed`
    - `GetSessionData`
    - `SetSessionData`
  - 但当前源码真相实际是：
    - `DoHandshake` / `IsHandshakeComplete` / `Renegotiate`
    - `WantRead` / `WantWrite` / `GetError`
    - `GetSelectedALPNProtocol`
    - `SetTimeout` / `GetTimeout`
    - `SetBlocking` / `GetBlocking`
    - `GetContext`
    - `GetStateString`
    - `GetSession` / `SetSession` / `IsSessionReused`
    - `GetVerifyResult` / `GetVerifyResultString`
    - `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus`

- 这个 drift 不只是“签名列表旧了”：
  - `GetNativeHandle` 已经不是核心 `ISSLConnection`，而属于可选 `ISSLNativeHandleAccess`
  - `ISSLConnectionInfo` 当前也已经承接：
    - `GetConnectionInfo`
    - `GetContext`
    - `GetSelectedALPNProtocol`
    - `GetStateString`
  - `WinSSL Session 管理` 示例里对 `ISSLSession` 的说明也还没跟上当前 `GetID` / `Serialize` / `Clone` surface

- 因此当前最安全也最值得立即落地的一批，不是直接动 `ISSLConnection` public signature，而是先做 `surface truth freeze`：
  - 把活跃文档修回当前源码真相
  - 把 compatibility-core mirrors 与 optional owners 说明写清楚
  - 用 focused shell contract 把旧接口名回流风险钉住

- 在沿着 `ISSLConnection` 主线继续下钻时，新的证据又把一个隐藏的工作流偏差暴露出来：
  - 我们原本准备从 `ISSLConnectionInfo` 这组 mirror 开第一刀
  - 但 live repo 重新核对后发现：
    - `ISSLConnectionInfo`
    - `ISSLSessionResumption`
    - `ISSLCertificateVerification`
    这几条连接层 optional surface 其实都已经有 execution result
  - 反而是另外 3 份更早的 connection-layer 旧计划还缺当前 execution receipt

- 这 3 条缺口都直接落在 `ISSLConnection` 主面，而不是外围文档噪音：
  - `backend-client-connection-sni-interface-alignment`
  - `backend-connection-native-handle-interface-alignment`
  - `backend-ocsp-connection-interface-alignment`
  - 如果不补，会继续制造“这些 connection surface 也许没真正验证过”的假象

- focused live revalidation 进一步证明这里的问题仍然是“证据缺口”，不是“实现缺口”：
  - `tests/contract/test_backend_contract.pas` 当前结果仍是：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`
  - `Contract 8` 当前 truth：
    - OpenSSL / WolfSSL / MbedTLS / FreePascal 都继续 PASS
    - WinSSL 继续按 Linux 主机边界 SKIP
  - `Contract 10` 当前 truth：
    - OpenSSL / WolfSSL / FreePascal 的 OCSP-capable connection 继续 PASS
    - MbedTLS 的 OCSP-absent 继续 PASS
    - WinSSL 继续 SKIP
  - `Contract 11` 当前 truth：
    - OpenSSL / WolfSSL / MbedTLS 的 native-handle surface 继续 PASS
    - FreePascal 的 absent 继续 PASS
    - WinSSL 继续 SKIP

- 因此连接层当前最准确的状态已经更明确：
  - 连接层 completion-audit contract 本身并不缺
  - 大部分连接层旧 plan 的 execution evidence 也不再缺
  - 真正残留的 connection-layer evidence gap，当前已经收缩到这 3 份旧 plan 的 focused receipt write-back
  - 这批补完之后，`ISSLConnection` 主线就可以更干净地转向真实的 `compatibility-core slimming`

- 在继续往 `ISSLConnection` slimming 主线推进时，又暴露出一个更纯粹的设计文档 drift：
  - `INTERFACE_DESIGN_V2.md` 里虽然已经在谈“最小 core + 扩展接口”
  - 但它自己对 `ISSLConnectionInfo` 这组 mirrors 的 owner 和迁移顺序并不自洽

- 当前 drift 主要集中在 4 个点：
  - 层次图漏掉 `ISSLConnectionInfo`
  - 仍保留 `ISSLAdvanced` 这个当前没有明确 public 落点的空壳名
  - `TBaseSSLConnection` 实现类示例没把 `ISSLConnectionInfo` 列进去
  - migration table 把 `GetConnectionInfo` 错归给 `ISSLDiagnostics`

- 更关键的是，设计文档在还没完成 Stage-A demotion 前，就过早把后续路线写死了：
  - `GetStateString` 直接写成“合并到 GetState”
  - `GetContext` 直接写成“通常不需要”
  - `GetSelectedALPNProtocol` 直接写成 `ISSLClientConnection`
  - 这会让下一批实现很容易跳过必要的中间层，直接做过度激进的收瘦

- 当前最安全的修法因此不是马上改 source，而是先冻结 Stage-A migration map：
  - 先承认这 4 个方法在当前 `v1.x` 里是 compatibility-core duplicates
  - 先把它们统一 demote 到 `ISSLConnectionInfo` 作为第一步设计锚点
  - 只有在这一步稳定后，才继续决定：
    - `GetSelectedALPNProtocol` 是否只留给客户端扩展
    - `GetStateString` 是否并入 `GetState`
    - `GetContext` 是否最终彻底退出 public surface

- 但仅有 migration map 还不够，因为 active docs 仍然会把用户拉回 core mirrors：
  - `API_REFERENCE.md` 还在示例 `LConn.GetConnectionInfo` / `LConn.GetSelectedALPNProtocol` / `LConn.GetStateString`
  - `INTEGRATION_GUIDE.md` 也还在直接教 `Conn.GetSelectedALPNProtocol` / `Conn.GetStateString`

- 这会形成一个典型的“设计和公开教学互相打架”的问题：
  - 设计文档在说“Stage A 先 demote 到 `ISSLConnectionInfo`”
  - 用户文档却还在教“直接从 core 上拿”
  - 后续一旦真的开始收 core，这类 active guidance 会立刻成为回流点

- 因此这条线的下一步应该是 active guidance de-emphasis，而不是再补一层抽象路线：
  - 先把用户可见示例统一成 `Supports(..., ISSLConnectionInfo, ...)`
  - 让公开教学路径开始与 Stage-A demotion map 同向
  - 然后再进入 source-facing slimming prep

- active guidance 对齐之后，source-facing gap 也变得更可见了：
  - 设计文档已经说清了 Stage-A map
  - 用户文档也已经开始优先走 `ISSLConnectionInfo`
  - 但 source 本身还没有明确说明这 4 个 mirrors 当前属于 `compatibility-core duplicates`

- 这会带来一个现实问题：
  - 下一批如果直接从 source 开刀，很容易又退回“这到底是正式 owner 还是偶然重复”的争论
  - 特别是 `src/fafafa.ssl.base.pas` 和 `src/fafafa.ssl.connection.base.pas` 本身还没把这件事写死

- 所以下一个安全动作是 source classification freeze：
  - 在 source comments 里把 Stage-A target 和 duplicate truth 写明
  - 让 source / 设计文档 / active docs 三层都对齐
  - 这样后续第一条真实实现切片才不会失去锚点

- source classification freeze 之后，再看 4 个 mirrors 的 live coupling，`GetContext` 已明显成为第一优先对象：
  - 活跃文档里只剩 `CAPABILITY_MATRIX_GUIDE.md` 一处仍直接教 `Conn.GetContext`
  - `API_REFERENCE.md` 虽然已经承认 `GetContext` 也由 `ISSLConnectionInfo` 暴露，但优先路径说明还没把它明确点出来
  - 生产源码里除 `TBaseSSLConnection.GetContext` 实现外，不再有额外活跃调用点

- 这和另外 3 个 mirrors 有明显差别：
  - `GetStateString` 还连着多份 integration/runtime 日志路径
  - `GetSelectedALPNProtocol` 带客户端 owner 语义
  - `GetConnectionInfo` 的使用面最广

- 因此当前最安全、也最有推进价值的动作不是直接碰 public signature，而是先收掉 `GetContext` 的 active guidance：
  - 让 capability 示例改走 `ISSLConnectionInfo.GetContext`
  - 把 API reference 的 first guidance 明确扩展到 `GetContext`
  - 这样下一批才适合进入 `GetContext` 的 source/class split feasibility

- `GetContext` active guidance 收掉之后，剩下最关键的 live coupling 就集中到了 contract 层：
  - `tests/contract/test_backend_contract.pas` 还在并列读取 `LConn.GetContext` 与 `LConnInfoAccess.GetContext`
  - 这让测试叙事看起来像在承认双 owner，而不是“optional owner + core mirror”

- 这也是为什么下一刀不该直接讨论 public deprecation：
  - 如果测试层还保留双 owner 话术，后续任何 `GetContext` 路线讨论都会继续模糊
  - 先把 contract 改成 `ISSLConnectionInfo.GetContext` 为主、`ISSLConnection.GetContext` 为 mirror proof，才算真正把 owner 语义压实

- 这一刀落下之后，`GetContext` 的 owner 语义终于在测试层也和路线图一致了：
  - `tests/contract/test_backend_contract.pas` 现在先验证 `ISSLConnectionInfo.GetContext` 对创建 context 的 owner truth
  - `ISSLConnection.GetContext` 只在 owner truth 通过后再做 mirror-equality proof
  - focused `test_backend_contract` 结果仍保持：
    - `Total Tests: 135`
    - `Passed: 111`
    - `Failed: 0`
    - `Skipped: 24`

- 这说明当前并没有引出新的 backend 行为漂移：
  - OpenSSL / WolfSSL / MbedTLS / FreePascal 的 connection-info contract 继续 PASS
  - WinSSL 继续保持 Linux 主机上的平台 skip truth
  - 因而下一批可以更放心地讨论 `GetContext` 的更强 feasibility / deprecation 路线，而不是回头怀疑 contract 是否已跟上

- 当前再做一层 source/class split 扫描后，`GetContext` 的 remaining live surface 已经小到足够直接 freeze：
  - 生产源码里没有新的 direct call dependency
  - 活跃文档只剩 `ConnInfo.GetContext`
  - non-script direct core `LConn.GetContext` 只剩 backend contract 的 mirror proof

- 这很关键，因为它意味着 `GetContext` 这条线已经不再是“还要继续考古”的状态：
  - 继续补 evidence 的收益已经很低
  - 更合理的下一步会是：
    - 要么真正进入 public deprecation wording route
    - 要么确认 `GetContext` 已足够干净，然后把主线切到下一条 mirror

- 把主线切到 `GetStateString` 后，最值钱的第一刀不是 backend runtime，而是普通测试路径：
  - `tests/connection/test_connection_basic.pas` 属于 generic smoke，仍直接教 `LConnection.GetStateString`
  - `tests/integration/test_real_https_connection.pas` 也还把 `Conn.GetStateString` 用作普通握手失败输出

- 这两类文件比 backend-specific runtime tests 更像“普通推荐路径”：
  - 它们更容易把新读者带回 core getter
  - 但又不需要先讨论 backend-specific owner 语义

- 所以 `GetStateString` 的当前最优路径是先做 active test de-emphasis：
  - 先把 generic/integration 测试切到 `ISSLConnectionInfo.GetStateString`
  - 让 residual direct core usage 收缩到 backend-specific runtime / contract 层
  - 然后再决定这些 residual 是做 allowlist freeze 还是继续 deeper migration

- 这批 focused 编译还顺手暴露出一个同文件的真实 drift，而且已经修掉：
  - `tests/connection/test_connection_basic.pas` 不只直接用了 `GetStateString`
  - 它还把 `GetNativeHandle` 当成 `ISSLConnection` 核心方法，并继续用 `FillChar` 初始化 `TSSLConfig`
  - 前者与当前 `ISSLNativeHandleAccess` truth 不一致，后者会误把 library-scoped logging 字段带进 `TSSLFactory.CreateContext(const AConfig)`

- 这类 drift 值得在同一批里一起修：
  - 因为它们都属于“普通 generic test 还在教旧 public surface”
  - 修完后 `test_connection_basic` 才重新变成可用的 focused proof，而不是一个本身就带着旧接口假设的弱信号

- `GetStateString` active-test de-emphasis 完成后，再看 residual 命中，范围已经很窄：
  - 普通测试和活跃文档都只剩 `ISSLConnectionInfo.GetStateString`
  - direct core `GetStateString` 只剩 backend contract mirror proof 与 OpenSSL / WolfSSL backend-specific runtime tests

- 这意味着 `GetStateString` 也已经到了适合直接 freeze allowlist 的时点：
  - 没必要再重复扫“普通路径到底还有没有 core getter”
  - 更合理的是把 residual file set 固定下来，然后把路线切到更强 wording 或下一条 mirror

- 把主线切到 `GetSelectedALPNProtocol` 后，第一刀同样不该先碰 backend-specific runtime：
  - `tests/integration/test_real_https_connection.pas` 还直接把 `Conn.GetSelectedALPNProtocol` 当普通握手成功路径
  - `tests/integration/test_cross_backend_consistency_contract.pas` 也还把 `Conn.GetSelectedALPNProtocol` 当归一化探测输出

- 这两个 ordinary integration/contract 文件比 backend-specific runtime ALPN tests 更像“公开推荐路径”：
  - 它们更容易把新读者带回 core getter
  - 但又不需要提前决定 `GetSelectedALPNProtocol` 最终是否只留给客户端扩展

- 所以 `GetSelectedALPNProtocol` 的当前最优路径，是先做 active test de-emphasis：
  - 先把 ordinary integration/contract 测试切到 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - 让 residual direct core usage 收缩到 backend-specific runtime / contract 层
  - 然后再决定这些 residual 是做 allowlist freeze，还是继续进入更强 client-owner 讨论

- 这批落下后，`GetSelectedALPNProtocol` 的 ordinary 测试路径已经和 Stage-A demotion map 对齐：
  - `tests/integration/test_real_https_connection.pas` 现在通过 helper 走 `ISSLConnectionInfo`
  - `tests/integration/test_cross_backend_consistency_contract.pas` 也不再直接读 core getter
  - 当前 residual direct-core `GetSelectedALPNProtocol` 已收缩到 backend contract mirror proof 与 MbedTLS/WinSSL backend-specific runtime files

- 当 `GetSelectedALPNProtocol` 的 residual surface 缩到这 4 个文件后，继续重复扫 ordinary 路径的收益已经很低：
  - `tests/contract/test_backend_contract.pas` 属于 connection-info mirror proof
  - `tests/mbedtls/test_mbedtls_alpn.pas` 与两份 WinSSL 测试都属于 backend-specific runtime residuals

- 这意味着 ALPN 这条线已经到了适合直接 freeze allowlist 的时点：
  - source comments 需要把 preferred-access / owner / residual-surface truth 写明
  - focused contract 需要把剩余 direct-core file set 固定下来
  - 做完后，这条线就不再需要反复拉起 residual archaeology

- 当前这层 freeze 落下后，`GetSelectedALPNProtocol` 的 design state 已经更明确：
  - ordinary docs/tests 全部优先走 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - direct core getter 只剩 contract mirror proof 与 backend-specific runtime residuals
  - 因而下一刀更适合讨论 stronger client-owner / deprecation wording，或者把主线切到 `GetConnectionInfo`

- `GetConnectionInfo` 和前 3 条 mirror 不一样，它在开始这批之前就已经没有 ordinary docs/tests 的 direct-core 教学残留：
  - direct core `GetConnectionInfo` 命中一开始就只剩 backend contract mirror proof 与 backend-specific OpenSSL/WinSSL files
  - 这意味着它不需要再走一轮 active-test de-emphasis，可以直接 freeze allowlist

- 当 `GetConnectionInfo` 的 residual surface 缩到这组 backend-specific / contract files 后，这条线也进入了和另外 3 条 mirror 相同的状态：
  - ordinary docs/tests 全部优先走 `ISSLConnectionInfo.GetConnectionInfo`
  - source comments 明确 preferred-access / owner / residual-surface truth
  - direct core getter 只剩 contract mirror proof 与 backend-specific runtime/contract residuals

- 到这一步，`ISSLConnectionInfo` 这 4 条 Stage-A mirror 路线已经全部完成 residual freeze：
  - `GetConnectionInfo`
  - `GetContext`
  - `GetSelectedALPNProtocol`
  - `GetStateString`
  - 因而主线应从“继续找残余”切换成“更强 owner/deprecation 决策”或“backend implementation-completeness 审查”

- 当前把主线真正切到 backend implementation-completeness 审查后，`GetConnectionInfo` 立刻暴露出一个共享层缺口：
  - `TSSLConnectionInfo` 活跃文档仍容易给人“完整信息”的印象
  - 但共享基类 `TBaseSSLConnection.GetConnectionInfo` 之前只填最小字段
  - 这不是某个 backend override 漏写，而是 shared layer 没有把已存在的 connection metadata 折进 record

- 这批最小且真实的共享层补齐点已经明确并落地：
  - `ServerName`
    - 对所有 client-capable backend 来说，连接对象自身已经持有 `FServerName`
    - 共享 `GetConnectionInfo` 可以安全补齐，不需要等待 backend-specific cipher/path 信息
  - `SessionId`
    - 对已经 connected / handshake-complete 且当前 session 可用的连接，`ISSLSession.GetID` 已足够作为 shared metadata 来源
    - 因而它也适合在 shared layer 补齐，而不是留空等 backend override

- 这次实现还确认了一个重要的 Pascal/接口引用计数边界：
  - 不应在 `TBaseSSLConnection.GetConnectionInfo` 中直接对 `Self` 执行 `Supports(Self, ISSLClientConnection, ...)`
  - `TBaseSSLConnection` 与 backend connection 都是 `TInterfacedObject`
  - 多份 focused test 会直接以 concrete object 实例创建连接，而不是先拿 interface ref
  - 在这种路径下，临时 interface 引用可能把对象推入错误的 `_Release` 生命周期，从而触发 fresh-connection `EAccessViolation`
  - 更安全的 shared design 是让基类走 protected virtual hook，由各 backend override 返回已有 `FServerName`

- 当前这条 `GetConnectionInfo` completeness 路线已经不再需要反复验证“ServerName/SessionId 到底能不能从共享层拿”：
  - focused mock contract 已证明 `ConnectionInfo.ServerName` 会镜像 `ISSLClientConnection.GetServerName`
  - focused OpenSSL cipher guard contract 已证明这次 shared enrichment 没有重新引入 fresh-connection AV
  - 因此下一批真正剩下的不是这两项 metadata，而是：
    - `PeerCertificate`
    - backend-specific crypto detail fields：
      - `CipherSuiteId`
      - `KeyExchange`
      - `Cipher`
      - `Hash`
      - `KeySize`
      - `MacSize`

- 继续沿着 implementation-completeness 主线盘点后，`PeerCertificate` 被证实和 `ServerName` / `SessionId` 属于同一类共享层缺口：
  - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 都已经实现了 `DoGetPeerCertificate`
  - 各 backend 的 `ISSLCertificate.GetInfo` 也都已经存在
  - 当前只有 WinSSL override 会显式把 `PeerCertificate` 写回 `TSSLConnectionInfo`
  - 因而这不是“底层能力不够”，而是 shared `GetConnectionInfo` 还没有统一折叠当前对端证书信息

- 这使得 `PeerCertificate` 成为当前最值得优先修的 completeness 字段：
  - 它不要求先做每个 backend 的 cipher ID / kex / hash 枚举映射
  - 它也不需要引入新的 backend-specific runtime patch
  - shared base 只要在 `GetPeerCertificate <> nil` 时取 `GetInfo`，就能让非 WinSSL 路径不再继续空着

- focused mock proof 现在已经把这条 shared truth 钉住：
  - `ConnectionInfo.PeerCertificate.Subject` 会镜像 `ISSLCertificate.GetInfo.Subject`
  - `ConnectionInfo.PeerCertificate.Issuer` 会镜像 `ISSLCertificate.GetInfo.Issuer`
  - 这说明 `PeerCertificate` 不再只是 WinSSL override 局部拥有的 metadata，而是共享 `GetConnectionInfo` 的通用输出

- 当前 `GetConnectionInfo` 剩余的 completeness debt 因而又收缩了一层：
  - 已经由 shared layer 补齐：
    - `ServerName`
    - `SessionId`
    - `PeerCertificate`
  - 真正还需要 backend-specific 审查/修补的只剩：
    - `CipherSuiteId`
    - `KeyExchange`
    - `Cipher`
    - `Hash`
    - `KeySize`
    - `MacSize`

- 对剩余 6 个 crypto detail 字段做静态盘点后，shared-vs-backend 的边界已经更清楚了：
  - `CipherSuiteId` / `MacSize`
    - 当前更偏底层库/平台专属 detail
    - WinSSL override 已经直接掌握这些信息
    - OpenSSL 也有部分低层来源，但这条线不适合先做 shared 归一
  - `Cipher` / `Hash` / `KeySize`
    - 在 OpenSSL / MbedTLS / WolfSSL / FreePascal 上，很多时候已经能从 negotiated `CipherSuite` 名称稳定推导
    - 因而它们更适合作为 implementation-completeness 的 first shared slice
  - `KeyExchange`
    - 只在 cipher-suite name 仍显式携带 legacy prefix 时适合 shared 推导
    - TLS 1.3 标准名字本身不会给出这项 detail，因此不应在 shared layer 里假装“总能推出来”

- 这使得当前最稳妥的修法不再是“一次补全 6 项”，而是先做 name-derived normalization：
  - `Cipher`
  - `Hash`
  - `KeySize`
  - 以及带 legacy prefix 时的 `KeyExchange`

- focused mock proof 现在已经把这条新 shared truth 钉住：
  - 对 negotiated cipher-suite name `ECDHE-RSA-AES128-GCM-SHA256`
  - `ConnectionInfo.KeyExchange` 会推导为 `sslKexECDHE_RSA`
  - `ConnectionInfo.Cipher` 会推导为 `sslCipherAES128GCM`
  - `ConnectionInfo.Hash` 会推导为 `sslHashSHA256`
  - `ConnectionInfo.KeySize` 会推导为 `128`

- 因而当前 `GetConnectionInfo` implementation-completeness 主线又收缩了一层：
  - 已由 shared layer 补齐：
    - `ServerName`
    - `SessionId`
    - `PeerCertificate`
    - `Cipher`
    - `Hash`
    - `KeySize`
    - 以及 legacy-prefix case 的 `KeyExchange`
  - 更值得继续深入的剩余问题已经缩到：
    - `CipherSuiteId`
    - `MacSize`
    - 以及无法只靠名字稳定推导的更细平台差异

- `CipherSuiteId` 之所以适合作为下一刀，是因为它已经具备了比 `MacSize` 更强的双重 truth：
  - shared layer 能对标准 TLS 1.3 suite name 稳定推导 `CipherSuiteId`
  - OpenSSL 也能通过 low-level helper 给出直接值
  - 这使它适合先形成一个 shared + backend 双闭环，而不是继续停留在“待盘点”状态

- 这批还确认了一个具体的 OpenSSL API 结构性小坑：
  - `TSSL_CIPHER_get_protocol_id` 之前只在 `fafafa.ssl.openssl.api.core` 中出现了类型声明
  - 但 `TOpenSSLConnection` 实际使用的 active loader / var export 路径在 `fafafa.ssl.openssl.api.ssl`
  - 结果就是 connection 层一旦直接引用 `SSL_CIPHER_get_protocol_id`，会先遇到编译时符号缺口
  - 因而真正需要补的是 `api.ssl` 的导出与加载链，而不是只在 connection unit 侧增加 `uses`

- OpenSSL focused guard 也暴露出一个值得记录的 contract 细节：
  - 旧测试用 `StubSSLGetCurrentCipherNonNil` 返回假指针 `Pointer(1)` 来模拟 “有 current cipher，但 helper 缺失”
  - 在引入 `CipherSuiteId` low-level helper 之后，如果仍保留真实 `SSL_CIPHER_get_protocol_id` / `SSL_CIPHER_get_id`，测试会因为对假指针做 low-level 解引用而触发 AV
  - 这不是产品路径新崩溃，而是 contract 需要同步扩展：
    - 当测试场景声明 cipher helper unavailable 时，也必须一并置空 `protocol_id` / `get_id`
  - 单独的 truth contract 再去证明 low-level helper 可用时的 `CipherSuiteId` 回填行为

- 因而当前 `GetConnectionInfo` implementation-completeness 主线再往下推进时，最合理的剩余焦点已经变成：
  - `MacSize`
  - 以及无法只靠名字或统一 low-level helper 稳定归一的更细平台差异

- 在继续盘 `MacSize` 时，静态审查暴露出一个比“字段未统一”更先要修的 WinSSL truth bug：
  - `TSecPkgContext_ConnectionInfo.aiCipher` 在仓库定义里明确是“加密算法 ID”
  - 同一份 WinSSL 代码也一直把它当算法字段来生成 cipher 名称和 `Cipher` 枚举
  - 但 `GetConnectionInfo` 之前却直接执行：
    - `Result.CipherSuiteId := Word(ConnInfo.aiCipher)`
  - 这说明当前问题不是“WinSSL 还没补 suite id”，而是“已经把错误来源写进了 suite id”

- 这也修正了我们上一批对 WinSSL 的一个过度乐观假设：
  - 之前把 WinSSL 视为“已经直接掌握 `CipherSuiteId` / `MacSize` 的 backend”
  - 但从当前静态证据看：
    - WinSSL 通过 `SECPKG_ATTR_CONNECTION_INFO` 掌握的是算法级字段
    - 真实 cipher-suite id/name 更应该走 Schannel `SECPKG_ATTR_CIPHER_INFO`
  - 因而 WinSSL 这条线需要先做 truth correction，不能直接拿旧实现当 completeness 参考

- 这次 WinSSL 修复后的更准确结论是：
  - `CipherSuiteId`
    - shared TLS 1.3 路径已有 name-derived truth
    - OpenSSL 路径已有 low-level truth
    - WinSSL 路径现在也改为官方 `CipherInfo.dwCipherSuite` truth
  - `MacSize`
    - 当前全仓只有 WinSSL 在填值
    - 但它用的是 `dwHashStrength div 8`
    - 这更像 hash-strength proxy，而不是已经跨 backend 统一定义好的“记录层 MAC/tag 长度”
  - 因而下一批不该直接照着 WinSSL 现值去扩散实现，而应先把语义矩阵盘清楚

- `MacSize` 语义矩阵现在已经盘清到一个可稳定复用的层次：
  - shared layer 之前确实完全没有统一 `MacSize`
  - OpenSSL / FreePascal / MbedTLS / WolfSSL 主要都依赖 shared `GetConnectionInfo`
  - WinSSL 则独自把 `dwHashStrength div 8` 写进 `MacSize`
  - 因而“WinSSL 有值、其他 backend 没值”并不代表 WinSSL 更接近统一 truth，只代表它先填了一个 backend-local proxy

- 当前最安全、也最能跨 backend 复用的收法已经验证成立：
  - 对可识别 AEAD suite name，shared layer 可以稳定给出 auth-tag 长度：
    - `GCM` / `POLY1305` / `OCB` / `CCM` -> `16`
    - `CCM_8` -> `8`
  - 这条共享语义同时覆盖：
    - OpenSSL
    - FreePascal
    - MbedTLS
    - WolfSSL
    - 以及改成 inherited-first 之后的 WinSSL

- WinSSL 因而也不该再把 `dwHashStrength div 8` 当作无条件 truth：
  - 它现在更适合扮演：
    - shared path 已经识别出 AEAD suite 时不覆盖
    - shared path 没有稳定 `MacSize` 时的 legacy fallback
  - 这把 WinSSL 的 `MacSize` 从“可能误导 AEAD/TLS1.3 的主值”降格成了“缺省保底的 backend-local best-effort”

- 这次收口也把 `MacSize` 主线的剩余边界说清楚了：
  - 还没统一的不是 AEAD/TLS 1.3 这组场景
  - 而是 legacy non-AEAD suites 是否值得继续补更强 low-level `MacSize` truth
  - 如果后续不想继续在 connection-info completeness 上深挖，就可以比较放心地把主线切回 owner / deprecation wording route

- OpenSSL 这条 legacy/non-AEAD `MacSize` 路径现在也已经从“理论上能做”落成了真实实现：
  - 在这批之前，OpenSSL connection-info 虽然已经能拿到 current cipher，但并没有把：
    - `SSL_CIPHER_is_aead`
    - `SSL_CIPHER_get_digest_nid`
    - `EVP_get_digestbynid`
    - `EVP_MD_size`
    这组能力接进 `MacSize`
  - 而且问题不只在 connection unit：
    - `api.ssl` 的 active export/binding path 缺 `is_aead` / `digest_nid`
    - `api.evp` 的 active export/binding path 缺 `EVP_get_digestbynid`

- 当前修完后的更准确结论是：
  - shared 仍然是 AEAD `MacSize` 的第一 owner
  - OpenSSL 现在额外补齐了 legacy/non-AEAD digest truth
  - 因而 OpenSSL 已经不再属于“legacy `MacSize` 完全空白”的 backend

- 这也进一步缩小了剩余未统一面：
  - 高价值未完成项不再是 “OpenSSL 要不要补”
  - 而是：
    - WinSSL fallback 是否还值得强化
    - MbedTLS / WolfSSL 是否有同等级 low-level source
  - 如果这几条静态盘点后收益不高，就应该把默认主线切回 owner / deprecation wording，而不是继续在 `MacSize` 这一个字段上无限细抠

- WolfSSL 这条 legacy/non-AEAD `MacSize` 路径现在也已经从“头文件里有线索”落成了真实实现：
  - active binding 现在已经把：
    - `wolfSSL_GetHmacSize`
    接进 `TWolfSSLConnection.GetConnectionInfo`
  - 而且保持了与前两批一致的 owner discipline：
    - shared 仍然是 AEAD `MacSize` 第一 owner
    - WolfSSL 只在 shared path 仍未给值时，才回退到 backend-local HMAC truth

- 这次 WolfSSL focused proof 也补出了一条值得记住的测试前置条件：
  - optional backend 的 focused contract 不能只引 `wolfssl.api` / `wolfssl.connection`
  - 如果测试要经过 `TSSLFactory.CreateContext(..., sslWolfSSL)`，还必须：
    - define `ENABLE_WOLFSSL`
    - `uses fafafa.ssl.wolfssl.lib`
  - 否则工厂按设计会直接拒绝：
    - `WolfSSL backend is not enabled (define ENABLE_WOLFSSL)`
  - 这不是产品 bug，而是 optional backend registration truth，后续不该再重复误判

- 当前修完后的更准确结论是：
  - shared 仍然拥有 AEAD `MacSize` truth
  - OpenSSL 现在拥有 legacy digest truth
  - WolfSSL 现在拥有 legacy HMAC truth
  - WinSSL 保留 guarded fallback

- 因而当前 `GetConnectionInfo` implementation-completeness 主线剩余的 `MacSize` 面，又进一步缩小到了：
  - MbedTLS 是否也有值得接入的 low-level source
  - 如果 MbedTLS 实现成本高或真相不够稳，就该收住这条线并切回 owner / deprecation wording route

- MbedTLS 这条 `GetConnectionInfo` 路径现在也已经从“头文件里有 source，但仓库没接 runtime truth”落成了真实实现：
  - active binding 现在已经把：
    - `mbedtls_ssl_get_ciphersuite_id`
    - `mbedtls_ssl_get_ciphersuite_id_from_ssl`
    - `mbedtls_ssl_ciphersuite_from_id`
    - `mbedtls_ssl_ciphersuite_get_cipher_key_bitlen`
    接进 `TMbedTLSConnection.GetConnectionInfo`
  - MbedTLS 现在不只补 `MacSize`：
    - 也补了更稳的 `CipherSuiteId`
    - 以及 `KeySize`

- 这批还暴露并修正了一个更底层的 MbedTLS interface truth bug：
  - `src/fafafa.ssl.mbedtls.base.pas` 里原先把：
    - `MBEDTLS_MD_SHA1`
    - `MBEDTLS_MD_RIPEMD160`
    的枚举值写反了
  - 这不一定会立刻打出长度错误，因为 SHA1 / RIPEMD160 都是 20 字节
  - 但它会让任何依赖 `mbedtls_md_info_from_type(MBEDTLS_MD_SHA1)` 的真实摘要路径走到错误算法
  - 当前 focused runtime proof 已用 canonical SHA1(`abc`) 把这条常量真相钉住

- 这批还顺手确认了一个 shared completeness gap：
  - shared cipher-suite parser 虽然已经能理解很多 OpenSSL/WolfSSL 风格名字
  - 但对 MbedTLS 常见的连字符命名：
    - `TLS-RSA-...`
    - `AES-128[-GCM]`
    - `AES-256[-GCM]`
    之前并不完整
  - 当前已经补齐
  - 因而 helpers unavailable 时，MbedTLS 的 shared baseline 也比之前更接近统一 truth

- 当前修完后的更准确结论是：
  - shared 继续拥有 AEAD `MacSize` truth
  - OpenSSL 现在拥有 legacy digest truth
  - WolfSSL 现在拥有 legacy HMAC truth
  - MbedTLS 现在拥有 ciphersuite-info + digest truth
  - WinSSL 保留 guarded fallback

- 因而当前 `GetConnectionInfo` implementation-completeness 这条 backend-truth 路线，已经接近一个自然收口点：
  - 下一步更应该做一次 completion audit
  - 确认 FreePascal 是否仍然存在必须单独补的缺口
  - 如果没有，就该把默认主线切回 owner / deprecation wording route，而不是继续机械地往每个 backend 里找新 helper

- `FreePascal` completion audit 现在已经把这个“自然收口点”真正坐实了：
  - `TFreePascalConnection` 没有 dedicated `GetConnectionInfo` override
  - 它当前的 backend truth 主要是：
    - client / server runtime 把 negotiated TLS 1.3 suite 写成标准 `FCipherName`
    - session / resumption state 保留 `FCipherSuite: Word`
  - focused server/client proof 进一步证明 shared `GetConnectionInfo` 已能稳定补齐：
    - `CipherSuiteId`
    - `KeySize`
    - `MacSize`
    - `ServerName`
    - `SessionId`
    - `IsResumed`

- 因而当前更准确的 route-level 结论是：
  - `FreePascal` 不需要像 `OpenSSL` / `WolfSSL` / `MbedTLS` 那样继续补 backend-local helper
  - 当前 `GetConnectionInfo` implementation-completeness 主线已经可以视为基本完成
  - 默认主线应切回 owner / deprecation wording route，而不是继续按 backend 名单机械深挖

- 顺着这条主线继续审查后，又暴露出一个更偏 workflow 的真问题：
  - `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
    之前仍假定 direct core `GetConnectionInfo` 只剩 10 个命中
  - 但后续 completeness / proof 批次已经把这个 residual surface 扩张到了 15 个命中
  - 这说明路线图说“`ISSLConnectionInfo` 是 owner”，测试面却还在默默扩大 direct core getter 的使用

- 当前修法没有去“改大数字掩盖问题”，而是把 owner/mirror 路线真正收紧：
  - `tests/contract/test_backend_contract.pas` 的 `Contract 19` 现在先验证：
    - `ISSLConnectionInfo.GetConnectionInfo`
  - 再验证：
    - `ISSLConnection.GetConnectionInfo`
      只是 mirror
  - FreePascal / OpenSSL / WolfSSL / MbedTLS 的 completeness proof 与 shared builder proof
    也都改成优先走 `ISSLConnectionInfo`

- 这批还顺手确认了一个测试层面的生命周期坑：
  - 在 OpenSSL / WolfSSL focused tests 里，
    concrete connection object 一旦被 `ISSLConnectionInfo` 接口引用接管，
    就不能再继续走手工 `Free`
  - 否则会在测试自身打出 `EInvalidPointer` / `EAccessViolation`
  - 当前已经把这些 helper 收成“接口接管生命周期，失败分支才手工释放”的模式

- 因而当前更准确的 `GetConnectionInfo` route 结论是：
  - stale residual allowlist 已被修正
  - residual direct-core surface 已缩回真正值得保留的少量 mirror/core-surface proof
  - 下一步应进入更强的 owner / deprecation wording route，
    而不是继续容忍普通 completeness proof 留在 direct core getter 上

- 继续静态审剩余 residual 之后，WinSSL 这两份 direct-core 文件的性质也已经更清楚：
  - `tests/winssl/test_winssl_connection_info.pas`
  - `tests/winssl/test_winssl_connection_edge_cases.pas`
  - 它们不是普通 completeness proof 漏迁移
  - 它们实际在覆盖的是 WinSSL compatibility-core surface 本身：
    - `GetConnectionInfo`
    - `GetProtocolVersion`
    - `GetCipherName`
    的 direct core 行为与一致性

- 因而当前更准确的收口方式不是继续把这两份文件也迁到 `ISSLConnectionInfo`：
  - 而是把它们显式归类为：
    - `INTENTIONAL_CORE_SURFACE`
  - 然后用 focused source contract 守住这条边界

- 这也把 `GetConnectionInfo` route 的残余不确定性进一步降到最小：
  - backend contract 负责 mirror/owner proof
  - WinSSL residual files 负责 intentional direct-core surface proof
  - 默认下一步就可以直接进入更强的 wording / deprecation 路线，而不必再在 residual 分类上打转

- 顺着这条 route 再看 source/doc truth，当前剩下的真实问题已经不是实现漂移，而是公开表述仍然偏弱：
  - `src/fafafa.ssl.base.pas` 虽然已经写了 preferred-access / compatibility note
  - 但 `API_REFERENCE` 与 `INTERFACE_DESIGN_V2` 仍可能让读者把 `ISSLConnection.GetConnectionInfo` 理解成“还在 core 上，所以还是正常主入口”
  - 尤其 `INTERFACE_DESIGN_V2` 的旧例子只写了 `LConn.GetConnectionInfo;  // 仍然存在`

- 这类漂移不需要动 runtime/ABI 就能修：
  - 只要把 source comment、公开 API 文档、v2 迁移文档统一成更强的 owner/mirror wording
  - 再用 focused shell contract 守住
  - 就能避免后续从“它还在 core 里”这个误区重新拉起路线讨论

- 当前修完后的更准确结论是：
  - `ISSLConnectionInfo.GetConnectionInfo` 已经不只是“推荐路径”
  - 它现在在 source/doc truth 上都明确成为默认 owner
  - `ISSLConnection.GetConnectionInfo` 则只作为 `v1.x` compatibility-core mirror 保留

- 因而 `GetConnectionInfo` 路线下一步不该再重复做 wording 清扫：
  - 真正还剩的路线问题已经变成：
    - 是否要进入第一条 public slimming slice
    - 以及这条 slice 是 compiler-level deprecation feasibility，还是到此为止后转去下一条 mirror

- 对这条 feasibility 再继续静态审后，结论已经足够明确：
  - production source 当前没有继续扩散 `.GetConnectionInfo(...)` 调用
  - active docs 也已经不再把它当主入口
  - direct core residual 调用只剩：
    - backend contract mirror proof
    - WinSSL intentional core-surface tests
  - 因而 `ISSLConnection.GetConnectionInfo` 进入 compiler-level deprecation 是可行的，而不是会打穿普通实现面的大动作

- 当前这一刀真正需要处理的风险不是 runtime，而是 compile noise：
  - 如果直接把声明标成 `deprecated`，intentional residual tests 会重新开始吐 warning
  - 但这些 residual 文件已经足够小，完全可以像 `.WithSNI(...)` 一样做局部 warning quarantine

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetConnectionInfo` 现在在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - 这条 getter 的第一条真正 public slimming slice 已经落地
  - 后续不该再在它身上反复做 wording/deprecation archaeology，而应切去下一条 mirror 的 feasibility / slimming 选择

- `GetContext` 这条线在这一轮之前，其实已经比 `GetConnectionInfo` 更“干净”：
  - active docs 不再教 `Conn.GetContext`
  - owner primacy 已固定为 `ISSLConnectionInfo.GetContext`
  - source/class split allowlist 也已经 freeze 到只剩：
    - core/interface declarations
    - shared base implementation
    - backend contract mirror proof

- 这意味着它真正还缺的不是新的实现迁移，而是最后一层 compiler-surface truth：
  - public declaration 自身还不是 compiler `deprecated`
  - 因而 source/doc 虽然已经说它只是 compatibility mirror，但编译器层面还没有把这件事说死

- 当前这一刀需要处理的风险也很小：
  - non-script direct core `GetContext` 已只剩 backend contract 一处
  - 因而只要给这处 mirror proof 做本地 warning quarantine，就不会像更大的 surface 那样带来到处补 suppression 的副作用

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetContext` 现在也已经在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - `GetContext` 的第一条真正 public slimming slice 已经落地
  - 后续不该再在它身上反复做 wording/deprecation archaeology，而应把主线切去下一条 mirror 的 feasibility / slimming 选择

- `GetStateString` 这条线在进入 compiler deprecation 之前，其实也已经足够“干净”：
  - active generic/integration tests 已经切到 `ISSLConnectionInfo.GetStateString`
  - residual direct-core surface 已 freeze 到：
    - backend contract mirror proof
    - OpenSSL server OCSP stapling runtime proof
    - WolfSSL server OCSP stapling runtime proof
  - ordinary docs/tests 已不再把 `Conn.GetStateString` 当推荐路径

- 这意味着它真正还缺的，也不是新的 backend/runtime 迁移，而是最后一层 compiler-surface truth：
  - source/doc 虽然已经写明 owner 是 `ISSLConnectionInfo.GetStateString`
  - 但 public core declaration 自身还没进入 compiler `deprecated`
  - 因而编译器层面还没有把“compatibility-only mirror”这件事彻底说死

- 当前这一刀的风险同样是 compile noise，而不是 runtime 行为：
  - non-script direct core `GetStateString` 已只剩 3 个 residual 文件
  - 它们都属于 intentional mirror/runtime proof，不需要迁移行为
  - 因而完全适合沿用前两批模式：
    - declaration 做 compiler `deprecated`
    - residual callsite 做局部 warning quarantine
    - 用 focused shell contract + backend contract proof 收口

- 因而当前更准确的路线判断是：
  - `GetStateString` 的下一步不是继续做 residual archaeology
  - 而是直接进入 compiler deprecation alignment
  - 做完之后，这条 getter 也应像 `GetConnectionInfo` / `GetContext` 一样，退出反复拉起的 wording/compiler 治理队列

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetStateString` 现在也已经在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - `GetStateString` 的第一条真正 public slimming slice 已经落地
  - 这条线后续不该再继续做 wording/deprecation archaeology，而应把主线切去下一条 mirror 的 feasibility / slimming 选择

- 这批还顺手暴露出一个 workflow 层的小真相：
  - `tests/scripts/test_isslconnectioninfo_active_guidance_contract.sh`
    仍依赖一条较旧的共享 guidance 句式：
    - “如果你在写新代码，并且需要连接信息 / ALPN / 状态字符串这组 mirrors”
  - 我们在强化 `GetContext` 时把总句拆散了，但脚本契约还没跟着调整
  - 当前做法不是弱化新文案，而是补回这条总句，同时保留单独的 `GetContext` / `GetStateString` compiler-deprecated guidance

- `GetSelectedALPNProtocol` 这条线在进入 compiler deprecation 之前，也已经足够“干净”：
  - active integration/contract tests 已经切到 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - residual direct-core surface 已 freeze 到：
    - backend contract mirror proof
    - MbedTLS runtime proof
    - WinSSL ALPN/SNI runtime proof
    - WinSSL edge-case runtime proof
  - ordinary docs/tests 已不再把 `Conn.GetSelectedALPNProtocol` 当推荐路径

- 这意味着它真正还缺的，也不是新的 backend/runtime 迁移，而是最后一层 compiler-surface truth：
  - source/doc 虽然已经写明 owner 是 `ISSLConnectionInfo.GetSelectedALPNProtocol`
  - 但 public core declaration 自身还没进入 compiler `deprecated`
  - 因而编译器层面还没有把“compatibility-only mirror”这件事彻底说死

- 当前这一刀的风险同样是 compile noise，而不是 runtime 行为：
  - non-script direct core `GetSelectedALPNProtocol` 已只剩 4 个 residual 文件
  - 它们都属于 intentional mirror/runtime proof，不需要迁移行为
  - 因而完全适合沿用前面三批模式：
    - declaration 做 compiler `deprecated`
    - residual callsite 做局部 warning quarantine
    - 用 focused shell contract + backend contract proof 收口

- 这批还顺手暴露出第二条 workflow 层的小真相：
  - `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
    一直还锁着 4 条 mirrors 的旧迁移表文案
  - 之前因为没重跑它，这个漂移一直潜伏着
  - 当前修法不是只改 ALPN 一条，而是把 4 条 mirror row 的 required truth 一起同步到当前 compiler-deprecated wording，避免后续再被旧契约反复误拦

- 当前修完后的更准确结论是：
  - `ISSLConnection.GetSelectedALPNProtocol` 现在也已经在 source/doc/compiler 三层都被明确定义为 compatibility-only mirror
  - `GetSelectedALPNProtocol` 的第一条真正 public slimming slice 已经落地
  - 到这一步，`ISSLConnectionInfo` 这 4 条 Stage-A mirrors 都已经完成 compiler-surface 收口
  - 因而下一步不该再继续做 mirror wording/deprecation archaeology，而应把主线切回 interface-design completeness / implementation-completeness 审查
