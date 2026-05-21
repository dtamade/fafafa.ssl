# fafafa.ssl 接口设计审查

**状态:** REFRESHED_STATIC_AUDIT
**范围:** 公开 Pascal 接口、工厂、builder、门面单元与文档对齐  
**方式:** 静态审查，已按 2026-05-19 当前源码/活跃文档真相刷新

## 结论
当前接口层不是“能不能用”的问题，而是“历史兼容表面和当前推荐路径并存，容易把设计判断带偏”。

相比最初审计，context-level SNI 传播和 `ISSLServerConnection` 文档失真这两条 live drift 已明显收窄。
当前最值得继续压的活跃问题，主要集中在三类：
- 核心 `ISSLConnection` 仍然太胖
- `TSSLConfig` 仍然是 mixed-scope public record
- 门面单元仍同时导出多条历史路径

另外还有三条已经从 live drift 收成 compatibility / asymmetry baggage 的边界，仍需要在路线判断里明确记住：
- context-level SNI 家族当前已经是 warning/reject/ignore 的 frozen compatibility surface
- `ISSLServerConnection` 当前不再是活跃文档失真，但 server-side 对称扩展仍然缺位
- capability public surface 的 runtime/source truth 已经不再是未分类的双真相冲突。
- paired feature 当前已经收敛到 support-level-first；legacy `Supports*` 更接近 compatibility projection baggage。

## 主要问题

### 1. 核心连接接口过胖，optional 分层失效
**证据**
- `src/fafafa.ssl.base.pas:1141-1317`
- `docs/reference/INTERFACE_DESIGN_V2.md:23-62`
- `docs/reference/INTERFACE_DESIGN_V2.md:200-225`

`ISSLConnection` 里已经直接包含：
- 会话复用：`GetSession` / `SetSession` / `IsSessionReused`
- 证书验证详情：`GetPeerCertificateChain` / `GetVerifyResult` / `GetVerifyResultString`
- 诊断：`GetHealthStatus` / `IsHealthy` / `GetDiagnosticInfo` / `GetPerformanceMetrics`
- OCSP：`GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus`
- 连接信息：`GetConnectionInfo` / `GetContext` / `GetStateString`

这些能力又被拆成了独立 optional interface。结果是“可选接口”只是第二份入口，分层意义基本被掏空。

**影响**
- 第三方框架无法靠接口本身判断真正的最小能力集。
- 后续新增能力只会继续把 core 撑大。
- `Supports(...)` 变成形式化装饰，而不是清晰的 capability gate。

**建议**
- 继续保留 `ISSLConnection` 作为最小 core，只保留连接生命周期、读写、非阻塞状态、协议结果和原生句柄。
- 把诊断、会话、证书验证、OCSP、连接信息全部移到扩展接口或包装层。
- `GetStateString` / `GetContext` 这类已进入 compatibility-mirror 路线的方法，继续优先往 owner surface 收口。
- 但对 `ReadString` / `WriteString` / `SetTimeout` / `SetBlocking` 这组 convenience 方法，当前更准确的 shipped truth 是：
  - `v1.5.0` source 仍正式保留它们，builder 与活跃 guides 也仍在使用
  - 当前应先完成 classification / recommendation truth 收口，而不是把它们误报成“源码已经移除”
  - 如果未来真的要把它们退出 core，应作为单独的 v2 API surgery 批次推进

### 2. context-level SNI 已冻结成 compatibility-only surface，但 public API 仍背着历史包袱
**证据**
- `src/fafafa.ssl.base.pas:1042-1052`
- `src/fafafa.ssl.factory.pas:495-528, 1145-1193`
- `src/fafafa.ssl.context.builder.pas:73-77, 1514-1523`
- `src/fafafa.ssl.openssl.backed.pas:1315-1382`
- `src/fafafa.ssl.winssl.lib.pas:803-852`
- `src/fafafa.ssl.mbedtls.lib.pas:641-688`
- `src/fafafa.ssl.wolfssl.lib.pas:607-663`
- `src/fafafa.ssl.freepascal.lib.pas:1616-1663`

`ISSLContext.SetServerName` 已经明确标成 deprecated，推荐路径是 `ISSLClientConnection.SetServerName`。
但当前高层主路径已经不是“继续主动传播旧语义”：
- `TSSLFactory.CreateContext(...)` 对 `TSSLConfig.ServerName` 现在是 warning + ignore
- `TSSLContextBuilder.WithSNI(...)` 已经是 compile-time deprecated compatibility-only fluent surface
- `BuildClient` / `BuildServer` 当前都是 warning + ignore
- direct-library `CreateContext(...)` 当前也已经统一成：
  - server-side reject
  - client-side warning + ignore

高层 factory / builder 主路径现在已经是 warning + ignore，不再把 `ServerName` 写回新建 context。

**影响**
- 这条线已经不再是 live drift，但 public surface 仍然带着一整家 deprecated compatibility API。
- 新调用方如果只看类型名，仍然可能误以为 context-level SNI 是普通配置路径。
- 路线讨论时如果忽略最近几轮收口，很容易把已经冻结的 compatibility surface 误判成“当前实现缺口”。

**建议**
- 把这条线视为 `v1.x` frozen compatibility surface，而不是当前待恢复的推荐入口。
- 新代码继续统一走：
  - `ISSLClientConnection.SetServerName(...)`
  - `TSSLConnectionBuilder.WithHostname(...)`
  - `TSSLConnector.Connect*(..., ServerName)`
- 如果未来要继续收紧，应该作为独立 `v2` public-surface surgery 处理，而不是回头重开“factory/builder 还在主动传播”的旧问题。

### 3. 对称 server 扩展仍缺位，但活跃文档已不再假装 `ISSLServerConnection` 存在
**证据**
- `docs/ARCHITECTURE.md:141-150`
- `docs/reference/INTERFACE_DESIGN_V2.md:17-28`
- 源码搜索没有发现任何 `ISSLServerConnection` 声明或实现

当前 public Pascal surface 仍然只有 `ISSLClientConnection`。
但与最初审计不同，活跃架构/设计文档已经不再承诺这个接口“应该已经存在”：
- `docs/ARCHITECTURE.md` 已明确写出当前只声明了 `ISSLClientConnection`
- `docs/reference/INTERFACE_DESIGN_V2.md` 也已明确注明当前 public source 尚未声明 `ISSLServerConnection`

活跃架构/设计文档现在已经显式说明当前 public Pascal source 尚未声明 `ISSLServerConnection`。

**影响**
- 这已经不再是“文档比代码更对称”的 active docs drift。
- 但 public surface 仍然只有 client-side 显式扩展，server 侧能力继续散落在 context 扩展、工厂逻辑和 backend-specific surface 里。
- 如果未来还想让 client/server story 更对称，当前真正要解决的是接口建模，而不是文档更正。

**建议**
- 当前版本线继续保持文档与源码一致，不要再把不存在的 server 扩展画回活跃图里。
- 如果未来要补 server-side 对称扩展，应先明确它到底承载：
  - handshake role
  - accepted-connection metadata
  - server-only policy hooks
- 否则就继续把模型表述成“client 扩展 + 若干 server-side context/owner surface”，不要为了对称而对称。

### 4. `TSSLConfig` 仍是 mixed-scope public record，但部分边界已经显式 reject / warn
**证据**
- `src/fafafa.ssl.base.pas:382-428`
- `src/fafafa.ssl.factory.pas:426-528`
- `src/fafafa.ssl.context.config.pas:80-101`
- `src/fafafa.ssl.factory.pas:900-1005`
- `src/fafafa.ssl.pas:228-314`
- `src/fafafa.ssl.debug.utils.pas:323-324`

这个 record 把下面几层混在一起：
- 库级：`LibraryType`、日志字段
- 上下文级：协议、证书、验证、cipher、options
- 连接级：`ServerName`
- 后端私有：early-data replay-store 文件/目录

此外：
- `LogLevel` / `LogCallback` 在 factory 路径里会被直接拒绝
- `ServerName` 在 factory / builder / direct-library create-path 上当前已经降格成 warning / reject / ignore 的 compatibility field
- `BufferSize` / `HandshakeTimeout` 在 factory / direct-library 路径上当前是显式 reject，不是 silent inert。

**影响**
- 看起来像一个万能配置，实际上不是。
- 调用方仍然需要知道哪些字段是：
  - context truth
  - compatibility bridge
  - library-scoped reject
  - connection-scoped reject
- 配置层次和生命周期边界都不清晰。

**建议**
- 短期内继续把 source comment / API reference 维持成当前这套显式 scope 说明，不要让这些字段重新漂回“普通推荐配置”。
- 中长期如果要做 public-surface surgery，仍建议拆成至少四类：
  - library
  - context
  - client-context
  - server-context
- 当前最该避免的不是“所有字段立刻重构”，而是让 mixed-scope record 再次长出新的幻觉字段。

### 5. capability public surface 仍带着 compatibility projection baggage，但主真相已收口
**证据**
- `src/fafafa.ssl.base.pas:623-694`
- `src/fafafa.ssl.capability.serializer.pas:273-294, 488-526, 665-686, 823-861`
- `src/fafafa.ssl.capability.diff.pas:228-246`
- `src/fafafa.ssl.backend.selector.pas:382-386`
- `src/fafafa.ssl.openssl.backed.pas:989-1051`
- `src/fafafa.ssl.winssl.lib.pas:493-536`
- `src/fafafa.ssl.mbedtls.lib.pas:471-506`
- `src/fafafa.ssl.wolfssl.lib.pas:416-454`
- `docs/BACKEND_CAPABILITY_MATRIX.md:34-45`
- `docs/MIGRATION_GUIDE_V1.1.md:470-471`
- `docs/reference/API_REFERENCE.md:1901-1902`

public type 上仍然同时暴露：
- legacy bool
  - `SupportsSNI`
  - `SupportsOCSPStapling`
  - `SupportsSessionTickets`
- support-level
  - `SNISupport`
  - `OCSPStaplingSupport`
  - `SessionTicketsSupport`

**影响**
- 类型表面仍然偏重，调用方第一次看到 record 时，仍可能疑惑为什么 paired feature 同时挂着 bool 和 support-level。
- capability public surface 的 runtime/source truth 已经不再是未分类的双真相冲突。
- serializer / diff / selector / 活跃文档入口 当前都已经按 support-level-first 收平。

**建议**
- 继续把 support-level 视为 paired feature 的主真相。
- 继续把 legacy `Supports*` 视为 compatibility projection，而不是普通一等推荐读取面。
- 如果未来要做 `v2` public-type slimming，再讨论是否把这组 legacy projection 从主 record 降级或拆出。

### 6. 门面单元还在同时导出多条历史路径
**证据**
- `src/fafafa.ssl.pas:1-130`
- `src/fafafa.ssl.pas:228-246`

`fafafa.ssl` 同时导出：
- `TSSLFactory`
- `TSSLHelper`
- `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`
- `CreateDefaultConfig`
- `QuickServer`
- `CreateOCSPClient`
- `CreateCRLManager`

**影响**
- 推荐入口不够单纯。
- 老 helper 容易继续把旧用法延续下去。

**建议**
- 明确一个主入口，其余 helper 降级为 compatibility 级别。
- 文档里把“推荐路径”和“兼容路径”分开写。

## 总体判断
这套接口不是缺功能，而是层次太多、边界不干净、旧语义还在往新入口里流。

如果目标是 v1.x 稳定可用，当前最该守住的是：
1. core 接口别再加肥
2. SNI 只走连接级
3. 配置 record 别再塞跨层字段
4. 文档必须跟源码同一张图
