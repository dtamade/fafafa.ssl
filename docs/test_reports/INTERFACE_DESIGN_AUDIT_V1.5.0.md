# fafafa.ssl 接口设计审查

**状态:** DRAFT  
**范围:** 公开 Pascal 接口、工厂、builder、门面单元与文档对齐  
**方式:** 静态审查，未改实现

## 结论
当前接口层不是“能不能用”的问题，而是“边界已经开始失真”。

最大的问题有四个：核心 `ISSLConnection` 太胖、`TSSLConfig` 把不同层级的配置混在一起、SNI 仍然通过被弃用的上下文入口传播、文档里还承诺了源码里没有的 `ISSLServerConnection`。

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
- `ReadString` / `WriteString` / `SetTimeout` / `SetBlocking` / `GetStateString` / `GetContext` 这类 convenience 方法应退出 core。

### 2. context-level SNI 已弃用，但高层入口仍在主动写入
**证据**
- `src/fafafa.ssl.base.pas:1042-1052`
- `src/fafafa.ssl.factory.pas:933-938`
- `src/fafafa.ssl.factory.pas:996-997`
- `src/fafafa.ssl.context.builder.pas:1117-1120`
- `src/fafafa.ssl.context.builder.pas:1280-1284`
- `src/fafafa.ssl.context.builder.pas:1389-1400`

`ISSLContext.SetServerName` 已经明确标成 deprecated，推荐路径是 `ISSLClientConnection.SetServerName`。
但 factory 和 builder 仍然把 `ServerName` 写回 context，甚至 server context 也会收到这个值。builder 只是在验证阶段给警告，没有真正切断旧语义。

**影响**
- 推荐路径和实际高层入口不一致。
- 同一个字段在 client/server context 上语义不同，容易让调用方误判。
- 迁移期会一直拖着旧心智模型走。

**建议**
- 高层 API 不再默认写 context-level SNI。
- 若要兼容，只保留显式兼容入口或单独的 migration shim。
- server context 不应再把这个字段当成正常配置项。

### 3. 文档承诺了 `ISSLServerConnection`，源码里没有
**证据**
- `docs/ARCHITECTURE.md:136-146`
- `docs/reference/INTERFACE_DESIGN_V2.md:13-18`
- 源码搜索没有发现任何 `ISSLServerConnection` 声明或实现

架构图把 `ISSLConnection` 分成 client / server 两个扩展，但 public Pascal surface 只有 `ISSLClientConnection`。

**影响**
- 架构文档比代码更对称，容易误导调用方。
- server 侧能力只能散落在 context 扩展、工厂逻辑或隐式行为里。

**建议**
- 要么补出真实的 server 扩展接口，要么把文档改成“client 扩展 + 若干能力型 context 扩展”，别再画不存在的对称层次。

### 4. `TSSLConfig` 不是可靠的单一配置契约
**证据**
- `src/fafafa.ssl.base.pas:382-428`
- `src/fafafa.ssl.factory.pas:426-528`
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
- `BufferSize` / `HandshakeTimeout` 只在默认值/调试里出现，主创建路径没有看到实际消费

**影响**
- 看起来像一个万能配置，实际上不是。
- 调用方很容易以为某个字段会生效，结果被静默忽略或直接报错。
- 配置层次和生命周期边界都不清晰。

**建议**
- 拆成至少四类：library / context / client-context / server-context，必要时再加 connection config。
- 不要让 factory 接受会被直接拒绝的字段。
- inert 字段要么落地，要么删掉，别挂在公共 record 里充当幻觉开关。

### 5. 能力矩阵存在双真相
**证据**
- `src/fafafa.ssl.base.pas:623-694`
- `src/fafafa.ssl.capability.serializer.pas:273-294, 488-526, 665-686, 823-861`
- `src/fafafa.ssl.capability.diff.pas:228-246`
- `src/fafafa.ssl.backend.selector.pas:382-386`
- `src/fafafa.ssl.openssl.backed.pas:989-1051`
- `src/fafafa.ssl.winssl.lib.pas:493-536`
- `src/fafafa.ssl.mbedtls.lib.pas:471-506`
- `src/fafafa.ssl.wolfssl.lib.pas:416-454`

既有布尔字段 `SupportsSNI` / `SupportsOCSPStapling` / `SupportsSessionTickets`，又有新字段 `SNISupport` / `OCSPStaplingSupport` / `SessionTicketsSupport`。

**影响**
- 两套字段都能被序列化、diff、选择器消费，长期会漂移。
- API 使用者不知道该信哪一套。

**建议**
- 选一套作为真相源，另一套只做兼容派生。
- 过渡期里至少在 serializer 和 selector 里明确 precedence。

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

