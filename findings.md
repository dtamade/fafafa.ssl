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

- 第一批 live 代码交叉验证已经确认：context-level `ServerName` 不只是“deprecated 但没人用了”的旧接口，而是仍然被当前实现主动传播：
  - `TSSLFactory.CreateContext(...)` 与 `TSSLContextBuilderImpl.Build*` 仍在对 context 调 `SetServerName(...)`
  - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 的 connection 构造器仍会把 `AContext.GetServerName` 复制到连接实例
  - 现有测试 `tests/test_factory_server_name_scope_clarification.pas` 与 `tests/test_sslctxboth_client_capability_clarification.pas` 还把这种 fallback 继承锁成当前预期
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

- focused 验证结果进一步支持当前路线判断：
  - `tests/test_factory_connection_scope_clarification.pas` PASS，证明 `BufferSize` / `HandshakeTimeout` 是显式 scope gate，而不是静默失效
  - `tests/test_factory_server_name_scope_clarification.pas` PASS，证明 client-side context `ServerName` 仍被正式支持为兼容路径
  - `tests/test_sslctxboth_client_capability_clarification.pas` PASS，证明多 backend 连接构造器仍主动继承 context-level `ServerName` fallback
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
    - builder `BuildClient` / `BuildServer` 仍都会保留 `WithSNI(...) -> context.SetServerName(...)` 的兼容写入
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
