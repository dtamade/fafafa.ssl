# fafafa.ssl Interface And Backend Verification

**日期:** 2026-05-18  
**状态:** PARTIAL_CLOSEOUT  
**范围:** public interface、factory/builder/config、capability truth、各 backend 实现对齐  
**方式:** 静态审查 + focused contracts/tests + 最小真相修复

## 目标

验证 `fafafa.ssl` 的接口设计与各 backend 实现是否还保持同一套真相源，并留下稳定记录，避免后续再从 release closeout 或旧 roadmap 入口重新启动。

## 本轮结论

当前问题不是“项目没实现”，而是“public surface 与兼容语义还在背着历史包袱前进”。

这轮已经确认三类真问题：

1. **文档承诺漂移**
   - 活跃文档曾把 `ISSLServerConnection` 画进 public interface graph。
   - live source 没有这个接口。
   - 这类漂移已经在本批修复，并用静态合同守住。

2. **context-level SNI 旧语义仍然是当前实现真相**
   - `ISSLContext.SetServerName` 虽然 deprecated，但 factory、builder、各 backend connection 构造器仍在使用。
   - 多份 focused 测试还把这种 fallback 继承当成兼容行为固定了下来。
   - 这不是一个局部脏点，而是一条已经被实现和合同共同保护的兼容路径。

3. **capability model 仍存在系统性双真相**
   - backend `GetCapabilities` 仍同时发布 legacy boolean 与 support-level。
   - selector 信 support-level。
   - serializer / diff 同时 round-trip 两套字段。
   - backend contract 中仍有部分 optional interface alignment 主要看旧布尔字段。
   - 这意味着 capability compatibility layer 还没有完成“兼容字段从属化”。

## 已确认但不属于本批 bugfix 的设计债

### 1. `TSSLConfig` 跨层字段仍然偏重

- `BufferSize` / `HandshakeTimeout` 看起来像配置字段，但在 factory 路径里会被显式拒绝。
- 这说明它们不是 silent no-op，而是 connection-scoped config 被继续挂在公共 record 上。
- 这是设计负担，不是本批发现的新隐藏 bug。

### 2. SNI 迁移必须视作兼容性迁移

- 不能把“删掉 context-level ServerName fallback”当成一次小修。
- 它会同时影响：
  - `TSSLFactory`
  - `TSSLContextBuilderImpl`
  - OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL connection 构造器
  - 多份 focused tests / contracts

## 本批修复

- 更新 [docs/ARCHITECTURE.md](../ARCHITECTURE.md)
  - 不再把 `ISSLServerConnection` 画进当前 public interface graph。
- 更新 [docs/reference/INTERFACE_DESIGN_V2.md](../reference/INTERFACE_DESIGN_V2.md)
  - 不再把 `ISSLServerConnection` 当作活跃接口层次的一部分。
- 新增 `tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - 静态守护“活跃文档不能再承诺源码里不存在的 public interface”。

## 增量收口：runtime capability bool/support 真相对齐

- `src/fafafa.ssl.base.pas` 新增 `NormalizeLegacyCapabilityBooleans(...)`
  - 规则非常明确：runtime truth 以 `SNISupport` / `ALPNSupport` / `OCSPStaplingSupport` / `CertTransparencySupport` / `SessionTicketsSupport` 为准。
  - legacy boolean 现在只作为 compatibility projection，由 shared helper 统一回填。

- 以下 backend 的 `GetCapabilities` 已接入统一归一化：
  - OpenSSL
  - FreePascal
  - WinSSL
  - MbedTLS
  - WolfSSL

- focused contracts 已同步切换到 support-level truth：
  - `tests/contract/test_capabilities_contract.pas`
    - major backend 的 capability presence 改为检查 `SNISupport` / `ALPNSupport <> None`
    - 新增 SNI / ALPN / OCSP / CT / SessionTickets 的 bool/support-level 一致性断言
  - `tests/contract/test_backend_contract.pas`
    - SNI / CT / OCSP optional interface alignment 改为信 `*Support <> None`

- 这一步的意义不是删除 legacy 字段，而是先把 runtime 侧的双真相压成单一来源：
  - backend 不再各自手填一套可能漂移的旧布尔值
  - contract 也不再把 legacy boolean 当作主真相字段

- focused 验证：
  - `bash tests/scripts/test_capability_legacy_bool_normalization_contract.sh`
    - PASS
  - `tests/contract/test_capabilities_contract.pas`
    - PASS (`63 passed, 0 failed, 1 skipped`)
  - `tests/contract/test_backend_contract.pas`
    - PASS (`111 passed, 0 failed, 24 skipped`)
  - `git diff --check`
    - PASS

## 增量收口：deserializer precedence 与 diff support-level truth

- 这轮继续往下审，确认 `serializer / deserializer / diff` 线不是“概念上双真相”，而是已经有两处具体行为缺口：
  1. `JSONToCapabilities(...)` / `XMLToCapabilities(...)`
     - 对同时带 legacy boolean 与 `*Support` 的 payload，没有真相优先级
     - 冲突输入会把旧布尔值错误保留下来
  2. `CompareCapabilities(...)`
     - 原先几乎只比较 legacy boolean
     - 会漏掉 `SNISupport`、`EarlyDataSupport` 这类 v1.2 support-level 真实变化

- 本批修复后的规则：
  - 反序列化时：
    - 若 payload 包含某个 `*Support` 字段，则该字段为真相源，并回填对应 legacy boolean
    - 若 payload 只有旧 boolean，则继续保持旧输入兼容，不强行猜测 support-level
  - diff 时：
    - paired capability 优先比较 `*Support`
    - support-level-only capability 也纳入 diff
    - legacy boolean 只作为没有 support-level truth 时的 fallback

- 新增 focused regressions：
  - `tests/test_capability_deserialization_truth_precedence.pas`
    - 先红后绿，钉住“support-level 覆盖冲突 legacy boolean”
  - `tests/test_capability_diff_support_level_truth.pas`
    - 先红后绿，钉住“diff 不能忽略 support-level 变化”

- 兼容确认：
  - 现有 `tests/test_capability_deserialization_roundtrip.pas`
    - PASS
    - 说明这次修的是 precedence / truth，对既有 JSON/XML round-trip 没有打穿

## 增量收口：internal context ServerName warning quarantine

- 继续沿着 SNI 主线静态审查后，确认 warning 治理这条线也存在“旧计划入口失真”的问题：
  - 旧计划点名的 `factory` / `builder` / `openssl.connection` / `openssl.backed` 已不再是当前 live compile 噪音主来源
  - `tests/test_builder_integration.pas` 也不再稳定暴露 `ISSLContext.Get/SetServerName` deprecated warning

- 当前真正有效的 compile probe 是：
  - `tests/contract/test_capabilities_contract.pas`
  - 这条 compile path 在修复前会稳定打出：
    - `src/fafafa.ssl.wolfssl.connection.pas` 的两处 `ISSLContext.GetServerName` deprecated warning
    - `src/fafafa.ssl.mbedtls.connection.pas` 的两处 `ISSLContext.GetServerName` deprecated warning

- `WinSSL` 在当前 Linux host 上不走这条 live compile path，但源码静态审查也确认：
  - `src/fafafa.ssl.winssl.connection.pas` 仍有两处 direct `AContext.GetServerName` 兼容读取
  - 它们也应与其他 backend 一样被局部 quarantine，而不是等到 Windows lane 再反复重提

- 本批修法保持非常克制：
  - 不改 factory / builder
  - 不改 runtime fallback 兼容语义
  - 只在 `wolfssl` / `mbedtls` / `winssl` 的内部兼容读取点加局部 `{$PUSH}{$WARN 6058 off}{$WARN SYMBOL_DEPRECATED OFF}` / `{$POP}`

- focused contract 也同步收敛到当前真相：
  - `tests/scripts/test_internal_context_servername_warning_contract.sh`
    - 改为编译 `tests/contract/test_capabilities_contract.pas`
    - grep `wolfssl` / `mbedtls` 的 deprecated warning 必须不存在
    - 对 `WinSSL` 增加静态 source guard
    - 并在 contract 内直接运行编译出的 `test_capabilities_contract`

- 验证结果：
  - `bash tests/scripts/test_internal_context_servername_warning_contract.sh`
    - PASS
  - focused compile log 中不再出现上述 `GetServerName` deprecated warning

- 这一步的意义不是“完成了 SNI 迁移”，而是：
  - 把当前仍然需要保留的兼容路径从“反复刷 warning 的噪音源”收成“显式承认的内部兼容区”
  - 让下一步真正的 compatibility migration 可以在更干净的 compile 基线上继续推进

## 增量收口：capability serialization truth projection

- 继续深挖 capability 双真相后，确认 serializer 输出面也确实存在 live 外部漂移：
  - `CapabilitiesToJSON(...)` / `CapabilitiesToXML(...)` 之前直接输出 record 里的 legacy boolean
  - 这意味着一个已经携带 support-level truth 的 record，仍可能导出自相矛盾的 payload，例如：
    - `supportsSNI=false`
    - `sniSupport="stable"`

- 这个问题不能靠 round-trip 测试掩盖：
  - 因为现有 deserializer precedence 会在读回时再次用 `*Support` 覆盖 legacy boolean
  - 所以本批新增的是直接检查输出字符串的 focused RED，而不是只跑 serialize -> deserialize

- 当前修法：
  - `src/fafafa.ssl.capability.serializer.pas`
    - 新增内部 helper，先判断 record 是否已携带任意 v1.2 support-level truth
    - 若是，则在 JSON/XML 输出前先回填一份本地 legacy boolean compatibility projection
  - 这一步只修“support-level-aware record 的输出 truth”
  - 不去假装解决“纯 legacy-only in-memory record 缺少 presence bit”这条本来就无法从 record 本身判定的歧义

- focused 验证：
  - `tests/test_capability_serialization_truth_projection.pas`
    - RED -> GREEN
    - 直接钉住 JSON/XML 不得再导出 bool/support-level 自相矛盾
  - `tests/test_capability_deserialization_roundtrip.pas`
    - PASS
    - 说明外部输出 truth 收口后，既有 round-trip 兼容链路仍保持绿色

## 增量收口：context ServerName 兼容迁移路线图

- 继续向前推进后，这条线的“当前真相”和“下一步顺序”终于不再含糊：
  - factory client path 仍会写入 context-level `ServerName`
  - factory server path 已 reject `ServerName`
  - builder `BuildClient` / `BuildServer` 仍都会保留 `WithSNI(...)` 的兼容写入
  - connector 已经走 per-connection `SetServerName(...)`，是目标形态
  - 五个 backend connection constructor 仍共同实现 context fallback 继承

- 因此主线已经明确：
  - 现在不该直接硬删 backend fallback
  - 应先缩窄高层写入面，再把 backend fallback 提成共享 compatibility shim，最后再谈真正删除历史继承

- 为了避免以后迁移时又漏掉“其实是故意保留的兼容语义”，这轮还把兼容锁点统一固化了：
  - `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - 现在覆盖：
      - `test_context_builder_server_servername_runtime_consistency`
  - 这些测试现在都必须带 `INTENTIONAL_COMPAT:` 标签

- 这一步的价值不在于“又修了个 bug”，而在于：
  - 我们已经把 SNI 迁移从一团历史行为，压成了一条有 phase、有锁点、有执行顺序的主路线
  - 下次继续时，不需要再重新考古“哪些行为是现状，哪些是故意保留”

## 增量收口：builder surface narrowing compatibility marker

- 迁移路线图冻结后，这轮已经落下 Phase B 的第一刀实现：
  - `src/fafafa.ssl.context.builder.pas`
    - JSON/INI export 继续保留 `server_name`
    - 但只要该字段非空，就会额外导出 `server_name_mode=deprecated_context_sni`
  - `ImportFromJSON(...)` / `ImportFromINI(...)`
    - 显式接受这个 marker
    - 但把它视为纯 compatibility metadata，不影响 runtime state

- 这一步的意义不是改变当前兼容行为，而是：
  - 把 builder 继续暴露旧语义这件事，从“看起来像正常推荐字段”改成“显式自带 deprecated compatibility 标记”
  - 同时保持 legacy-only JSON/INI 输入还可以无痛导入

- focused RED -> GREEN 结果说明这刀是安全的：
  - 新增 `tests/config/test_context_builder_server_name_compat_marker.pas`
    - 直接检查 JSON/INI export 是否带 `server_name_mode`
    - 直接检查 bare `server_name` 的 legacy JSON/INI 输入在 re-export 时会自动升级出 marker
  - 邻接回归：
    - `tests/config/test_config_import_export.pas` PASS (`96 passed, 0 failed`)
    - `tests/config/test_context_builder_merge_advanced_option_snapshot_semantics.pas` PASS (`13 passed, 0 failed`)
    - `tests/config/test_config_snapshot_clone.pas` PASS (`57 passed, 0 failed`)

- 这条线还顺手收敛了一个测试方法问题：
  - builder JSON 导出是 pretty-printed
  - 所以新测试对 JSON 一律解析字段值，而不是做 substring 硬匹配
  - 这样以后不会因为空格/换行格式差异把兼容行为误报成回归

## 增量收口：factory/config compatibility warning

- 在 builder surface 收窄之后，factory/config 这条高层写入面原先仍然有一个明显缺口：
  - `TSSLFactory.CreateContext(AContextType, ALibType)` 的 client default-config path 会静默吃掉 `TSSLConfig.ServerName`
  - `TSSLFactory.CreateContext(const AConfig)` 的 one-shot client path 也一样静默
  - 这会让 `TSSLConfig.ServerName` 看起来仍像是正常推荐主路径，而不是兼容入口

- 这轮修法保持兼容行为不变，只把它们显式降格：
  - `src/fafafa.ssl.factory.pas`
    - 新增 `LogContextLevelServerNameCompatibilityWarning(...)`
    - 在两条 client-side compatibility write 路径上发出 `TSecurityLog.Warning('Factory', ...)`
    - warning 文本直接点名：
      - `TSSLConfig.ServerName`
      - `deprecated context-level SNI compatibility`
      - 推荐迁移到 `ISSLClientConnection.SetServerName(...)` / `TSSLConnector.Connect*(..., ServerName)`
  - `src/fafafa.ssl.base.pas`
    - `TSSLConfig.ServerName` 字段注释改成 compatibility-only
  - `docs/reference/API_REFERENCE.md`
    - 新增 `Client SNI Compatibility Note`

- focused RED -> GREEN 结果说明第二刀也是安全的：
  - 新增 `tests/test_factory_server_name_compatibility_warning.pas`
    - 先红后绿，直接钉住：
      - default-config client path 要发 warning
      - one-shot client path 要发 warning
      - 没有 `ServerName` 时保持安静
  - 邻接回归：
    - `tests/test_factory_server_name_scope_clarification.pas` PASS
    - `tests/test_factory_config_server_name_isolation.pas` PASS
    - `tests/test_factory_logging_scope_clarification.pas` PASS
    - `tests/scripts/test_active_docs_no_context_level_sni_guidance_contract.sh` PASS

- 这一步完成后，`context-level ServerName` 的高层写入面已经不再“沉默”：
  - builder export/import 会带 compatibility marker
  - factory/config runtime path 会发 explicit warning
  - 下一步真正高价值的收口点，已经不是继续修高层文案，而是把 backend constructor 里的 fallback 读取提成 shared compatibility shim

## 增量收口：backend shared compatibility shim

- 在 Phase B 的高层写入面收窄之后，backend constructor fallback 仍然有一个明显的结构问题：
  - OpenSSL / FreePascal / WolfSSL / MbedTLS / WinSSL 五个实现各自 direct read deprecated context `GetServerName`
  - 这让 compile warning、后续迁移、以及行为删改的控制面继续分散

- 这轮 Phase C 第一刀只做 seam consolidation，不做 behavior migration：
  - 新增 `src/fafafa.ssl.context.compat.pas`
    - 提供 `GetContextLevelServerNameCompatibilityValue(...)`
    - 统一封装 client-role gate、deprecated read、以及 local warning suppression
  - 五个 backend constructor fallback 全部改走 shared helper
  - direct deprecated `AContext.GetServerName` / `FContext.GetServerName` 读取从目标构造路径中移除

- 这一步最关键的设计约束是“side effect 不变”：
  - OpenSSL / MbedTLS 仍继续走 `SetServerName(...)`
  - FreePascal / WolfSSL / WinSSL 仍继续走字段赋值路径
  - 所以这批不是在偷删兼容，而是在把兼容读取的真实入口压成一条可继续治理的 seam

- focused source contract 和 runtime regressions 都说明这刀是安全的：
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - RED -> GREEN
    - 直接钉住 shared helper 存在、五个 backend 已接入、backend-local direct read 已移除
  - `tests/test_sslctxboth_client_capability_clarification.pas`
    - PASS (`28 passed, 0 failed, 1 skipped`)
    - 跨 backend context-to-connection fallback 兼容真相保持不变
  - `tests/test_factory_server_name_scope_clarification.pas`
    - PASS (`6 passed, 0 failed`)
    - factory/client compatibility behavior 在 backend seam 提取后仍保持绿色

- 这也把主线再次往前推进了一步：
  - backend constructor 已不再是五份分散的 direct deprecated reads
  - 下一批真正该讨论的是 public/high-level surface cleanup 和 behavior migration RED，而不是再回头逐个 backend 做重复治理

## 增量收口：builder runtime compatibility warning

- shared shim 落地后，高层 surface 还剩最后一个明显不对称：
  - `ValidateClient` / `ValidateServer` 已经会对 `WithSNI(...)` 发 deprecated compatibility warning
  - 但真实 `BuildClient` / `BuildServer` 路径此前仍然静默应用同一份兼容写入
  - 这让 builder 的 validation truth 与 runtime truth 分裂，也让 `WithSNI(...)` 继续像一个正常主路径

- 这轮修法把 builder 的 runtime truth 收回到和 validation 同一条线上：
  - `src/fafafa.ssl.context.builder.pas`
    - 新增 `LogBuilderContextLevelServerNameCompatibilityWarning(...)`
    - `BuildClient` 在应用 `WithSNI(...)` 时，显式提示这是 deprecated context-level SNI compatibility
    - `BuildServer` 在应用 `WithSNI(...)` 时，显式提示这只是 deprecated context-level ServerName compatibility，且 server-side connections ignore it
  - `ValidateClient` / `ValidateServer` 也同步沿用同样的 compatibility 术语
  - `ISSLContextBuilder.WithSNI(...)` 的接口注释与 `docs/reference/API_REFERENCE.md` 也已经一起降格成 compatibility-only 入口

- focused RED -> GREEN 证明这个缺口是真问题，而且已经被最小收口：
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - 初始 8 条断言失败，直接证明 builder runtime path 原先完全静默
    - 修复后 `12 passed, 0 failed`
  - `tests/config/test_config_validation.pas`
    - PASS (`53 passed, 0 failed`)
    - 说明 validation 语义在文案对齐后仍保持绿色
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - PASS (`6 passed, 0 failed`)
    - 说明 builder client/server 当前兼容行为没有被 runtime warning 误伤

- 这一步的价值不是再加一层日志，而是把 high-level write surfaces 的状态真正统一了：
  - builder import/export 有 compatibility marker
  - builder runtime path 有 warning
  - factory/config runtime path 有 warning
  - backend fallback 有 shared shim
  - 因此接下来终于可以直接讨论第一条 behavior migration RED，而不是继续清理“沉默兼容入口”

## 增量收口：WinSSL 普通客户端流迁到 per-connection SNI

- 在 builder/factory/runtime/shared-shim 都显式化之后，剩余活跃 `SetServerName(...)` 命中里还混着一批真正的普通客户端连接流：
  - `test_winssl_error_mapping_online`
  - `test_winssl_https_client`
  - `test_winssl_revocation_online`
  - `test_winssl_mtls_e2e_local`
  - 这些文件的共同点是：它们都在做真实客户端连接/握手/验证，不是在证明 legacy compatibility，也不是在做 context API-surface 契约

- 这批已经把它们统一迁到正确方向：
  - 先 `CreateConnection(...)`
  - 再拿 `ISSLClientConnection`
  - 然后在 `Connect` / `DoHandshake` 前调用 `SetServerName(...)`
  - 原有测试关注点保持不变：
    - 证书错误映射
    - HTTPS 基本连通
    - 吊销错误
    - 本地 mTLS 握手

- focused 证据也足够扎实：
  - `tests/scripts/test_winssl_client_flow_tests_no_context_level_sni_guidance_contract.sh`
    - RED -> GREEN
    - 直接守住“这些普通流文件不再教 context-level SNI”
  - 本地 Linux-target 直接编 `test_winssl_https_client.pas`
    - 仍然会卡在 `fafafa.ssl.winssl.lib` 对 `Windows` 单元的依赖
    - 这说明此类文件的本地静态编译验证本来就该走 Win64/Windows 路径
  - `fpc -Twin64` 对四个文件的交叉编译全部成功

- 这步完成之后，剩余活跃 context-level `SetServerName(...)` 命中已经明显更接近“故意保留”的集合：
  - precedence / connector / cross-backend compatibility contracts
  - backend context contracts / framework tests
  - WinSSL comprehensive / library-basic / skeleton 这类更偏 API-surface 或未完成分类的文件
  - 因而下一步已经不是继续随机扫普通流，而是把剩余活跃测试面彻底分清 intentional 与 ordinary

## 增量收口：残余模糊测试面分类与 WinSSL mTLS skeleton 握手迁移

- 上一刀之后，仍有一小批活跃 context-level `SetServerName(...)` 命中处在“看起来像普通用法，但其实有些是 intentional coverage”的模糊状态：
  - `test_tls_connector_early_data_contract`
  - `test_mbedtls_context_contract`
  - `test_wolfssl_context_contract`
  - `test_winssl_library_basic`
  - `test_winssl_mtls_skeleton`

- 这批已经把它们彻底分清：
  - `test_tls_connector_early_data_contract`
    - 补 `INTENTIONAL_COMPAT`
    - 说明该 connector early-data contract 故意从 inherited context fallback 起步
  - `test_mbedtls_context_contract`
  - `test_wolfssl_context_contract`
  - `test_winssl_library_basic`
    - 补 `INTENTIONAL_API_SURFACE`
    - 说明它们是在覆盖 deprecated context setter/getter surface，而不是推荐客户端主路径
  - `test_winssl_mtls_skeleton`
    - 配置 smoke 段的 `SetServerName('test.example.com')` 补 `INTENTIONAL_API_SURFACE`
    - 真实 `TestMTLSHandshake` 路径改成：
      - `CreateConnection(...)`
      - `ISSLClientConnection.SetServerName(ServerHost)`
      - `DoHandshake`

- focused 证据说明这批既不是纸面标签，也没有把 WinSSL 骨架编坏：
  - `tests/scripts/test_residual_context_sni_classification_contract.sh`
    - RED -> GREEN
    - 直接守住显式分类与 `test_winssl_mtls_skeleton` 握手流不再使用 `Ctx.SetServerName(ServerHost)`
  - Linux-safe focused compile:
    - `tests/test_tls_connector_early_data_contract.pas`
    - `tests/mbedtls/test_mbedtls_context_contract.pas`
    - `tests/wolfssl/test_wolfssl_context_contract.pas`
    - 全部通过
  - Win64 focused cross-compile:
    - `tests/winssl/test_winssl_library_basic.pas`
    - `tests/winssl/test_winssl_mtls_skeleton.pas`
    - 全部通过

- 这一步完成后，残余活跃 context-level `SetServerName(...)` 命中已经基本不再混着普通客户端流指导语义，而主要是 intentional compatibility / API-surface coverage

## 增量收口：BuildServer WithSNI dead compatibility 收口

- 残余分类完成后，第一条真正适合落地的 behavior migration 并不是直接打 client fallback，而是先收掉一个 server-only dead compatibility：
  - `TSSLContextBuilder.BuildServer` 之前会一边 warning “server-side connections ignore it”，一边仍把 `WithSNI(...)` 写进 built context
  - 这会制造一个没有真实 server-side 消费者、但还在 runtime state 里残留的 client-only `ServerName`

- 这批已经把这条裂缝收掉：
  - `BuildServer` 继续保留 compatibility warning
  - 但 built server context 不再保留 `WithSNI(...)` 对应的 `ServerName`
  - `ValidateServer` 也同步改成明确术语：
    - `BuildServer ignores it and server-side connections ignore it`

- focused RED -> GREEN 证据：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - 初始 1 条断言失败，直接证明 built server context 之前还保留 legacy `ServerName`
    - 修复后 `6 passed, 0 failed`
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - 初始 2 条断言失败，直接证明 warning 术语与 runtime truth 还不一致
    - 修复后 `14 passed, 0 failed`
  - `tests/config/test_config_validation.pas`
    - 初始 1 条断言失败
    - 修复后 `53 passed, 0 failed`

- 这一步的意义不是“完成了 SNI 迁移”，而是：
  - 第一条真正的 behavior migration 已经开始进入生产代码，而不是继续停留在路线图层
  - 下一步可以把注意力集中到 client-side intentional fallback 收缩，而不必反复处理这个 server-only dead compatibility

## 增量收口：sslCtxBoth dual-role context 不再继承 context-level SNI fallback

- 下一条最小的 client-side behavior migration 也已经落地，而且范围仍然很克制：
  - `sslCtxBoth` 早就已经在握手层要求显式选择 role
  - 但 shared compatibility shim 之前仍会把 deprecated context-level `ServerName` 静默继承给新连接
  - 这会让 dual-role context 一边拒绝猜 handshake role，一边又继续猜 client-side SNI fallback

- 当前修法只动 shared shim：
  - `GetContextLevelServerNameCompatibilityValue(...)` 现在遇到 `sslCtxBoth` 会直接返回空字符串
  - dual-role connection 仍然保留 `ISSLClientConnection` surface
  - 但调用方如果真要走 client role，必须在 connection 上显式 `SetServerName(...)`

- focused RED -> GREEN 证据：
  - `tests/test_sslctxboth_client_capability_clarification.pas`
    - 初始 5 条断言失败，证明四个 backend 的 dual-context stream path 和 FreePascal socket path 之前都还在继承旧 fallback
    - 修复后 `28 passed, 0 failed, 1 skipped`
  - `tests/test_sslctxboth_roleless_handshake_clarification.pas`
    - PASS (`24 passed, 0 failed`)
    - 说明既有 roleless-handshake fail-fast 边界保持不变
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - PASS
    - 说明 `sslCtxBoth` 已经从 intentional compatibility label 集合里移除，其余保留集合仍然稳定

- 这一步完成后，下一条真正需要继续收缩的 client-side fallback 已经更明确：
  - `sslCtxBoth` 不再是未处理的兼容面
  - 剩下主要是 `sslCtxClient` direct / builder / factory 这组 still-intentional inherited fallback

## 增量收口：cross-backend 网络合同改用 per-connection SNI

- 再往下审之后，发现还有两份文件虽然已经不是普通客户端流，但也不应该继续挂在 intentional compatibility 集合里：
  - `test_cross_backend_consistency_contract`
  - `test_cross_backend_errors_contract`

- 这两份合同真正要锁的是：
  - 跨 backend 的结果一致性
  - 跨 backend 的错误归一化
  - 而不是 deprecated context-level SNI fallback 本身

- 因而本批把它们统一迁到更准确的连接语义：
  - `CreateConnection(...)`
  - `ISSLClientConnection.SetServerName(...)`
  - `Connect`
  - `test_cross_backend_errors_contract` 里的 `www.google.com:80` 握手失败分支也同步改成同一路径

- 这一步之后，它们已经从 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 移除，不再要求 `INTENTIONAL_COMPAT:` 标签

- focused 证据：
  - `tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
    - RED -> GREEN
    - 直接守住“两份 cross-backend 网络合同不再教 `Ctx.SetServerName(...)`”
  - `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - PASS
    - 说明 intentional compatibility 集合在收缩后仍然稳定
  - `tests/integration/test_cross_backend_consistency_contract.pas`
    - PASS
    - compile/run shape 保持绿色；本机因 `FAFAFA_RUN_NETWORK_TESTS!=1` 跳过 live network probe
  - `tests/integration/test_cross_backend_errors_contract.pas`
    - PASS
    - compile/run shape 保持绿色；本机因 `FAFAFA_RUN_NETWORK_TESTS!=1` 跳过 live network probe

- 这一步的意义是：
  - cross-backend 网络合同不再继续给 `sslCtxClient` inherited fallback 当假锁点
  - 在当时的迁移队列里，下一步真正的 client-side behavior migration 已经可以更直接地瞄准 `tests/test_freepascal_context_server_name_inheritance.pas`

## 增量收口：FreePascal 客户端连接不再继承 context-level SNI fallback

- 接着把第一条 dedicated `sslCtxClient` behavior migration 落到真实 backend 上之后，FreePascal 这条 inherited fallback 已经不再只是路线图候选：
  - `src/fafafa.ssl.freepascal.connection.pas`
  - socket / stream 两个 client 构造器都不再读取 `GetContextLevelServerNameCompatibilityValue(AContext)`

- 这意味着：
  - `TSSLContextBuilder.BuildClient.WithSNI(...)` 仍然会留下 deprecated compatibility warning
  - direct context `SetServerName(...)` 也仍然是 deprecated surface
  - 但 FreePascal runtime 已经不再静默消费这份 context state
  - 调用方如果真要走 FreePascal client path，必须显式对 connection 调 `SetServerName(...)`

- dedicated regression 也已经从“保留继承”翻成“禁止继承”：
  - `tests/test_freepascal_context_server_name_inheritance.pas`
    - builder socket path 现在要求 `GetServerName = ''`
    - direct-context stream path 现在也要求 `GetServerName = ''`
  - 这份文件已经从 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 移除

- focused 证据：
  - `tests/scripts/test_freepascal_client_connections_no_context_servername_fallback.sh`
    - RED -> GREEN
    - 先直接钉出两处 compat read，再守住它们已经从 FreePascal 构造器中消失
  - `tests/test_freepascal_context_server_name_inheritance.pas`
    - RED -> GREEN
    - 说明 dedicated FreePascal runtime regression 已经真正翻到 no-fallback 语义
  - `tests/test_connection_builder_hostname_precedence.pas`
    - PASS
  - `tests/test_tls_connector_hostname_override_precedence.pas`
    - PASS
    - 说明剩余 intentional mock precedence surface 仍然稳定，没有被这刀顺带打穿

- 这一步之后，remaining client-side intentional compatibility surface 再次缩小：
  - `test_connection_builder_hostname_precedence`
  - `test_tls_connector_hostname_override_precedence`
  - `test_context_builder_server_servername_runtime_consistency`

- 因而在那个时点，下一批最合理的 RED 不再是 FreePascal 专项测试，而是：
  - `tests/test_connection_builder_hostname_precedence.pas`
  - 因为它当时成了最直接、最下层、仍明确保留 inherited fallback 的 client-side mock precedence 契约

## 增量收口：connection builder 默认不再保留 inherited fallback

- 接着继续向上收后，`TSSLConnectionBuilder` 这条 client path 也已经不再当 context-level SNI fallback 的透传通道：
  - `src/fafafa.ssl.connection.builder.pas`
  - `TryBuildClient` 现在只要拿到的是 `ISSLClientConnection`，就会接管 per-connection hostname state
  - 若调用方没有 `WithHostname(...)`，builder 会显式写入 `''`，而不是保留 context fallback

- 这意味着这条 builder 契约现在已经从“保留 inherited fallback”翻成了“默认 clear inherited fallback”：
  - `tests/test_connection_builder_hostname_precedence.pas`
    - case 1 现在要求 no `WithHostname(...)` -> `GetServerName = ''`
    - case 2 继续要求 explicit override
    - case 3 继续要求 explicit empty clear
  - 这份测试也已经从 `tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh` 移除

- focused 证据：
  - `tests/test_connection_builder_hostname_precedence.pas`
    - RED -> GREEN
    - 初始只红 case 1，直接证明 `TryBuildClient` 之前还在保留 inherited context fallback
  - `tests/test_tls_connector_hostname_override_precedence.pas`
    - PASS
    - 说明 connector override precedence 没有被 builder 这刀误伤
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - PASS
    - 说明 intentional compatibility 集合在继续收缩后仍然稳定

- 这一步之后，剩余最直接的高层 client-side intentional compatibility surface 再次收窄：
  - `test_tls_connector_hostname_override_precedence`
  - `test_context_builder_server_servername_runtime_consistency`
  - 以及单独分类管理的 `test_tls_connector_early_data_contract`

- 因而下一批最合理的 RED 已再次前移：
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - 然后再评估 `tests/test_tls_connector_early_data_contract.pas` 是否还需要继续从 inherited context fallback 起步

## 增量收口：connector override precedence 不再依赖 context fallback 输入

- 继续往上收之后，`TSSLConnector` 的 override precedence 契约也已经不再需要 inherited context fallback 这个历史搭景：
  - `tests/test_tls_connector_hostname_override_precedence.pas`
  - 现在只保留 connector 自己真正需要证明的两件事：
    - 非空 override 胜出
    - 空 override 保持空字符串

- 这批没有改生产 `TSSLConnector` 实现：
  - `src/fafafa.ssl.tls.pas` 本来就走纯连接级 `ISSLClientConnection.SetServerName(...)`
  - 这次只是把测试和合同真相收回到这个事实上

- focused 证据：
  - `tests/scripts/test_tls_connector_override_no_context_level_sni_guidance.sh`
    - PASS
    - 直接守住这份测试不再教 `Ctx.SetServerName(...)`
  - `tests/test_tls_connector_hostname_override_precedence.pas`
    - PASS
    - 去掉 inherited fallback 输入后，override precedence 仍然稳定
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - PASS
    - 说明 intentional compatibility 集合在继续收缩后仍然稳定

- 这一步之后，剩余最直接的 connector-side intentional compatibility 输入再次收窄：
  - `test_tls_connector_early_data_contract`
  - 以及服务端兼容语义的 `test_context_builder_server_servername_runtime_consistency`

- 因而下一批最合理的 RED 已再次前移：
  - `tests/test_tls_connector_early_data_contract.pas`
  - 再决定 `test_context_builder_server_servername_runtime_consistency` 何时从当前 intentional 集合中拆开

## 增量收口：connector early-data 合同不再依赖 context fallback 输入

- 继续沿着 connector 真相收之后，`TSSLConnector` 的 early-data convenience contract 也已经不再需要 inherited context fallback 这个旧搭景：
  - `tests/test_tls_connector_early_data_contract.pas`
  - 现在真正锁住的是：
    - session 先被应用
    - 显式 server name 被写入连接
    - early data 在 connect 前排队
    - unsupported early-data 路径继续给出既有失败语义

- 这批同样没有改生产 `TSSLConnector` 实现：
  - `src/fafafa.ssl.tls.pas` 的正式语义本来就是 `ApplyClientOptions(..., AServerName)` 后再尝试 `TryQueueEarlyData(...)`
  - 这次只是把测试和合同真相继续收回到这条显式连接级路径

- focused 证据：
  - `tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
    - PASS
    - 直接守住这份测试不再教 `Ctx.SetServerName(...)`
  - `tests/test_tls_connector_early_data_contract.pas`
    - PASS
    - 去掉 inherited fallback 输入后，session/servername/earlydata/connect 顺序仍然稳定
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - PASS
    - 说明 intentional compatibility label 集合继续稳定缩到服务端控制案例

- 这一步之后，connector-side 的 inherited context fallback 输入已经全部从普通 contract 真相里移出：
  - override precedence 已收口
  - early-data convenience contract 已收口
  - 剩余 intentional compatibility label 集合只剩 `test_context_builder_server_servername_runtime_consistency`

- 因而下一批最合理的 bounded review 再次前移：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - 再决定 `TSSLConfig.ServerName` / `WithSNI(...)` 这类 public compatibility surface 何时继续收紧

## 增量收口：FreePascal client context-ServerName focused contracts 与 runtime 真相重新对齐

- 在继续审最后那条 server-side control case 时，live focused retest 反而先暴露出一个更实在的问题：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - 这三份 FreePascal-focused 合同仍然在断言：
    - `BuildClient.WithSNI(...)` 会让新连接继承 context `ServerName`
    - factory client `ServerName` config 会让新连接继承 context `ServerName`

- 但这已经不是 live runtime truth：
  - `src/fafafa.ssl.freepascal.connection.pas` 之前那一刀已经把 socket / stream client constructor 的 inherited context fallback 切掉了
  - 所以 FreePascal 现在的真实边界是：
    - deprecated context state 仍然会留在 context 上
    - 但新 client connection 不再自动继承它

- 这批因此没有改生产代码，而是修正 focused contracts 自身的失真：
  - `test_context_builder_server_servername_runtime_consistency`
    - `BuildClient` 继续断言 built context 保留 `ServerName`
    - 但 FreePascal client connection 现在明确断言为 `''`
  - `test_factory_server_name_scope_clarification`
    - client default-config / one-shot config 继续断言 context 保留 `ServerName`
    - 但 FreePascal client connection 现在明确断言为 `''`
  - `test_factory_config_server_name_isolation`
    - default-path / one-shot path 继续覆盖 context state isolation
    - 但不再错误宣称 FreePascal connection 会继承旧 fallback

- focused 证据：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - RED -> GREEN
  - `tests/test_factory_server_name_scope_clarification.pas`
    - RED -> GREEN
  - `tests/test_factory_config_server_name_isolation.pas`
    - RED -> GREEN
  - `bash tests/scripts/test_intentional_context_level_sni_compatibility_labels_contract.sh`
    - PASS

- 这一步把路线判断也一起纠正了：
  - 当前最先暴露出来的问题已经不再是“要不要继续保留 direct server context 的 legacy state”
  - 而是：
    - FreePascal 已经进入 no-inheritance
    - OpenSSL / WolfSSL / MbedTLS / WinSSL 仍在 shared shim 上保留 inherited client fallback
  - 所以下一批真正该做的是跨 backend 的 shared client fallback 对齐，而不是继续围着旧 server-side control case 打转

## 增量收口：shared client fallback 跨 backend 对齐

- 上一轮路线判断现在已经完成闭环：
  - 新增 `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - 先直接跨 backend 打 RED，明确证明：
    - FreePascal 新 client connection 已经不继承 context-level `ServerName`
    - OpenSSL / WolfSSL / MbedTLS 仍会继承 `"client.example.com"`
    - WinSSL 在当前 Linux host 上因为 backend unavailable 被跳过，但源码仍在 shared helper 路径上

- 这说明当时的分歧不是“测试写法问题”，而是真正存在的 shared-seam runtime 差异：
  - FreePascal 已经切进 no-inheritance
  - 其余 shared-helper backend 还停在 inherited fallback

- 当前修法保持非常克制，只动 shared seam：
  - `src/fafafa.ssl.context.compat.pas`
    - 不再读取 deprecated context-level `GetServerName`
    - 对任意非空 context 一律返回 `''`
  - 这意味着 OpenSSL / WolfSSL / MbedTLS / WinSSL 仍可保留 shared seam 控制面
  - 但 shared seam 已经不再把 legacy state 自动流入新 client connection

- source contract 也必须跟着真相更新，否则它本身会变成错误阻塞：
  - `tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - 现在要求 helper 只出现在：
      - OpenSSL
      - WolfSSL
      - MbedTLS
      - WinSSL
    - 明确禁止 FreePascal 重新走 helper
    - 并禁止 helper 与所有 backend source 直接读 `(AContext|FContext).GetServerName`

- focused 结果说明这条 shared client fallback 分歧已经真正被收掉：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    - RED -> GREEN
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - PASS
  - `tests/test_factory_server_name_scope_clarification.pas`
    - PASS
  - `tests/test_factory_config_server_name_isolation.pas`
    - PASS
  - `tests/test_sslctxboth_client_capability_clarification.pas`
    - PASS
  - `bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
    - RED -> GREEN

- 这一步再次把主线往前推了一格：
  - “shared client fallback divergence” 已经不是未决问题
  - 当前剩下的更尖锐边界回到了最后一个 direct server-context legacy-state control case：
    - builder / factory 为什么还要继续保留 context state
    - 当这份 state 已经不再对任何新 client connection 产生 inherited fallback

## 增量收口：high-level context ServerName write surfaces 改成 warning + ignore

- shared client fallback 跨 backend 对齐之后，最后一个高层遗留面已经被真正打进 RED：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - `tests/test_factory_server_name_scope_clarification.pas`
  - `tests/test_factory_config_server_name_isolation.pas`
  - 它们共同暴露的是同一个问题：
    - 既然所有新 client connection 都已不再继承 deprecated context-level `ServerName`
    - builder / factory 就不该继续把这份 legacy state 写回新建 context

- 当前修法把这层高层遗留面统一切成了 `warning + ignore`：
  - `src/fafafa.ssl.context.builder.pas`
    - `BuildClient` 不再把 `WithSNI(...)` 写回 built client context
    - warning / validation 文案同步改成 `BuildClient ignores it...`
  - `src/fafafa.ssl.factory.pas`
    - client default-config / one-shot `CreateContext(...)` 路径
      不再把 `TSSLConfig.ServerName` 写回新建 context
    - warning 文案同步改成 `CreateContext ignores it for new contexts...`
  - `docs/reference/API_REFERENCE.md`
    - `WithSNI(...)` 与 `TSSLConfig.ServerName` 都已同步成 compatibility-only 且 high-level path 会 ignore

- 这一步的实际结果很关键：
  - deprecated context-level `ServerName` 已不再通过 builder / factory 这些高层新建入口流入新的 context state
  - 所有 client backend 也早已不再把它继承进新 client connection
  - 所以当前最后仍可观察的 compatibility surface，已经只剩 direct `ISSLContext.SetServerName/GetServerName` 本身和少量显式 API-surface coverage

- focused 结果说明这一步不是纸面降格，而是真正的行为收口：
  - `tests/test_context_builder_server_servername_runtime_consistency.pas`
    - PASS (`6 passed, 0 failed`)
  - `tests/test_factory_server_name_scope_clarification.pas`
    - PASS (`6 passed, 0 failed`)
  - `tests/test_factory_config_server_name_isolation.pas`
    - PASS (`6 passed, 0 failed`)
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - PASS (`16 passed, 0 failed`)
  - `tests/test_factory_server_name_compatibility_warning.pas`
    - PASS (`16 passed, 0 failed`)
  - `tests/config/test_config_validation.pas`
    - PASS (`53 passed, 0 failed`)
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    - PASS (`20 passed, 0 failed, 1 skipped`)

## 增量收口：OpenSSL direct library default-config ServerName alignment

- 在继续做 final public surface cleanup prep 之前，又摸出一个此前没被 focused 合同覆盖的高层漏口：
  - `src/fafafa.ssl.openssl.backed.pas`
    的 `TOpenSSLLibrary.CreateContext(...)`
    仍然会把 `FDefaultConfig.ServerName` 写回新建 context
  - 这意味着 generic factory 和 builder 虽然已经统一成 `warning + ignore`
  - 但 direct OpenSSL library path 还在偷偷保留旧注入语义

- 当前修法把这条 backend-specific 入口也收回到了和主线一致的真相：
  - `TOpenSSLLibrary.CreateContext(sslCtxClient)`
    - 不再把 `FDefaultConfig.ServerName` 写回 built context
    - 如果配置了 library log callback，会发出明确的 compatibility warning
  - `TOpenSSLLibrary.CreateContext(sslCtxServer)`
    - 若 default-config 带 `ServerName`，现在会 fail-fast 抛 `ESSLConfigurationException`
  - reject 也已经是真正的 fail-fast：
    - 不再先创建 context 再抛错

- 这一步的意义不只是“再修一个 OpenSSL 特例”：
  - 现在 builder、generic factory、direct OpenSSL library 这三条仍算高层的新建入口，都已经不再把 deprecated `ServerName` 流入新 context
  - 因而剩下的 public-surface 主问题，才真正只剩最后的 compatibility API 形状，而不是还有某个 backend-specific 高层口子在漏

- focused 结果说明这条新发现的实现分歧已经真正收掉：
  - `tests/test_openssl_library_default_config_server_name_clarification.pas`
    - RED (`3 passed, 8 failed`) -> GREEN (`13 passed, 0 failed`)
  - 邻接 retest：
    - `tests/test_cross_backend_client_context_server_name_clarification.pas`
      PASS (`20 passed, 0 failed, 1 skipped`)
    - 说明 OpenSSL direct-library 对齐没有碰坏当前 cross-backend no-inheritance 真相

## 增量收口：deprecated builder/config ServerName surface classification

- 在继续 final public surface cleanup prep 时，静态审查又暴露出一个工作流层面的残余漂移：
  - `tests/test_quick.pas` 这种普通 smoke 测试还在链式调用 `.WithSNI('example.com')`
  - `tests/winssl/test_winssl_connection_edge_cases.pas` 这种普通 edge-case 也还在顺手写 `LConfig.ServerName := ...`
  - 它们并不是在测 compatibility 语义本身，却会持续把 deprecated builder/config surface 伪装成正常主路径

- 这不是新的 backend bug，而是“public compatibility surface 还没有被彻底分类清楚”的工作流问题：
  - runtime 真相早已变成 `warning + ignore`
  - 但普通测试文本如果继续示范旧入口，后续审查仍会被反复拉回旧语义

- 当前这一刀的修法保持纯静态、纯护栏：
  - `tests/test_quick.pas` 去掉普通 builder smoke 里的 `.WithSNI('example.com')`
  - `tests/winssl/test_winssl_connection_edge_cases.pas` 去掉不再参与行为语义的 `LConfig.ServerName := ...`
  - 剩余必须保留 `WithSNI(...)` / `TSSLConfig.ServerName` 的测试文件，全部补上 `INTENTIONAL_COMPAT`
  - 新增 `tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    直接守住：
    - allowlist 文件必须显式带 `INTENTIONAL_COMPAT`
    - 普通测试若重新出现 deprecated builder/config ServerName surface，立刻红灯

- 这让 final public surface cleanup prep 真正进入了下一阶段：
  - 普通测试面已经不再教旧入口
  - 剩余命中都变成“明确知道自己在测 compatibility surface”的显式集合
  - 因而下一步终于可以直接讨论最终 API 形状，而不是继续做测试面排污

- focused 结果：
  - `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    - PASS
  - `tests/test_quick.pas`
    - PASS
    - 普通 builder smoke 在不依赖 `.WithSNI(...)` 的情况下仍正常构建 client/server context
  - `git diff --check`
    - PASS

## 增量收口：active direct-context ServerName surface classification

- 在普通 builder/config 测试面收干净后，public compatibility surface 还剩最后一类容易反复拉起的噪音：
  - active tests 里残余的 `Ctx.SetServerName(...)`
  - 其中有些已经带 `INTENTIONAL_API_SURFACE`
  - 但也有一批关键 compatibility regressions 还没有被统一纳入 repo-level 分类护栏

- 这会留下一个工作流漏洞：
  - 我们知道 runtime 已经不再把 context-level `ServerName` 传播进新连接
  - 但如果这些 direct-context 测试命中没有统一分类，下次重新审查时仍会先花时间判断它们是不是“普通流程又漏教旧入口”

- 当前这一刀做的是 direct-context 测试面的总分类，而不是行为迁移：
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - `tests/test_sslctxboth_client_capability_clarification.pas`
  - `tests/test_connection_builder_hostname_precedence.pas`
    现在都显式带上 `INTENTIONAL_COMPAT`
  - 新增 `tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
    直接守住：
    - active tests 里所有 real direct-context `SetServerName(...)` 命中都必须在 allowlist 中
    - 每个 allowlist 文件都必须带正确分类：`INTENTIONAL_COMPAT` 或 `INTENTIONAL_API_SURFACE`
    - allowlist 外若重新出现 direct context setter，直接红灯

- 这意味着 final public surface cleanup prep 已经再往前推进了一步：
  - 普通 builder/config 旧入口不再泄漏到 ordinary tests
  - active direct-context 命中也全部变成了“明确知道自己在测什么”的显式集合
  - 因而下一步可以不再做测试面排查，直接进入最终 API 形状决策

- focused 结果：
  - `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
    - PASS
  - `tests/test_cross_backend_client_context_server_name_clarification.pas`
    - PASS (`20 passed, 0 failed, 1 skipped`)
  - `tests/test_connection_builder_hostname_precedence.pas`
    - PASS (`9 passed, 0 failed`)
  - `tests/test_sslctxboth_client_capability_clarification.pas`
    - PASS (`28 passed, 0 failed, 1 skipped`)
  - `git diff --check`
    - PASS

## 验证证据

- `bash tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - PASS
- `tests/test_factory_connection_scope_clarification.pas`
  - PASS
- `tests/test_factory_server_name_scope_clarification.pas`
  - PASS
- `tests/test_sslctxboth_client_capability_clarification.pas`
  - PASS
- `bash tests/scripts/test_cross_backend_network_contracts_no_context_level_sni_guidance.sh`
  - PASS
- `bash tests/scripts/test_context_server_name_compat_shim_contract.sh`
  - PASS
- `tests/integration/test_cross_backend_consistency_contract.pas`
  - PASS
- `tests/integration/test_cross_backend_errors_contract.pas`
  - PASS
- `tests/test_cross_backend_client_context_server_name_clarification.pas`
  - PASS
- `tests/test_openssl_library_default_config_server_name_clarification.pas`
  - PASS
- `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
  - PASS
- `bash tests/scripts/test_active_direct_context_servername_surface_classification_contract.sh`
  - PASS
- `tests/test_quick.pas`
  - PASS
- `tests/test_connection_builder_hostname_precedence.pas`
  - PASS
- `tests/test_sslctxboth_client_capability_clarification.pas`
  - PASS
- `tests/test_context_builder_server_servername_runtime_consistency.pas`
  - PASS
- `tests/test_context_builder_server_name_compatibility_warning.pas`
  - PASS
- `bash tests/scripts/test_tls_connector_early_data_no_context_level_sni_guidance.sh`
  - PASS
- `tests/test_factory_server_name_compatibility_warning.pas`
  - PASS
- `tests/test_factory_config_server_name_isolation.pas`
  - PASS
- `tests/config/test_config_validation.pas`
  - PASS
- `tests/test_tls_connector_early_data_contract.pas`
  - PASS
- `tests/test_sslctxboth_client_capability_clarification.pas`
  - PASS
- `git diff --check`
  - PASS

## 当前最重要的路线判断

### 不要再迷失到这些线

- 不要回到 `v1.5.0` release closeout
- 不要回到旧的 SHA384 parity 重复验证
- 不要把 context-level SNI 问题误判成“一两个 setter 就能删掉”的局部清理

### 下一批最值得做的事

1. 进入 final public surface cleanup prep
   - builder / factory 的高层写入面已经全部变成 `warning + ignore`
   - `WithSNI(...)` 现在也已经是 compiler-level `deprecated`
   - direct OpenSSL library default-config path 也已完成对齐
   - direct `ISSLContext.SetServerName/GetServerName` 已成为最后仍可观察的 context-level compatibility surface
   - 下一步应优先评估：
     - `TSSLConfig.ServerName` 是否继续保留当前字段位置
     - `WithSNI(...)` 是否继续保留当前命名/入口，还是继续朝更窄的 compatibility surface 收口
     - direct context compatibility API 未来如何降格、替代或加 contract 护栏

2. `TSSLConfig` 拆层与 capability model presence bits 仍然排在后面
   - 它们是更大的设计债，不是当前 SNI 迁移主线的下一刀

## 总结

这轮已经把“接口设计是不是出了问题”和“这些问题有没有扩散到实现层”两件事都钉实了：

- **有问题，而且不是猜测。**
- **其中一部分已经是实现真相。**
- **最小高价值修复已经落地。**
- **更大的迁移方向也已经明确，不需要下次再重新判断。**

## 增量收口：WithSNI compiler-level deprecated truth

- 当主线已经收敛到 final public surface 时，`WithSNI(...)` 还剩最后一个不一致点：
  - runtime warning、validation wording、API 文档都已经把它降格成 compatibility-only
  - 但 public declaration 本身还没有编译期 `deprecated`
  - 这会让 fluent builder 用户在源码层拿不到与当前迁移路线一致的信号

- 这批已经把这条漏口补齐：
  - `src/fafafa.ssl.context.builder.pas`
    - `ISSLContextBuilder.WithSNI(...)`
    - `TSSLContextBuilderImpl.WithSNI(...)`
      都已经标成 compiler `deprecated`
  - deprecated message 统一导向：
    - `TSSLConnectionBuilder.WithHostname(...)`
    - `ISSLClientConnection.SetServerName(...)`

- 这一步保持 runtime 行为不变：
  - `BuildClient` / `BuildServer` 继续是 warning + ignore
  - 这批收的是 source-level truth，不是再改一轮行为语义

- 这批还同步处理了 intentional compatibility tests 的 compile 噪音：
  - 仍故意调用 `.WithSNI(...)` 的 focused compatibility tests 现在都做了局部 warning quarantine
  - 这样 focused compile 输出不再反复夹带这条我们已经明确接受的 deprecated warning

- focused 结果：
  - `bash tests/scripts/test_withsni_compiler_deprecated_contract.sh`
    - PASS
  - `bash tests/scripts/test_deprecated_context_servername_compat_surface_labels_contract.sh`
    - PASS
  - `tests/test_context_builder_server_name_compatibility_warning.pas`
    - PASS (`16 passed, 0 failed`)
  - `tests/config/test_config_validation.pas`
    - PASS (`53 passed, 0 failed`)
  - `git diff --check`
    - PASS

- 这意味着 `WithSNI(...)` 这半边已经真正收实：
  - 它不再只是“文档上说别用了”
  - 而是已经同时具备：
    - compatibility-only 定位
    - runtime warning + ignore
    - compiler deprecated declaration
    - explicit compatibility-test classification

## 当前最重要的路线判断补充

- `WithSNI(...)` 已不再属于“还要继续证明它是不是 compatibility-only”的问题
- 下一步真正未决的只剩最终 public surface 取舍：
  - `TSSLConfig.ServerName` 是否继续留在当前 record 上
  - `WithSNI(...)` 是否继续保留当前 fluent naming / placement
  - direct `ISSLContext.SetServerName/GetServerName` 是否继续作为最后 compatibility API 保留
