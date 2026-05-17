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
      - `test_connection_builder_hostname_precedence`
      - `test_tls_connector_hostname_override_precedence`
      - `test_freepascal_context_server_name_inheritance`
      - `test_context_builder_server_servername_runtime_consistency`
      - `test_sslctxboth_client_capability_clarification`
      - `test_cross_backend_consistency_contract`
      - `test_cross_backend_errors_contract`
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

## 验证证据

- `bash tests/scripts/test_interface_docs_no_nonexistent_isserverconnection_contract.sh`
  - PASS
- `tests/test_factory_connection_scope_clarification.pas`
  - PASS
- `tests/test_factory_server_name_scope_clarification.pas`
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

1. 进入 **context-level SNI compatibility migration** 的 Phase D 准备
   - 现在 shared compatibility shim 已经存在
   - 下一步应评估 `TSSLConfig.ServerName` / builder `WithSNI(...)` 的最终 surface cleanup 切口

2. 再决定 behavior migration 的第一条 RED
   - 明确哪些 intentional-compat tests 会被改写
   - 明确新优先级应该怎样从 context-level 迁到 per-connection hostname 路径

3. `TSSLConfig` 拆层与 capability model presence bits 仍然排在后面
   - 它们是更大的设计债，不是当前 SNI 迁移主线的下一刀

## 总结

这轮已经把“接口设计是不是出了问题”和“这些问题有没有扩散到实现层”两件事都钉实了：

- **有问题，而且不是猜测。**
- **其中一部分已经是实现真相。**
- **最小高价值修复已经落地。**
- **更大的迁移方向也已经明确，不需要下次再重新判断。**
