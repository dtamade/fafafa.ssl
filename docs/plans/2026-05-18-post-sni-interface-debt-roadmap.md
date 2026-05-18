# Post-SNI Interface Debt Roadmap

## Goal

在 `context-level SNI` compatibility family 已经在 `v1.x` 冻结完成之后，选择下一条最值得推进的 broader interface-design route，避免重新掉回旧兼容语义清理。

## Current Candidates

### 1. `TSSLConfig` 跨层字段拆分 / slimming

Current evidence already on disk:

- `BufferSize` / `HandshakeTimeout`
  - factory path 已显式拒绝
  - 说明它们是 connection-scoped，不是 context/factory config
- `LogLevel` / `LogCallback`
  - factory path 已显式拒绝
  - 说明它们是 library-scoped，不是 context/factory config
- `EnableSessionTickets` / `EnableOCSPStapling` / `EnableCompression`
  - 仍然通过 config 归一化进 `Options`
  - 是仍在 public record 中承担兼容桥接的字段
- `ServerName`
  - 已冻结成 `v1.x` compatibility-only field

Why this route is attractive now:

- 已有大量 scope truth 和 focused contracts
- 现有设计债已经足够清楚，不需要重新考古
- 可以先做“字段分层 roadmap / compatibility buckets”而不立刻做破坏性移除

### 2. `ISSLConnection` 核心 surface slimming

Current evidence already on disk:

- 旧审查已指出 `ISSLConnection` 过胖
- 当前 interface 同时承载：
  - 生命周期 / 读写
  - 连接信息
  - 健康状态
  - 性能指标
  - 诊断信息
- 这条线会直接波及：
  - core public interface
  - 所有 backend connection 实现
  - 大量测试 / mock / helper

Why this route is riskier as the immediate next step:

- write scope 太大
- 需要先决定哪些能力拆成 optional interface，哪些仍保留在 core
- 更容易把当前已经稳定的 backend/test 面一起拖进重构

## Recommendation

先做 **`TSSLConfig` 跨层字段拆分 / slimming roadmap**，暂不直接开 `ISSLConnection` 大手术。

Recommended first bounded batch:

1. 盘点 `TSSLConfig` 各字段的真实 scope：
   - library-scoped
   - context-scoped
   - connection-scoped
   - compatibility-only
2. 把这些字段分成明确 buckets，并落盘到新的 design/plan 文档。
3. 先补 source/doc/test contract，守住“不再继续往 `TSSLConfig` 塞跨层字段”的方向。
4. 只有在 buckets 稳定后，才决定是否做轻量 API 补面或 legacy bridge 缩减。

## Progress Since This Roadmap Was Written

- 已交付：
  - `TSSLConfig` scope buckets truth
  - fresh default-config option-bridge truth parity
  - option-bridge conflict precedence freeze
  - option-bridge surface truth freeze
  - active guidance cleanup
  - public-surface slimming roadmap

- 当前更准确的 next step：
  - 不再继续讨论 “`Options` vs legacy booleans 到底谁赢”
  - 不再继续补 option-bridge public wording / test labels
  - 不再继续把 `TSSLConfig` 线当成默认下一步
  - 当前最新暴露出来的高优先级问题，是活跃 `API_REFERENCE` 中 `ISSLConnection` / `ISSLSession` source truth 明显漂移
  - 因此应先做 `ISSLConnection surface truth freeze`：
    - 修正文档签名和示例
    - 明确 compatibility-core mirrors 与 optional owners
    - 加 focused contract，防止旧接口名回流
  - 只有在这条文档/contract 真相冻结稳定后，再决定第一条真实 slimming slice
  - `LogLevel` / `LogCallback` library-default detachment 继续保留为后续候选，但不再是默认 immediate next batch

## Progress Since The Truth-Freeze Batch

- 已交付：
  - `ISSLConnection` / `ISSLSession` active-doc truth freeze
  - focused contract `tests/scripts/test_isslconnection_surface_truth_contract.sh`
  - 连接层剩余三份历史 completion-audit plan 的 focused execution receipt 补齐：
    - client-connection SNI
    - connection native-handle
    - connection OCSP interface

- 当前更准确的 next step：
  - 不再继续停留在历史 execution receipt closeout
  - `ISSLConnectionInfo` / `ISSLSessionResumption` / `ISSLCertificateVerification` 也都已有 current execution evidence
  - 连接层当前真正剩下的，已经主要是 `compatibility-core slimming` 设计债，而不是“有没有验证过”
  - 因此下一批应进入第一条真正的 slimming slice
  - 推荐优先顺序：
    1. 先做 `ISSLConnectionInfo` mirror demotion / migration-map batch
    2. 再决定是否进入 `GetStateString` / `GetContext` / `GetSelectedALPNProtocol` 这组 convenience mirror 的实际收瘦路线

## Progress Since The Connection-Surface Revalidation Batch

- 已交付：
  - `ISSLConnectionInfo` mirror demotion / migration-map batch
  - `INTERFACE_DESIGN_V2` 已补出 `ISSLConnectionInfo`，并纠正 Stage-A demotion target
  - focused contract `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`

- 当前更准确的 next step：
  - 不再继续修正 `INTERFACE_DESIGN_V2` 内部 owner 冲突
  - `ISSLConnectionInfo` 这组 mirrors 的 Stage-A demotion map 已有稳定设计锚点
  - active docs 也已开始把 connection-info mirrors 从 core teaching 路径切到 `ISSLConnectionInfo`
  - 下一批应进入 source-facing slimming prep：
    1. 先给 `TBaseSSLConnection` / source comments 补一条 focused source-truth contract，
       锁住这 4 个 mirrors 当前确实是 compatibility-core duplicates
    2. 然后再决定第一条真正的实现切片是：
       - 先只做 source/classification freeze
       - 还是直接开某个 mirror 的 de-emphasis / deprecation 路线

## Progress Since The Active-Guidance Batch

- 已交付：
  - source-facing classification freeze for the `ISSLConnectionInfo` mirror group
  - `src/fafafa.ssl.base.pas` 与 `src/fafafa.ssl.connection.base.pas` 已补 Stage-A classification notes
  - focused contract `tests/scripts/test_isslconnectioninfo_source_classification_contract.sh`

- 当前更准确的 next step：
  - 不再继续补 source comments / owner labels
  - 设计文档、active docs、source comments 现在都已经承认这 4 个 mirrors 是 `compatibility-core duplicates`
  - 下一批应真正决定第一条实现切片：
    1. 先做某个 mirror 的 de-emphasis / deprecation 路线
    2. 或先做更细的 source/class split feasibility batch

## Progress Since The Source-Classification Freeze

- 已交付：
  - residual `GetContext` active-guidance cut
  - `docs/CAPABILITY_MATRIX_GUIDE.md` 不再把 `Conn.GetContext` 当 capability example 的推荐路径
  - `docs/reference/API_REFERENCE.md` 已把 `GetContext` 明确纳入 `ISSLConnectionInfo` first guidance
  - focused contract `tests/scripts/test_isslconnectioninfo_getcontext_active_guidance_contract.sh`

- 当前更准确的 next step：
  - `GetContext` 现在已经是 4 个 mirrors 里最干净、最适合先开的第一优先对象
  - 活跃文档不再教 `Conn.GetContext`
  - 生产源码里除基类实现外，残余 live coupling 已收缩到 contract mirror-equality proof
  - 下一批应优先做 `GetContext` 的 source/class split feasibility：
    1. 先决定 contract 层是否继续把 core getter 视为强 owner，还是只保留 mirror-equality 约束
    2. 再决定是否进入更强的 deprecation / removal 路线

## Progress Since The GetContext Guidance Cut

- 已交付：
  - `GetContext` contract owner primacy
  - `tests/contract/test_backend_contract.pas` 现在先验证 `ISSLConnectionInfo.GetContext` 对创建 context 的 owner truth
  - `ISSLConnection.GetContext` 只保留为 mirror-equality proof
  - focused source guard `tests/scripts/test_isslconnectioninfo_getcontext_contract_owner_contract.sh`

- 当前更准确的 next step：
  - `GetContext` 已经不再被 active docs 或 contract 叙事当作双 owner 路径
  - 下一批可以直接进入更强的 `GetContext` source/class split feasibility
  - 如果 focused feasibility 继续支持当前方向，再决定是否进入 public deprecation / removal route

## Progress Since The GetContext Owner-Primacy Batch

- 已交付：
  - `GetContext` source/class split feasibility freeze
  - `src/fafafa.ssl.base.pas` / `src/fafafa.ssl.connection.base.pas` 已明确写出 owner / mirror / preferred-access 语义
  - focused allowlist contract `tests/scripts/test_isslconnectioninfo_getcontext_source_class_split_contract.sh`

- 当前更准确的 next step：
  - `GetContext` 的 remaining live surface 已稳定冻结到：
    - base/interface declarations
    - one shared base implementation
    - one backend-contract core mirror proof
    - one active-doc `ConnInfo.GetContext` example
  - 这说明它已经不再需要继续做 evidence cleanup
  - 下一批应在两条路径里二选一：
    1. 直接进入 `GetContext` 的 public deprecation wording route
    2. 把主线切到下一条 mirror（更可能是 `GetStateString`）

## Progress Since The GetContext Split-Freeze Batch

- 已交付：
  - `GetStateString` active generic/integration test de-emphasis
  - `tests/connection/test_connection_basic.pas` 不再把 direct core `GetStateString` 当普通路径
  - `tests/integration/test_real_https_connection.pas` 的握手失败输出已转到 `ISSLConnectionInfo`-first helper
  - focused contract `tests/scripts/test_isslconnectioninfo_getstatestring_active_test_contract.sh`

- 当前更准确的 next step：
  - `GetStateString` 已不再由 generic/integration tests 教回 core getter
  - 剩余 direct core `GetStateString` 主要收缩到：
    - backend-specific runtime tests
    - backend contract mirror proof
  - 下一批应决定：
    1. 把这些 residual uses 做 intentional classification / allowlist freeze
    2. 或切到 `GetSelectedALPNProtocol`

## Progress Since The GetStateString Active-Test Batch

- 已交付：
  - `GetStateString` residual classification freeze
  - `src/fafafa.ssl.base.pas` / `src/fafafa.ssl.connection.base.pas` 已补 preferred-access / owner / residual-surface notes
  - focused allowlist contract `tests/scripts/test_isslconnectioninfo_getstatestring_residual_classification_contract.sh`

- 当前更准确的 next step：
  - `GetStateString` 的 remaining direct-core surface 已稳定冻结到：
    - backend contract mirror proof
    - OpenSSL / WolfSSL backend-specific runtime OCSP stapling tests
  - 这说明 `GetStateString` 当前也不再需要继续做 evidence cleanup
  - 下一批应在两条路径里二选一：
    1. 讨论 `GetStateString` 的更强 deprecation wording / owner route
    2. 把主线切到 `GetSelectedALPNProtocol`

## Progress Since The GetStateString Residual-Classification Batch

- 已交付：
  - `GetSelectedALPNProtocol` active generic/integration test de-emphasis
  - `tests/integration/test_real_https_connection.pas` 的 ALPN 成功路径已转到 `ISSLConnectionInfo`-first helper
  - `tests/integration/test_cross_backend_consistency_contract.pas` 的归一化 ALPN 探测已转到 `ISSLConnectionInfo`-first helper
  - focused contract `tests/scripts/test_isslconnectioninfo_getselectedalpn_active_test_contract.sh`

- 当前更准确的 next step：
  - ordinary integration/contract tests 已不再把 direct core `GetSelectedALPNProtocol` 当推荐路径
  - 剩余 direct core `GetSelectedALPNProtocol` 已收缩到：
    - backend contract mirror proof
    - MbedTLS backend-specific runtime test
    - WinSSL backend-specific runtime tests
  - 下一批应在两条路径里二选一：
    1. 把这些 residual uses 做 intentional classification / allowlist freeze
    2. 讨论 `GetSelectedALPNProtocol` 的更强 client-owner / deprecation wording route

## Progress Since The GetSelectedALPN Active-Test Batch

- 已交付：
  - `GetSelectedALPNProtocol` residual classification freeze
  - `src/fafafa.ssl.base.pas` / `src/fafafa.ssl.connection.base.pas` 已补 preferred-access / owner / residual-surface notes
  - focused allowlist contract `tests/scripts/test_isslconnectioninfo_getselectedalpn_residual_classification_contract.sh`

- 当前更准确的 next step：
  - `GetSelectedALPNProtocol` 的 remaining direct-core surface 已稳定冻结到：
    - backend contract mirror proof
    - MbedTLS backend-specific runtime ALPN test
    - WinSSL backend-specific runtime ALPN tests
  - 这说明 `GetSelectedALPNProtocol` 当前也不再需要继续做 evidence cleanup
  - 下一批应在两条路径里二选一：
    1. 讨论 `GetSelectedALPNProtocol` 的更强 client-owner / deprecation wording route
    2. 把主线切到 `GetConnectionInfo`

## Progress Since The GetSelectedALPN Residual-Classification Batch

- 已交付：
  - `GetConnectionInfo` residual classification freeze
  - `src/fafafa.ssl.base.pas` / `src/fafafa.ssl.connection.base.pas` 已补 preferred-access / owner / residual-surface notes
  - focused allowlist contract `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`

- 当前更准确的 next step：
  - `GetConnectionInfo` 的 remaining direct-core surface 已稳定冻结到：
    - backend contract mirror proof
    - OpenSSL backend-specific connection-info contract test
    - WinSSL backend-specific connection-info / edge-case tests
  - 这说明 `GetConnectionInfo` 当前也不再需要继续做 evidence cleanup
  - 并且 `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString`
    这 4 条 `ISSLConnectionInfo` Stage-A mirror 路线都已完成 residual freeze
  - 下一批应在两条路径里二选一：
    1. 进入更强的 owner / deprecation wording route
    2. 转向基于这些 residual files 的 backend implementation-completeness 审查

## Progress Since The GetConnectionInfo Residual-Classification Batch

- 已交付：
  - `GetConnectionInfo` shared-layer completeness enrichment for `ServerName` / `SessionId`
  - focused mock behavior proof in `tests/test_connection_builder_hostname_precedence.pas`
  - OpenSSL fresh-connection guard revalidation in `tests/test_openssl_connection_info_cipher_contract.pas`

- 当前更准确的 next step：
  - 不再继续拉起 `GetConnectionInfo` residual archaeology
  - 当前已经确认，共享层能安全补齐的低风险 metadata 已经先补完：
    - `ServerName`
    - `SessionId`
  - 这条线剩余的才是真正需要 backend-specific completeness 审查的字段：
    - `PeerCertificate`
    - `CipherSuiteId`
    - `KeyExchange`
    - `Cipher`
    - `Hash`
    - `KeySize`
    - `MacSize`
  - 若继续推进，应优先做一条 bounded batch：
    1. 先静态盘点这些字段在哪些 backend 已有现成来源
    2. 再挑一组最能共享或最容易 contract proof 的字段做下一刀
  - 更强 owner / deprecation wording route 继续保留为平行候选，但不应盖过当前 implementation-completeness 主线

## Progress Since The GetConnectionInfo ServerName-SessionId Batch

- 已交付：
  - `GetConnectionInfo` shared-layer completeness enrichment for `PeerCertificate`
  - focused mock proof still anchored in `tests/test_connection_builder_hostname_precedence.pas`
  - API truth updated so `PeerCertificate` is no longer described as purely backend-specific best-effort

- 当前更准确的 next step：
  - 不再把 `PeerCertificate` 当作剩余 completeness debt
  - shared layer 现在已经补齐的 `GetConnectionInfo` 字段是：
    - `ServerName`
    - `SessionId`
    - `PeerCertificate`
  - implementation-completeness 主线真正剩下的已经进一步收缩到 backend-specific crypto detail：
    - `CipherSuiteId`
    - `KeyExchange`
    - `Cipher`
    - `Hash`
    - `KeySize`
    - `MacSize`
  - 若继续推进，应优先做一条新的 bounded batch：
    1. 先静态盘点上述 6 个字段在 OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 里各自已有多少真实来源
    2. 再挑一组最容易共享归一或最适合 contract proof 的字段落第一刀
  - 更强 owner / deprecation wording route 继续保留为平行候选，但当前不应压过 implementation-completeness 主线

## Progress Since The GetConnectionInfo PeerCertificate Batch

- 已交付：
  - shared name-derived first slice for `GetConnectionInfo` crypto detail
  - shared `CipherSuite`-name normalization now derives:
    - `Cipher`
    - `Hash`
    - `KeySize`
    - `KeyExchange` when the negotiated suite name still carries a legacy prefix
  - focused mock proof still anchored in `tests/test_connection_builder_hostname_precedence.pas`

- 当前更准确的 next step：
  - 不再把 `Cipher` / `Hash` / `KeySize` 当作 shared-layer completeness debt
  - `KeyExchange` 也已经在 legacy-prefix case 上有了 shared best-effort truth
  - 当前真正还值得继续深挖的，已经更聚焦到：
    - `CipherSuiteId`
    - `MacSize`
    - 以及无法只靠名字稳定推导的更细平台差异
  - 若继续推进，应优先做一条新的 bounded batch：
    1. 先把 OpenSSL / WinSSL 已有的 low-level detail truth 做成静态矩阵
    2. 再决定下一刀是：
       - 收 `CipherSuiteId`
       - 还是先收 `MacSize`
  - 更强 owner / deprecation wording route 继续保留为平行候选，但当前不应压过 implementation-completeness 主线

## Progress Since The GetConnectionInfo Crypto Detail Name-Derived First Slice

- 已交付：
  - `GetConnectionInfo` `CipherSuiteId` first slice
  - shared layer 对标准 TLS 1.3 suite name 的 `CipherSuiteId` best-effort derivation
  - OpenSSL low-level truth：
    - 优先 `SSL_CIPHER_get_protocol_id`
    - 回退 `SSL_CIPHER_get_id` 低 16 位
  - focused mock proof 仍锚定在 `tests/test_connection_builder_hostname_precedence.pas`
  - focused OpenSSL contract 现在同时覆盖：
    - fresh-connection safe-degrade guard
    - low-level `CipherSuiteId` truth

- 当前更准确的 next step：
  - 不再把 `CipherSuiteId` 当作剩余 completeness debt
  - 当前真正还值得继续深挖的，已经进一步收缩到：
    - `MacSize`
    - 以及无法只靠名字或统一 low-level helper 稳定归一的更细平台差异
  - 若继续推进，应优先做一条新的 bounded batch：
    1. 先静态盘点 `MacSize` 在 OpenSSL / WinSSL / FreePascal / MbedTLS / WolfSSL 中到底有哪些真实来源与口径差异
    2. 再决定是：
       - 收 `MacSize`
       - 还是先补更高层的 owner / deprecation wording
  - 更强 owner / deprecation wording route 继续保留为平行候选，但当前不应压过 implementation-completeness 主线

## Progress Since The GetConnectionInfo WinSSL Cipher Truth Correction

- 已交付：
  - WinSSL `GetConnectionInfo` cipher truth correction
  - static audit proved `SecPkgContext_ConnectionInfo.aiCipher` is only an algorithm-level field, not a real TLS cipher-suite id source
  - WinSSL 现在会优先走 Schannel `SECPKG_ATTR_CIPHER_INFO` / `dwCipherSuite`
  - 当 Schannel 可返回真实 suite name 时，WinSSL `CipherSuite` 也会优先对齐该 truth

- 当前更准确的 next step：
  - 不再把 “WinSSL 已经直接掌握 `CipherSuiteId`” 当成既定事实
  - `CipherSuiteId` 这条线现在更稳了，但 `MacSize` 仍没有同等级的统一 truth
  - 若继续推进，应优先做一条新的 bounded batch：
    1. 先盘清 `MacSize` 在 WinSSL 当前是否只是 `dwHashStrength` proxy
    2. 再横向对 OpenSSL / FreePascal / MbedTLS / WolfSSL 形成语义矩阵
    3. 只有在口径统一后再决定是否补实现
  - 更强 owner / deprecation wording route 继续保留为平行候选，但当前不应压过 implementation-completeness 主线

## Progress Since The GetConnectionInfo MacSize Semantics Matrix

- 已交付：
  - `GetConnectionInfo` `MacSize` semantics matrix
  - shared suite-name AEAD-first `MacSize` derivation
  - WinSSL `inherited-first + guarded dwHashStrength fallback`

- 当前更准确的 next step：
  - 不再把 `MacSize` 当成“所有 backend 都完全缺值”的残余 debt
  - 当前已经统一收住的真相是：
    - 可识别 AEAD suite name -> shared `MacSize` truth
    - WinSSL `dwHashStrength div 8` -> 只剩 legacy fallback
  - 剩余未统一的更窄边界是：
    1. legacy non-AEAD suites 是否要继续保持 `0`
    2. 是否值得为 OpenSSL/WinSSL 再补更强的 low-level legacy `MacSize` truth
  - 若继续推进，应优先在两条路径里二选一：
    1. 继续做 legacy/non-AEAD `MacSize` truth feasibility
    2. 把主线切回更强的 owner / deprecation wording route

## Progress Since The OpenSSL GetConnectionInfo Legacy MacSize Truth Batch

- 已交付：
  - OpenSSL legacy/non-AEAD `MacSize` low-level truth
  - active export/binding chain for:
    - `SSL_CIPHER_is_aead`
    - `SSL_CIPHER_get_digest_nid`
    - `EVP_get_digestbynid`
  - focused OpenSSL contract now explicitly covers:
    - helper unavailable safe degrade
    - digest-truth non-AEAD `MacSize`
    - AEAD `MacSize` owner primacy

- 当前更准确的 next step：
  - 不再把 OpenSSL 当成 “只有 shared AEAD `MacSize` truth” 的 backend
  - 当前 `MacSize` 线上已经形成的稳定真相是：
    - shared AEAD suite-name truth
    - OpenSSL non-AEAD digest truth
    - WinSSL guarded legacy fallback
  - 剩余更窄的未统一边界是：
    1. WinSSL fallback 是否还值得继续强化成更强 legacy truth
    2. MbedTLS / WolfSSL 是否存在同等级、且值得接入的 low-level `MacSize` source
  - 若继续推进，应优先在两条路径里二选一：
    1. 先做剩余 backend 的 static `MacSize` low-level feasibility
    2. 把主线切回更强的 owner / deprecation wording route

## Progress Since The WolfSSL GetConnectionInfo Legacy MacSize Truth Batch

- 已交付：
  - WolfSSL legacy/non-AEAD `MacSize` low-level truth
  - `wolfssl.api` active export/binding chain now includes:
    - `wolfSSL_GetHmacSize`
  - focused WolfSSL contract now explicitly covers:
    - helper unavailable safe degrade
    - HMAC-truth non-AEAD `MacSize`
    - shared AEAD owner primacy

- 当前更准确的 next step：
  - 不再把 WolfSSL 当成 “只有 shared AEAD `MacSize` truth” 的 backend
  - 当前 `MacSize` 线上已经形成的稳定真相是：
    - shared AEAD suite-name truth
    - OpenSSL non-AEAD digest truth
    - WolfSSL non-AEAD HMAC truth
    - WinSSL guarded legacy fallback
  - 剩余更窄的未统一边界是：
    1. MbedTLS 是否也存在同等级、且值得接入的 low-level `MacSize` source
    2. 如果 MbedTLS 收益不高，是否就此结束 `MacSize` 深挖并切回 owner / deprecation wording route
  - 若继续推进，应优先做：
    1. MbedTLS static `MacSize` feasibility
    2. 再根据 binding 复杂度决定是实现还是收线

## Not The Next Step

- 不要现在就重开 `context-level SNI` 清理
- 不要直接改 `ISSLConnection` public interface
- 不要把 `TSSLConfig` slimming 和 backend runtime refactor 混成同一批

## Expected Outcome

- 下一条 interface-design 主线从“模糊的大债”变成“可执行的小批次路线”
- 继续保持每批次 focused、可验证、可提交
