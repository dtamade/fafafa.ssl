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

1. 完成 `context-level ServerName` Phase B 的第二刀：
   - 收窄 `factory/config` 写入面
   - 让 `TSSLConfig.ServerName` / `TSSLFactory.CreateContext(...)` 也更明确地表现为 compatibility-only，而不是继续像推荐主路径
   - 继续不要回退到 warning 噪音治理，也不要提前硬删 backend fallback
2. 在 Phase B 的高层 surface 稳定后，再评估 Phase C 的 shared compatibility shim extraction。
3. 在 capability 与 SNI 迁移边界都稳定后，再评估 `TSSLConfig` 跨层字段拆分时机。
4. 若未来要让 serializer 对“纯 legacy-only in-memory record”也具备完全无歧义的 projection，需要先为 capability model 补 presence/truth 元信息；当前批次不在无信号状态下瞎猜。

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
