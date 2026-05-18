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

## Not The Next Step

- 不要现在就重开 `context-level SNI` 清理
- 不要直接改 `ISSLConnection` public interface
- 不要把 `TSSLConfig` slimming 和 backend runtime refactor 混成同一批

## Expected Outcome

- 下一条 interface-design 主线从“模糊的大债”变成“可执行的小批次路线”
- 继续保持每批次 focused、可验证、可提交
