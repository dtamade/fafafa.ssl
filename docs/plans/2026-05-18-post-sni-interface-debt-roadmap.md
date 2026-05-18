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

- 当前更准确的 next step：
  - 不再继续讨论 “`Options` vs legacy booleans 到底谁赢”
  - 不再继续补 option-bridge public wording / test labels
  - 转向真正的 `TSSLConfig` public-surface slimming / migration 设计
  - 只有这条线稳定后，再考虑是否进入 `ISSLConnection` 核心 surface slimming

## Not The Next Step

- 不要现在就重开 `context-level SNI` 清理
- 不要直接改 `ISSLConnection` public interface
- 不要把 `TSSLConfig` slimming 和 backend runtime refactor 混成同一批

## Expected Outcome

- 下一条 interface-design 主线从“模糊的大债”变成“可执行的小批次路线”
- 继续保持每批次 focused、可验证、可提交
