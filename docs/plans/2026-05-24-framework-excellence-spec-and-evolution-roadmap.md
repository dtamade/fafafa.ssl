# 2026-05-24 Framework Excellence Spec And Evolution Roadmap

## Goal

为 `fafafa.ssl`
补上一份足够长期、足够系统、同时又与当前 `v1.5.0` shipped truth 对齐的
总体规格 / 架构原则 / 演进路线锚点，
让后续每一轮实现都围绕

- 正确性
- 先进性
- 优雅性
- 可维护性
- 性能

这五个目标推进，
而不是继续被局部 closeout 或单点 drift 带着走。

## Why Now

当前仓库已经不是“功能缺很多、先把东西跑起来”的阶段。

现状更接近：

- `v1.5.0` 已发布
- 多 backend 主体能力已经成型
- 大量局部路线已经完成真相冻结 / focused contract / compile gate
- 真正还活着的高价值问题，
  已经从“功能不存在”
  转向“架构边界是否足够清晰、长期是否能继续优雅演进”

如果继续只按局部 seam 往前推，
容易出现两种问题：

1. 把已经关闭的 family 当成下一步反复重开
2. getter-by-getter / warning-by-warning 地前进，
   却缺少一份统一解释这些动作为何正确的总体设计

## North Star

`fafafa.ssl`
应成为 FreePascal 领域里
最值得长期依赖的 TLS/SSL 框架之一，
并同时满足三种用户心智：

1. **普通应用开发者**
   - 用最少入口即可安全建立 TLS
   - 默认路径清晰，不需要理解全部 backend 细节
2. **框架/中间件集成者**
   - 能拿到稳定、精简、可组合的核心抽象
   - 不被 convenience mirrors 与历史 baggage 干扰
3. **高级/平台开发者**
   - 能显式选择 backend
   - 能获取 capability truth / native handle / advanced owner surface
   - 能做高性能与系统级集成

换句话说，
它必须同时是：

- 易用的 high-level facade
- 可信的 low-level abstraction layer
- 可验证的 multi-backend capability system
- 有竞争力的 Pascal-first TLS runtime

## What Excellent Means

### 1. Public API 必须有清晰分层

对调用方而言，
入口必须天然分成三层：

1. **普通主入口**
   - `uses fafafa.ssl, fafafa.ssl.context.builder;`
   - `TSSLContextBuilder`
   - `TSSLConnector` / `TSSLAcceptor` / `TSSLStream`
2. **高级固定-backend 入口**
   - `TSSLFactory.GetLibraryInstance(...)`
   - `Lib.CreateContext(...)`
3. **backend-private / runtime escape hatch**
   - optional interfaces
   - native handle
   - capability-gated backend surfaces

优秀框架不是把所有入口都做成“都能用”，
而是让调用方一眼知道：
哪个是默认路径，
哪个是高级路径，
哪个只是 escape hatch。

### 2. 安全真相必须优先于易用幻觉

只要涉及：

- certificate validation
- early data
- OCSP / CT
- session resumption
- filesystem-backed anti-replay

默认原则都应保持：

- fail-closed 优先
- capability truth 优先
- 不做 silent downgrade
- 不把 experimental surface 伪装成 stable baseline

### 3. 可维护性必须来自“架构边界干净”

优秀不只是“功能多”，
而是：

- core interface 小而稳
- optional owner surfaces 有明确 owner
- compatibility surface 被显式标注
- 文档、源码、contracts 指向同一张图

### 4. 性能必须是架构属性，不是事后优化

性能目标不应只停留在 benchmark 脚本，
而应体现在设计上：

- façade 不能强迫高层做多余分配
- core 不能为了对称把所有高级功能塞进热路径
- capability / diagnostics 不能污染普通 I/O 主线
- pure Pascal backend 必须持续追求 correctness 之外的实现质量

### 5. 文档与 contracts 是产品的一部分

在这个项目里，
文档不只是说明书，
而是 public surface 的一部分。

因此优秀框架必须做到：

- active docs 不教学旧入口
- API reference 不背离 source truth
- roadmap 不重复 historical closeout
- focused contracts 能守住关键 public truth

## Architecture Principles

### Principle 1: Small Core, Rich Owners

`ISSLConnection`
应继续朝最小核心抽象收敛：

- 生命周期
- 握手
- 读写
- 非阻塞状态
- 协商结果

其余能力默认通过 owner interface 暴露：

- `ISSLConnectionInfo`
- `ISSLConnectionControl`
- `ISSLConnectionTextIO`
- `ISSLDiagnostics`
- `ISSLSessionResumption`
- `ISSLCertificateVerification`
- `ISSLOCSPStapling`

这条原则的含义不是“马上破坏兼容”，
而是：

- `v1.x` 允许 compatibility mirrors 继续存在
- 但新设计、新文档、新 contracts 必须先承认 owner truth

### Principle 2: One Surface, One Responsibility

每个 public surface
都应尽量只承担一种主要职责：

- config 用于 build-stage configuration
- connection 用于 active transport
- capability 用于 feature truth
- diagnostics 用于 observability
- session interface 用于 resumption

一旦某个 surface
同时承担 build-stage、runtime、compatibility 三种语义，
长期维护成本就会明显上升。

### Principle 3: Capability-First, Not Helper-First

public truth
必须优先表达 backend 是否真的支持某项能力，
而不是只表达“某个 helper 名字还在不在”。

因此：

- paired feature 继续保持 `support-level-first`
- legacy `Supports*` 继续视为 compatibility projection
- optional interface 继续按 capability / runtime truth 暴露

### Principle 4: Scope-Correct Configuration

配置必须与生命周期对齐。

长期目标里，
我们应把 public 配置心智拆清楚：

- library-scoped
- context-scoped
- connection-scoped
- compatibility-only
- backend-private opt-in

`TSSLConfig`
当前仍是 mixed-scope record，
所以短期重点不是“立刻拆掉”，
而是：

- 不再新增跨层幻觉字段
- 继续把 scope truth 明确记录清楚
- 为后续 v2 surgery 准备分桶设计

### Principle 5: Compatibility Must Be Explicit And Bounded

兼容面不是罪，
但未标注的兼容面会持续腐蚀设计。

因此任何兼容 surface
都应明确属于以下之一：

- convenience mirror
- compatibility-core mirror
- deprecated compatibility field
- frozen compatibility family

不能再出现“源码保留了，但文档还把它当普通主入口”的情况。

### Principle 6: Symmetry Is Valuable, But Fake Symmetry Is Harmful

当前没有 `ISSLServerConnection`
并不是 bug；
真正的问题是 server-side 语义是否已经足够稳定、
足够值得抽象成对称 public interface。

因此：

- 不为了“client 有，所以 server 也要有”而硬补接口
- 先确认 server-side 能力边界
- 再决定是否需要公开对称扩展

### Principle 7: The Facade Must Be Curated

`fafafa.ssl`
主门面不是“历史所有入口的大仓库”。

长期目标应是：

- façade 只保留最值得普通用户依赖的高价值 surface
- advanced / raw helper 路径显式降级
- 普通文档只教 curated 主路径

### Principle 8: Pure Pascal Backend Is A Strategic Asset

`FreePascal` backend
不只是“少一个 backend”。

它是这个项目区别于普通 wrapper 的战略资产：

- 零外部 SSL 动态库
- Pascal-first 生态价值
- 可控的 TLS runtime
- 更强的可移植性与可调试性

因此它的目标不该只是“能工作”，
而应持续向：

- completeness
- correctness
- performance
- observability
- packaging simplicity

推进。

### Principle 9: Verification Is Part Of Architecture

好的架构必须可验证。

因此每条高价值设计判断，
都应尽量落到：

- source comment truth
- API/reference truth
- focused contract
- compile/gate evidence

而不是只停留在设计文档。

## Target Architecture Model

### Layer 1: Curated Public Facade

面向绝大多数调用方：

- `fafafa.ssl`
- `fafafa.ssl.context.builder`
- `TSSLConnector`
- `TSSLAcceptor`
- `TSSLStream`

目标：

- 路径短
- 语义稳定
- 默认安全
- 普通示例不需要掉回 `fafafa.ssl.base`

### Layer 2: Stable Core Abstractions

最重要的长期资产：

- `ISSLLibrary`
- `ISSLContext`
- `ISSLConnection`
- `ISSLCertificate`
- `ISSLCertificateStore`
- `ISSLSession`

其中 `ISSLConnection`
要继续从“过胖的现实”
向“精炼但兼容的核心抽象”演进。

### Layer 3: Optional Owner Surfaces

这是当前架构里最值得坚持的方向：

- 把高级能力拆到 owner interfaces
- 让 capability / runtime truth 决定是否暴露
- 让第三方框架能只依赖自己需要的最小能力面

### Layer 4: Backend Adapters

每个 backend
都应实现同一组核心 contract，
并且只在真实支持时暴露高级 surface。

backend 之间不必假装完全同构，
但必须在 public truth 上可比较、可解释、可验证。

### Layer 5: Native / Backend-Specific Escape Hatches

保留高级能力，
但不能反向污染主门面或核心心智。

比如：

- `ISSLNativeHandleAccess`
- backend-private helper
- capability-specific runtime details

### Truth Sources

长期必须保持以下分工：

- `src/fafafa.ssl.base.pas`
  - public source truth
- `src/fafafa.ssl.pas`
  - curated facade export truth
- `docs/reference/API_REFERENCE.md`
  - canonical public API reference
- `docs/ARCHITECTURE.md`
  - 当前 shipped architecture 总览
- `docs/ROADMAP.md`
  - 当前阶段 / 路线选择入口
- focused contracts
  - 防止 public truth 回漂

## Key Design Decisions

### Decision 1: `ISSLConnection` 继续作为长期核心，但不再扩肥

理由：

- 它仍然是第三方框架最关键的集成入口
- 如果继续塞高级能力，optional 分层就会失去意义
- 如果贸然大手术移除，又会损伤 `v1.x` 稳定性

因此正确路线是：

- 继续做 owner-first + compatibility-mirror 明确化
- 在 `v1.x` 里完成分类和推荐路径收口
- 在未来 `v2` 再评估真正的 breaking surgery

### Decision 2: `ReadString` / `WriteString` 与 timeout/blocking 不做仓促清除

理由：

- 它们仍是当前 shipped truth 的一部分
- 也确实有日常易用性价值
- 但 framework / transport 集成不应把它们误当主路径

因此：

- `ReadString` / `WriteString`
  视为 `v1.x` convenience-core mirrors
- timeout / blocking
  视为 connection-control convenience mirrors，
  owner path 已是 `ISSLConnectionControl`

### Decision 3: `TSSLConfig` 当前以“冻结边界 + 分桶设计”优先

理由：

- 这条线的重要性很高
- 但涉及 public record surgery，
  贸然动实现面风险很大

因此：

- 短期先完成 scope taxonomy 与 future split blueprint
- 中期再决定 additive builder / context config surfaces
- 长期再考虑是否真正拆 record

### Decision 4: server-side 对称扩展晚于 connection/core clarity

理由：

- 当前更紧迫的问题仍然是 core-too-fat 与 facade/history baggage
- server-side 对称扩展如果太早引入，
  容易把不稳定语义过早公开

因此 server-side symmetry
应排在：

- `ISSLConnection` 边界清晰化之后
- `TSSLConfig` scope 清晰化之后

### Decision 5: pure Pascal backend 是长期主竞争力之一

因此未来路线不能只围绕
“OpenSSL / WinSSL 文档和 wrapper 整理”。

真正的长期价值
还包括：

- pure Pascal completeness
- performance tuning
- interop robustness
- packaging simplicity

## Evolution Roadmap

### Wave A: Architecture Anchor And Whole-Surface Map

目标：

- 建立统一的 excellence spec
- 给当前 public surfaces 建立全量分类心智
- 停止继续被旧 closeout / 局部 drift 牵引

完成条件：

- 当前规格/原则/路线有统一锚点
- 后续实现批次能明确归属于哪条长期路线

### Wave B: `ISSLConnection` Boundary Completion

目标：

- 完成 owner-first recommendation truth
- 补齐 remaining mirror families 的统一策略
- 把 `ISSLConnection` 从“过胖但模糊”
  推进到“过胖但边界清晰”

推荐顺序：

1. 完成 remaining `ISSLConnectionInfo` mirror family 的整体路线收口
2. 为 diagnostics / session / certificate-verification / OCSP 这四组建立统一 demotion 心智
3. 明确哪些 convenience surfaces 在 `v1.x` 只是保留，
   哪些是真正要进入未来 breaking queue

完成条件：

- connection-side owner taxonomy 成体系
- 不再继续只靠单个 getter 的局部 archaeology 推进

### Wave C: `TSSLConfig` Scope Surgery Blueprint

目标：

- 形成 library/context/connection/backend-private 分桶蓝图
- 明确哪些字段永远不该再回到默认推荐路径
- 为未来更小、更清晰的配置模型做设计准备

完成条件：

- `TSSLConfig` 不再被当作万能配置心智
- 新增功能不再继续往 mixed-scope record 里堆

### Wave D: Facade Simplification

目标：

- 把 `fafafa.ssl` 收成真正的 curated public facade
- 将历史 helper / secondary path 明确降级
- 普通开发者一眼可知正确入口

完成条件：

- façade 与 builder/connector 主路径清晰
- active docs 普通示例不再回流历史路径

### Wave E: Server-Side Symmetry Review

目标：

- 明确 server-side 是否值得形成独立对称扩展接口
- 若值得，先定义语义与 owner 边界，再考虑公开

完成条件：

- 不再让“缺少 `ISSLServerConnection`”成为语义含混的模糊债

### Wave F: Pure Pascal Excellence

目标：

- 继续推进 pure Pascal backend 在 correctness 之外的产品竞争力
- 完整度、互操作性、性能、调试体验一起提升

完成条件：

- pure Pascal backend 不只是“零依赖可用”，
  而是“值得优先选择”的 Pascal-first 路径

### Wave G: Operational And Performance Excellence

目标：

- benchmark / CI / contracts / docs / release workflow
  全部围绕长期演进服务
- 让性能结论、能力声明、发布状态都可追溯

完成条件：

- 关键架构判断都能在验证系统中找到证据

## Immediate Next Recommendation

如果按“最大长期价值 / 最小方向漂移”排序，
taxonomy batch 落地后，下一条最该推进的不是重新回头收早已关闭的 family，
也不是继续做零散的 getter archaeology。

最值得开的下一批应是：

1. **remaining `ISSLConnectionInfo` family**
   - 把 `GetConnectionInfo` / `GetContext` / `GetSelectedALPNProtocol` / `GetStateString` 这组 compatibility-core mirrors 继续收口到更清晰的 owner / deprecation truth
2. **diagnostics / session / certificate-verification / OCSP cluster strategy**
   - 按 taxonomy 之后的顺序推进，先让残余 mirrors 的 owner family 叙事更统一
3. **`TSSLConfig` v2 blueprint**

这样做的好处是：

- 不会再只做局部修补
- 每条实现都能回到统一设计图
- 更符合“打造领域顶级框架”的推进方式

## Not To Reopen By Default

在没有 fresh RED 前，
以下路线不应再被默认重开：

- 已关闭的 early-data durability families
- 已关闭的 direct-library connection-scope clarification
- 已关闭的旧 receipt gap
- 已冻结的 context-level SNI compatibility family
- 已关闭的 OCSP / CT / validation closeout 线

## Files

- Add:
  - `docs/plans/2026-05-24-framework-excellence-spec-and-evolution-roadmap.md`
- Update:
  - `docs/ROADMAP.md`
  - `docs/ARCHITECTURE.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Verification

```bash
bash tests/scripts/test_active_roadmap_references_contract.sh
bash tests/scripts/test_architecture_current_route_truth_contract.sh
bash tests/scripts/test_architecture_current_public_entrypoint_truth_contract.sh
git diff --check
```

## Expected Result

- 项目从这一轮开始有一份统一的 excellence spec / architecture roadmap
- 后续实现批次不再只靠局部 drift 驱动
- `ROADMAP` / `ARCHITECTURE` 能把读者引到这份新锚点
