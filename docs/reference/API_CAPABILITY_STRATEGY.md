# API Capability Strategy

这份文档定义 fafafa.ssl 当前 capability / fallback 策略。

它回答 4 个问题：
- `TSSLBackendCapabilities` 到底表示什么
- Core API 应不应该直接消费 capability
- Advanced API 应如何消费 capability
- `capability` / `dependency` / `unsupported` / `fallback` 应如何区分

## `TSSLBackendCapabilities`

`TSSLBackendCapabilities` 表达的是 **runtime truth**。

这意味着它反映的是：
- 当前 backend
- 当前实现
- 当前运行环境

共同决定出来的真实能力面。

它不是：
- 愿景清单
- 营销列表
- “以后可能会支持”的暗示

所以像这些字段都必须按当前真相填写：
- `SupportsTLS13`
- `SupportsALPN`
- `SupportsSNI`
- `RequiresExternalLibrary`
- `SupportsSystemCertStore`
- `SupportsPKCS11`
- `CompatibilityLevel`
- `KnownIssues`

## Core API

Core API 不应该要求普通业务开发者先读 capability 再决定是否能用。

Core API 的策略是：
- 默认安全
- 默认后端无关
- capability 差异尽量被 builder / connector / context 层吸收

因此对普通业务代码来说：
- capability 主要是调试和解释面
- 不是主入口
- 不是业务逻辑的第一分支条件

## Advanced API

Advanced API 可以直接消费 capability。

适合的消费场景：
- backend selection
- framework-level feature gating
- fallback strategy
- richer diagnostics
- 部署/依赖判断

框架作者可以基于 capability 做决策，但普通业务代码不应被强迫这样写。

## `RequiresExternalLibrary`

`RequiresExternalLibrary` 用来表达：
- 当前 backend 是否依赖外部原生库

典型用途：
- 区分 pure Pascal vs C-library backend
- 部署和打包策略
- 零依赖路径判断

## `SupportsSystemCertStore`

`SupportsSystemCertStore` 用来表达：
- 当前 backend 在当前环境下是否能接入系统证书存储

它是 runtime truth，而不是“代码里写过相关逻辑就算支持”。

## `SupportsPKCS11`

`SupportsPKCS11` 用来表达：
- 当前 backend/runtime 是否支持 PKCS#11 能力

它属于 Advanced API / capability strategy 范畴，不应反客为主变成普通业务入口。

## `capability`

`capability` 表示：
- 当前 backend 或当前实现本身不具备某能力

即使环境没问题，也依然不能成立。

## `dependency`

`dependency` 表示：
- 当前环境缺依赖
- 例如所需外部库、系统组件、模块未就绪

这和 `capability` 不同，因为修复方式不同。

## `unsupported`

`unsupported` 是 contract，不是模糊文案。

当能力在当前 backend / 当前模式下不支持时：
- 应明确表达 unsupported
- 不应伪装成 success
- 也不应模糊成“以后也许会实现”

## `fallback`

`fallback` 表示：
- 系统从理想路径降级到兼容路径
- 或从推荐入口降级到 bridge/compatibility surface

`fallback` 必须是可发现的，而不是偷偷发生。

## 推荐消费方式

### 对普通业务开发者
- 先用 Core API
- 不先查 capability 再写主流程
- 失败时再通过错误/日志/文档理解是不是 capability 问题

### 对框架作者
- capability 是一等输入
- 可以据此决定：
  - backend 选择
  - feature gating
  - fallback
  - richer diagnostics

## 与当前设计冻结的关系

当前路线图里，这份文档服务于：
- `docs/reference/ARCHITECTURE.md`
- `docs/reference/API_CONTRACT_CURRENT_INDEX.md`
- pure Pascal client M1 checklist

它的作用不是代替 capability matrix 文档，而是先把 capability 的**解释规则**固定下来。
