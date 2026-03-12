# API Error Model

这份文档定义 fafafa.ssl 当前 API 的错误模型。

目标不是枚举所有异常类，而是回答：
- Core API 默认如何暴露错误
- Advanced API 何时应该暴露更细粒度语义
- `TSSLOperationResult` / `TSSLDataResult` 与异常如何分工
- warning 在 contract 中承担什么角色

## Core API

Core API 面向普通业务开发者，默认应满足：
- 错误语义稳定
- 不要求调用方理解 backend-specific 细节
- 默认优先提供“可直接处理”的错误面

### Core API 默认错误面

- 直接失败场景：
  - 使用稳定异常类型
  - 例如基于 `ESSLException` 及其常见子类
- 可选/无异常路径：
  - 使用 `TSSLOperationResult`
  - 使用 `TSSLDataResult`

### `TSSLOperationResult`

`TSSLOperationResult` 适合：
- `Try*` 风格 API
- 不希望用异常控制正常流程的调用方
- 需要稳定的 `ErrorCode + ErrorMessage` 组合

在 Core API 中，它承担的是：
- “操作是否成功”
- “失败时的统一错误码”
- “人类可读的错误消息”

而不是：
- 暴露 backend-specific 原始错误栈
- 暴露过多底层 native 细节

### `TSSLDataResult`

`TSSLDataResult` 适合：
- 需要返回字节数据或 payload 的操作
- 同时又要保留统一失败语义

Core API 里，`TSSLDataResult` 的职责和 `TSSLOperationResult` 类似：
- 成功时携带结果
- 失败时携带统一错误信息

### Core API 异常

Core API 默认围绕 `ESSLException` 家族工作。

原则：
- 参数错误：优先 `ESSLInvalidArgument`
- 配置错误：优先 `ESSLConfigurationException`
- 连接/握手/证书等关键失败：使用对应稳定异常
- 不把“当前 backend 的奇怪原生错误”直接作为主用户面对的 contract

## Advanced API

Advanced API 面向框架作者和高阶调用方。

这里允许暴露更细粒度的错误语义，例如：
- `unsupported`
- `configuration`
- structured capability mismatch
- backend-specific diagnostics
- richer exception subtypes

### `unsupported`

`unsupported` 不是“以后可能会实现”的模糊说法，而是 contract。

当某能力在当前 backend / 当前模式下就是不支持时：
- 应使用明确的 unsupported 语义
- 不应误导成 “not implemented yet”
- 对应的错误码、异常或消息应稳定可判断

### `configuration`

`configuration` 与 `unsupported` 必须分开。

例如：
- 缺少证书 / 私钥 / CA 材料：`configuration`
- 当前 backend 不支持某协议或特性：`unsupported`

这条区分很重要，因为：
- 调用方的恢复动作不同
- 框架作者的 fallback 策略也不同

### richer exception surface

Advanced API 可以使用更细粒度的异常：
- `ESSLCertificateException`
- `ESSLHandshakeException`
- `ESSLTimeoutException`
- backend/domain-specific exception families

但这些 richer exceptions 不应反向污染 Core API 的默认使用门槛。

## warning 的角色

`warning` 在当前 contract 中不是装饰信息，而是用来表达：
- precedence
- fallback
- ambiguity
- non-fatal but important risk

典型场景：
- file / PEM 同时存在时，谁会生效
- `pkcs11_uri` 与本地私钥材料并存时，谁会生效
- 老 TLS 版本仍可用但不推荐
- verify 被关闭但配置仍然“技术上可运行”

换句话说：
- error = 阻止继续
- warning = 允许继续，但必须把关键语义说清

## Core API vs Advanced API 的边界

### Core API
- 默认异常 / Result 应该足够简单
- 默认错误消息应以业务调用方可理解为主
- 默认不要求理解 native handle、raw backend code、capability internals

### Advanced API
- 可以暴露更丰富的结构化信息
- 可以给框架作者更细粒度的错误分类
- 可以包含 capability / fallback / warning 解释面

## 当前 contract 真相

当前仓库已经明确了几类高价值错误 contract：
- `unsupported` 语义要明确，而不是含糊文案
- `configuration` 语义要与 unsupported 区分
- `warning` 要承担 mixed-input / precedence 的解释责任
- `Try*` 路径通过 `TSSLOperationResult` / `TSSLDataResult` 保持统一风格

相关入口：
- `docs/reference/ARCHITECTURE.md`
- `docs/reference/API_CONTRACT_CURRENT_INDEX.md`
- `docs/guides/ERROR_HANDLING_BEST_PRACTICES.md`
- `docs/reference/RETURN_TYPE_CONVENTIONS.md`

## 结论

当前 error model 的推荐理解是：
- Core API：稳定异常 + 简洁 Result
- Advanced API：更细粒度异常 + structured warning / unsupported / diagnostics
- 文档 contract：必须把 `warning`、`unsupported`、`configuration` 三类边界说清楚
