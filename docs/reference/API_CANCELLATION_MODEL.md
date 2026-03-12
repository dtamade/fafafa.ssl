# API Cancellation Model

这份文档定义 fafafa.ssl 当前的 cancellation truth。

目标不是设计一个“理想中的未来 API”，而是回答：
- 今天什么算 cancel
- timeout 和 cancel 有什么区别
- `Close` / `Shutdown` 各自承担什么语义
- 框架作者现在应该怎么做

## 当前真相

当前没有独立的 connection-level Cancel API。

也就是说，今天的 `ISSLConnection` 没有一个单独的：
- `Cancel`
- `Abort`
- `Interrupt`

这不是遗漏说明，而是当前 contract 真相。

## 现在怎么取消

`Close` 是当前唯一的强制中断原语。

它的语义是：
- 立即终止连接
- 不要求发送 `close_notify`
- 不承诺 graceful TLS shutdown

如果调用方需要“马上停”，当前应该把它理解成：
- 强制打断
- 资源回收入口
- 当前最接近 cancel 的原语

## `Shutdown` 不是 cancel

`Shutdown` 是 graceful close，不是 cancel。

它的职责是：
- 尝试发送 TLS `close_notify`
- 尽量用协议允许的方式结束连接

因此它适合：
- 正常关闭
- 读写完成后的 graceful teardown

它不适合：
- 外层 deadline 已经超时，必须立刻停
- 调用方想主动打断当前阻塞操作

## `SetTimeout(...)` 不是 cancel

`SetTimeout(...)` 负责 deadline / budget，不等于 cancel。

它表达的是：
- 这次 read / write / handshake 最多等多久

它不表达：
- 立刻中止当前连接
- 外部控制流取消
- “用户点了取消按钮”

当前推荐理解是：
- timeout = 等待预算耗尽
- cancel = 调用方主动要求停止

这两类事件的恢复动作不同，不能混成一个词。

## 当前建议

### 对普通业务开发者

- 需要正常关闭时，用 `Shutdown`
- 需要立刻停时，用 `Close`
- 需要预算控制时，用 `SetTimeout(...)`

### 对框架作者

如果你今天要提供“取消请求”能力，建议显式建模成两层：

- soft stop：
  - 先用 timeout / 状态机停止继续发起新操作
- hard stop：
  - 最终调用 `Close`

换句话说，框架层不应把 `Shutdown` 包装成“取消”按钮。

## 为什么先固定文档

现在仓库已经把 pure Pascal 的 timeout/error model 补到了：
- blocking read timeout
- blocking write timeout
- client handshake timeout
- nonblocking retry

在这个阶段，最需要避免的是：
- 某些调用方把 timeout 当 cancel
- 某些调用方把 `Shutdown` 当 abort
- 某些后端以后偷偷长出自己的私有 cancel 语义

先把这层文档固定下来，后面如果真的新增独立 `Cancel` API，才有稳定的对比基线。

## 如果以后新增 `Cancel`

未来如果要新增独立 cancel surface，建议遵守三条：

- additive：
  - 不改变 `Close` / `Shutdown` 现有语义
- explicit：
  - `Cancel` 只表达主动打断，不复用 timeout 文案
- cross-backend：
  - Core API 只暴露各后端都能稳定实现的最小共同语义

在那之前，当前 contract 保持不变：
- `Close` 是当前唯一的强制中断原语
- `Shutdown` 是 graceful close，不是 cancel
- `SetTimeout(...)` 负责 deadline / budget，不等于 cancel
