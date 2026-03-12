# API Entrypoint Governance

这是 fafafa.ssl 当前入口治理文档，用于回答一个问题：

**普通业务开发者、框架作者、历史调用方，分别应该从哪个入口进入？**

## 推荐主入口

### `TSSLContextBuilder`

`TSSLContextBuilder` 是当前唯一推荐主入口。

原因：
- 配置语义最完整
- 文档最完整
- contract 最严格
- backend 一致性最强

适合：
- 普通业务开发者
- 新框架集成
- 所有新代码

推荐路径：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithSystemRoots
  .BuildClient;
```

server 场景：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithCertificatePEM(CertPEM)
  .WithPrivateKeyPEM(KeyPEM)
  .BuildServer;
```

## 兼容/底层入口

### `TSSLFactory + TSSLConfig`

`TSSLFactory + TSSLConfig` 继续保留，但定位已经固定为兼容/底层入口。

它们适合：
- 现有调用方兼容
- backend 实例创建
- library-scope default 管理
- 测试夹具 / 低层工厂用法

它们不再适合：
- 作为主 DSL 暴露给新业务代码
- 承担 builder-only 语义
- 充当“所有配置都能塞进去”的总入口

治理规则：
- request/context-scope 材料继续走 builder 或 request-path create
- library defaults 只保留 stable defaults
- `TSSLConfig` 继续瘦身，而不是继续膨胀

### `TSSLConnector / TSSLStream`

`TSSLConnector / TSSLStream` 仍然是合法入口，但它们属于**快捷消费层**，不是主配置入口。

适合：
- 业务代码快速接入
- 在已有 `ISSLContext` 之上快速建立连接

不适合：
- 绕开 `TSSLContextBuilder` 重新发明配置流程
- 承担配置 canon 的职责

因此当前推荐顺序是：
- 先用 `TSSLContextBuilder` 产出 `ISSLContext`
- 再用 `TSSLConnector / TSSLStream` 消费这个 context

## Deprecated / Bridge Surface

### `ISSLContext.ServerName`

`ISSLContext.ServerName` 当前属于 deprecated / bridge surface。

它的角色：
- backward-compatible fallback
- 只影响后续 client connection 默认值

它不是：
- 推荐的新 SNI 入口
- 连接运行期唯一真相源

当前推荐路径是：
- 先 `CreateConnection(...)`
- 再对 `ISSLClientConnection` 调 `SetServerName(...)`

### WolfSSL standalone shim

`src/fafafa.ssl.wolfssl.connection.pas` 当前是 WolfSSL standalone compatibility shim。

它保留的原因：
- 兼容历史 `uses fafafa.ssl.wolfssl.connection`
- 继续桥接旧公开类名

它不再承担：
- 第二套完整 runtime 实现
- 新设计演进中心

运行时真相源已经固定在：
- `fafafa.ssl.wolfssl.context`

## 使用建议

### 对普通业务开发者
- 只学 `TSSLContextBuilder`
- 连接建立优先走 `TSSLConnector / TSSLStream`
- 不主动接触 backend-specific 入口

### 对框架作者
- 以 `TSSLContextBuilder` 作为主配置入口
- 需要能力探测时，再进入 capability / advanced surface
- 只在确有必要时使用 backend-specific 入口

### 对历史调用方
- `TSSLFactory + TSSLConfig` 继续可用
- 但应逐步迁移到 builder canon
- `ISSLContext.ServerName` 继续可用，但不再建议推广

## 后续治理方向

- `TSSLContextBuilder` 继续强化为唯一主入口
- `TSSLFactory + TSSLConfig` 继续瘦身
- `TSSLConnector / TSSLStream` 保持快捷消费层定位
- deprecated / bridge surface 不删除历史兼容，但停止在新文档/新示例里继续推广
