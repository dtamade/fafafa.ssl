# fafafa.ssl API Canon

> 更新：2026-03-10
> 角色：这是当前 SSL/TLS 接口设计真相文档，不是历史里程碑记录。
> 路线图入口：`docs/plans/2026-03-10-api-canon-and-implementation-roadmap.md`

## 状态

fafafa.ssl 当前已经从“多后端功能堆叠”转入“API canon 设计冻结”阶段。

当前文档的目标不是复述所有实现细节，而是明确：
- 哪些接口是推荐主路径
- 哪些接口只属于兼容层
- 哪些 contract 已经固定
- 纯 Pascal 后端接下来要朝什么目标演进

## 设计优先级

### 北极星
- 第一优先：SSL/TLS 接口设计要全面、合理、优雅
- 第二优先：实现完整度
- 重点投资方向：纯 Pascal 后端

### 最高原则
- `API 易用与一致性` 优先于抽象炫技、后端特色暴露、或局部极限性能

### 目标用户
- 主用户：普通业务开发者
- 次用户：框架作者

### 平台优先级
- 一等平台：`Linux + Windows`
- 纯 Pascal 后端优先覆盖：Linux

### 兼容性策略
- 设计期允许受控 breaking changes
- 但必须同时具备：
  - 明确迁移说明
  - 能保留 deprecated 过渡层时尽量保留
  - contract/test 与文档真相同步更新

## API Canon

### Core API

Core API 面向普通业务开发者，目标是覆盖 80% 的常见 HTTPS/TLS 场景，并尽量屏蔽后端差异。

Core API 的核心组成：
- `TSSLContextBuilder`
- `ISSLContext`
- `ISSLConnection`
- `ISSLClientConnection`
- `TSSLConnector` / `TSSLStream`

Core API 的基本要求：
- 默认安全
- 默认可读
- 默认后端无关
- contract 清晰，避免“字段看起来能配、其实不生效”
- 取消 / timeout / close 必须边界清楚

### Advanced API

Advanced API 面向框架作者和高阶调用方，用来表达 Core API 不应直接暴露的高级能力。

Advanced API 典型内容：
- backend selection / capability matrix
- certificate / CA / PKCS12 / PKCS7 / mTLS
- advanced builder fields
- diagnostics / native handle / runtime capability surface
- Result / exception / structured warning semantics

原则：
- 可以更强，但不能污染 Core API
- 必须有明确 contract，而不是“能调就算支持”

### Backend-Specific API

Backend-Specific API 仅在确有必要时保留。

它只应承担两类职责：
- 暴露后端确实独有、且无法自然抽象进 Core/Advanced 的能力
- 提供兼容桥接，帮助旧调用方迁移

规则：
- 必须显式标注 backend 归属
- 必须说明 fallback / unavailable 语义
- 不应成为普通业务代码的默认入口

## 入口治理

### `TSSLContextBuilder` 是唯一推荐主入口

`TSSLContextBuilder` 是当前唯一推荐主入口。

它必须成为：
- 配置语义最完整的入口
- 文档最完整的入口
- contract 最严格的入口
- backend 一致性最强的入口

主路径示意：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithVerifyPeer
  .WithSystemRoots
  .BuildClient;
```

server 路径示意：

```pascal
Ctx := TSSLContextBuilder.Create
  .WithCertificatePEM(CertPEM)
  .WithPrivateKeyPEM(KeyPEM)
  .BuildServer;
```

### `TSSLFactory + TSSLConfig` 仅保留为兼容/底层入口

`TSSLFactory + TSSLConfig` 不再是推荐 DSL，只保留为兼容/底层入口。

它们的职责：
- 创建 backend / context / store / certificate 实例
- 承载 library-scope stable defaults
- 服务现有兼容调用方

它们不应继续承载：
- builder-only 语义
- PEM/PKCS11 等高级材料 DSL
- 伪“全能配置对象”定位

重点 contract：
- `ISSLLibrary.SetDefaultConfig(...)` 只负责 library-scope stable defaults
- request/context-scope 字段必须通过 `TSSLFactory.CreateContext(const AConfig)` 或 builder 应用
- `TSSLConfig` 继续瘦身，避免与 builder 形成两套平行 DSL

### `TSSLConnector` / `TSSLStream`

`TSSLConnector` / `TSSLStream` 是业务快捷入口，用于把 `ISSLContext` 消费成更直接的连接体验。

它们的定位：
- 业务代码易用层
- 不重新发明配置 DSL
- 以消费 `ISSLContext` 为主，而不是绕开 builder/factory 再造一套配置入口

## Core Contract 真相

### 单次 backend 解析

- `TSSLContextBuilder.BuildClient` / `BuildServer` 会先 resolve 一次 concrete backend
- 后续 `TSSLFactory.CreateContext(...)` 与 `TSSLFactory.CreateCertificateStore(...)` 共享同一个 resolved backend
- `WithSystemRoots` 不再触发第二次 autodetect
- `WithBackend(sslAutoDetect)` 与隐式默认路径都会先收口到 concrete backend 再执行

### 配置作用域分离

- library scope：
  - `LogLevel`
  - `LogCallback`
  - 其它可被 backend 稳定保存的 default
- request/context scope：
  - `CertificateFile`
  - `PrivateKeyFile`
  - `PrivateKeyPassword`
  - `CAFile`
  - `CAPath`

`TSSLConfig` 当前不能再被视作“所有配置都能放进去”的总入口。

### owner fields normalize

- `LibraryType` / `ContextType` 在 library default path 上是 owner fields
- backend 保存 defaults 时会把它们 normalize 回 backend-owned baseline
- `GetDefaultConfig(...)` 返回的是 backend 当前可生效的默认真相，而不是调用方输入镜像

### runtime-only dead fields 显式失败

以下字段当前不属于 context 创建 contract：
- `BufferSize`
- `HandshakeTimeout`

它们不能再被静默当作“已生效配置”接受。

### `ServerName` 策略

- 推荐路径：per-connection SNI
- 顺序：先 `CreateConnection(...)`，再对 `ISSLClientConnection` 调 `SetServerName(...)`
- contract：
  - `connection override > context default > empty`
  - 显式空 override 也属于有效 override

`ISSLContext.ServerName` 当前仅保留兼容桥接语义，不再是推荐新接口。

### 材料优先级 contract

#### 证书
- `certificate_pem > certificate_file`
- `ImportFromJSON(...)` / `Merge(...)` 留下的双态不应再误走 file-first

#### 私钥
- `PKCS#11 > private_key_pem > private_key_file`
- `ImportFromJSON(...)` / `Merge(...)` 留下的双态不应再误走 file-first

#### PKCS11
- `UsePKCS11(...)` 只替代私钥来源
- `UsePKCS11(...)` 只替代私钥来源，不替代 server 证书要求
- server 证书仍需通过 `WithCertificate` 或 `WithCertificatePEM` 提供
- 当 `pkcs11_uri` 与本地私钥材料并存时，validation/build 都会显式按 `PKCS#11` 优先生效

## Backend Model

| Backend | 角色 | 一等平台 | 外部依赖 | 当前定位 |
|---|---|---|---|---|
| `OpenSSL` | Linux 现实基线 | Linux | 需要 | 生产主力 |
| `WinSSL` | Windows 现实基线 | Windows | 系统自带 | 生产主力 |
| `MbedTLS` | 可选轻量 backend | Linux/Windows | 需要 | 可选适配层 |
| `WolfSSL` | 可选兼容 backend | Linux/Windows | 需要 | 可选适配层 |
| `纯 Pascal / FreePascal` | 无原生依赖的可移植后端 | Linux 优先 | 不需要 | 战略重点，优先建设中 |

### backend-specific 保留策略

- OpenSSL / WinSSL 继续作为现实生产基线
- MbedTLS / WolfSSL 保持可选
- backend-specific 能力必须通过 capability / contract 明确边界
- 兼容 shim 可以保留，但不应再成为新演进中心

## 纯 Pascal 后端

### 战略定位

纯 Pascal 后端的目标不是“教学样例”，而是：
- 无原生依赖
- 可移植
- 可部署
- 可优化

同时要求：
- 保持可接受性能
- 不把“无依赖”当作“低标准”借口

### 第一里程碑

纯 Pascal 后端的第一个实现里程碑是：
- `HTTPS/TLS 客户端生产可用`

### M1 验收标准

1. `TLS 1.2 / 1.3` 稳定握手
2. 默认开启证书链校验
3. 默认开启 hostname verification
4. 支持系统根证书
5. 支持自定义 CA / CA bundle
6. 支持 SNI
7. 支持 ALPN
8. 支持超时、取消、明确错误语义
9. 支持稳定的流式读写与关闭语义
10. 支持可观测性：日志 / 握手失败原因 / 对端证书信息

### M1 暂不强制

- OCSP / CRL 强校验
- PKCS#11
- mTLS
- HTTP/2 完整协议层
- 激进性能目标

## 错误与观测模型

当前 API canon 要求：
- 错误不是“能抛就行”，而是要有统一语义
- warning 不是“可有可无”，而是要承担 mixed-input / fallback / precedence 的解释责任
- Result / exception / log 三条面要各司其职

推荐方向：
- Core API：稳定异常 / 结果语义
- Advanced API：更细粒度诊断、capability、native detail
- docs：必须明确什么是 error、什么是 warning、什么是 fallback

## 兼容与废弃策略

以下 surface 当前属于兼容层，而不是推荐新设计：
- `ISSLContext.ServerName`
- `TSSLFactory + TSSLConfig` 作为主 DSL 的用法
- 各 backend 的历史 shim 入口

废弃策略：
- 先降级为 compatibility surface
- 再停止在新文档/新示例里推广
- 最后在 contract 稳定后再考虑更强 deprecation

## 当前不再推荐的理解方式

以下理解已经过时：
- “所有配置都应该塞进 `TSSLConfig`”
- “context-level `ServerName` 是推荐主路径”
- “builder / factory / config 是同构入口”
- “PKCS11 能替代整个 server identity”
- “后端差异可以长期靠实现细节自然对齐”

## 与路线图的关系

这份文档对应路线图的 `Wave 1`：
- 先冻结 API canon
- 再抽 contract index
- 然后把 pure Pascal M1 contract 列出来，进入实现期

后续权威入口：
- 总路线图：`docs/plans/2026-03-10-api-canon-and-implementation-roadmap.md`
- 月度真相汇总：`docs/plans/2026-03-current-summary.md`
