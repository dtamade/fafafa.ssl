# fafafa.ssl 渐进式披露指南

**创建日期**: 2026-01-09
**目的**: 帮助不同水平的开发者快速找到适合自己的入口点

---

## 概述

fafafa.ssl 提供三个层次的 API，按复杂度递增：

```
Level 1 (Consumer)     → 1-3 行代码完成任务
Level 2 (Engineer)     → Builder 模式 + Result 类型
Level 3 (Architect)    → 完整 ISSLContext 控制
```

---

## Level 1: 快速入门 (Consumer)

**适用场景**: 快速原型、简单 HTTPS 请求、基础加密

**推荐 API**: `TSSLQuick`, `SSL.Quick.*`

### 示例：HTTPS GET 请求

```pascal
uses fafafa.ssl.quick;

var
  Response: string;
begin
  Response := TSSLQuick.Get('https://api.example.com/data');
  WriteLn(Response);
end;
```

### 示例：SHA256 哈希

```pascal
uses fafafa.ssl.crypto.utils;

var
  Hash: string;
begin
  Hash := TCryptoUtils.SHA256('Hello World');
  WriteLn(Hash);
end;
```

### Level 1 相关文件

| 文件 | 描述 |
|------|------|
| `docs/QUICKSTART.md` | 30 秒快速开始 |
| `examples/hello_ssl.pas` | 最简示例 |
| `examples/simple_https_demo.pas` | 简单 HTTPS 演示 |
| `src/fafafa.ssl.quick.pas` | Quick API 实现 |

---

## Level 2: 工程实践 (Engineer)

**适用场景**: 生产应用、需要错误处理、配置定制

**推荐 API**: `TSSLConnectionBuilder`, `TSSLDataResult`, `Try*` 方法

### 示例：带错误处理的 HTTPS 连接

```pascal
uses
  fafafa.ssl.connection.builder,
  fafafa.ssl.result.utils;

var
  Result: TSSLDataResult;
begin
  Result := TSSLConnectionBuilder.Create
    .WithHost('api.example.com')
    .WithPort(443)
    .WithTLS12OrHigher
    .WithVerifyPeer
    .Build
    .Get('/api/data');

  if Result.IsOk then
    WriteLn('Success: ', Result.Value)
  else
    WriteLn('Error: ', Result.Error.Message);
end;
```

### 示例：安全随机数生成

```pascal
uses fafafa.ssl.secure;

var
  RandomResult: TSSLDataResult;
begin
  RandomResult := TSecureRandom.TryGenerateBytes(32);
  if RandomResult.IsOk then
    // 使用随机字节
  else
    // 处理错误
end;
```

### Level 2 相关文件

| 文件 | 描述 |
|------|------|
| `docs/GETTING_STARTED.md` | 入门指南 |
| `docs/API_DESIGN_GUIDE.md` | API 设计原则 |
| `examples/example_result_type.pas` | Result 类型示例 |
| `examples/demo_fluent_api.pas` | 流式 API 演示 |
| `examples/example_error_handling.pas` | 错误处理示例 |
| `src/fafafa.ssl.connection.builder.pas` | Connection Builder |
| `src/fafafa.ssl.result.utils.pas` | Result 类型工具 |

---

## Level 3: 架构控制 (Architect)

**适用场景**: 自定义后端、高级配置、性能优化、企业集成

**推荐 API**: `ISSLContext`, `ISSLLibrary`, `TSSLFactory`

### 示例：完整上下文控制

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed;

var
  Ctx: ISSLContext;
  Conn: ISSLConnection;
begin
  // 创建并配置上下文
  Ctx := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
  Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  Ctx.SetCipherList('ECDHE+AESGCM:ECDHE+CHACHA20');
  Ctx.SetVerifyMode([sslVerifyPeer]);
  Ctx.SetVerifyDepth(4);
  Ctx.LoadCAFile('/etc/ssl/certs/ca-certificates.crt');

  // 创建连接
  Conn := Ctx.CreateConnection(Socket);
  Conn.SetServerName('api.example.com');

  if Conn.Connect then
  begin
    Conn.Write('GET / HTTP/1.1'#13#10);
    // ...
  end;
end;
```

### 示例：多后端切换

```pascal
uses
  fafafa.ssl.factory,
  fafafa.ssl.openssl.backed
  {$IFDEF WINDOWS}, fafafa.ssl.winssl.lib{$ENDIF};

var
  Libs: TSSLLibraryTypes;
  Lib: TSSLLibraryType;
begin
  // 查询可用后端
  Libs := TSSLFactory.GetAvailableLibraries;

  for Lib in Libs do
    WriteLn('Available: ', TSSLFactory.GetLibraryDescription(Lib));

  // 指定使用 WinSSL（Windows 零依赖）
  {$IFDEF WINDOWS}
  TSSLFactory.SetDefaultLibrary(sslWinSSL);
  {$ENDIF}
end;
```

### Level 3 相关文件

| 文件 | 描述 |
|------|------|
| `docs/ARCHITECTURE.md` | 架构设计文档 |
| `docs/API_REFERENCE.md` | 完整 API 参考 |
| `docs/WINSSL_DESIGN.md` | WinSSL 后端设计 |
| `examples/example_factory_usage.pas` | 工厂模式示例 |
| `examples/08_mutual_tls.pas` | 双向 TLS 认证 |
| `examples/09_winssl_fips.pas` | WinSSL FIPS 模式 |
| `src/fafafa.ssl.factory.pas` | 工厂实现 |
| `src/fafafa.ssl.base.pas` | 核心接口定义 |

---

## 示例目录结构

```
examples/
├── Basic/                    # Level 1 - 基础示例
│   ├── hello_ssl.pas
│   ├── simple_https_demo.pas
│   └── hash_calculator.pas
│
├── Production/               # Level 2 - 生产级示例
│   ├── https_client_simple.pas
│   ├── https_client_auth.pas
│   └── https_server_simple.pas
│
├── Advanced/                 # Level 3 - 高级示例
│   ├── 08_mutual_tls.pas
│   ├── 09_winssl_fips.pas
│   └── example_factory_usage.pas
│
└── Scenarios/                # 场景化示例
    ├── digital_signature/
    ├── file_encrypt/
    └── https_client/
```

---

## 选择指南

| 你的需求 | 推荐层级 | 入口文件 |
|----------|----------|----------|
| 快速发起 HTTPS 请求 | Level 1 | `docs/QUICKSTART.md` |
| 生产环境 HTTPS 客户端 | Level 2 | `docs/GETTING_STARTED.md` |
| 需要错误处理和重试 | Level 2 | `examples/example_error_handling.pas` |
| 自定义 TLS 配置 | Level 3 | `docs/API_REFERENCE.md` |
| Windows 零依赖部署 | Level 3 | `docs/ZERO_DEPENDENCY_DEPLOYMENT.md` |
| 双向 TLS 认证 | Level 3 | `examples/08_mutual_tls.pas` |

---

## 迁移路径

```
Level 1 → Level 2
├── 当需要处理错误时
├── 当需要自定义超时时
└── 当需要会话复用时

Level 2 → Level 3
├── 当需要切换 SSL 后端时
├── 当需要自定义证书验证时
└── 当需要性能优化时
```

---

*文档版本: 1.0*
*最后更新: 2026-01-09*
