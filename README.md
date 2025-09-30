# fafafa.ssl - 统一的 SSL/TLS 库

**fafafa.ssl** 是一个为 Free Pascal/Lazarus 设计的统一 SSL/TLS 抽象层库，支持多个后端实现。

## 特性

- 🔐 **多后端支持**: OpenSSL, WolfSSL, MbedTLS, Windows Schannel
- 🎯 **统一接口**: 所有后端使用相同的 API
- 🚀 **自动检测**: 自动选择最佳可用的 SSL 库
- 🛡️ **类型安全**: 强类型定义，减少运行时错误
- 📦 **零配置**: Windows 平台可使用系统自带的 Schannel，无需额外依赖
- 🔄 **会话复用**: 支持 SSL 会话缓存和复用
- 📜 **证书管理**: 完整的 X.509 证书处理功能

## 支持的后端

| 后端 | 平台 | 特点 | 状态 |
|------|------|------|------|
| **OpenSSL** | 全平台 | 功能最全面，应用最广泛 | 开发中 |
| **WolfSSL** | 全平台 | 轻量级，适合嵌入式 | 计划中 |
| **MbedTLS** | 全平台 | ARM 优化，模块化设计 | 计划中 |
| **WinSSL** | Windows | 系统原生，无需额外依赖 | 开发中 |

## 安装

### 依赖要求

- Free Pascal 3.2.0 或更高版本
- Lazarus 2.0.0 或更高版本（可选，用于 IDE 支持）

### 安装步骤

1. 克隆或下载本项目到你的库目录
2. 在你的项目中添加 `fafafa.ssl` 到 uses 列表
3. 根据需要安装相应的 SSL 库（OpenSSL、WolfSSL 等）

## 快速开始

### 最简单的 HTTPS 客户端

```pascal
uses
  fafafa.ssl;

var
  LConn: ISSLConnection;
begin
  // 自动检测并使用最佳可用的 SSL 库
  LConn := QuickConnect('www.example.com', 443);
  // 连接已建立，可以进行数据传输
end;
```

### 创建 SSL 客户端

```pascal
uses
  fafafa.ssl;

var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LSocket: THandle;
begin
  // 创建客户端上下文
  LContext := CreateSSLContext(sslCtxClient);
  
  // 配置 SSL 参数
  LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  LContext.SetVerifyMode([sslVerifyPeer]);
  LContext.SetServerName('www.example.com'); // SNI
  
  // 创建并连接 socket（这里需要你自己的 socket 实现）
  LSocket := ConnectToServer('www.example.com', 443);
  
  // 创建 SSL 连接
  LConnection := LContext.CreateConnection(LSocket);
  
  // 执行 SSL 握手
  if LConnection.Connect then
  begin
    // SSL 连接建立成功
    WriteLn('连接成功！');
    WriteLn('协议版本: ', ProtocolVersionToString(LConnection.GetProtocolVersion));
    WriteLn('密码套件: ', LConnection.GetCipherName);
  end;
end;
```

### 创建 SSL 服务端

```pascal
uses
  fafafa.ssl;

var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LClientSocket: THandle;
begin
  // 创建服务端上下文
  LContext := CreateSSLContext(sslCtxServer);
  
  // 加载证书和私钥
  LContext.LoadCertificate('server.crt');
  LContext.LoadPrivateKey('server.key');
  
  // 配置 SSL 参数
  LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  LContext.SetCipherList('ECDHE+AESGCM:ECDHE+AES256');
  
  // 接受客户端连接（这里需要你自己的 socket 实现）
  LClientSocket := AcceptClient;
  
  // 创建 SSL 连接
  LConnection := LContext.CreateConnection(LClientSocket);
  
  // 执行 SSL 握手
  if LConnection.Accept then
  begin
    // SSL 连接建立成功，可以进行安全通信
    WriteLn('客户端已连接');
  end;
end;
```

### 证书验证

```pascal
uses
  fafafa.ssl;

var
  LCert: ISSLCertificate;
  LInfo: TSSLCertificateInfo;
begin
  // 加载证书
  LCert := LoadCertificate('certificate.pem');
  
  // 获取证书信息
  LInfo := LCert.GetInfo;
  WriteLn('主题: ', LInfo.Subject);
  WriteLn('颁发者: ', LInfo.Issuer);
  WriteLn('有效期: ', DateTimeToStr(LInfo.NotBefore), ' - ', DateTimeToStr(LInfo.NotAfter));
  WriteLn('SHA256 指纹: ', LInfo.FingerprintSHA256);
  
  // 验证证书
  if ValidateCertificate('certificate.pem') then
    WriteLn('证书有效')
  else
    WriteLn('证书无效');
end;
```

### 指定使用特定的 SSL 库

```pascal
uses
  fafafa.ssl;

var
  LContext: ISSLContext;
begin
  // 强制使用 OpenSSL
  LContext := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
  
  // 或者在 Windows 上使用系统原生 SSL
  {$IFDEF WINDOWS}
  LContext := TSSLFactory.CreateContext(sslCtxClient, sslWinSSL);
  {$ENDIF}
  
  // 检查可用的 SSL 库
  if TSSLFactory.IsLibraryAvailable(sslOpenSSL) then
    WriteLn('OpenSSL 可用');
end;
```

### 错误处理

```pascal
uses
  fafafa.ssl;

var
  LContext: ISSLContext;
begin
  try
    LContext := CreateSSLContext(sslCtxClient);
    // ... SSL 操作 ...
  except
    on E: ESSLHandshakeException do
      WriteLn('握手失败: ', E.Message);
    on E: ESSLCertificateException do
      WriteLn('证书错误: ', E.Message);
    on E: ESSLException do
      WriteLn('SSL 错误 [', SSL_ERROR_MESSAGES[E.ErrorCode], ']: ', E.Message);
  end;
end;
```

## API 参考

### 主要接口

- `ISSLLibrary` - SSL 库管理接口
- `ISSLContext` - SSL 上下文（配置和设置）
- `ISSLConnection` - SSL 连接（实际的加密通道）
- `ISSLCertificate` - X.509 证书处理
- `ISSLCertificateStore` - 证书存储和验证
- `ISSLSession` - SSL 会话（用于会话复用）

### 工厂类

- `TSSLFactory` - 创建和管理 SSL 实例
- `TSSLHelper` - 提供便捷的辅助方法

### 快捷函数

- `CreateSSLContext()` - 创建 SSL 上下文
- `CreateSSLCertificate()` - 创建证书对象
- `QuickConnect()` - 快速建立客户端连接
- `CheckSSLSupport()` - 检查 SSL 支持状态

## 配置选项

```pascal
var
  LConfig: TSSLConfig;
begin
  LConfig := CreateDefaultConfig(sslCtxClient);
  
  // 基本配置
  LConfig.LibraryType := sslOpenSSL;  // 指定使用的库
  LConfig.ContextType := sslCtxClient; // 客户端或服务端
  
  // 协议配置
  LConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  LConfig.VerifyMode := [sslVerifyPeer, sslVerifyFailIfNoPeerCert];
  
  // 证书配置
  LConfig.CertificateFile := 'client.crt';
  LConfig.PrivateKeyFile := 'client.key';
  LConfig.CAFile := 'ca-bundle.crt';
  
  // 性能配置
  LConfig.BufferSize := 16384;
  LConfig.HandshakeTimeout := 30000;
  LConfig.SessionCacheSize := 1024;
  
  // 创建配置好的上下文
  LContext := TSSLFactory.CreateContext(LConfig);
end;
```

## 开发状态

- ✅ 核心架构设计
- ✅ 基础类型定义
- ✅ 接口定义
- ✅ 工厂模式实现
- 🚧 OpenSSL 后端实现
- 📋 WolfSSL 后端实现
- 📋 MbedTLS 后端实现
- 🚧 WinSSL 后端实现
- 📋 单元测试
- 📋 示例程序
- 📋 完整文档

## 贡献

欢迎提交 Issue 和 Pull Request！

### 开发规范

请参考 [WARP.md](WARP.md) 文件了解项目开发规范。

## 许可证

本项目采用 MIT 许可证。详见 [LICENSE](LICENSE) 文件。

## 致谢

- OpenSSL 项目
- WolfSSL 团队
- ARM Mbed TLS
- Free Pascal 社区

---

**注意**: 本项目正在积极开发中，API 可能会有变化。建议在生产环境使用前进行充分测试。