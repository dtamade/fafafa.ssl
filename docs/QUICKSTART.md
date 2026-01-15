# fafafa.ssl 快速开始（Quickstart）

本指南用于 **5 分钟**内跑通：
- 密码学工具（SHA-256 / 安全随机数）
- TLS 客户端握手（Connector + Stream）

fafafa.ssl 是 **TLS 库**，不是 HTTP 库。

## 1) 安装与编译

### 依赖
- FreePascal 3.2.0+
- Linux/macOS: OpenSSL 1.1.1+ 或 3.x（运行时动态加载）
- Windows: 默认可用 WinSSL（Schannel），无需 OpenSSL DLL

### 本地编译
```bash
# 在仓库根目录
fpc -Mobjfpc -Sh -Fu./src -Fi./src your_program.pas
```

## 2) 密码学工具（高层封装）

```pascal
program quick_crypto;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.crypto.utils;

var
  Bytes: TBytes;
  Hex: string;
begin
  Hex := TCryptoUtils.SHA256Hex('Hello');
  WriteLn('SHA256: ', Hex);

  Bytes := TCryptoUtils.SecureRandom(32);
  WriteLn('Random length: ', Length(Bytes));
end.
```

## 3) TLS 客户端（Connector + Stream）

> 你需要自己创建并连接 TCP socket（或使用 Synapse/Indy/lNet 等网络库）。
> fafafa.ssl 的职责是：在“已连接的传输”之上提供 TLS。

```pascal
program quick_tls;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
  TLS: TSSLConnector;
  Stream: TSSLStream;
  YourConnectedSocket: THandle;

begin
  // 1) Context：协议 + 验证 + 系统根证书
  Ctx := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots
    .BuildClient;

  // 2) Connector：把 ServerName 设置到“连接”上（SNI + hostname verification）
  TLS := TSSLConnector.FromContext(Ctx);
  Stream := TLS.ConnectSocket(YourConnectedSocket, 'example.com');
  try
    WriteLn('TLS OK, cipher: ', Stream.Connection.GetCipherName);
    // Stream.Read/Write ...
  finally
    Stream.Free;
  end;
end.
```

## 4) 证书快速生成（Quick API）

```pascal
program quick_cert;

{$mode ObjFPC}{$H+}

uses
  fafafa.ssl.quick,
  fafafa.ssl.cert.builder;

var
  KeyPair: IKeyPairWithCertificate;
begin
  KeyPair := TSSLQuick.GenerateSelfSigned('localhost');
  KeyPair.SaveToFiles('server.crt', 'server.key');
end.
```

## 5) 下一步

- 示例：`examples/`
- 构建与测试：
  - `bash build_linux.sh`
  - `./ci_pipeline.sh build`
  - `./ci_pipeline.sh test`
