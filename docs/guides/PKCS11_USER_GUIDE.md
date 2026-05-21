# PKCS#11 用户指南

> **版本**: v1.0.0
> **更新**: 2026-03-20

本指南介绍如何在 fafafa.ssl 中使用 PKCS#11 硬件安全模块 (HSM)。

当前边界先说清楚：

- 当前 published PKCS#11 private-key path 只在 `OpenSSL` backend 暴露。
- 当前 capability truth 跟随 `TPKCS11BackendFactory.IsBackendAvailable(btAuto)`。
- 其它 backend 当前都不发布 `SupportsPKCS11` capability。

---

## 概述

PKCS#11 是访问加密令牌（如智能卡、HSM）的标准 API。fafafa.ssl 当前通过 `OpenSSL` backend 的 published PKCS#11 path 支持：

- **SoftHSM2** - 软件模拟的 HSM（用于开发和测试）
- **硬件 HSM** - 各种 PKCS#11 兼容设备
- **智能卡** - 支持 PKCS#11 的智能卡

如果当前 OpenSSL runtime 既没有可用 Provider path，也没有可用 ENGINE path，`SupportsPKCS11` 会降为 `False`，builder / auto-backend selection 也不会把它继续发布成可用 capability。

---

## 快速开始

### 1. 安装 SoftHSM2（开发环境）

```bash
# Ubuntu/Debian
sudo apt-get install softhsm2

# macOS
brew install softhsm

# 验证安装
softhsm2-util --version
```

### 2. 初始化令牌

```bash
# 创建测试令牌
softhsm2-util --init-token --slot 0 --label "TestToken" --pin 1234 --so-pin 12345678
```

### 3. 生成密钥对

```bash
# 需要 OpenSC 工具
pkcs11-tool --module /usr/lib/softhsm/libsofthsm2.so \
  --token-label TestToken --login --pin 1234 \
  --keypairgen --key-type rsa:2048 --id 01 --label TestKey
```

---

## 使用 PKCS#11 URI

fafafa.ssl 支持 RFC 7512 PKCS#11 URI 格式：

```
pkcs11:token=TestToken;object=TestKey;type=private?pin-value=1234&module-path=/usr/lib/softhsm/libsofthsm2.so
```

### URI 组件

| 组件          | 说明             | 示例                                          |
| ------------- | ---------------- | --------------------------------------------- |
| `token`       | 令牌标签         | `token=TestToken`                             |
| `object`      | 密钥标签         | `object=TestKey`                              |
| `type`        | 对象类型         | `type=private`                                |
| `pin-value`   | PIN 码           | `pin-value=1234`                              |
| `module-path` | PKCS#11 模块路径 | `module-path=/usr/lib/softhsm/libsofthsm2.so` |

---

## 代码示例

### 使用 Context Builder

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.context.builder;

var
  Ctx: ISSLContext;
begin
  // BuildServer 选择服务端上下文
  Ctx := TSSLContextBuilder.Create
    .WithCertificate('/path/to/cert.pem')
    .UsePKCS11('pkcs11:token=TestToken;object=TestKey;type=private')
    .WithPKCS11PIN('1234')
    .WithVerifyNone  // 普通单向 TLS server；如需 mTLS 改用 WithMutualTLS(...)
    .BuildServer;
end;
```

### Builder 支持的 PIN 来源

`TSSLContextBuilder` 当前支持以下 PKCS#11 PIN 获取方式：

- 直接 PIN 值：`.WithPKCS11PIN('1234')`，默认使用 `pmValue`
- 环境变量：`.WithPKCS11PIN('PKCS11_PIN_ENV').WithPKCS11PINMethod(pmEnvironment)`
- 文件：`.WithPKCS11PIN('/run/secrets/pkcs11-pin').WithPKCS11PINMethod(pmFile)`

Builder 不支持 `pmCallback` 和 `pmInteractive`。如果你需要回调式或交互式 PIN 获取，请直接使用
`TPKCS11Config` 和底层 PKCS#11 backend API。

当 `pmEnvironment` 或 `pmFile` 使用空 source value 时，builder validation 会直接失败；环境变量或文件缺失时，
build 阶段也会返回确定性的 source-resolution error。

#### 从环境变量读取 PIN

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.pkcs11.types;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithCertificate('/path/to/cert.pem')
    .UsePKCS11('pkcs11:token=TestToken;object=TestKey;type=private')
    .WithPKCS11PIN('PKCS11_PIN_ENV')
    .WithPKCS11PINMethod(pmEnvironment)
    .WithVerifyNone  // 普通单向 TLS server；如需 mTLS 改用 WithMutualTLS(...)
    .BuildServer;
end;
```

#### 从文件读取 PIN

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.pkcs11.types;

var
  Ctx: ISSLContext;
begin
  Ctx := TSSLContextBuilder.Create
    .WithCertificate('/path/to/cert.pem')
    .UsePKCS11('pkcs11:token=TestToken;object=TestKey;type=private')
    .WithPKCS11PIN('/run/secrets/pkcs11-pin')
    .WithPKCS11PINMethod(pmFile)
    .WithVerifyNone  // 普通单向 TLS server；如需 mTLS 改用 WithMutualTLS(...)
    .BuildServer;
end;
```

### 使用 PIN 回调（底层 backend API）

```pascal
uses
  fafafa.ssl.pkcs11.types,
  fafafa.ssl.pkcs11.backend;

type
  TTokenPINProvider = class
    function RequestPIN(const TokenLabel: string; out PIN: string): Boolean;
  end;

function TTokenPINProvider.RequestPIN(const TokenLabel: string; out PIN: string): Boolean;
begin
  // 从用户界面或配置获取 PIN
  PIN := GetPINFromUser(TokenLabel);
  Result := PIN <> '';
end;

var
  Provider: TTokenPINProvider;
  Config: TPKCS11Config;
  Backend: IPKCS11Backend;
begin
  Provider := TTokenPINProvider.Create;
  try
    Config := TPKCS11ConfigDefault;
    Config.ModulePath := '/usr/lib/softhsm/libsofthsm2.so';
    Config.TokenLabel := 'TestToken';
    Config.KeyLabel := 'TestKey';
    Config.PINMethod := pmCallback;
    Config.PINCallback := @Provider.RequestPIN;

    Backend := TPKCS11BackendFactory.CreateBackend;
    Backend.LoadPrivateKey(Config);
    // 使用密钥...
  finally
    Provider.Free;
  end;
end;
```

`pmInteractive` 也属于同一层能力。它不会通过 `TSSLContextBuilder` 自动接入交互式输入。

---

## 后端选择

fafafa.ssl 当前在 `OpenSSL` backend 下提供两种 PKCS#11 runtime backend：

| 后端         | OpenSSL 版本 | 说明                                |
| ------------ | ------------ | ----------------------------------- |
| **Provider** | 3.0+         | 推荐，使用 OpenSSL 3.x provider API |
| **ENGINE**   | 1.1.1+       | 兼容模式，使用 ENGINE API           |

系统会自动选择当前可用的最佳 OpenSSL runtime backend。

补充边界：

- `SupportsPKCS11=True` 不再代表“仓库里有 PKCS#11 代码就算支持”
- 它只代表当前 runtime 至少有一条可工作的 Provider / ENGINE path
- `WinSSL` / `FreePascal` / `MbedTLS` / `WolfSSL` 当前都不发布 `SupportsPKCS11` capability

---

## 故障排除

### 常见问题

**1. "Token not found" 错误**

```bash
# 检查令牌是否存在
softhsm2-util --show-slots
```

**2. "PIN incorrect" 错误**

- 确认 PIN 码正确
- 检查 PIN 是否被锁定（多次错误输入）

**3. "Module not found" 错误**

```bash
# 查找 PKCS#11 模块位置
find /usr -name "libsofthsm*.so" 2>/dev/null
```

### 调试模式

```pascal
// 启用详细日志
TSecurityLog.SetLevel(logDebug);
```

---

## 安全建议

1. **不要硬编码 PIN** - 使用环境变量或安全存储
2. **限制令牌访问** - 配置适当的文件权限
3. **定期备份** - 备份 SoftHSM2 数据目录
4. **生产环境使用硬件 HSM** - SoftHSM2 仅用于开发

---

## 相关文档

- [PKCS#11 架构设计](../reference/PKCS11_ARCHITECTURE.md)
- [RFC 7512 - PKCS#11 URI](https://tools.ietf.org/html/rfc7512)
- [SoftHSM2 文档](https://github.com/opendnssec/SoftHSMv2)
