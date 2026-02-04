# PKCS#11 用户指南

> **版本**: v1.0.0
> **更新**: 2026-02-05

本指南介绍如何在 fafafa.ssl 中使用 PKCS#11 硬件安全模块 (HSM)。

---

## 概述

PKCS#11 是访问加密令牌（如智能卡、HSM）的标准 API。fafafa.ssl 支持：

- **SoftHSM2** - 软件模拟的 HSM（用于开发和测试）
- **硬件 HSM** - 各种 PKCS#11 兼容设备
- **智能卡** - 支持 PKCS#11 的智能卡

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

| 组件 | 说明 | 示例 |
|------|------|------|
| `token` | 令牌标签 | `token=TestToken` |
| `object` | 密钥标签 | `object=TestKey` |
| `type` | 对象类型 | `type=private` |
| `pin-value` | PIN 码 | `pin-value=1234` |
| `module-path` | PKCS#11 模块路径 | `module-path=/usr/lib/softhsm/libsofthsm2.so` |

---

## 代码示例

### 使用 Context Builder

```pascal
uses
  fafafa.ssl.context.builder;

var
  Builder: ISSLContextBuilder;
  Ctx: ISSLContext;
begin
  Builder := TSSLContextBuilder.Create;

  // 配置 PKCS#11
  Ctx := Builder
    .ForServer
    .WithCertificate('/path/to/cert.pem')
    .WithPKCS11Key('pkcs11:token=TestToken;object=TestKey')
    .WithPKCS11PIN('1234')
    .Build;
end;
```

### 使用 PIN 回调

```pascal
uses
  fafafa.ssl.pkcs11.types,
  fafafa.ssl.pkcs11.engine;

// PIN 回调函数
function MyPINCallback(const TokenLabel: string; out PIN: string): Boolean;
begin
  // 从用户界面或配置获取 PIN
  PIN := GetPINFromUser(TokenLabel);
  Result := PIN <> '';
end;

var
  Config: TPKCS11Config;
  Backend: TEngineBackend;
  Key: PEVP_PKEY;
begin
  Config := Default(TPKCS11Config);
  Config.ModulePath := '/usr/lib/softhsm/libsofthsm2.so';
  Config.TokenLabel := 'TestToken';
  Config.KeyLabel := 'TestKey';
  Config.PINCallback := @MyPINCallback;

  Backend := TEngineBackend.Create;
  try
    Key := Backend.LoadPrivateKey(Config);
    // 使用密钥...
  finally
    Backend.Free;
  end;
end;
```

---

## 后端选择

fafafa.ssl 提供两种 PKCS#11 后端：

| 后端 | OpenSSL 版本 | 说明 |
|------|-------------|------|
| **Provider** | 3.0+ | 推荐，使用 OpenSSL 3.x provider API |
| **ENGINE** | 1.1.1+ | 兼容模式，使用 ENGINE API |

系统会自动选择最佳后端。

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
