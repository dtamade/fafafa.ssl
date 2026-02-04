# DANE/DNSSEC 用户指南

> **版本**: v1.0.0
> **更新**: 2026-02-05

本指南介绍如何在 fafafa.ssl 中使用 DANE (DNS-Based Authentication of Named Entities) 和 DNSSEC。

---

## 概述

DANE 允许通过 DNS TLSA 记录发布证书信息，提供额外的身份验证层。

### 主要功能

- **TLSA 记录查询** - 从 DNS 获取证书绑定信息
- **DNSSEC 验证** - 确保 DNS 响应的完整性
- **证书匹配** - 验证服务器证书是否匹配 TLSA 记录

### 依赖说明

ldns 库是**可选依赖**。如果不可用：
- DANE 功能自动禁用
- 其他 SSL/TLS 功能正常工作
- 不会导致程序崩溃

---

## 安装 ldns

```bash
# Ubuntu/Debian
sudo apt-get install libldns-dev ldnsutils

# macOS
brew install ldns

# 验证安装
drill --version
```

---

## 快速开始

### 基本 TLSA 验证

```pascal
uses
  fafafa.ssl.dane;

var
  Validator: TDANEValidator;
  Cert: PX509;
begin
  // 创建验证器
  Validator := TDANEValidator.Create('example.com', 443);
  try
    // 查询 TLSA 记录
    if Validator.QueryTLSARecords('example.com', 443) then
    begin
      WriteLn('找到 ', Validator.GetRecordCount, ' 条 TLSA 记录');

      // 验证证书
      if Validator.ValidateCertificate(Cert) then
        WriteLn('证书匹配 TLSA 记录')
      else
        WriteLn('证书不匹配');
    end
    else
      WriteLn('未找到 TLSA 记录或 ldns 不可用');
  finally
    Validator.Free;
  end;
end;
```

### 扩展验证（含 DNSSEC）

```pascal
uses
  fafafa.ssl.dane;

var
  Validator: TDANEValidatorEx;
begin
  Validator := TDANEValidatorEx.Create('example.com', 443);
  try
    // 设置 DNS 解析器
    Validator.SetDNSResolver('8.8.8.8');
    Validator.SetDNSTimeout(5000);

    // 查询并验证
    if Validator.QueryTLSARecords('example.com', 443) then
    begin
      // 检查 DNSSEC 状态
      WriteLn('DNSSEC 状态: ', Validator.GetDNSSECStatus);

      if Validator.VerifyDNSSEC then
        WriteLn('DNSSEC 验证通过')
      else
        WriteLn('DNSSEC 验证失败或不可用');
    end;
  finally
    Validator.Free;
  end;
end;
```

---

## TLSA 记录格式

TLSA 记录格式: `_port._protocol.hostname`

例如: `_443._tcp.example.com`

### 记录字段

| 字段 | 值 | 说明 |
|------|------|------|
| **Usage** | 0-3 | 证书用途 |
| **Selector** | 0-1 | 匹配内容 |
| **Matching Type** | 0-2 | 匹配方式 |
| **Certificate Data** | hex | 证书数据或哈希 |

### Usage 值

| 值 | 名称 | 说明 |
|----|------|------|
| 0 | PKIX-TA | CA 约束 |
| 1 | PKIX-EE | 服务证书约束 |
| 2 | DANE-TA | 信任锚声明 |
| 3 | DANE-EE | 域颁发证书 |

### Selector 值

| 值 | 名称 | 说明 |
|----|------|------|
| 0 | Full | 完整证书 |
| 1 | SPKI | 公钥信息 |

### Matching Type 值

| 值 | 名称 | 说明 |
|----|------|------|
| 0 | Exact | 精确匹配 |
| 1 | SHA-256 | SHA-256 哈希 |
| 2 | SHA-512 | SHA-512 哈希 |

---

## 手动添加 TLSA 记录

用于测试或缓存：

```pascal
var
  Validator: TDANEValidator;
  CertHash: TBytes;
begin
  Validator := TDANEValidator.Create('example.com', 443);

  // 添加 DANE-EE + SPKI + SHA-256 记录
  CertHash := GetSHA256HashOfCertSPKI(Cert);
  Validator.AddTLSARecord(
    duDomainIssuedCert,    // Usage = 3
    dsSubjectPublicKeyInfo, // Selector = 1
    dmSHA256,               // Matching = 1
    CertHash
  );

  // 现在可以验证
  if Validator.ValidateCertificate(Cert) then
    WriteLn('验证通过');
end;
```

---

## 配置选项

```pascal
var
  Validator: TDANEValidator;
begin
  Validator := TDANEValidator.Create('example.com', 443);

  // 是否要求 DNSSEC（默认 True）
  Validator.RequireDNSSEC := True;

  // 启用缓存（默认 True）
  Validator.EnableCache := True;

  // 缓存超时（秒，默认 3600）
  Validator.CacheTimeout := 3600;
end;
```

---

## 检查 ldns 可用性

```pascal
uses
  fafafa.ssl.dns.ldns;

begin
  if LoadLdns then
    WriteLn('ldns 已加载，DANE 功能可用')
  else
    WriteLn('ldns 不可用: ', GetLdnsLoadError);
end;
```

---

## 命令行工具验证

使用 `drill` 或 `dig` 查询 TLSA 记录：

```bash
# 使用 drill (ldns)
drill TLSA _443._tcp.example.com

# 使用 dig
dig TLSA _443._tcp.example.com

# 验证 DNSSEC
drill -S example.com
```

---

## 故障排除

### ldns 加载失败

```bash
# 检查库是否安装
ldconfig -p | grep ldns

# 手动查找
find /usr -name "libldns*" 2>/dev/null
```

### TLSA 记录未找到

1. 确认域名支持 DANE
2. 检查 DNS 解析器是否支持 TLSA 查询
3. 使用公共 DNS（如 8.8.8.8）测试

### DNSSEC 验证失败

- 域名可能未启用 DNSSEC
- DNS 解析器可能不支持 DNSSEC 验证
- 检查系统时间是否正确

---

## 安全建议

1. **启用 DNSSEC 要求** - 没有 DNSSEC 的 DANE 不安全
2. **定期刷新缓存** - 防止过期记录
3. **结合传统 PKI** - DANE 作为额外验证层
4. **监控 DNS 变更** - 及时发现异常

---

## 相关文档

- [RFC 6698 - DANE TLSA](https://tools.ietf.org/html/rfc6698)
- [RFC 7671 - DANE 操作指南](https://tools.ietf.org/html/rfc7671)
- [ldns 文档](https://www.nlnetlabs.nl/projects/ldns/)
