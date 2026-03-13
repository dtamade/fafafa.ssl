# Certificate Transparency (CT) 实现指南

**版本**: 1.0  
**创建日期**: 2026-01-30  
**适用于**: fafafa.ssl v1.0+

---

## 📋 概述

本指南介绍如何在 fafafa.ssl 项目中使用证书透明度（Certificate Transparency, CT）功能，包括 SCT 验证和 CT 日志服务器集成。

### 什么是证书透明度？

证书透明度（CT）是一个开放的框架，用于监控和审计 SSL/TLS 证书的颁发。它通过要求证书颁发机构（CA）将颁发的证书记录到公开的、仅追加的日志中，来提高证书系统的安全性和透明度。

### 核心概念

- **SCT (Signed Certificate Timestamp)**：由 CT 日志服务器签名的时间戳，证明证书已被记录
- **CT 日志服务器**：维护证书记录的公开服务器
- **CT 策略**：定义需要多少个有效 SCT 才能信任证书

---

## 🚀 快速开始

### 1. 基本 SCT 验证

```pascal
uses
  fafafa.ssl.ct.sct,
  fafafa.ssl.openssl.api.x509;

var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
  Cert, Issuer: PX509;
begin
  // 创建默认验证选项
  Options := CreateDefaultValidationOptions;
  
  // 创建验证器
  Validator := TSCTValidator.Create(Options);
  try
    // 从 X.509 证书扩展验证 SCT
    Results := Validator.ValidateFromX509(Cert, Issuer);
    
    // 检查是否满足策略要求
    if Validator.CheckPolicy(Results) then
      WriteLn('证书通过 CT 验证')
    else
      WriteLn('证书未通过 CT 验证');
  finally
    Validator.Free;
  end;
end;
```

### 2. 从 TLS 连接验证 SCT

```pascal
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
  SSL: PSSL;
  Cert, Issuer: PX509;
begin
  Options := CreateDefaultValidationOptions;
  Validator := TSCTValidator.Create(Options);
  try
    // 从 TLS 扩展提取并验证 SCT
    Results := Validator.ValidateFromTLS(SSL, Cert, Issuer);
    
    // 检查验证结果
    if Validator.CheckPolicy(Results) then
      WriteLn('TLS 连接通过 CT 验证');
  finally
    Validator.Free;
  end;
end;
```

---

## ⚙️ 配置选项

### TSCTValidationOptions

```pascal
type
  TSCTValidationOptions = record
    RequireValidSCTs: Boolean;      // 是否要求至少一个有效 SCT
    MinimumSCTCount: Integer;       // 最少 SCT 数量（默认 2）
    AllowUnknownLogs: Boolean;      // 是否允许未知日志
    ClockDriftTolerance: Integer;   // 时钟漂移容差（毫秒，默认 300000 = 5分钟）
    LogStoreFile: string;           // CT 日志存储文件路径
  end;
```

### 创建自定义选项

```pascal
var
  Options: TSCTValidationOptions;
begin
  Options := CreateDefaultValidationOptions;
  
  // 自定义配置
  Options.RequireValidSCTs := True;      // 要求有效 SCT
  Options.MinimumSCTCount := 2;          // 至少 2 个 SCT
  Options.AllowUnknownLogs := False;     // 不允许未知日志
  Options.ClockDriftTolerance := 300000; // 5 分钟容差
  Options.LogStoreFile := '/path/to/ct_log_list.json';
end;
```

---

## 📊 验证结果

### TSCTValidationResult

```pascal
type
  TSCTValidationResult = record
    IsValid: Boolean;           // 是否有效
    Status: Integer;            // 验证状态
    ErrorMessage: string;       // 错误消息
    LogName: string;            // 日志名称
    Timestamp: UInt64;          // 时间戳（毫秒）
  end;
```

### 验证状态

| 状态 | 值 | 含义 |
|------|-----|------|
| `SCT_VALIDATION_STATUS_NOT_SET` | 0 | 未设置 |
| `SCT_VALIDATION_STATUS_UNKNOWN_LOG` | 1 | 未知日志 |
| `SCT_VALIDATION_STATUS_VALID` | 2 | 有效 |
| `SCT_VALIDATION_STATUS_INVALID` | 3 | 无效 |
| `SCT_VALIDATION_STATUS_UNVERIFIED` | 4 | 未验证 |
| `SCT_VALIDATION_STATUS_UNKNOWN_VERSION` | 5 | 未知版本 |

### 处理验证结果

```pascal
var
  Results: TSCTValidationResultArray;
  I: Integer;
begin
  Results := Validator.ValidateFromX509(Cert, Issuer);
  
  for I := 0 to High(Results) do
  begin
    WriteLn('SCT #', I + 1);
    WriteLn('  有效: ', Results[I].IsValid);
    WriteLn('  状态: ', GetSCTValidationStatusName(Results[I].Status));
    WriteLn('  日志: ', Results[I].LogName);
    WriteLn('  时间: ', FormatSCTTimestamp(Results[I].Timestamp));
    
    if not Results[I].IsValid then
      WriteLn('  错误: ', Results[I].ErrorMessage);
  end;
end;
```

---

## 🌐 CT 日志服务器集成

### 使用 TCTLogClient

> 注意：`fafafa.ssl` 不实现网络通信；`LoadFromGoogleCTLogList` 会通过 `fafafa.ssl.net.hooks` 调用上层注入的 HTTP GET。

```pascal
uses
  fafafa.ssl.base,
  fafafa.ssl.net.hooks,
  fafafa.ssl.ct.log;

var
  Scope: TSSLHTTPHooksScope;
  Client: TCTLogClient;
begin
  // 由上层注入 HTTP GET（示例：线程局部 hooks）
  // Scope := TSSLHTTPHooksScope.Push(TSSLHTTPHooks.Create(@YourTransport.HTTPGet, nil));
  // try

  // 创建客户端（带缓存）
  Client := TCTLogClient.Create('ct_cache.json', True);
  try
    // 从 Google CT 日志列表加载
    if Client.LoadFromGoogleCTLogList then
      WriteLn('成功加载 ', Client.GetUsableLogCount, ' 个可用日志');
    
    // 获取日志存储（用于 SCT 验证）
    Validator := TSCTValidator.Create(Options);
    // 注意：当前实现中，日志存储由 Validator 内部管理
  finally
    Client.Free;
  end;

  // finally
  //   Scope.Pop;
  // end;
end;
```

### 查找日志信息

```pascal
var
  LogInfo: TCTLogInfo;
begin
  LogInfo := Client.FindLogByID('base64-encoded-log-id');
  
  if LogInfo.LogID <> '' then
  begin
    WriteLn('日志名称: ', LogInfo.Description);
    WriteLn('运营商: ', LogInfo.OperatorName);
    WriteLn('URL: ', LogInfo.URL);
    WriteLn('可用: ', LogInfo.IsUsable);
  end;
end;
```

---

## 🔧 高级用法

### 自定义 CT 日志存储

```pascal
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
begin
  Options := CreateDefaultValidationOptions;
  Options.LogStoreFile := '/custom/path/to/ct_logs.conf';
  
  Validator := TSCTValidator.Create(Options);
  try
    // 验证器会自动加载指定的日志存储
    if Validator.LoadLogStore(Options.LogStoreFile) then
      WriteLn('成功加载自定义日志存储');
  finally
    Validator.Free;
  end;
end;
```

### 策略检查

```pascal
function CheckCTPolicy(const Results: TSCTValidationResultArray): Boolean;
var
  ValidCount: Integer;
  I: Integer;
begin
  ValidCount := 0;
  
  for I := 0 to High(Results) do
  begin
    if Results[I].IsValid then
      Inc(ValidCount);
  end;
  
  // 自定义策略：至少 2 个有效 SCT
  Result := ValidCount >= 2;
end;
```

---

## 🧪 测试

### 运行单元测试

```bash
# 编译测试程序
fpc -Mobjfpc -Sh -Fu./src -Fi./src -FE./tests/ct tests/ct/test_ct_sct_validation.pas

# 运行测试
./tests/ct/test_ct_sct_validation
```

### 测试覆盖

当前测试覆盖以下功能：
- ✅ 创建默认验证选项
- ✅ 创建 SCT 验证器
- ✅ 获取 SCT 验证状态名称
- ✅ 格式化 SCT 时间戳
- ✅ 加载 CT 日志存储
- ✅ 策略检查（无 SCT、不足 SCT、足够 SCT、混合 SCT）
- ✅ 从 TLS/X509/OCSP 验证（空输入处理）

---

## 📝 最佳实践

### 1. 始终验证 SCT

在生产环境中，应该始终验证证书的 SCT，以确保证书已被记录到 CT 日志中。

```pascal
// 推荐：在证书验证流程中集成 CT 验证
if not VerifyCertificateChain(Cert) then
  Exit(False);

if not VerifySCT(Cert, Issuer) then
  Exit(False);
```

### 2. 使用合理的策略

根据安全需求设置合理的 CT 策略：

```pascal
// 高安全场景
Options.RequireValidSCTs := True;
Options.MinimumSCTCount := 2;
Options.AllowUnknownLogs := False;

// 兼容性场景
Options.RequireValidSCTs := False;
Options.MinimumSCTCount := 1;
Options.AllowUnknownLogs := True;
```

### 3. 缓存 CT 日志列表

CT 日志列表不经常变化，应该缓存以提高性能：

```pascal
Client := TCTLogClient.Create('ct_cache.json', True);
// 客户端会自动从缓存加载，避免每次都下载
```

### 4. 处理验证失败

优雅地处理 CT 验证失败：

```pascal
Results := Validator.ValidateFromX509(Cert, Issuer);

if not Validator.CheckPolicy(Results) then
begin
  // 记录详细错误信息
  for I := 0 to High(Results) do
  begin
    if not Results[I].IsValid then
      LogError('SCT 验证失败: ' + Results[I].ErrorMessage);
  end;
  
  // 根据策略决定是否继续
  if Options.RequireValidSCTs then
    Exit(False);
end;
```

---

## 🐛 故障排查

### 问题：SCT 验证失败（UNKNOWN_LOG）

**原因**：CT 日志不在日志存储中

**解决方案**：
1. 更新 CT 日志列表
2. 检查日志存储文件是否正确加载
3. 考虑设置 `AllowUnknownLogs := True`（仅用于测试）

### 问题：SCT 验证失败（UNVERIFIED）

**原因**：缺少颁发者证书

**解决方案**：
1. 确保提供了正确的颁发者证书
2. 检查证书链是否完整

### 问题：时间戳验证失败

**原因**：系统时间不准确或时钟漂移超过容差

**解决方案**：
1. 同步系统时间
2. 增加 `ClockDriftTolerance` 值（不推荐）

---

## 📚 参考资料

### RFC 标准

- [RFC 6962](https://tools.ietf.org/html/rfc6962) - Certificate Transparency
- [RFC 6066](https://tools.ietf.org/html/rfc6066) - TLS Extensions (OCSP Stapling)

### 相关文档

- [阶段 1 实施计划](PHASE_1_SECURITY_PERFORMANCE_PLAN.md)
- [阶段 1 执行总结](PHASE_1_EXECUTION_SUMMARY.md)
- [架构设计](ARCHITECTURE.md)
- [安全指南](SECURITY_GUIDE.md)

### 外部资源

- [Google CT 日志列表](https://www.gstatic.com/ct/log_list/v3/all_logs_list.json)
- [Certificate Transparency 官网](https://certificate.transparency.dev/)

---

## 🔄 更新日志

| 版本 | 日期 | 变更内容 |
|------|------|---------|
| 1.0 | 2026-01-30 | 初始版本，包含 SCT 验证和 CT 日志集成 |

---

**文档维护**:  
- 创建者: Claude Code (Sisyphus)  
- 创建日期: 2026-01-30  
- 版本: 1.0  
- 下次审查: 2026-03-01
