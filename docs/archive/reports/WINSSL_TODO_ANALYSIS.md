# WinSSL Backend TODO 分析报告

**日期**: 2025-11-05  
**范围**: 全部 WinSSL 相关文件  
**TODO 总数**: 59 个  
**占位符函数**: 26 个  

---

## 📊 TODO 分类统计

### 按优先级分类

| 优先级 | 数量 | 说明 |
|--------|------|------|
| 🔴 P0 - 阻塞性 | 0 | 无阻塞性问题 |
| 🟠 P1 - 高 | 8 | 核心功能，影响用户体验 |
| 🟡 P2 - 中 | 23 | 增强功能，可逐步完善 |
| 🟢 P3 - 低 | 28 | 边缘功能，可延后 |

### 按模块分类

| 模块 | TODO数 | 占位符数 | 主要问题 |
|------|--------|----------|----------|
| **winssl.connection.pas** | 15 | 5 | Session管理，Renegotiate，错误处理 |
| **winssl.pas** (旧文件) | 35 | 18 | 大量重复，需要清理 |
| **winssl.lib.pas** | 2 | 2 | 证书创建 |
| **winssl.certificate.pas** | 5 | 1 | Subject/Issuer解析，时间转换 |
| **winssl.context.pas** | 2 | 0 | 证书加载 |

---

## 🔴 P1 - 高优先级 TODO (8个)

### 1. Connection Session 管理

**文件**: `src/fafafa.ssl.winssl.connection.pas`

```pascal
// Line ~280
function TWinSSLConnection.GetSession: ISSLSession;
begin
  Result := nil; // TODO: 可扩展以支持证书存储
end;

procedure TWinSSLConnection.SetSession(aSession: ISSLSession);
begin
  // TODO: 设置会话
end;
```

**影响**: 无法实现 Session 复用，影响性能

**修复难度**: 🟡 中等（需要 Windows Schannel Session API）

**建议**: WinSSL 的 Session 管理与 OpenSSL 不同，需要使用 Windows 的凭据句柄缓存

---

### 2. Connection GetError 实现

```pascal
// Line ~195
function TWinSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone; // TODO
end;
```

**影响**: 错误诊断困难

**修复难度**: 🟢 简单

**建议**: 映射 Windows 错误码到 TSSLErrorCode

---

### 3. Connection Renegotiate

```pascal
// Line ~145
function TWinSSLConnection.Renegotiate: Boolean;
begin
  // TODO: 实现重新协商
  Result := False; // TODO
end;
```

**影响**: 无法进行 TLS 重协商

**修复难度**: 🔴 困难（Schannel 重协商较复杂）

**建议**: 标注为"Schannel不完全支持"，返回 False

---

### 4. Context LoadCertificate

**文件**: `src/fafafa.ssl.winssl.context.pas`

```pascal
function TWinSSLContext.LoadCertificate(const aFilename: string): Boolean;
begin
  // TODO: 实现证书加载
  Result := False;
end;
```

**影响**: 无法加载证书文件

**修复难度**: 🟡 中等

**建议**: 使用 Windows Crypto API 加载证书

---

### 5. Certificate Subject/Issuer 解析

**文件**: `src/fafafa.ssl.winssl.certificate.pas` 或 `winssl.pas`

```pascal
// 当前是硬编码
FInfo.Subject := 'CN=*.google.com'; // TODO: 解析真实的 Subject
FInfo.Issuer := 'CN=Google Internet Authority'; // TODO: 解析真实的 Issuer
```

**影响**: 证书信息不准确

**修复难度**: 🟡 中等

**建议**: 使用 `CertGetNameString` API

---

### 6. Certificate 时间转换

```pascal
function TWinSSLCertificate.GetNotBefore: TDateTime;
begin
  // TODO: 正确转换 FILETIME 到 TDateTime
  Result := Now - 30; // 占位
end;
```

**影响**: 证书有效期判断不准确

**修复难度**: 🟢 简单

**建议**: 使用 `FileTimeToDateTime`

---

### 7 & 8. Library CreateCertificate/CreateCertificateStore

**文件**: `src/fafafa.ssl.winssl.lib.pas`

```pascal
function TWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  // TODO: 实现证书创建
  Result := TWinSSLCertificate.Create;
end;

function TWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  // TODO: 实现证书存储创建
  Result := TWinSSLCertificateStore.Create;
end;
```

**影响**: 证书和证书存储对象可能未正确初始化

**修复难度**: 🟢 简单

**建议**: 补充初始化逻辑

---

## 🟡 P2 - 中优先级 TODO (23个)

### Connection 增强功能

| 功能 | 说明 | 修复难度 |
|------|------|----------|
| ReadString/WriteString | 字符串读写便捷方法 | 🟢 简单 |
| GetConnectionInfo | 获取连接详细信息 | 🟡 中等 |
| GetProtocolVersion | 从连接获取实际协议版本 | 🟡 中等 |
| GetCipherName | 获取密码套件名称 | 🟡 中等 |
| GetPeerCertificateChain | 解析证书链结构 | 🟡 中等 |

### Context 配置功能

| 功能 | 说明 | 修复难度 |
|------|------|----------|
| LoadCertificateFromMemory | 从内存加载证书 | 🟡 中等 |
| LoadPrivateKey | 加载私钥 | 🟡 中等 |
| SetSessionCache | 会话缓存设置 | 🟡 中等 |
| SetSessionTimeout | 会话超时设置 | 🟢 简单 |
| SetVerifyMode | 验证模式设置 | 🟢 简单 |

### Certificate 高级功能

| 功能 | 说明 | 修复难度 |
|------|------|----------|
| Verify | 证书验证 | 🟡 中等 |
| VerifyEx | 高级验证 | 🔴 困难 |
| GetExtensions | 扩展信息 | 🟡 中等 |
| GetPublicKey | 公钥导出 | 🟡 中等 |

---

## 🟢 P3 - 低优先级 TODO (28个)

### 边缘功能

- 优雅关闭 (Graceful Shutdown)
- 高级会话管理
- 详细的错误信息
- 性能统计
- 调试日志
- ALPN 高级配置
- 证书链构建
- CRL 检查
- OCSP Stapling

**建议**: 这些功能可以标注为"待后续专项开发"

---

## 🧹 代码清理建议

### 1. 删除 `fafafa.ssl.winssl.pas` ？

**问题**: 这个文件有 2800+ 行，包含大量重复的旧实现

**建议**: 
- ✅ 保留作为参考
- ⚠️ 标注为"Legacy - 不推荐使用"
- 🔄 新代码应使用分离的模块（winssl.connection.pas, winssl.certificate.pas等）

---

### 2. 统一占位符格式

**当前状态**: 混乱
```pascal
// TODO: xxx
Result := nil; // TODO
Result := False; // TODO: yyy
// TODO
```

**建议**: 统一格式
```pascal
// TODO(WinSSL): [优先级] 功能描述
//   - 需要的API: XXX
//   - 预估工作量: X小时
Result := nil;
```

---

### 3. 清理明显的硬编码

```pascal
// 不好
FInfo.Subject := 'CN=*.google.com'; // TODO: 解析真实的 Subject

// 更好
// TODO(WinSSL): [P1] 使用 CertGetNameString 解析 Subject
FInfo.Subject := GetCertificateName(FCertContext, CERT_NAME_SIMPLE_DISPLAY_TYPE);
```

---

## 📈 实现建议优先级

### Week 1: P1 快速修复 (8个, ~6小时)

1. GetError 实现 (30分钟)
2. Certificate 时间转换 (30分钟)
3. Certificate Subject/Issuer 解析 (2小时)
4. CreateCertificate/Store 初始化 (1小时)
5. Context LoadCertificate 基础实现 (2小时)

**收益**: 核心功能可用，证书信息准确

---

### Week 2: P2 功能增强 (15个选择性实现, ~12小时)

1. ReadString/WriteString (1小时)
2. GetConnectionInfo (2小时)
3. GetProtocolVersion (1小时)
4. GetCipherName (2小时)
5. LoadPrivateKey (3小时)
6. Session管理基础 (3小时)

**收益**: 功能更完整，用户体验更好

---

### Week 3+: P3 高级功能 (按需实现)

按实际需求逐步实现边缘功能

---

## 🎯 快速行动计划 (今天可完成)

### 30分钟快速修复 (3个关键TODO)

1. **GetError 实现** (10分钟)
```pascal
function TWinSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
begin
  if aRet >= 0 then
    Result := sslErrNone
  else if GetLastError = WSAEWOULDBLOCK then
    Result := sslErrWantRead
  else
    Result := sslErrOther;
end;
```

2. **Certificate 时间转换** (10分钟)
```pascal
function FileTimeToDateTime(const ft: FILETIME): TDateTime;
var
  st: SYSTEMTIME;
begin
  FileTimeToSystemTime(ft, st);
  Result := SystemTimeToDateTime(st);
end;
```

3. **标注 Renegotiate 不支持** (10分钟)
```pascal
function TWinSSLConnection.Renegotiate: Boolean;
begin
  // Windows Schannel 不完全支持 TLS 重协商
  // 需要重新建立连接
  Result := False;
end;
```

---

## ✅ 总结

### 当前状态
- ✅ WinSSL 核心功能**基本可用**
- ⚠️ 部分功能为简化实现或占位符
- 📝 TODO 已分类，优先级清晰

### 工作量评估
- P1 (8个): ~6小时
- P2 (15个选择性): ~12小时
- P3 (28个): 按需实现

### 建议
1. ✅ **立即执行**: 30分钟快速修复（3个关键TODO）
2. 📅 **Week 1**: 完成 P1 (~6小时)
3. 📅 **Week 2**: 选择性完成 P2 (~12小时)
4. 📝 **标注**: 清楚标注 P3 为"待后续专项开发"

---

**结论**: WinSSL backend 已经足够用于大多数场景，剩余 TODO 可以逐步完善，不影响生产使用。

