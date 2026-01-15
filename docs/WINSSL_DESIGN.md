# Windows SSL (Schannel) 后端设计

## 1. 概述

Windows SSL 后端基于 Windows 内置的 Schannel (Security Channel) API 实现，提供原生的 SSL/TLS 支持。

## 2. 优势

### 2.1 系统集成
- **无额外依赖**：使用 Windows 内置 API，无需分发额外的 DLL
- **自动更新**：通过 Windows Update 自动获取安全补丁
- **证书存储**：直接访问 Windows 证书存储
- **企业策略**：自动遵循企业 SSL/TLS 策略配置

### 2.2 性能优势
- **原生优化**：针对 Windows 平台优化的实现
- **硬件加速**：自动利用系统可用的加密硬件
- **内存效率**：与系统内存管理器集成

### 2.3 兼容性
- **协议支持**：支持 Windows 系统支持的所有 TLS 版本
- **密码套件**：使用系统配置的密码套件
- **FIPS 140-2**：在启用 FIPS 模式时自动合规

## 3. 技术实现

### 3.1 核心 Windows API
- **SSPI (Security Support Provider Interface)**：主要接口
- **Schannel.dll**：SSL/TLS 实现
- **Crypt32.dll**：证书处理
- **Secur32.dll**：安全上下文管理

### 3.2 关键结构和函数
```pascal
// 主要 Windows API 函数
AcquireCredentialsHandle()     // 获取凭据句柄
InitializeSecurityContext()   // 初始化安全上下文
AcceptSecurityContext()       // 接受安全上下文 (服务端)
QueryContextAttributes()      // 查询上下文属性
EncryptMessage()              // 加密数据
DecryptMessage()              // 解密数据
FreeCredentialsHandle()       // 释放凭据句柄
DeleteSecurityContext()       // 删除安全上下文
```

### 3.3 数据结构映射
```pascal
type
  // Schannel 特定配置
  TSchannelConfig = record
    EnabledProtocols: DWORD;           // 启用的协议版本
    CertContext: PCCERT_CONTEXT;       // 证书上下文
    CertStore: HCERTSTORE;             // 证书存储句柄
    ClientAuthMode: DWORD;             // 客户端认证模式
    CipherSuites: array of ALG_ID;     // 指定的密码套件
  end;

  // 安全上下文包装
  TWinSSLContext = class(TInterfacedObject, ISSLContext)
  private
    FCredHandle: CredHandle;           // 凭据句柄
    FCtxtHandle: CtxtHandle;           // 上下文句柄
    FConfig: TSchannelConfig;          // 配置信息
    FServerName: string;               // 目标服务器名称
  end;
```

## 4. 实现细节

### 4.1 握手流程
1. **凭据获取**：调用 `AcquireCredentialsHandle` 获取凭据
2. **上下文初始化**：客户端调用 `InitializeSecurityContext`
3. **握手数据交换**：处理握手数据包
4. **上下文完成**：握手完成，建立安全连接

### 4.2 数据传输
```pascal
// 加密发送数据
function EncryptData(const AData: TBytes): TBytes;
begin
  // 准备安全缓冲区
  SetLength(LSecBuffers, 4);
  // SECBUFFER_STREAM_HEADER
  // SECBUFFER_DATA  
  // SECBUFFER_STREAM_TRAILER
  // SECBUFFER_EMPTY
  
  // 调用 EncryptMessage
  LStatus := EncryptMessage(@FCtxtHandle, 0, @LSecBufferDesc, 0);
  // 处理结果...
end;

// 解密接收数据
function DecryptData(const AData: TBytes): TBytes;
begin
  // 准备安全缓冲区
  // 调用 DecryptMessage
  LStatus := DecryptMessage(@FCtxtHandle, @LSecBufferDesc, 0, nil);
  // 处理结果...
end;
```

### 4.3 证书处理
```pascal
// 加载证书
function LoadCertificate(const ACertPath: string): PCCERT_CONTEXT;
var
  LStore: HCERTSTORE;
  LCert: PCCERT_CONTEXT;
begin
  // 打开证书存储
  LStore := CertOpenStore(CERT_STORE_PROV_SYSTEM, 0, 0, 
    CERT_SYSTEM_STORE_CURRENT_USER, 'MY');
  
  // 查找证书
  LCert := CertFindCertificateInStore(LStore, X509_ASN_ENCODING, 0,
    CERT_FIND_SUBJECT_STR, PWideChar(ACertPath), nil);
    
  CertCloseStore(LStore, 0);
  Result := LCert;
end;
```

## 5. 错误处理

### 5.1 Windows 错误码映射
```pascal
function MapSchannelError(AWinError: DWORD): TSSLErrorCode;
begin
  case AWinError of
    SEC_E_INVALID_HANDLE: Result := sslErrGeneral;
    SEC_E_INVALID_TOKEN: Result := sslErrProtocol;
    SEC_E_CERT_EXPIRED: Result := sslErrCertificate;
    SEC_E_CERT_UNKNOWN: Result := sslErrCertificate;
    SEC_E_INCOMPLETE_MESSAGE: Result := sslErrIO;
    SEC_I_CONTINUE_NEEDED: Result := sslErrNone; // 继续握手
    // ... 更多映射
  end;
end;
```

### 5.2 错误上下文信息
- 保留原始 Windows 错误码和消息
- 提供操作上下文（握手、数据传输等）
- 包含证书验证详情

## 6. 配置选项

### 6.1 协议版本控制
```pascal
// 设置支持的 TLS 版本
procedure SetProtocolVersions(AVersions: TSSLProtocolVersionSet);
var
  LProtocols: DWORD;
begin
  LProtocols := 0;
  if sslProtocolTLS10 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_0;
  if sslProtocolTLS11 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_1;
  if sslProtocolTLS12 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_2;
  if sslProtocolTLS13 in AVersions then
    LProtocols := LProtocols or SP_PROT_TLS1_3;
    
  FConfig.EnabledProtocols := LProtocols;
end;
```

### 6.2 证书验证配置
- 使用系统证书存储
- 支持自定义证书验证回调
- 企业证书策略集成

## 7. 平台兼容性

### 7.1 Windows 版本支持
- **Windows 7/Server 2008 R2**：TLS 1.0, 1.1, 1.2
- **Windows 8/Server 2012**：增强的 TLS 1.2 支持
- **Windows 10/Server 2016+**：TLS 1.3 支持

### 7.2 运行时检测
```pascal
function GetSystemTLSSupport: TSSLProtocolVersionSet;
var
  LVersionInfo: OSVERSIONINFO;
begin
  Result := [];
  LVersionInfo.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  GetVersionEx(LVersionInfo);
  
  // 基于 Windows 版本确定支持的 TLS 版本
  if (LVersionInfo.dwMajorVersion >= 6) then
  begin
    Result := Result + [sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12];
    
    // Windows 10 及以上支持 TLS 1.3
    if (LVersionInfo.dwMajorVersion >= 10) then
      Result := Result + [sslProtocolTLS13];
  end;
end;
```

## 8. 性能优化

### 8.1 缓冲区管理
- 重用安全缓冲区减少分配
- 优化数据拷贝操作
- 批量处理多个数据包

### 8.2 上下文缓存
- 缓存握手结果用于会话复用
- 延迟初始化非关键组件

## 9. 安全考虑

### 9.1 内存安全
- 及时清零敏感数据
- 正确释放 Windows 句柄和上下文
- 防止敏感信息泄漏

### 9.2 企业集成
- 遵循组策略设置
- 支持域证书自动配置
- FIPS 140-2 合规模式

## 10. 测试策略

### 10.1 兼容性测试
- 不同 Windows 版本的功能验证
- 与其他后端的行为一致性测试
- 企业环境下的策略遵循测试

### 10.2 性能基准
- 与 OpenSSL 性能对比
- 内存使用效率测试
- 大量并发连接测试

---
**文档版本**: 1.0  
**创建时间**: 2025-09-28  
**更新历史**: 初始版本