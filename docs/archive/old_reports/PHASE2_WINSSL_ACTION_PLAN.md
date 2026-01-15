# Phase 2: WinSSL 后端开发行动计划

**创建日期**: 2025-10-04  
**优先级**: ⭐⭐⭐⭐⭐ **最高优先级**  
**预计工作量**: 45 小时（约 1-2 周兼职）

---

## 📋 目标

实现 fafafa.ssl 的 **Windows Schannel (WinSSL)** 后端，这是项目的**杀手级功能**和核心差异化优势。

### 为什么这是最重要的？

1. **差异化价值**: 这是 fafafa.ssl 与其他 Pascal SSL 库的最大区别
2. **零依赖部署**: Windows 用户无需分发 OpenSSL DLL
3. **企业需求**: 自动集成 Windows 证书策略，FIPS 合规
4. **市场定位**: Pascal/Lazarus 的主要平台是 Windows

---

## 🗺️ 整体路线图

```
Phase 2.1: 核心 API 绑定       [20 小时] ████████████░░░░░░░░
Phase 2.2: 接口实现            [15 小时] ██████████░░░░░░░░░░
Phase 2.3: 证书集成            [10 小时] ██████░░░░░░░░░░░░░░
Phase 2.4: 测试和验证          [10 小时] ██████░░░░░░░░░░░░░░
Phase 2.5: 文档和示例          [ 5 小时] ███░░░░░░░░░░░░░░░░░
──────────────────────────────────────────
总计:                          [60 小时]
```

---

## 📦 Phase 2.1: 核心 Schannel API 绑定

**目标**: 绑定 Windows Schannel 所需的所有 API

**工作量**: 约 20 小时

### 任务清单

#### 2.1.1 创建 Windows API 类型定义文件

**文件**: `src/fafafa.ssl.winssl.types.pas`

- [ ] 定义 Schannel 相关结构体
  ```pascal
  type
    SecHandle = record
      dwLower: ULONG_PTR;
      dwUpper: ULONG_PTR;
    end;
    
    CredHandle = SecHandle;
    CtxtHandle = SecHandle;
    
    SCHANNEL_CRED = record
      dwVersion: DWORD;
      cCreds: DWORD;
      paCred: PPCCERT_CONTEXT;
      // ... 更多字段
    end;
    
    SecBuffer = record
      cbBuffer: ULONG;
      BufferType: ULONG;
      pvBuffer: Pointer;
    end;
    
    SecBufferDesc = record
      ulVersion: ULONG;
      cBuffers: ULONG;
      pBuffers: PSecBuffer;
    end;
  ```

- [ ] 定义常量
  ```pascal
  const
    SECPKG_CRED_INBOUND  = $00000001;
    SECPKG_CRED_OUTBOUND = $00000002;
    
    SECBUFFER_VERSION = 0;
    SECBUFFER_DATA = 1;
    SECBUFFER_STREAM_HEADER = 7;
    SECBUFFER_STREAM_TRAILER = 6;
    
    SP_PROT_TLS1_2_CLIENT = $00000800;
    SP_PROT_TLS1_3_CLIENT = $00002000;
    // ... 更多协议常量
  ```

- [ ] 定义错误码映射
  ```pascal
  const
    SEC_E_OK = 0;
    SEC_I_CONTINUE_NEEDED = $00090312;
    SEC_E_INCOMPLETE_MESSAGE = $80090318;
    SEC_E_CERT_EXPIRED = $80090328;
    // ... 更多错误码
  ```

**预计时间**: 5 小时

#### 2.1.2 绑定 Schannel 核心函数

**文件**: `src/fafafa.ssl.winssl.api.pas`

- [ ] 凭据管理函数
  ```pascal
  function AcquireCredentialsHandle(
    pszPrincipal: PWideChar;
    pszPackage: PWideChar;
    fCredentialUse: ULONG;
    pvLogonId: Pointer;
    pAuthData: Pointer;
    pGetKeyFn: Pointer;
    pvGetKeyArgument: Pointer;
    phCredential: PCredHandle;
    ptsExpiry: PTimeStamp
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  
  function FreeCredentialsHandle(
    phCredential: PCredHandle
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  ```

- [ ] 安全上下文函数
  ```pascal
  function InitializeSecurityContext(
    phCredential: PCredHandle;
    phContext: PCtxtHandle;
    pszTargetName: PWideChar;
    fContextReq: ULONG;
    Reserved1: ULONG;
    TargetDataRep: ULONG;
    pInput: PSecBufferDesc;
    Reserved2: ULONG;
    phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc;
    pfContextAttr: PULONG;
    ptsExpiry: PTimeStamp
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  
  function AcceptSecurityContext(
    phCredential: PCredHandle;
    phContext: PCtxtHandle;
    pInput: PSecBufferDesc;
    fContextReq: ULONG;
    TargetDataRep: ULONG;
    phNewContext: PCtxtHandle;
    pOutput: PSecBufferDesc;
    pfContextAttr: PULONG;
    ptsExpiry: PTimeStamp
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  
  function DeleteSecurityContext(
    phContext: PCtxtHandle
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  ```

- [ ] 数据加密/解密函数
  ```pascal
  function EncryptMessage(
    phContext: PCtxtHandle;
    fQOP: ULONG;
    pMessage: PSecBufferDesc;
    MessageSeqNo: ULONG
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  
  function DecryptMessage(
    phContext: PCtxtHandle;
    pMessage: PSecBufferDesc;
    MessageSeqNo: ULONG;
    pfQOP: PULONG
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  ```

- [ ] 上下文查询函数
  ```pascal
  function QueryContextAttributes(
    phContext: PCtxtHandle;
    ulAttribute: ULONG;
    pBuffer: Pointer
  ): SECURITY_STATUS; stdcall; external 'secur32.dll';
  ```

**预计时间**: 8 小时

#### 2.1.3 绑定证书相关函数

**文件**: `src/fafafa.ssl.winssl.cert.pas`

- [ ] 证书存储函数
  ```pascal
  function CertOpenStore(
    lpszStoreProvider: LPCSTR;
    dwEncodingType: DWORD;
    hCryptProv: HCRYPTPROV;
    dwFlags: DWORD;
    pvPara: Pointer
  ): HCERTSTORE; stdcall; external 'crypt32.dll';
  
  function CertCloseStore(
    hCertStore: HCERTSTORE;
    dwFlags: DWORD
  ): BOOL; stdcall; external 'crypt32.dll';
  ```

- [ ] 证书查找函数
  ```pascal
  function CertFindCertificateInStore(
    hCertStore: HCERTSTORE;
    dwCertEncodingType: DWORD;
    dwFindFlags: DWORD;
    dwFindType: DWORD;
    pvFindPara: Pointer;
    pPrevCertContext: PCCERT_CONTEXT
  ): PCCERT_CONTEXT; stdcall; external 'crypt32.dll';
  ```

- [ ] 证书验证函数
  ```pascal
  function CertGetCertificateChain(
    hChainEngine: HCERTCHAINENGINE;
    pCertContext: PCCERT_CONTEXT;
    pTime: PFileTime;
    hAdditionalStore: HCERTSTORE;
    pChainPara: PCERT_CHAIN_PARA;
    dwFlags: DWORD;
    pvReserved: Pointer;
    ppChainContext: PPCCERT_CHAIN_CONTEXT
  ): BOOL; stdcall; external 'crypt32.dll';
  ```

**预计时间**: 5 小时

#### 2.1.4 创建辅助工具函数

**文件**: `src/fafafa.ssl.winssl.utils.pas`

- [ ] 错误码转换
  ```pascal
  function MapSchannelError(dwError: DWORD): TSSLErrorCode;
  function GetSchannelErrorString(dwError: DWORD): string;
  ```

- [ ] 协议版本映射
  ```pascal
  function ProtocolVersionsToSchannelFlags(
    aVersions: TSSLProtocolVersions
  ): DWORD;
  ```

- [ ] 缓冲区管理
  ```pascal
  function AllocSecBuffer(aSize: ULONG; aType: ULONG): PSecBuffer;
  procedure FreeSecBuffer(pBuffer: PSecBuffer);
  ```

**预计时间**: 2 小时

---

## 🏗️ Phase 2.2: 接口实现

**目标**: 实现 fafafa.ssl 的统一接口

**工作量**: 约 15 小时

### 任务清单

#### 2.2.1 实现 ISSLLibrary 接口

**文件**: `src/fafafa.ssl.winssl.pas`

- [ ] TWinSSLLibrary 类
  ```pascal
  type
    TWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
    private
      FInitialized: Boolean;
      FDefaultConfig: TSSLConfig;
    public
      function Initialize: Boolean;
      procedure Finalize;
      function IsInitialized: Boolean;
      
      function GetLibraryType: TSSLLibraryType;
      function GetVersionString: string;
      
      function CreateContext(aType: TSSLContextType): ISSLContext;
      // ... 其他接口方法
    end;
  ```

- [ ] 初始化逻辑
  - 检测 Windows 版本
  - 检测 TLS 支持
  - 加载必要的 DLL

**预计时间**: 3 小时

#### 2.2.2 实现 ISSLContext 接口

**文件**: `src/fafafa.ssl.winssl.context.pas`

- [ ] TWinSSLContext 类
  ```pascal
  type
    TWinSSLContext = class(TInterfacedObject, ISSLContext)
    private
      FContextType: TSSLContextType;
      FCredHandle: CredHandle;
      FServerName: string;
      FProtocolVersions: TSSLProtocolVersions;
      FCertContext: PCCERT_CONTEXT;
      FVerifyMode: TSSLVerifyModes;
    public
      constructor Create(aType: TSSLContextType);
      destructor Destroy; override;
      
      procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
      procedure LoadCertificate(const aFileName: string); overload;
      procedure LoadCertificateFromStore(const aStore, aSubject: string);
      
      function CreateConnection(aSocket: THandle): ISSLConnection;
      // ... 其他接口方法
    end;
  ```

- [ ] 凭据管理
  - AcquireCredentialsHandle 调用
  - SCHANNEL_CRED 配置
  - 证书加载

**预计时间**: 5 小时

#### 2.2.3 实现 ISSLConnection 接口

**文件**: `src/fafafa.ssl.winssl.connection.pas`

- [ ] TWinSSLConnection 类
  ```pascal
  type
    TWinSSLConnection = class(TInterfacedObject, ISSLConnection)
    private
      FContext: TWinSSLContext;
      FCtxtHandle: CtxtHandle;
      FSocket: THandle;
      FHandshakeState: TSSLHandshakeState;
      FReadBuffer: TBytes;
      FWriteBuffer: TBytes;
    public
      constructor Create(aContext: TWinSSLContext; aSocket: THandle);
      destructor Destroy; override;
      
      function Connect: Boolean;
      function Accept: Boolean;
      function DoHandshake: TSSLHandshakeState;
      
      function Read(var aBuffer; aCount: Integer): Integer;
      function Write(const aBuffer; aCount: Integer): Integer;
      
      function Shutdown: Boolean;
      // ... 其他接口方法
    end;
  ```

- [ ] 握手流程实现
  - InitializeSecurityContext 调用序列
  - 握手数据缓冲管理
  - 状态机实现

- [ ] 数据传输实现
  - EncryptMessage 封装
  - DecryptMessage 封装
  - 缓冲区管理

**预计时间**: 7 小时

---

## 🔐 Phase 2.3: 证书集成

**目标**: 集成 Windows 证书存储

**工作量**: 约 10 小时

### 任务清单

#### 2.3.1 实现 ISSLCertificate 接口

**文件**: `src/fafafa.ssl.winssl.certificate.pas`

- [ ] TWinSSLCertificate 类
  ```pascal
  type
    TWinSSLCertificate = class(TInterfacedObject, ISSLCertificate)
    private
      FCertContext: PCCERT_CONTEXT;
    public
      function GetSubject: string;
      function GetIssuer: string;
      function GetSerialNumber: string;
      function GetNotBefore: TDateTime;
      function GetNotAfter: TDateTime;
      function IsValid: Boolean;
      // ... 其他接口方法
    end;
  ```

**预计时间**: 3 小时

#### 2.3.2 实现 ISSLCertificateStore 接口

**文件**: `src/fafafa.ssl.winssl.certstore.pas`

- [ ] TWinSSLCertificateStore 类
  ```pascal
  type
    TWinSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
    private
      FStoreHandle: HCERTSTORE;
      FStoreName: string;
    public
      constructor Create(const aStoreName: string);
      destructor Destroy; override;
      
      function FindCertificate(const aSubject: string): ISSLCertificate;
      function FindCertificates(const aFilter: string): TSSLCertificateArray;
      // ... 其他接口方法
    end;
  ```

- [ ] 支持的存储
  - MY (个人证书)
  - ROOT (受信任根证书)
  - CA (中间证书)
  - TRUST (企业信任)

**预计时间**: 4 小时

#### 2.3.3 证书验证集成

- [ ] 自定义验证回调
- [ ] 证书链验证
- [ ] 撤销检查集成 (OCSP/CRL)

**预计时间**: 3 小时

---

## 🧪 Phase 2.4: 测试和验证

**目标**: 确保 WinSSL 后端稳定可靠

**工作量**: 约 10 小时

### 任务清单

#### 2.4.1 单元测试

**文件**: `tests/unit/test_winssl_unit.pas`

- [ ] 库初始化测试
- [ ] 上下文创建测试
- [ ] 凭据管理测试
- [ ] 错误处理测试

**预计时间**: 3 小时

#### 2.4.2 集成测试

**文件**: `tests/integration/test_winssl_integration.pas`

- [ ] TLS 握手测试（连接 google.com, github.com）
- [ ] 数据传输测试
- [ ] 证书验证测试
- [ ] 协议版本测试 (TLS 1.2, 1.3)

**预计时间**: 4 小时

#### 2.4.3 行为一致性测试

**文件**: `tests/integration/test_backend_consistency.pas`

- [ ] 与 OpenSSL 后端对比测试
- [ ] 相同输入产生相同输出
- [ ] 错误处理一致性

**预计时间**: 2 小时

#### 2.4.4 性能基准测试

**文件**: `tests/performance/test_winssl_vs_openssl.pas`

- [ ] 握手性能对比
- [ ] 数据传输性能对比
- [ ] 内存使用对比

**预计时间**: 1 小时

---

## 📝 Phase 2.5: 文档和示例

**目标**: 完善文档，提供实用示例

**工作量**: 约 5 小时

### 任务清单

#### 2.5.1 WinSSL 使用指南

**文件**: `docs/WINSSL_USER_GUIDE.md`

- [ ] WinSSL 简介
- [ ] 快速开始
- [ ] API 参考
- [ ] 常见问题

**预计时间**: 2 小时

#### 2.5.2 零依赖部署指南

**文件**: `docs/ZERO_DEPENDENCY_DEPLOYMENT.md`

- [ ] Windows 部署优势
- [ ] 部署步骤
- [ ] 疑难解答
- [ ] 最佳实践

**预计时间**: 1 小时

#### 2.5.3 企业场景示例

**文件**: `examples/enterprise_winssl/`

- [ ] 使用企业证书示例
- [ ] FIPS 模式示例
- [ ] 组策略集成示例

**预计时间**: 2 小时

---

## ✅ 验收标准

### 功能完整性
- [ ] 实现所有 ISSLLibrary 接口方法
- [ ] 实现所有 ISSLContext 接口方法
- [ ] 实现所有 ISSLConnection 接口方法
- [ ] 支持 TLS 1.2 和 TLS 1.3
- [ ] 支持 Windows 证书存储访问

### 质量标准
- [ ] 所有单元测试通过
- [ ] 所有集成测试通过
- [ ] 无内存泄漏
- [ ] 错误处理完善
- [ ] 代码覆盖率 > 80%

### 文档完整性
- [ ] API 文档完整
- [ ] 使用指南清晰
- [ ] 示例代码可运行
- [ ] 已知问题记录

### 性能标准
- [ ] 握手性能与 OpenSSL 相当
- [ ] 数据传输性能与 OpenSSL 相当或更好
- [ ] 内存使用合理

---

## 📊 进度追踪

### 当前状态
- [ ] Phase 2.1: 核心 API 绑定 (0%)
- [ ] Phase 2.2: 接口实现 (0%)
- [ ] Phase 2.3: 证书集成 (0%)
- [ ] Phase 2.4: 测试和验证 (0%)
- [ ] Phase 2.5: 文档和示例 (0%)

### 里程碑
- [ ] M1: API 绑定完成 (预计: 第 1 周)
- [ ] M2: 基础功能实现 (预计: 第 2 周)
- [ ] M3: 测试通过 (预计: 第 3 周)
- [ ] M4: 文档完善 (预计: 第 3 周)
- [ ] M5: Phase 2 完成！🎉

---

## 🚀 开始第一步

### 立即可做：

1. **创建 WinSSL 类型定义文件**
   ```bash
   # 创建文件
   touch src/fafafa.ssl.winssl.types.pas
   
   # 开始定义基础类型
   # 参考: WINSSL_DESIGN.md
   ```

2. **设置开发环境**
   - 确保 Windows SDK 可用
   - 确保可以链接 secur32.dll 和 crypt32.dll
   - 准备测试证书

3. **创建第一个简单测试**
   ```pascal
   // tests/test_winssl_hello.pas
   // 验证可以调用 Schannel API
   ```

### 下一步行动

完成本文档阅读后：

1. ✅ 阅读 `PROJECT_VISION.md` 确保理解整体架构
2. ✅ 阅读 `docs/WINSSL_DESIGN.md` 了解设计细节
3. 🔄 开始 Phase 2.1.1：创建类型定义文件
4. 🔄 实现第一个 Hello World 级别的 Schannel 调用

---

**让我们开始吧！这将是 fafafa.ssl 项目最激动人心的里程碑！** 🎯

**文档维护**: fafafa.ssl 开发团队  
**最后更新**: 2025-10-04
