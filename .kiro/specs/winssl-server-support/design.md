# 设计文档: WinSSL 服务端支持

## 概述

本设计文档描述了 fafafa.ssl 项目中 WinSSL 后端的服务端 TLS 握手和连接管理功能的实现方案。WinSSL 后端基于 Windows Schannel API,目前客户端功能已完成 95%,本设计将完成剩余的服务端功能(当前完成度 10%),使其能够在 Windows 平台上实现零依赖的 HTTPS 服务器应用。

### 设计目标

1. **完整的服务端 TLS 握手**: 实现 TLS 1.2 和 TLS 1.3 的服务端握手流程
2. **客户端证书验证**: 支持可选和必需的双向 TLS 认证
3. **会话管理**: 实现 TLS 会话复用机制以提高性能
4. **接口一致性**: 与现有的 OpenSSL 后端保持接口一致,实现透明切换
5. **Windows 原生集成**: 充分利用 Windows 证书存储和 Schannel API

### 技术栈

- **语言**: Pascal (Free Pascal/Delphi)
- **平台**: Windows 10+, Windows Server 2016+
- **核心 API**: Windows Schannel (SSPI), Crypt32
- **TLS 版本**: TLS 1.2, TLS 1.3
- **证书格式**: PFX/P12, Windows Certificate Store

## 架构设计

### 整体架构

WinSSL 后端采用分层架构,与现有的客户端实现保持一致:

```
┌─────────────────────────────────────────────────────────┐
│              应用层 (HTTPS Server)                       │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│         统一接口层 (ISSLContext, ISSLConnection)         │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│              WinSSL 后端实现层                           │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │ TWinSSLContext│  │TWinSSLConnection│ │TWinSSLSession│  │
│  │  (服务端上下文)│  │  (服务端连接)  │  │  (会话管理)  │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│           Windows Schannel API 层                        │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐  │
│  │AcceptSecurity│  │EncryptMessage│  │CertGetChain  │  │
│  │   Context    │  │DecryptMessage│  │CertVerify... │  │
│  └──────────────┘  └──────────────┘  └──────────────┘  │
└─────────────────────────────────────────────────────────┘
```

### 核心组件

#### 1. TWinSSLContext (服务端上下文)

**职责**:
- 管理服务器证书和私钥
- 配置 TLS 协议版本和密码套件
- 配置客户端证书验证模式
- 创建服务端连接实例

**关键字段**:
```pascal
FContextType: TSSLContextType;        // sslCtxServer
FCredHandle: CredHandle;              // Schannel 凭据句柄
FCertContext: PCCERT_CONTEXT;         // 服务器证书上下文
FVerifyMode: TSSLVerifyModes;         // 客户端证书验证模式
FProtocolVersions: TSSLProtocolVersions; // 支持的 TLS 版本
```

**新增方法**:
- 无需新增公共方法,使用现有的 ISSLContext 接口
- 内部增强 `EnsureCredentialsAcquired` 以支持服务端凭据获取

#### 2. TWinSSLConnection (服务端连接)

**职责**:
- 执行服务端 TLS 握手
- 验证客户端证书(如果需要)
- 加密/解密应用数据
- 管理连接会话

**关键字段**:
```pascal
FCtxtHandle: CtxtHandle;              // Schannel 安全上下文
FHandshakeState: TSSLHandshakeState;  // 握手状态
FRecvBuffer: array of Byte;           // 接收缓冲区
FDecryptedBuffer: array of Byte;      // 解密数据缓冲区
```

**核心方法**:
- `Accept: Boolean` - 执行服务端握手
- `ServerHandshake: Boolean` - 服务端握手实现
- `ValidatePeerCertificate: Boolean` - 验证客户端证书


#### 3. TWinSSLSession (会话管理)

**职责**:
- 存储会话元数据
- 支持会话复用
- 管理会话生命周期

**关键字段**:
```pascal
FID: string;                          // 会话唯一标识
FCreationTime: TDateTime;             // 创建时间
FTimeout: Integer;                    // 超时时间(秒)
FProtocolVersion: TSSLProtocolVersion; // 协议版本
FCipherName: string;                  // 密码套件名称
```

### 模块依赖关系

```
fafafa.ssl.winssl.context
    ├── fafafa.ssl.base (接口定义)
    ├── fafafa.ssl.winssl.api (Schannel API)
    ├── fafafa.ssl.winssl.utils (辅助函数)
    └── fafafa.ssl.winssl.connection (创建连接)

fafafa.ssl.winssl.connection
    ├── fafafa.ssl.base (接口定义)
    ├── fafafa.ssl.winssl.api (Schannel API)
    ├── fafafa.ssl.winssl.certificate (证书处理)
    └── fafafa.ssl.winssl.context (获取上下文信息)

fafafa.ssl.winssl.certificate
    ├── fafafa.ssl.base (接口定义)
    └── fafafa.ssl.winssl.api (证书 API)
```

## 组件和接口

### 服务端上下文接口

TWinSSLContext 实现 ISSLContext 接口,无需新增公共方法。关键是内部实现的增强:

```pascal
type
  TWinSSLContext = class(TInterfacedObject, ISSLContext)
  private
    // 现有字段保持不变
    FContextType: TSSLContextType;
    FCredHandle: CredHandle;
    FCertContext: PCCERT_CONTEXT;
    
    // 内部方法增强
    procedure EnsureCredentialsAcquired;  // 支持服务端凭据
    
  public
    // 实现 ISSLContext 接口
    function CreateConnection(ASocket: THandle): ISSLConnection; overload;
    function CreateConnection(AStream: TStream): ISSLConnection; overload;
    // ... 其他接口方法
  end;
```

**关键实现点**:

1. **凭据获取** (`EnsureCredentialsAcquired`):
   - 客户端: `dwDirection := SECPKG_CRED_OUTBOUND`
   - 服务端: `dwDirection := SECPKG_CRED_INBOUND`
   - 服务端必须包含证书: `SchannelCred.cCreds := 1; SchannelCred.paCred := @FCertContext`

2. **验证模式配置**:
   - 不验证: 不设置 `ASC_REQ_MUTUAL_AUTH` 标志
   - 可选验证: 设置 `ASC_REQ_MUTUAL_AUTH`,但允许无证书连接
   - 必需验证: 设置 `ASC_REQ_MUTUAL_AUTH`,拒绝无证书连接

