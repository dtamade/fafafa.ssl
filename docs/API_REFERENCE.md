# fafafa.ssl API 参考文档

> **版本**: v0.8  
> **最后更新**: 2025-10-24

## 目录

- [核心接口](#核心接口)
- [OpenSSL 后端](#openssl-后端)
- [WinSSL 后端](#winssl-后端)
- [数据类型](#数据类型)
- [错误处理](#错误处理)
- [工具函数](#工具函数)

---

## 核心接口

### ISSLLibrary

SSL/TLS 库的主接口，提供库管理和实例创建功能。

```pascal
ISSLLibrary = interface
  // 初始化与配置
  function Initialize: Boolean;
  procedure Finalize;
  function IsInitialized: Boolean;
  
  // 库信息
  function GetLibraryType: TSSLLibraryType;
  function GetVersionString: string;
  function GetVersionNumber: Cardinal;
  
  // 功能检测
  function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
  function IsCipherSupported(const aCipherName: string): Boolean;
  function IsFeatureSupported(const aFeatureName: string): Boolean;
  
  // 错误处理
  function GetLastError: Integer;
  function GetLastErrorString: string;
  procedure ClearError;
  
  // 日志
  procedure SetLogCallback(aCallback: TSSLLogCallback);
  procedure Log(aLevel: TSSLLogLevel; const aMessage: string);
  
  // 工厂方法
  function CreateContext(aType: TSSLContextType): ISSLContext;
  function CreateCertificate: ISSLCertificate;
  function CreateCertificateStore: ISSLCertificateStore;
end;
```

**使用示例**:
```pascal
var
  LLib: ISSLLibrary;
begin
  LLib := CreateOpenSSLLibrary;
  if LLib.Initialize then
  begin
    WriteLn('版本: ', LLib.GetVersionString);
    // 使用库...
    LLib.Finalize;
  end;
end;
```

---

### ISSLContext

SSL/TLS 上下文接口，管理连接配置。

```pascal
ISSLContext = interface
  // 上下文类型
  function GetContextType: TSSLContextType;
  
  // 协议版本
  procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
  function GetProtocolVersions: TSSLProtocolVersions;
  
  // 证书与密钥
  procedure LoadCertificate(const aFileName: string); overload;
  procedure LoadCertificate(aStream: TStream); overload;
  procedure LoadCertificate(aCert: ISSLCertificate); overload;
  procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
  procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
  
  // CA 证书
  procedure LoadCAFile(const aFileName: string);
  procedure LoadCAPath(const aPath: string);
  procedure SetCertificateStore(aStore: ISSLCertificateStore);
  
  // 验证配置
  procedure SetVerifyMode(aMode: TSSLVerifyModes);
  function GetVerifyMode: TSSLVerifyModes;
  procedure SetVerifyDepth(aDepth: Integer);
  function GetVerifyDepth: Integer;
  procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);
  
  // 密码套件
  procedure SetCipherList(const aCipherList: string);
  function GetCipherList: string;
  procedure SetCipherSuites(const aCipherSuites: string);
  function GetCipherSuites: string;
  
  // 会话管理
  procedure SetSessionCacheMode(aEnabled: Boolean);
  function GetSessionCacheMode: Boolean;
  procedure SetSessionTimeout(aTimeout: Integer);
  function GetSessionTimeout: Integer;
  
  // 连接创建
  function CreateConnection(aSocket: THandle): ISSLConnection; overload;
  function CreateConnection(aStream: TStream): ISSLConnection; overload;
  
  // 状态
  function IsValid: Boolean;
  function GetNativeHandle: Pointer;
end;
```

**使用示例**:
```pascal
var
  LContext: ISSLContext;
begin
  LContext := LLib.CreateContext(sslCtxClient);
  LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
  LContext.LoadCAFile('ca-bundle.crt');
  LContext.SetVerifyMode([sslVerifyPeer]);
  // 创建连接...
end;
```

---

### ISSLCertificate

X.509 证书接口。

```pascal
ISSLCertificate = interface
  // 加载与保存
  function LoadFromFile(const aFileName: string): Boolean;
  function LoadFromStream(aStream: TStream): Boolean;
  function LoadFromPEM(const aPEM: string): Boolean;
  function LoadFromDER(const aDER: TBytes): Boolean;
  function SaveToFile(const aFileName: string): Boolean;
  function SaveToPEM: string;
  function SaveToDER: TBytes;
  
  // 证书信息
  function GetSubject: string;
  function GetIssuer: string;
  function GetSerialNumber: string;
  function GetNotBefore: TDateTime;
  function GetNotAfter: TDateTime;
  function GetPublicKey: string;
  function GetVersion: Integer;
  
  // 验证
  function Verify(aCAStore: ISSLCertificateStore): Boolean;
  function VerifyEx(aCAStore: ISSLCertificateStore; 
    aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
  function VerifyHostname(const aHostname: string): Boolean;
  function IsExpired: Boolean;
  function IsSelfSigned: Boolean;
  function IsCA: Boolean;
  
  // 扩展
  function GetSubjectAltNames: TStringList;
  function GetKeyUsage: TStringList;
  function GetExtendedKeyUsage: TStringList;
  
  // 指纹
  function GetFingerprintSHA1: string;
  function GetFingerprintSHA256: string;
end;
```

**使用示例**:
```pascal
var
  LCert: ISSLCertificate;
  LResult: TSSLCertVerifyResult;
begin
  LCert := LLib.CreateCertificate;
  if LCert.LoadFromFile('mycert.pem') then
  begin
    WriteLn('主题: ', LCert.GetSubject);
    WriteLn('有效期至: ', DateTimeToStr(LCert.GetNotAfter));
    
    // 增强验证
    if LCert.VerifyEx(LStore, [sslCertVerifyCheckRevocation], LResult) then
      WriteLn('验证成功')
    else
      WriteLn('验证失败: ', LResult.ErrorMessage);
  end;
end;
```

---

### ISSLConnection

SSL/TLS 连接接口。

```pascal
ISSLConnection = interface
  // 连接管理
  function Connect: Boolean;
  function Accept: Boolean;
  procedure Shutdown;
  procedure Close;
  
  // 数据传输
  function Read(var aBuffer; aCount: Integer): Integer;
  function Write(const aBuffer; aCount: Integer): Integer;
  function ReadString: string;
  function WriteString(const aData: string): Integer;
  
  // 状态查询
  function IsConnected: Boolean;
  function GetState: TSSLConnectionState;
  function GetProtocolVersion: TSSLProtocolVersion;
  function GetCipherName: string;
  function GetCipherBits: Integer;
  
  // 证书信息
  function GetPeerCertificate: ISSLCertificate;
  function GetPeerCertificateChain: TSSLCertificateArray;
  function VerifyPeerCertificate: Boolean;
  
  // 会话信息
  function GetSessionID: string;
  function IsSessionResumed: Boolean;
end;
```

**使用示例**:
```pascal
var
  LConn: ISSLConnection;
  LData: string;
begin
  LConn := LContext.CreateConnection(MySocket);
  if LConn.Connect then
  begin
    LConn.WriteString('GET / HTTP/1.1'#13#10#13#10);
    LData := LConn.ReadString;
    WriteLn('响应: ', LData);
    LConn.Shutdown;
  end;
end;
```

---

## 数据类型

### TSSLLibraryType
```pascal
TSSLLibraryType = (
  sslOpenSSL,  // OpenSSL 后端
  sslWinSSL,   // Windows Schannel 后端
  sslMbedTLS   // MbedTLS 后端（计划中）
);
```

### TSSLProtocolVersion
```pascal
TSSLProtocolVersion = (
  sslProtocolSSL20,   // SSL 2.0 (已废弃)
  sslProtocolSSL30,   // SSL 3.0 (已废弃)
  sslProtocolTLS10,   // TLS 1.0
  sslProtocolTLS11,   // TLS 1.1
  sslProtocolTLS12,   // TLS 1.2
  sslProtocolTLS13    // TLS 1.3
);
TSSLProtocolVersions = set of TSSLProtocolVersion;
```

### TSSLContextType
```pascal
TSSLContextType = (
  sslCtxClient,  // 客户端上下文
  sslCtxServer   // 服务端上下文
);
```

### TSSLVerifyMode
```pascal
TSSLVerifyMode = (
  sslVerifyNone,       // 不验证
  sslVerifyPeer,       // 验证对等方
  sslVerifyFailIfNoPeerCert,  // 无证书时失败
  sslVerifyClientOnce  // 仅验证一次客户端
);
TSSLVerifyModes = set of TSSLVerifyMode;
```

### TSSLCertVerifyFlag
```pascal
TSSLCertVerifyFlag = (
  sslCertVerifyDefault,         // 默认验证
  sslCertVerifyCheckRevocation, // 检查吊销（CRL）
  sslCertVerifyCheckOCSP,       // 使用 OCSP
  sslCertVerifyIgnoreExpiry,    // 忽略过期
  sslCertVerifyIgnoreHostname,  // 忽略主机名
  sslCertVerifyAllowSelfSigned, // 允许自签名
  sslCertVerifyStrictChain,     // 严格证书链
  sslCertVerifyCheckCRL         // 检查 CRL 列表
);
TSSLCertVerifyFlags = set of TSSLCertVerifyFlag;
```

### TSSLCertVerifyResult
```pascal
TSSLCertVerifyResult = record
  Success: Boolean;         // 验证是否成功
  ErrorCode: Cardinal;      // 错误代码
  ErrorMessage: string;     // 友好的错误消息
  ChainStatus: Cardinal;    // 证书链状态
  RevocationStatus: Cardinal; // 吊销状态
  DetailedInfo: string;     // 详细信息
end;
```

---

## 错误处理

### TSSLErrorCode
```pascal
TSSLErrorCode = (
  sslErrNone,              // 无错误
  sslErrGeneral,           // 一般错误
  sslErrNotInitialized,    // 未初始化
  sslErrInvalidParameter,  // 无效参数
  sslErrOutOfMemory,       // 内存不足
  sslErrTimeout,           // 超时
  sslErrConnectionClosed,  // 连接关闭
  sslErrHandshakeFailed,   // 握手失败
  sslErrCertificateVerifyFailed, // 证书验证失败
  sslErrCipherNotSupported,      // 不支持的密码
  sslErrProtocolNotSupported     // 不支持的协议
);
```

### 错误处理函数
```pascal
// OpenSSL
function GetOpenSSLError: Cardinal;
function GetOpenSSLErrorString(aError: Cardinal = 0): string;
procedure ClearOpenSSLErrors;
function ClassifyOpenSSLError(aError: Cardinal): TSSLErrorCode;
function GetFriendlyErrorMessage(aError: Cardinal): string;

// WinSSL
function GetWinSSLErrorMessageCN(aErrorCode: DWORD): string;
function GetWinSSLErrorMessageEN(aErrorCode: DWORD): string;
```

---

## 工具函数

### OpenSSL 工具
```pascal
// 库管理
function OpenSSLAvailable: Boolean;
function LoadOpenSSL(const aLibraryPath: string = ''): Boolean;
procedure UnloadOpenSSL;
function GetOpenSSLVersion: string;
function GetOpenSSLVersionNumber: Cardinal;

// 证书工具
function LoadCertificateFromFile(const aFileName: string): PX509;
function LoadPrivateKeyFromFile(const aFileName: string; const aPassword: string = ''): PEVP_PKEY;
function VerifyCertificate(aCert: PX509; aCAStore: PX509_STORE): Boolean;

// 协议工具
function ProtocolToOpenSSL(aProtocol: TSSLProtocolVersion): Integer;
function GetProtocolName(aProtocol: TSSLProtocolVersion): string;
```

### WinSSL 工具
```pascal
// 企业功能
function IsFipsModeEnabled: Boolean;
function GetEnterpriseTrustedRoots: TStringList;
function GetGroupPolicies: TStringList;
```

---

## 工厂函数

### 创建后端实例
```pascal
// OpenSSL
function CreateOpenSSLLibrary: ISSLLibrary;

// WinSSL
function CreateWinSSLLibrary: ISSLLibrary;

// 自动选择
function CreateSSLLibrary(aType: TSSLLibraryType = sslOpenSSL): ISSLLibrary;
```

---

## 回调类型

```pascal
// 日志回调
TSSLLogLevel = (sslLogDebug, sslLogInfo, sslLogWarning, sslLogError);
TSSLLogCallback = procedure(aLevel: TSSLLogLevel; const aMessage: string) of object;

// 验证回调
TSSLVerifyCallback = function(aPreverified: Boolean; aCert: ISSLCertificate): Boolean of object;

// 密码回调
TSSLPasswordCallback = function(const aHint: string; aMaxLen: Integer): string of object;

// 信息回调
TSSLInfoCallback = procedure(const aInfo: string) of object;
```

---

## 常量

### OpenSSL 常量
```pascal
// 验证标志
X509_V_FLAG_CRL_CHECK = $00000004;
X509_V_FLAG_CRL_CHECK_ALL = $00000008;
X509_V_FLAG_NO_CHECK_TIME = $00000200;
X509_V_FLAG_PARTIAL_CHAIN = $00080000;

// SSL 选项
SSL_OP_NO_SSLv2 = $01000000;
SSL_OP_NO_SSLv3 = $02000000;
SSL_OP_NO_TLSv1 = $04000000;
SSL_OP_NO_TLSv1_1 = $10000000;
SSL_OP_NO_TLSv1_2 = $08000000;
SSL_OP_NO_TLSv1_3 = $20000000;
```

### WinSSL 常量
```pascal
// 证书错误
CERT_E_EXPIRED = LONG($800B0101);
CERT_E_UNTRUSTEDROOT = LONG($800B0109);
CERT_E_CN_NO_MATCH = LONG($800B010F);
CERT_E_REVOKED = LONG($800B010C);

// 吊销检查
CERT_CHAIN_REVOCATION_CHECK_END_CERT = $10000000;
CERT_CHAIN_REVOCATION_CHECK_CHAIN = $20000000;
```

---

## 使用示例

### 完整客户端示例
```pascal
program ssl_client;

uses
  fafafa.ssl.openssl,
  fafafa.ssl.abstract.intf;

var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LConn: ISSLConnection;
  LCert: ISSLCertificate;
begin
  // 创建并初始化库
  LLib := CreateOpenSSLLibrary;
  if not LLib.Initialize then
  begin
    WriteLn('初始化失败');
    Exit;
  end;
  
  try
    // 创建客户端上下文
    LContext := LLib.CreateContext(sslCtxClient);
    LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
    LContext.LoadCAFile('/etc/ssl/certs/ca-bundle.crt');
    LContext.SetVerifyMode([sslVerifyPeer]);
    
    // 创建连接
    LConn := LContext.CreateConnection(MySocket);
    if LConn.Connect then
    begin
      // 验证证书
      LCert := LConn.GetPeerCertificate;
      if LCert.VerifyHostname('example.com') then
      begin
        // 发送和接收数据
        LConn.WriteString('Hello, SSL!');
        WriteLn('收到: ', LConn.ReadString);
      end;
      
      LConn.Shutdown;
    end;
  finally
    LLib.Finalize;
  end;
end.
```

---

## 参考资源

- **OpenSSL 文档**: https://www.openssl.org/docs/
- **Windows Schannel**: https://docs.microsoft.com/en-us/windows/win32/secauthn/schannel
- **RFC 5280** (X.509): https://tools.ietf.org/html/rfc5280
- **RFC 8446** (TLS 1.3): https://tools.ietf.org/html/rfc8446

---

**版本历史**:
- v0.8 (2025-10-24): 添加 VerifyEx 方法和 WinSSL 企业功能
- v0.7 (2025-10-01): 初始 API 文档

