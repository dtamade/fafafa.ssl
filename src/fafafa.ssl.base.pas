{
  fafafa.ssl.base - SSL/TLS 基础定义（类型+接口）
  
  版本: 2.0
  作者: fafafa.ssl 开发团队
  创建: 2025-11-05
  
  描述:
    定义 fafafa.ssl 库的所有基础类型、常量、枚举、异常类和接口。
    按照 fafafa.模块名.base.pas 命名规范，此文件包含：
    - 所有类型定义（从 abstract.types 迁移）
    - 所有接口定义（从 abstract.intf 迁移）
    
    这些定义完全独立于任何特定的 SSL/TLS 后端实现（OpenSSL, WinSSL 等）。
    所有后端实现必须使用这些类型和接口以保证 API 的一致性。
    
  架构位置:
    抽象层 - 不依赖于任何具体后端实现
}

unit fafafa.ssl.base;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes;

type
  // ============================================================================
  // 基础类型定义
  // ============================================================================
  
  { 通用过程类型 }
  TSSLProc = procedure of object;
  TSSLProcString = procedure(const aValue: string) of object;
  
  // ============================================================================
  // 枚举类型定义
  // ============================================================================
  
  { SSL/TLS 库后端类型 }
  TSSLLibraryType = (
    sslAutoDetect,   // 自动检测可用库
    sslOpenSSL,      // OpenSSL
    sslWolfSSL,      // WolfSSL  
    sslMbedTLS,      // MbedTLS
    sslWinSSL        // Windows Schannel (仅Windows)
  );
  TSSLLibraryTypes = set of TSSLLibraryType;

  { SSL/TLS 协议版本 }
  TSSLProtocolVersion = (
    sslProtocolUnknown,   // 未知/未指定协议版本
    sslProtocolSSL2,      // SSL 2.0 (已废弃，不推荐)
    sslProtocolSSL3,      // SSL 3.0 (已废弃，不推荐)
    sslProtocolTLS10,     // TLS 1.0
    sslProtocolTLS11,     // TLS 1.1
    sslProtocolTLS12,     // TLS 1.2
    sslProtocolTLS13,     // TLS 1.3
    sslProtocolDTLS10,    // DTLS 1.0 (基于TLS 1.1)
    sslProtocolDTLS12     // DTLS 1.2 (基于TLS 1.2)
  );
  TSSLProtocolVersions = set of TSSLProtocolVersion;

  { 证书验证模式 }
  TSSLVerifyMode = (
    sslVerifyNone,            // 不验证证书
    sslVerifyPeer,            // 验证对端证书
    sslVerifyFailIfNoPeerCert,// 如果没有对端证书则失败
    sslVerifyClientOnce,      // 仅在初次握手时验证客户端（服务端选项）
    sslVerifyPostHandshake    // 握手后验证（TLS 1.3）
  );
  TSSLVerifyModes = set of TSSLVerifyMode;

  { SSL 上下文类型 }
  TSSLContextType = (
    sslCtxClient,         // 客户端上下文
    sslCtxServer,         // 服务端上下文
    sslCtxBoth            // 同时支持客户端和服务端
  );

  { SSL 选项 }
  TSSLOption = (
    ssoEnableSNI,           // 启用 SNI（服务器名称指示）
    ssoEnableALPN,          // 启用 ALPN（应用层协议协商）
    ssoEnableSessionCache,  // 启用会话缓存
    ssoEnableSessionTickets,// 启用会话票据
    ssoDisableCompression,  // 禁用压缩
    ssoDisableRenegotiation,// 禁用重新协商
    ssoEnableOCSPStapling,  // 启用 OCSP 装订
    ssoSingleDHUse,         // 单次使用 DH 参数
    ssoSingleECDHUse,       // 单次使用 ECDH 参数
    ssoCipherServerPreference,// 服务器密码套件优先
    ssoNoSSLv2,             // 禁用 SSLv2
    ssoNoSSLv3,             // 禁用 SSLv3
    ssoNoTLSv1,             // 禁用 TLSv1.0
    ssoNoTLSv1_1,           // 禁用 TLSv1.1
    ssoNoTLSv1_2,           // 禁用 TLSv1.2
    ssoNoTLSv1_3            // 禁用 TLSv1.3
  );
  TSSLOptions = set of TSSLOption;

  { 证书验证标志 }
  TSSLCertVerifyFlag = (
    sslCertVerifyDefault,         // 默认验证
    sslCertVerifyCheckRevocation, // 检查吊销状态（CRL）
    sslCertVerifyCheckOCSP,       // 使用 OCSP 检查吊销
    sslCertVerifyIgnoreExpiry,    // 忽略过期
    sslCertVerifyIgnoreHostname,  // 忽略主机名验证
    sslCertVerifyAllowSelfSigned, // 允许自签名证书
    sslCertVerifyStrictChain,     // 严格证书链验证
    sslCertVerifyCheckCRL         // 检查 CRL 列表
  );
  TSSLCertVerifyFlags = set of TSSLCertVerifyFlag;

  { SSL 握手状态 }
  TSSLHandshakeState = (
    sslHsNotStarted,      // 未开始
    sslHsInProgress,      // 进行中
    sslHsCompleted,       // 已完成
    sslHsFailed,          // 失败
    sslHsRenegotiating    // 重新协商中
  );

  { SSL 错误代码 }
  TSSLErrorCode = (
    sslErrNone,              // 无错误
    sslErrGeneral,           // 一般错误
    sslErrMemory,            // 内存分配错误
    sslErrInvalidParam,      // 无效参数
    sslErrNotInitialized,    // 未初始化
    sslErrProtocol,          // 协议错误
    sslErrHandshake,         // 握手错误
    sslErrCertificate,       // 证书错误
    sslErrCertificateExpired,// 证书过期
    sslErrCertificateRevoked,// 证书被撤销
    sslErrCertificateUnknown,// 未知证书
    sslErrConnection,        // 连接错误
    sslErrTimeout,           // 超时
    sslErrIO,                // I/O错误
    sslErrWouldBlock,        // 非阻塞操作会阻塞
    sslErrWantRead,          // SSL需要读取
    sslErrWantWrite,         // SSL需要写入
    sslErrUnsupported,       // 不支持的功能
    sslErrLibraryNotFound,   // 库文件未找到
    sslErrFunctionNotFound,  // 函数未找到
    sslErrVersionMismatch,   // 版本不匹配
    sslErrConfiguration,     // 配置错误
    sslErrOther              // 其他错误
  );

  { SSL 日志级别 }
  TSSLLogLevel = (
    sslLogNone,      // 不记录日志
    sslLogError,     // 仅错误
    sslLogWarning,   // 警告和错误
    sslLogInfo,      // 信息、警告和错误
    sslLogDebug,     // 调试信息
    sslLogTrace      // 详细跟踪
  );

  { 密钥交换算法 }
  TSSLKeyExchange = (
    sslKexRSA,
    sslKexDHE_RSA,
    sslKexECDHE_RSA,
    sslKexDHE_DSS,
    sslKexECDHE_ECDSA,
    sslKexPSK,
    sslKexDHE_PSK,
    sslKexRSA_PSK
  );

  { 加密算法 }
  TSSLCipher = (
    sslCipherNone,
    sslCipherAES128,
    sslCipherAES256,
    sslCipherAES128GCM,
    sslCipherAES256GCM,
    sslCipherCHACHA20_POLY1305,
    sslCipher3DES,
    sslCipherDES,
    sslCipherRC4
  );

  { 哈希算法 }
  TSSLHash = (
    sslHashMD5,
    sslHashSHA1,
    sslHashSHA224,
    sslHashSHA256,
    sslHashSHA384,
    sslHashSHA512,
    sslHashSHA3_256,
    sslHashSHA3_512,
    sslHashBLAKE2b
  );

  // ============================================================================
  // 记录类型定义
  // ============================================================================

  { 证书验证结果 }
  TSSLCertVerifyResult = record
    Success: Boolean;               // 验证是否成功
    ErrorCode: Cardinal;            // 错误代码（平台相关）
    ErrorMessage: string;           // 友好的错误消息
    ChainStatus: Cardinal;          // 证书链状态
    RevocationStatus: Cardinal;     // 吊销状态
    DetailedInfo: string;           // 详细信息（可选）
  end;

  { SSL 证书信息 }
  TSSLCertificateInfo = record
    Subject: string;              // 证书主题
    Issuer: string;               // 证书颁发者
    SerialNumber: string;         // 序列号
    NotBefore: TDateTime;         // 有效期开始
    NotAfter: TDateTime;          // 有效期结束
    FingerprintSHA1: string;      // SHA1指纹
    FingerprintSHA256: string;    // SHA256指纹
    PublicKeyAlgorithm: string;   // 公钥算法
    PublicKeySize: Integer;       // 公钥长度（位）
    SignatureAlgorithm: string;   // 签名算法
    Version: Integer;             // 证书版本
    SubjectAltNames: TStringList; // 主题备用名称
    IsCA: Boolean;                // 是否为CA证书
    PathLength: Integer;          // 证书路径长度限制
    PathLenConstraint: Integer;   // 路径长度约束 (-1表示无限制)
    KeyUsage: Word;               // 密钥用途位字段
  end;
  PSSLCertificateInfo = ^TSSLCertificateInfo;

  { SSL 连接信息 }
  TSSLConnectionInfo = record
    ProtocolVersion: TSSLProtocolVersion;  // 协议版本
    CipherSuite: string;                   // 密码套件名称
    CipherSuiteId: Word;                   // 密码套件ID
    KeyExchange: TSSLKeyExchange;          // 密钥交换算法
    Cipher: TSSLCipher;                    // 加密算法
    Hash: TSSLHash;                        // 哈希算法
    KeySize: Integer;                      // 密钥长度（位）
    MacSize: Integer;                      // MAC长度（字节）
    IsResumed: Boolean;                    // 是否为恢复的会话
    SessionId: string;                     // 会话ID
    CompressionMethod: string;             // 压缩方法
    ServerName: string;                    // SNI服务器名称
    ALPNProtocol: string;                  // ALPN协商的协议
    PeerCertificate: TSSLCertificateInfo;  // 对端证书信息
  end;
  PSSLConnectionInfo = ^TSSLConnectionInfo;

  { 日志回调类型 }
  TSSLLogCallback = procedure(aLevel: TSSLLogLevel; const aMessage: string) of object;

  { SSL 配置 }
  TSSLConfig = record
    // 基本配置
    LibraryType: TSSLLibraryType;            // 使用的库类型
    ContextType: TSSLContextType;            // 上下文类型
    
    // 协议配置
    ProtocolVersions: TSSLProtocolVersions;  // 允许的协议版本
    PreferredVersion: TSSLProtocolVersion;   // 首选协议版本
    
    // 证书配置
    CertificateFile: string;                 // 证书文件路径
    PrivateKeyFile: string;                  // 私钥文件路径
    CAFile: string;                          // CA证书文件路径
    CAPath: string;                          // CA证书目录路径
    VerifyMode: TSSLVerifyModes;            // 证书验证模式
    VerifyDepth: Integer;                    // 验证深度
    
    // 密码套件配置
    CipherList: string;                      // 密码套件列表（OpenSSL格式）
    CipherSuites: string;                    // TLS 1.3密码套件
    
    // SSL 选项
    Options: TSSLOptions;                    // SSL 选项集合
    
    // 性能配置
    BufferSize: Integer;                     // 缓冲区大小
    HandshakeTimeout: Integer;               // 握手超时（毫秒）
    SessionCacheSize: Integer;              // 会话缓存大小
    SessionTimeout: Integer;                // 会话超时（秒）
    
    // 高级配置
    ServerName: string;                      // SNI服务器名称
    ALPNProtocols: string;                   // ALPN协议列表（逗号分隔）
    EnableCompression: Boolean;              // 启用压缩
    EnableSessionTickets: Boolean;           // 启用会话票据
    EnableOCSPStapling: Boolean;             // 启用OCSP装订
    
    // 日志配置
    LogLevel: TSSLLogLevel;                  // 日志级别
    LogCallback: TSSLLogCallback;            // 日志回调
  end;
  PSSLConfig = ^TSSLConfig;

  { SSL 统计信息 }
  TSSLStatistics = record
    ConnectionsTotal: Int64;        // 总连接数
    ConnectionsActive: Integer;     // 活动连接数
    HandshakesSuccessful: Int64;    // 成功握手次数
    HandshakesFailed: Int64;        // 失败握手次数
    BytesSent: Int64;               // 发送字节数
    BytesReceived: Int64;           // 接收字节数
    SessionCacheHits: Int64;        // 会话缓存命中次数
    SessionCacheMisses: Int64;      // 会话缓存未命中次数
    RenegotiationsCount: Int64;     // 重新协商次数
    AlertsSent: Int64;              // 发送的警报数
    AlertsReceived: Int64;          // 接收的警报数
  end;
  PSSLStatistics = ^TSSLStatistics;

  // ============================================================================
  // 异常类定义
  // ============================================================================

  { SSL 异常基类 }
  ESSLException = class(Exception)
  private
    FErrorCode: TSSLErrorCode;
    FLibraryType: TSSLLibraryType;
    FNativeError: Integer;
    FNativeErrorMessage: string;
    FContext: string;
  public
    constructor Create(const aMessage: string; aErrorCode: TSSLErrorCode;
                      aLibraryType: TSSLLibraryType = sslAutoDetect;
                      aNativeError: Integer = 0); overload;
    constructor Create(const aMessage: string; aErrorCode: TSSLErrorCode;
                      const aContext: string;
                      aLibraryType: TSSLLibraryType = sslAutoDetect;
                      aNativeError: Integer = 0); overload;
    
    property ErrorCode: TSSLErrorCode read FErrorCode;
    property LibraryType: TSSLLibraryType read FLibraryType;
    property NativeError: Integer read FNativeError;
    property NativeErrorMessage: string read FNativeErrorMessage write FNativeErrorMessage;
    property Context: string read FContext;
  end;

  { 特定异常类 }
  ESSLHandshakeException = class(ESSLException);
  ESSLCertificateException = class(ESSLException);
  ESSLProtocolException = class(ESSLException);
  ESSLConnectionException = class(ESSLException);
  ESSLTimeoutException = class(ESSLException);
  ESSLLibraryException = class(ESSLException);

  // ============================================================================
  // 回调类型定义
  // ============================================================================

  { 证书验证回调 }
  TSSLVerifyCallback = function(const aCertificate: TSSLCertificateInfo;
                                const aErrorCode: Integer;
                                const aErrorMessage: string): Boolean of object;

  { 密码回调 }
  TSSLPasswordCallback = function(var aPassword: string;
                                const aIsRetry: Boolean): Boolean of object;

  { 信息回调 }
  TSSLInfoCallback = procedure(const aWhere: Integer;
                              const aRet: Integer;
                              const aState: string) of object;

  { 数据传输回调 }
  TSSLDataCallback = procedure(const aData: Pointer;
                              const aSize: Integer;
                              const aIsOutgoing: Boolean) of object;

  // ============================================================================
  // 接口定义
  // ============================================================================

  // 前向声明
  ISSLContext = interface;
  ISSLConnection = interface;
  ISSLCertificate = interface;
  ISSLCertificateStore = interface;
  ISSLSession = interface;
  ISSLLibrary = interface;
  
  // 数组类型
  TSSLCertificateArray = array of ISSLCertificate;

  { ISSLLibrary - SSL库管理接口 }
  ISSLLibrary = interface
    ['{A0E8F4B1-7C3A-4D2E-9F5B-8C6D7E9A0B1C}']
    
    // 初始化和清理
    function Initialize: Boolean;
    procedure Finalize;
    function IsInitialized: Boolean;
    
    // 版本信息
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersion: string;  // 便捷方法，等同于 GetVersionString
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    // 功能支持查询
    function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const aCipherName: string): Boolean;
    function IsFeatureSupported(const aFeatureName: string): Boolean;
    
    // 库配置
    procedure SetDefaultConfig(const aConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    
    // 错误处理
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    
    // 统计信息
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    
    // 日志
    procedure SetLogCallback(aCallback: TSSLLogCallback);
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string);
    
    // 工厂方法
    function CreateContext(aType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

  { ISSLContext - SSL上下文接口 }
  ISSLContext = interface
    ['{B1F9E5C2-8D4B-5E3F-A06C-9D8E0F1A2B3D}']
    
    // 基本配置
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    
    // 证书和密钥管理
    procedure LoadCertificate(const aFileName: string); overload;
    procedure LoadCertificate(aStream: TStream); overload;
    procedure LoadCertificate(aCert: ISSLCertificate); overload;
    
    procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
    procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
    
    procedure LoadCAFile(const aFileName: string);
    procedure LoadCAPath(const aPath: string);
    procedure SetCertificateStore(aStore: ISSLCertificateStore);
    
    // 验证配置
    procedure SetVerifyMode(aMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(aDepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);
    
    // 密码套件配置
    procedure SetCipherList(const aCipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const aCipherSuites: string); // TLS 1.3
    function GetCipherSuites: string;
    
    // 会话管理
    procedure SetSessionCacheMode(aEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(aTimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(aSize: Integer);
    function GetSessionCacheSize: Integer;
    
    // 高级选项
    procedure SetOptions(aOptions: Cardinal);
    function GetOptions: Cardinal;
    procedure SetServerName(const aServerName: string); // SNI
    function GetServerName: string;
    procedure SetALPNProtocols(const aProtocols: string);
    function GetALPNProtocols: string;
    
    // 回调设置
    procedure SetPasswordCallback(aCallback: TSSLPasswordCallback);
    procedure SetInfoCallback(aCallback: TSSLInfoCallback);
    
    // 创建连接
    function CreateConnection(aSocket: THandle): ISSLConnection; overload;
    function CreateConnection(aStream: TStream): ISSLConnection; overload;
    
    // 状态查询
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;
  end;

  { ISSLConnection - SSL连接接口 }
  ISSLConnection = interface
    ['{C2A9F6D3-9E5C-6F40-B17D-AE9F102B4C5E}']
    
    // 连接管理
    function Connect: Boolean;              // 客户端连接
    function Accept: Boolean;               // 服务端接受
    function Shutdown: Boolean;             // 优雅关闭
    procedure Close;                        // 强制关闭
    
    // 握手控制
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    
    // 数据传输
    function Read(var aBuffer; aCount: Integer): Integer;
    function Write(const aBuffer; aCount: Integer): Integer;
    function ReadString(out aStr: string): Boolean;
    function WriteString(const aStr: string): Boolean;
    
    // 异步操作支持
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(aRet: Integer): TSSLErrorCode;
    
    // 连接信息
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    
    // 会话管理
    function GetSession: ISSLSession;
    procedure SetSession(aSession: ISSLSession);
    function IsSessionReused: Boolean;
    
    // ALPN/NPN
    function GetSelectedALPNProtocol: string;
    
    // 状态查询
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    
    // 选项设置
    procedure SetTimeout(aTimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(aBlocking: Boolean);
    function GetBlocking: Boolean;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  end;

  { ISSLCertificate - 证书接口 }
  ISSLCertificate = interface
    ['{D3B0A7E4-AF6D-7051-C28E-BF0A213C5D6F}']
    
    // 加载和保存
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(aStream: TStream): Boolean;
    function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
    function LoadFromPEM(const aPEM: string): Boolean;
    function LoadFromDER(const aDER: TBytes): Boolean;
    
    function SaveToFile(const aFileName: string): Boolean;
    function SaveToStream(aStream: TStream): Boolean;
    function SaveToPEM: string;
    function SaveToDER: TBytes;
    
    // 证书信息
    function GetInfo: TSSLCertificateInfo;
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetPublicKey: string;
    function GetPublicKeyAlgorithm: string;
    function GetSignatureAlgorithm: string;
    function GetVersion: Integer;
    
    // 证书验证
    function Verify(aCAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(aCAStore: ISSLCertificateStore; 
      aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const aHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    
    // 证书扩展
    function GetExtension(const aOID: string): string;
    function GetSubjectAltNames: TStringList;
    function GetKeyUsage: TStringList;
    function GetExtendedKeyUsage: TStringList;
    
    // 指纹
    function GetFingerprint(aHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    
    // 证书链
    procedure SetIssuerCertificate(aCert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  end;

  { ISSLCertificateStore - 证书存储接口 }
  ISSLCertificateStore = interface
    ['{E4C1B8F5-B07E-8162-D39F-C10B324D6E70}']
    
    // 证书管理
    function AddCertificate(aCert: ISSLCertificate): Boolean;
    function RemoveCertificate(aCert: ISSLCertificate): Boolean;
    function Contains(aCert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(aIndex: Integer): ISSLCertificate;
    
    // 加载证书
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromPath(const aPath: string): Boolean;
    function LoadSystemStore: Boolean; // 加载系统证书存储
    
    // 查找证书
    function FindBySubject(const aSubject: string): ISSLCertificate;
    function FindByIssuer(const aIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const aFingerprint: string): ISSLCertificate;
    
    // 验证
    function VerifyCertificate(aCert: ISSLCertificate): Boolean;
    function BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
  end;

  { ISSLSession - 会话接口 }
  ISSLSession = interface
    ['{F5D2C9F6-C18F-9273-E40A-D21C435E7F81}']
    
    // 会话信息
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(aTimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    
    // 会话属性
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    
    // 序列化
    function Serialize: TBytes;
    function Deserialize(const aData: TBytes): Boolean;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;
  end;

const
  // ============================================================================
  // 常量定义
  // ============================================================================

  // TSSLContextType 别名（兼容性）
  sslContextClient = sslCtxClient;
  sslContextServer = sslCtxServer;
  sslContextBoth = sslCtxBoth;

  // 库名称映射
  SSL_LIBRARY_NAMES: array[TSSLLibraryType] of string = (
    'Auto-Detect',
    'OpenSSL',
    'WolfSSL',
    'MbedTLS',
    'Windows Schannel'
  );

  // 协议版本字符串
  SSL_PROTOCOL_NAMES: array[TSSLProtocolVersion] of string = (
    'Unknown',
    'SSL 2.0',
    'SSL 3.0',
    'TLS 1.0',
    'TLS 1.1',
    'TLS 1.2',
    'TLS 1.3',
    'DTLS 1.0',
    'DTLS 1.2'
  );

  // 错误代码描述
  SSL_ERROR_MESSAGES: array[TSSLErrorCode] of string = (
    '无错误',
    '一般错误',
    '内存分配失败',
    '无效参数',
    '未初始化',
    '协议错误',
    '握手错误',
    '证书错误',
    '证书过期',
    '证书被撤销',
    '未知证书',
    '连接错误',
    '超时',
    'I/O错误',
    '操作将阻塞',
    'SSL需要读取',
    'SSL需要写入',
    '不支持的功能',
    '库文件未找到',
    '函数未找到',
    '版本不匹配',
    '配置错误',
    '其他错误'
  );

  // 默认配置值
  SSL_DEFAULT_BUFFER_SIZE = 16384;           // 16KB
  SSL_DEFAULT_HANDSHAKE_TIMEOUT = 30000;     // 30秒
  SSL_DEFAULT_SESSION_CACHE_SIZE = 1024;     // 1024个会话
  SSL_DEFAULT_SESSION_TIMEOUT = 300;         // 5分钟
  SSL_DEFAULT_VERIFY_DEPTH = 10;             // 验证深度

  // TLS 1.3 默认密码套件
  SSL_DEFAULT_TLS13_CIPHERSUITES = 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256';
  
  // TLS 1.2 及以下默认密码套件
  SSL_DEFAULT_CIPHER_LIST = 'ECDHE+AESGCM:ECDHE+AES256:!aNULL:!MD5:!DSS';

// ============================================================================
// 辅助函数
// ============================================================================

function SSLErrorToString(aError: TSSLErrorCode): string;
function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
function LibraryTypeToString(aLibType: TSSLLibraryType): string;

implementation

{ ESSLException }

constructor ESSLException.Create(const aMessage: string;
  aErrorCode: TSSLErrorCode; aLibraryType: TSSLLibraryType;
  aNativeError: Integer);
begin
  inherited Create(aMessage);
  FErrorCode := aErrorCode;
  FLibraryType := aLibraryType;
  FNativeError := aNativeError;
  FContext := '';
  FNativeErrorMessage := '';
end;

constructor ESSLException.Create(const aMessage: string;
  aErrorCode: TSSLErrorCode; const aContext: string;
  aLibraryType: TSSLLibraryType; aNativeError: Integer);
begin
  inherited Create(aMessage);
  FErrorCode := aErrorCode;
  FLibraryType := aLibraryType;
  FNativeError := aNativeError;
  FContext := aContext;
  FNativeErrorMessage := '';
end;

{ 辅助函数实现 }

function SSLErrorToString(aError: TSSLErrorCode): string;
begin
  Result := SSL_ERROR_MESSAGES[aError];
end;

function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
begin
  Result := SSL_PROTOCOL_NAMES[aVersion];
end;

function LibraryTypeToString(aLibType: TSSLLibraryType): string;
begin
  Result := SSL_LIBRARY_NAMES[aLibType];
end;

end.



