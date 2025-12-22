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
{$modeswitch advancedrecords}
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
  TSSLProcString = procedure(const AValue: string) of object;

  { Result 类型回调函数类型 - 使用 of object 以支持嵌套函数 }
  TProcedureOfConstTBytes = procedure(const AData: TBytes) of object;
  TProcedureOfConstString = procedure(const AValue: string) of object;
  TPredicateTBytes = function(const AData: TBytes): Boolean of object;
  TPredicateString = function(const AValue: string): Boolean of object;

  { TBytesView - 零拷贝字节视图 (Phase 2.3.2)

    类似 Rust 的 &[u8] 借用语义，提供对现有 TBytes 的只读视图，
    避免不必要的内存拷贝。适用于：
    - 加密操作的输入参数（零拷贝读取）
    - 哈希计算（避免输入拷贝）
    - 大数据处理（减少内存分配）

    注意：TBytesView 不拥有数据，只是引用。调用者必须确保
    源数据在视图使用期间保持有效。
  }
  TBytesView = record
    Data: PByte;      // 指向数据的指针
    Length: Integer;  // 数据长度（字节数）

    { 从 TBytes 创建视图（零拷贝）
      注意：使用 var 参数避免复制，确保指针指向调用者的数据 }
    class function FromBytes(var ABytes: TBytes): TBytesView; static;

    { 从指针和长度创建视图 }
    class function FromPtr(AData: PByte; ALength: Integer): TBytesView; static;

    { 创建空视图 }
    class function Empty: TBytesView; static;

    { 转换为 TBytes（需要拷贝） }
    function AsBytes: TBytes;

    { 创建子视图（切片） }
    function Slice(AStart, ALength: Integer): TBytesView;

    { 检查视图是否为空 }
    function IsEmpty: Boolean;

    { 检查视图是否有效（指针非空） }
    function IsValid: Boolean;

    { 获取指定索引的字节（带边界检查，Rust-quality 安全访问） }
    function GetByte(AIndex: Integer): Byte;

    { 获取指定索引的字节（无边界检查，性能关键代码使用） }
    function GetByteUnchecked(AIndex: Integer): Byte; inline;
  end;

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

  { SSL 特性枚举（Phase 1.3 - 消除字符串类型状态）

    类型安全的SSL特性标识符，替代stringly-typed模式。
    符合Rust质量标准 - 编译时类型检查，避免拼写错误。
  }
  TSSLFeature = (
    sslFeatSNI,                    // Server Name Indication (服务器名称指示)
    sslFeatALPN,                   // Application-Layer Protocol Negotiation (应用层协议协商)
    sslFeatSessionCache,           // Session Cache (会话缓存)
    sslFeatSessionTickets,         // Session Tickets (会话票据)
    sslFeatRenegotiation,          // Renegotiation (重新协商)
    sslFeatOCSPStapling,          // OCSP Stapling (OCSP装订)
    sslFeatCertificateTransparency // Certificate Transparency (证书透明度)
  );
  TSSLFeatures = set of TSSLFeature;

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
    // 新增错误码 - Phase 4
    sslErrInvalidData,       // 数据格式错误
    sslErrDecryptionFailed,  // 解密失败
    sslErrEncryptionFailed,  // 加密失败
    sslErrParseFailed,       // 解析失败
    sslErrLoadFailed,        // 加载失败
    sslErrVerificationFailed,// 验证失败
    sslErrKeyDerivationFailed,// 密钥派生失败
    sslErrInvalidFormat,     // 格式无效
    sslErrBufferTooSmall,    // 缓冲区太小
    sslErrResourceExhausted, // 资源耗尽
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

  { 通用字符串数组，用于只读字段传递 }
  TSSLStringArray = array of string;

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
    SubjectAltNames: TSSLStringArray; // 主题备用名称（只读快照）
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
  TSSLLogCallback = procedure(ALevel: TSSLLogLevel; const AMessage: string) of object;

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
    PrivateKeyPassword: string;              // 私钥口令（可选）
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
  // Result 类型定义（借鉴 Rust Result<T, E> 模式）
  // ============================================================================

  { SSL 操作结果 - 类似 Rust Result<(), E> }
  TSSLOperationResult = record
    Success: Boolean;
    ErrorCode: TSSLErrorCode;
    ErrorMessage: string;

    class function Ok: TSSLOperationResult; static;
    class function Err(ACode: TSSLErrorCode; const AMsg: string): TSSLOperationResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
    procedure Expect(const AMsg: string);  // 失败时抛出带自定义消息的异常
    function UnwrapErr: TSSLErrorCode;  // 返回错误码（成功时抛异常）
  end;

  { SSL 数据结果 - 类似 Rust Result<TBytes, E> }
  TSSLDataResult = record
    Success: Boolean;
    Data: TBytes;
    ErrorCode: TSSLErrorCode;
    ErrorMessage: string;

    class function Ok(const AData: TBytes): TSSLDataResult; static;
    class function Err(ACode: TSSLErrorCode; const AMsg: string): TSSLDataResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
    function Unwrap: TBytes;  // 失败时抛异常
    function UnwrapOr(const ADefault: TBytes): TBytes;  // 失败时返回默认值
    function Expect(const AMsg: string): TBytes;  // 失败时抛出带自定义消息的异常
    function UnwrapErr: TSSLErrorCode;  // 返回错误码（成功时抛异常）
    function IsOkAnd(APredicate: TPredicateTBytes): Boolean;  // 检查是否Ok且满足条件
    function Inspect(ACallback: TProcedureOfConstTBytes): TSSLDataResult;  // 检查但不消耗值
  end;

  { SSL 字符串结果 - 类似 Rust Result<String, E> }
  TSSLStringResult = record
    Success: Boolean;
    Value: string;
    ErrorCode: TSSLErrorCode;
    ErrorMessage: string;

    class function Ok(const AValue: string): TSSLStringResult; static;
    class function Err(ACode: TSSLErrorCode; const AMsg: string): TSSLStringResult; static;

    function IsOk: Boolean;
    function IsErr: Boolean;
    function Unwrap: string;  // 失败时抛异常
    function UnwrapOr(const ADefault: string): string;  // 失败时返回默认值
    function Expect(const AMsg: string): string;  // 失败时抛出带自定义消息的异常
    function UnwrapErr: TSSLErrorCode;  // 返回错误码（成功时抛异常）
    function IsOkAnd(APredicate: TPredicateString): Boolean;  // 检查是否Ok且满足条件
    function Inspect(ACallback: TProcedureOfConstString): TSSLStringResult;  // 检查但不消耗值
  end;

  { Builder 配置验证结果 - Phase 2.1.2 }
  TBuildValidationResult = record
    IsValid: Boolean;          // 是否有效（无错误）
    Warnings: array of string; // 警告消息（不阻止构建）
    Errors: array of string;   // 错误消息（阻止构建）

    class function Ok: TBuildValidationResult; static;
    class function WithWarnings(const AWarn: array of string): TBuildValidationResult; static;
    class function WithErrors(const AErrs: array of string): TBuildValidationResult; static;

    procedure AddWarning(const AMessage: string);
    procedure AddError(const AMessage: string);
    function HasWarnings: Boolean;
    function HasErrors: Boolean;
    function WarningCount: Integer;
    function ErrorCount: Integer;
  end;

  // ============================================================================
  // 异常类定义已移至 fafafa.ssl.exceptions.pas
  // 所有模块应使用 fafafa.ssl.exceptions 中定义的异常类
  // ============================================================================

  // ============================================================================
  // 回调类型定义
  // ============================================================================

  { 证书验证回调 }
  TSSLVerifyCallback = function(const ACertificate: TSSLCertificateInfo;
                                const AErrorCode: Integer;
                                const AErrorMessage: string): Boolean of object;

  { 密码回调 }
  TSSLPasswordCallback = function(var APassword: string;
                                const AIsRetry: Boolean): Boolean of object;

  { 信息回调 }
  TSSLInfoCallback = procedure(const AWhere: Integer;
                              const ARet: Integer;
                              const AState: string) of object;

  { 数据传输回调 }
  TSSLDataCallback = procedure(const AData: Pointer;
                              const ASize: Integer;
                              const AIsOutgoing: Boolean) of object;

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
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    // 功能支持查询
    function IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const ACipherName: string): Boolean;
    function IsFeatureSupported(AFeature: TSSLFeature): Boolean;
    
    // 库配置
    procedure SetDefaultConfig(const AConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    
    // 错误处理
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    
    // 统计信息
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    
    // 日志
    procedure SetLogCallback(ACallback: TSSLLogCallback);
    procedure Log(ALevel: TSSLLogLevel; const AMessage: string);
    
    // 工厂方法
    function CreateContext(AType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

  { ISSLContext - SSL上下文接口 }
  ISSLContext = interface
    ['{B1F9E5C2-8D4B-5E3F-A06C-9D8E0F1A2B3D}']
    
    // 基本配置
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    
    // 证书和密钥管理
    procedure LoadCertificate(const AFileName: string); overload;
    procedure LoadCertificate(AStream: TStream); overload;
    procedure LoadCertificate(ACert: ISSLCertificate); overload;
    
    procedure LoadPrivateKey(const AFileName: string; const APassword: string = ''); overload;
    procedure LoadPrivateKey(AStream: TStream; const APassword: string = ''); overload;
    
    // PEM 字符串直接加载（企业级接口增强）
    procedure LoadCertificatePEM(const APEM: string);
    procedure LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');
    
    procedure LoadCAFile(const AFileName: string);
    procedure LoadCAPath(const APath: string);
    procedure SetCertificateStore(AStore: ISSLCertificateStore);
    
    // 验证配置
    procedure SetVerifyMode(AMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(ADepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(ACallback: TSSLVerifyCallback);
    
    // 密码套件配置
    procedure SetCipherList(const ACipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const ACipherSuites: string); // TLS 1.3
    function GetCipherSuites: string;
    
    // 会话管理
    procedure SetSessionCacheMode(AEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(ATimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(ASize: Integer);
    function GetSessionCacheSize: Integer;
    
    // 高级选项
    procedure SetOptions(const AOptions: TSSLOptions);
    function GetOptions: TSSLOptions;
    procedure SetServerName(const AServerName: string); // SNI
    function GetServerName: string;
    procedure SetALPNProtocols(const AProtocols: string);
    function GetALPNProtocols: string;

    // 证书验证标志（OCSP/CRL 等）
    procedure SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
    function GetCertVerifyFlags: TSSLCertVerifyFlags;

    // 回调设置
    procedure SetPasswordCallback(ACallback: TSSLPasswordCallback);
    procedure SetInfoCallback(ACallback: TSSLInfoCallback);
    
    // 创建连接
    function CreateConnection(ASocket: THandle): ISSLConnection; overload;
    function CreateConnection(AStream: TStream): ISSLConnection; overload;
    
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
    function Read(var ABuffer; ACount: Integer): Integer;
    function Write(const ABuffer; ACount: Integer): Integer;
    function ReadString(out AStr: string): Boolean;
    function WriteString(const AStr: string): Boolean;
    
    // 异步操作支持
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(ARet: Integer): TSSLErrorCode;
    
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
    procedure SetSession(ASession: ISSLSession);
    function IsSessionReused: Boolean;
    
    // ALPN/NPN
    function GetSelectedALPNProtocol: string;
    
    // 状态查询
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    
    // 选项设置
    procedure SetTimeout(ATimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(ABlocking: Boolean);
    function GetBlocking: Boolean;
    
    // 原生句柄
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  end;

  {**
   * ISSLCertificate - Full-featured certificate interface for SSL operations
   *
   * This interface provides comprehensive certificate management capabilities:
   * - Loading from various sources (file, stream, PEM, DER)
   * - Certificate validation and verification
   * - Certificate chain management
   * - Extension and fingerprint access
   *
   * @note This is DIFFERENT from ICertificate in fafafa.ssl.cert.builder:
   *   - ISSLCertificate: Full-featured, for SSL operations (load, verify, chains)
   *   - ICertificate: Lightweight, for builder output, read-only
   *
   * @see ICertificate for builder-generated certificates
   *}
  ISSLCertificate = interface
    ['{D3B0A7E4-AF6D-7051-C28E-BF0A213C5D6F}']
    
    // 加载和保存
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function LoadFromMemory(const AData: Pointer; ASize: Integer): Boolean;
    function LoadFromPEM(const APEM: string): Boolean;
    function LoadFromDER(const ADER: TBytes): Boolean;
    
    function SaveToFile(const AFileName: string): Boolean;
    function SaveToStream(AStream: TStream): Boolean;
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
    function Verify(ACAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(ACAStore: ISSLCertificateStore;
      AFlags: TSSLCertVerifyFlags; out AResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const AHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;

    // 便利方法 (P2 增强)
    function GetDaysUntilExpiry: Integer;  // 返回证书到期天数，已过期返回负数
    function GetSubjectCN: string;         // 直接获取 Subject 中的 Common Name

    // 证书扩展
    function GetExtension(const AOID: string): string;
    function GetSubjectAltNames: TSSLStringArray;
    function GetKeyUsage: TSSLStringArray;
    function GetExtendedKeyUsage: TSSLStringArray;
    
    // 指纹
    function GetFingerprint(AHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    
    // 证书链
    procedure SetIssuerCertificate(ACert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;  // 返回内部引用，不转移所有权

    // 原生句柄
    function GetNativeHandle: Pointer;  // 返回原生句柄，不转移所有权
    function Clone: ISSLCertificate;    // P3-21: 创建新实例，调用者拥有所有权
  end;

  { ISSLCertificateStore - 证书存储接口 }
  ISSLCertificateStore = interface
    ['{E4C1B8F5-B07E-8162-D39F-C10B324D6E70}']
    
    // 证书管理
    function AddCertificate(ACert: ISSLCertificate): Boolean;
    function RemoveCertificate(ACert: ISSLCertificate): Boolean;
    function Contains(ACert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(AIndex: Integer): ISSLCertificate;
    
    // 加载证书
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromPath(const APath: string): Boolean;
    function LoadSystemStore: Boolean; // 加载系统证书存储
    
    // 查找证书
    function FindBySubject(const ASubject: string): ISSLCertificate;
    function FindByIssuer(const AIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const AFingerprint: string): ISSLCertificate;
    
    // 验证
    function VerifyCertificate(ACert: ISSLCertificate): Boolean;
    function BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
    
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
    procedure SetTimeout(ATimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    
    // 会话属性
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    
    // 序列化
    function Serialize: TBytes;
    function Deserialize(const AData: TBytes): Boolean;
    
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

  // 错误代码描述（中文）
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
    // 新增错误消息 - Phase 4
    '数据格式错误',
    '解密失败',
    '加密失败',
    '解析失败',
    '加载失败',
    '验证失败',
    '密钥派生失败',
    '格式无效',
    '缓冲区太小',
    '资源耗尽',
    '其他错误'
  );

  // 错误代码描述（英文）
  SSL_ERROR_MESSAGES_EN: array[TSSLErrorCode] of string = (
    'No error',
    'General error',
    'Memory allocation failed',
    'Invalid parameter',
    'Not initialized',
    'Protocol error',
    'Handshake error',
    'Certificate error',
    'Certificate expired',
    'Certificate revoked',
    'Unknown certificate',
    'Connection error',
    'Timeout',
    'I/O error',
    'Operation would block',
    'SSL needs read',
    'SSL needs write',
    'Unsupported feature',
    'Library file not found',
    'Function not found',
    'Version mismatch',
    'Configuration error',
    // New error messages - Phase 4
    'Invalid data format',
    'Decryption failed',
    'Encryption failed',
    'Parse failed',
    'Load failed',
    'Verification failed',
    'Key derivation failed',
    'Invalid format',
    'Buffer too small',
    'Resource exhausted',
    'Other error'
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
  SSL_DEFAULT_CIPHER_LIST = 'ECDHE+AESGCM:ECDHE+AES256:!ANULL:!MD5:!DSS';

// ============================================================================
// 辅助函数
// ============================================================================

function SSLErrorToString(AError: TSSLErrorCode): string;
function ProtocolVersionToString(AVersion: TSSLProtocolVersion): string;
function LibraryTypeToString(ALibType: TSSLLibraryType): string;

implementation

uses
  fafafa.ssl.errors,      // Stage 2.1 P2 - Standardized error handling
  fafafa.ssl.exceptions;  // Phase 3.3 P0 - 统一异常定义（修复重复定义问题）

{ TBytesView - 零拷贝字节视图实现 (Phase 2.3.2) }

class function TBytesView.FromBytes(var ABytes: TBytes): TBytesView;
begin
  Result.Length := System.Length(ABytes);
  if Result.Length > 0 then
    Result.Data := @ABytes[0]  // 获取第一个元素的地址（指向调用者的数据）
  else
    Result.Data := nil;
end;

class function TBytesView.FromPtr(AData: PByte; ALength: Integer): TBytesView;
begin
  Result.Data := AData;
  Result.Length := ALength;
end;

class function TBytesView.Empty: TBytesView;
begin
  Result.Data := nil;
  Result.Length := 0;
end;

function TBytesView.AsBytes: TBytes;
var
  I: Integer;
begin
  Result := nil;  // Phase 3.3 P0 - 初始化 Result，避免编译器警告
  SetLength(Result, Length);
  if (Length > 0) and (Data <> nil) then
  begin
    for I := 0 to Length - 1 do
      Result[I] := Data[I];
  end;
end;

function TBytesView.Slice(AStart, ALength: Integer): TBytesView;
begin
  // 边界检查
  if (AStart < 0) or (AStart >= Length) then
  begin
    Result := TBytesView.Empty;
    Exit;
  end;

  // 调整长度
  if AStart + ALength > Length then
    ALength := Length - AStart;

  if ALength <= 0 then
  begin
    Result := TBytesView.Empty;
    Exit;
  end;

  // 创建子视图
  Result.Data := Data + AStart;
  Result.Length := ALength;
end;

function TBytesView.IsEmpty: Boolean;
begin
  Result := (Length = 0) or (Data = nil);
end;

function TBytesView.IsValid: Boolean;
begin
  Result := (Data <> nil) and (Length > 0);
end;

function TBytesView.GetByte(AIndex: Integer): Byte;
begin
  // Rust-quality: Bounds checking like Rust's [] operator
  if (AIndex < 0) or (AIndex >= Length) then
    raise ERangeError.CreateFmt('TBytesView index %d out of bounds [0..%d)', [AIndex, Length]);
  Result := Data[AIndex];
end;

function TBytesView.GetByteUnchecked(AIndex: Integer): Byte;
begin
  // Rust-quality: Unchecked access like Rust's get_unchecked()
  // Use only when bounds are already verified
  Result := Data[AIndex];
end;

{ 异常类实现已移至 fafafa.ssl.exceptions.pas }

{ 辅助函数实现 }

function SSLErrorToString(AError: TSSLErrorCode): string;
begin
  Result := SSL_ERROR_MESSAGES[AError];
end;

function ProtocolVersionToString(AVersion: TSSLProtocolVersion): string;
begin
  Result := SSL_PROTOCOL_NAMES[AVersion];
end;

function LibraryTypeToString(ALibType: TSSLLibraryType): string;
begin
  Result := SSL_LIBRARY_NAMES[ALibType];
end;

{ TSSLOperationResult }

class function TSSLOperationResult.Ok: TSSLOperationResult;
begin
  Result.Success := True;
  Result.ErrorCode := sslErrNone;
  Result.ErrorMessage := '';
end;

class function TSSLOperationResult.Err(ACode: TSSLErrorCode; const AMsg: string): TSSLOperationResult;
begin
  Result.Success := False;
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSLOperationResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSLOperationResult.IsErr: Boolean;
begin
  Result := not Success;
end;

procedure TSSLOperationResult.Expect(const AMsg: string);
begin
  if not Success then
    raise ESSLException.Create(AMsg + ': ' + ErrorMessage, ErrorCode);
end;

function TSSLOperationResult.UnwrapErr: TSSLErrorCode;
begin
  if Success then
    RaiseInvalidData('UnwrapErr on Ok value');
  Result := ErrorCode;
end;

{ TSSLDataResult }

class function TSSLDataResult.Ok(const AData: TBytes): TSSLDataResult;
begin
  Result.Success := True;
  Result.Data := AData;
  Result.ErrorCode := sslErrNone;
  Result.ErrorMessage := '';
end;

class function TSSLDataResult.Err(ACode: TSSLErrorCode; const AMsg: string): TSSLDataResult;
begin
  Result.Success := False;
  Result.Data := nil;
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSLDataResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSLDataResult.IsErr: Boolean;
begin
  Result := not Success;
end;

function TSSLDataResult.Unwrap: TBytes;
begin
  if not Success then
    raise ESSLException.Create(ErrorMessage, ErrorCode);
  Result := Data;
end;

function TSSLDataResult.UnwrapOr(const ADefault: TBytes): TBytes;
begin
  if Success then
    Result := Data
  else
    Result := ADefault;
end;

function TSSLDataResult.Expect(const AMsg: string): TBytes;
begin
  if not Success then
    raise ESSLException.Create(AMsg + ': ' + ErrorMessage, ErrorCode);
  Result := Data;
end;

function TSSLDataResult.UnwrapErr: TSSLErrorCode;
begin
  if Success then
    RaiseInvalidData('UnwrapErr on Ok value');
  Result := ErrorCode;
end;

function TSSLDataResult.IsOkAnd(APredicate: TPredicateTBytes): Boolean;
begin
  Result := Success and APredicate(Data);
end;

function TSSLDataResult.Inspect(ACallback: TProcedureOfConstTBytes): TSSLDataResult;
begin
  if Success then
    ACallback(Data);
  Result := Self;
end;

{ TSSLStringResult }

class function TSSLStringResult.Ok(const AValue: string): TSSLStringResult;
begin
  Result.Success := True;
  Result.Value := AValue;
  Result.ErrorCode := sslErrNone;
  Result.ErrorMessage := '';
end;

class function TSSLStringResult.Err(ACode: TSSLErrorCode; const AMsg: string): TSSLStringResult;
begin
  Result.Success := False;
  Result.Value := '';
  Result.ErrorCode := ACode;
  Result.ErrorMessage := AMsg;
end;

function TSSLStringResult.IsOk: Boolean;
begin
  Result := Success;
end;

function TSSLStringResult.IsErr: Boolean;
begin
  Result := not Success;
end;

function TSSLStringResult.Unwrap: string;
begin
  if not Success then
    raise ESSLException.Create(ErrorMessage, ErrorCode);
  Result := Value;
end;

function TSSLStringResult.UnwrapOr(const ADefault: string): string;
begin
  if Success then
    Result := Value
  else
    Result := ADefault;
end;

function TSSLStringResult.Expect(const AMsg: string): string;
begin
  if not Success then
    raise ESSLException.Create(AMsg + ': ' + ErrorMessage, ErrorCode);
  Result := Value;
end;

function TSSLStringResult.UnwrapErr: TSSLErrorCode;
begin
  if Success then
    RaiseInvalidData('UnwrapErr on Ok value');
  Result := ErrorCode;
end;

function TSSLStringResult.IsOkAnd(APredicate: TPredicateString): Boolean;
begin
  Result := Success and APredicate(Value);
end;

function TSSLStringResult.Inspect(ACallback: TProcedureOfConstString): TSSLStringResult;
begin
  if Success then
    ACallback(Value);
  Result := Self;
end;

{ TBuildValidationResult - Phase 2.1.2 }

class function TBuildValidationResult.Ok: TBuildValidationResult;
begin
  Result.IsValid := True;
  SetLength(Result.Warnings, 0);
  SetLength(Result.Errors, 0);
end;

class function TBuildValidationResult.WithWarnings(const AWarn: array of string): TBuildValidationResult;
var
  I: Integer;
begin
  Result.IsValid := True;
  SetLength(Result.Warnings, Length(AWarn));
  for I := 0 to High(AWarn) do
    Result.Warnings[I] := AWarn[I];
  SetLength(Result.Errors, 0);
end;

class function TBuildValidationResult.WithErrors(const AErrs: array of string): TBuildValidationResult;
var
  I: Integer;
begin
  Result.IsValid := False;
  SetLength(Result.Warnings, 0);
  SetLength(Result.Errors, Length(AErrs));
  for I := 0 to High(AErrs) do
    Result.Errors[I] := AErrs[I];
end;

procedure TBuildValidationResult.AddWarning(const AMessage: string);
var
  L: Integer;
begin
  L := Length(Warnings);
  SetLength(Warnings, L + 1);
  Warnings[L] := AMessage;
end;

procedure TBuildValidationResult.AddError(const AMessage: string);
var
  L: Integer;
begin
  IsValid := False;
  L := Length(Errors);
  SetLength(Errors, L + 1);
  Errors[L] := AMessage;
end;

function TBuildValidationResult.HasWarnings: Boolean;
begin
  Result := Length(Warnings) > 0;
end;

function TBuildValidationResult.HasErrors: Boolean;
begin
  Result := Length(Errors) > 0;
end;

function TBuildValidationResult.WarningCount: Integer;
begin
  Result := Length(Warnings);
end;

function TBuildValidationResult.ErrorCount: Integer;
begin
  Result := Length(Errors);
end;

end.

