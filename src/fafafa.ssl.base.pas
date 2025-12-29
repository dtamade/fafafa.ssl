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

  { TSSLBackendCapabilities - 后端能力矩阵 (P2-2) }
  TSSLBackendCapabilities = record
    SupportsTLS13: Boolean;           // TLS 1.3 支持
    SupportsALPN: Boolean;            // 应用层协议协商
    SupportsSNI: Boolean;             // 服务器名称指示
    SupportsOCSPStapling: Boolean;    // OCSP 装订
    SupportsCertificateTransparency: Boolean;  // 证书透明度
    SupportsSessionTickets: Boolean;  // 会话票据
    SupportsECDHE: Boolean;           // ECDHE 密钥交换
    SupportsChaChaPoly: Boolean;      // ChaCha20-Poly1305 加密
    SupportsPEMPrivateKey: Boolean;   // PEM 格式私钥加载支持
    MinTLSVersion: TSSLProtocolVersion;  // 支持的最低 TLS 版本
    MaxTLSVersion: TSSLProtocolVersion;  // 支持的最高 TLS 版本
  end;

  {**
   * ISSLLibrary - SSL库管理接口
   *
   * 提供 SSL/TLS 库的初始化、版本查询、功能检测和工厂方法。
   * 这是使用 fafafa.ssl 的入口点接口。
   *
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *
   * @example
   *   var Lib: ISSLLibrary;
   *   begin
   *     Lib := TOpenSSLLibrary.Create;
   *     if Lib.Initialize then
   *       WriteLn('OpenSSL version: ', Lib.GetVersionString);
   *   end;
   *}
  ISSLLibrary = interface
    ['{A0E8F4B1-7C3A-4D2E-9F5B-8C6D7E9A0B1C}']

    {** 初始化 SSL 库。必须在使用其他功能前调用。
        @returns True 如果初始化成功，False 如果失败 *}
    function Initialize: Boolean;

    {** 清理 SSL 库资源。应在程序退出前调用 *}
    procedure Finalize;

    {** 检查库是否已初始化
        @returns True 如果已初始化 *}
    function IsInitialized: Boolean;

    {** 获取库后端类型（OpenSSL, WinSSL 等）
        @returns TSSLLibraryType 枚举值 *}
    function GetLibraryType: TSSLLibraryType;

    {** 获取库版本字符串（如 "OpenSSL 3.0.13"）
        @returns 版本描述字符串 *}
    function GetVersionString: string;

    {** 获取库版本号（数值格式，便于比较）
        @returns 版本号，格式因后端而异 *}
    function GetVersionNumber: Cardinal;

    {** 获取库编译标志
        @returns 编译配置字符串 *}
    function GetCompileFlags: string;

    {** 检查是否支持指定协议版本
        @param AProtocol 要检查的协议版本
        @returns True 如果支持该协议 *}
    function IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;

    {** 检查是否支持指定密码套件
        @param ACipherName 密码套件名称（如 "AES256-GCM-SHA384"）
        @returns True 如果支持该密码套件 *}
    function IsCipherSupported(const ACipherName: string): Boolean;

    {** 检查是否支持指定特性
        @param AFeature 要检查的特性（SNI, ALPN 等）
        @returns True 如果支持该特性 *}
    function IsFeatureSupported(AFeature: TSSLFeature): Boolean;

    {** 获取后端能力矩阵，一次性查询所有支持的特性
        @returns TSSLBackendCapabilities 记录 *}
    function GetCapabilities: TSSLBackendCapabilities;

    {** 设置默认配置，影响后续创建的上下文
        @param AConfig 配置记录 *}
    procedure SetDefaultConfig(const AConfig: TSSLConfig);

    {** 获取当前默认配置
        @returns 当前配置记录的副本 *}
    function GetDefaultConfig: TSSLConfig;

    {** 获取最后一个错误码
        @returns 平台相关的错误码 *}
    function GetLastError: Integer;

    {** 获取最后一个错误的描述字符串
        @returns 错误描述 *}
    function GetLastErrorString: string;

    {** 清除错误状态 *}
    procedure ClearError;

    {** 获取统计信息（连接数、握手次数等）
        @returns 统计信息记录 *}
    function GetStatistics: TSSLStatistics;

    {** 重置统计计数器 *}
    procedure ResetStatistics;

    {** 设置日志回调函数
        @param ACallback 日志回调，nil 禁用日志 *}
    procedure SetLogCallback(ACallback: TSSLLogCallback);

    {** 记录日志消息
        @param ALevel 日志级别
        @param AMessage 日志内容 *}
    procedure Log(ALevel: TSSLLogLevel; const AMessage: string);

    {** 创建 SSL 上下文
        @param AType 上下文类型（客户端/服务端/双向）
        @returns 新创建的上下文接口 *}
    function CreateContext(AType: TSSLContextType): ISSLContext;

    {** 创建空证书对象
        @returns 新创建的证书接口 *}
    function CreateCertificate: ISSLCertificate;

    {** 创建证书存储
        @returns 新创建的证书存储接口 *}
    function CreateCertificateStore: ISSLCertificateStore;
  end;

  {**
   * ISSLContext - SSL上下文接口
   *
   * 管理 SSL/TLS 连接的配置，包括协议版本、证书、密码套件等。
   * 上下文可以创建多个连接，共享配置。
   *
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *
   * @example
   *   var Ctx: ISSLContext;
   *   begin
   *     Ctx := Lib.CreateContext(sslCtxClient);
   *     Ctx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
   *     Ctx.LoadCertificate('client.crt');
   *     Ctx.LoadPrivateKey('client.key', 'password');
   *   end;
   *}
  ISSLContext = interface
    ['{B1F9E5C2-8D4B-5E3F-A06C-9D8E0F1A2B3D}']

    {** 获取上下文类型（客户端/服务端/双向）
        @returns TSSLContextType 枚举值 *}
    function GetContextType: TSSLContextType;

    {** 设置允许的协议版本。废弃协议会触发警告日志
        @param AVersions 协议版本集合 *}
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);

    {** 获取当前允许的协议版本
        @returns 协议版本集合 *}
    function GetProtocolVersions: TSSLProtocolVersions;

    {** 从文件加载证书
        @param AFileName 证书文件路径（PEM 或 DER 格式）
        @raises ESSLCertificateException 加载失败时 *}
    procedure LoadCertificate(const AFileName: string); overload;

    {** 从流加载证书
        @param AStream 包含证书数据的流
        @raises ESSLCertificateException 加载失败时 *}
    procedure LoadCertificate(AStream: TStream); overload;

    {** 从证书接口加载证书
        @param ACert 证书接口 *}
    procedure LoadCertificate(ACert: ISSLCertificate); overload;

    {** 从文件加载私钥
        @param AFileName 私钥文件路径
        @param APassword 私钥密码（可选）
        @raises ESSLKeyException 加载失败时 *}
    procedure LoadPrivateKey(const AFileName: string; const APassword: string = ''); overload;

    {** 从流加载私钥
        @param AStream 包含私钥数据的流
        @param APassword 私钥密码（可选）
        @raises ESSLKeyException 加载失败时 *}
    procedure LoadPrivateKey(AStream: TStream; const APassword: string = ''); overload;

    {** 从 PEM 字符串直接加载证书（无需临时文件）
        @param APEM PEM 格式的证书字符串
        @raises ESSLCertificateException 加载失败时 *}
    procedure LoadCertificatePEM(const APEM: string);

    {** 从 PEM 字符串直接加载私钥
        @param APEM PEM 格式的私钥字符串
        @param APassword 私钥密码（可选）
        @raises ESSLKeyException 加载失败时 *}
    procedure LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');

    {** 加载 CA 证书文件（用于验证对端证书）
        @param AFileName CA 证书文件路径 *}
    procedure LoadCAFile(const AFileName: string);

    {** 加载 CA 证书目录
        @param APath 包含 CA 证书的目录路径 *}
    procedure LoadCAPath(const APath: string);

    {** 设置证书存储（用于验证）
        @param AStore 证书存储接口 *}
    procedure SetCertificateStore(AStore: ISSLCertificateStore);

    {** 设置证书验证模式
        @param AMode 验证模式集合 *}
    procedure SetVerifyMode(AMode: TSSLVerifyModes);

    {** 获取当前验证模式
        @returns 验证模式集合 *}
    function GetVerifyMode: TSSLVerifyModes;

    {** 设置证书链验证深度
        @param ADepth 最大验证深度（默认 10） *}
    procedure SetVerifyDepth(ADepth: Integer);

    {** 获取当前验证深度
        @returns 验证深度 *}
    function GetVerifyDepth: Integer;

    {** 设置自定义验证回调
        @param ACallback 验证回调函数 *}
    procedure SetVerifyCallback(ACallback: TSSLVerifyCallback);

    {** 设置密码套件列表（TLS 1.2 及以下）
        @param ACipherList OpenSSL 格式的密码套件字符串 *}
    procedure SetCipherList(const ACipherList: string);

    {** 获取当前密码套件列表
        @returns 密码套件字符串 *}
    function GetCipherList: string;

    {** 设置 TLS 1.3 密码套件
        @param ACipherSuites TLS 1.3 密码套件字符串 *}
    procedure SetCipherSuites(const ACipherSuites: string);

    {** 获取 TLS 1.3 密码套件
        @returns TLS 1.3 密码套件字符串 *}
    function GetCipherSuites: string;

    {** 启用/禁用会话缓存
        @param AEnabled True 启用，False 禁用 *}
    procedure SetSessionCacheMode(AEnabled: Boolean);

    {** 获取会话缓存状态
        @returns True 如果已启用 *}
    function GetSessionCacheMode: Boolean;

    {** 设置会话超时时间
        @param ATimeout 超时秒数 *}
    procedure SetSessionTimeout(ATimeout: Integer);

    {** 获取会话超时时间
        @returns 超时秒数 *}
    function GetSessionTimeout: Integer;

    {** 设置会话缓存大小
        @param ASize 最大缓存会话数 *}
    procedure SetSessionCacheSize(ASize: Integer);

    {** 获取会话缓存大小
        @returns 最大缓存会话数 *}
    function GetSessionCacheSize: Integer;

    {** 设置 SSL 选项
        @param AOptions 选项集合 *}
    procedure SetOptions(const AOptions: TSSLOptions);

    {** 获取当前 SSL 选项
        @returns 选项集合 *}
    function GetOptions: TSSLOptions;

    {** 设置 SNI 服务器名称（客户端使用）
        @param AServerName 服务器主机名 *}
    procedure SetServerName(const AServerName: string);

    {** 获取 SNI 服务器名称
        @returns 服务器主机名 *}
    function GetServerName: string;

    {** 设置 ALPN 协议列表
        @param AProtocols 逗号分隔的协议列表（如 "h2,http/1.1"） *}
    procedure SetALPNProtocols(const AProtocols: string);

    {** 获取 ALPN 协议列表
        @returns 协议列表字符串 *}
    function GetALPNProtocols: string;

    {** 设置证书验证标志（OCSP、CRL 等）
        @param AFlags 验证标志集合 *}
    procedure SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);

    {** 获取证书验证标志
        @returns 验证标志集合 *}
    function GetCertVerifyFlags: TSSLCertVerifyFlags;

    {** 设置密码回调（用于加密私钥）
        @param ACallback 密码回调函数 *}
    procedure SetPasswordCallback(ACallback: TSSLPasswordCallback);

    {** 设置信息回调（用于调试）
        @param ACallback 信息回调函数 *}
    procedure SetInfoCallback(ACallback: TSSLInfoCallback);

    {** 从套接字创建 SSL 连接
        @param ASocket 已连接的套接字句柄
        @returns 新创建的连接接口 *}
    function CreateConnection(ASocket: THandle): ISSLConnection; overload;

    {** 从流创建 SSL 连接
        @param AStream 底层传输流
        @returns 新创建的连接接口 *}
    function CreateConnection(AStream: TStream): ISSLConnection; overload;

    {** 检查上下文是否有效
        @returns True 如果上下文已正确初始化 *}
    function IsValid: Boolean;

    {** 获取原生句柄（用于高级操作）
        @returns 平台相关的原生句柄指针 *}
    function GetNativeHandle: Pointer;
  end;

  {**
   * ISSLConnection - SSL连接接口
   *
   * 表示一个活动的 SSL/TLS 连接，提供握手、数据传输和状态查询功能。
   * 连接从 ISSLContext.CreateConnection 创建。
   *
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *
   * @example
   *   var Conn: ISSLConnection;
   *   begin
   *     Conn := Ctx.CreateConnection(Socket);
   *     Conn.SetServerName('example.com');
   *     if Conn.Connect then
   *       Conn.WriteString('GET / HTTP/1.1'#13#10);
   *   end;
   *}
  ISSLConnection = interface
    ['{C2A9F6D3-9E5C-6F40-B17D-AE9F102B4C5E}']

    {** 执行客户端 SSL 连接（包含握手）
        @returns True 如果连接成功 *}
    function Connect: Boolean;

    {** 接受服务端 SSL 连接（包含握手）
        @returns True 如果接受成功 *}
    function Accept: Boolean;

    {** 优雅关闭 SSL 连接（发送 close_notify）
        @returns True 如果关闭成功 *}
    function Shutdown: Boolean;

    {** 强制关闭连接（不发送 close_notify） *}
    procedure Close;

    {** 执行/继续 SSL 握手
        @returns 握手状态 *}
    function DoHandshake: TSSLHandshakeState;

    {** 检查握手是否完成
        @returns True 如果握手已完成 *}
    function IsHandshakeComplete: Boolean;

    {** 发起重新协商
        @returns True 如果重新协商成功启动 *}
    function Renegotiate: Boolean;

    {** 读取解密数据
        @param ABuffer 接收缓冲区
        @param ACount 要读取的最大字节数
        @returns 实际读取的字节数，-1 表示错误 *}
    function Read(var ABuffer; ACount: Integer): Integer;

    {** 写入数据（将被加密发送）
        @param ABuffer 数据缓冲区
        @param ACount 要写入的字节数
        @returns 实际写入的字节数，-1 表示错误 *}
    function Write(const ABuffer; ACount: Integer): Integer;

    {** 读取字符串
        @param AStr 输出字符串
        @returns True 如果读取成功 *}
    function ReadString(out AStr: string): Boolean;

    {** 写入字符串
        @param AStr 要发送的字符串
        @returns True 如果写入成功 *}
    function WriteString(const AStr: string): Boolean;

    {** 检查是否需要读取更多数据（非阻塞模式）
        @returns True 如果 SSL 层需要读取 *}
    function WantRead: Boolean;

    {** 检查是否需要写入数据（非阻塞模式）
        @returns True 如果 SSL 层需要写入 *}
    function WantWrite: Boolean;

    {** 获取操作错误码
        @param ARet 操作返回值
        @returns 对应的错误码 *}
    function GetError(ARet: Integer): TSSLErrorCode;

    {** 获取连接详细信息
        @returns 连接信息记录 *}
    function GetConnectionInfo: TSSLConnectionInfo;

    {** 获取协商的协议版本
        @returns 协议版本枚举 *}
    function GetProtocolVersion: TSSLProtocolVersion;

    {** 获取协商的密码套件名称
        @returns 密码套件名称字符串 *}
    function GetCipherName: string;

    {** 获取对端证书
        @returns 对端证书接口，无证书时返回 nil *}
    function GetPeerCertificate: ISSLCertificate;

    {** 获取对端证书链
        @returns 证书数组，从叶证书到根证书 *}
    function GetPeerCertificateChain: TSSLCertificateArray;

    {** 获取证书验证结果码
        @returns 平台相关的验证结果码 *}
    function GetVerifyResult: Integer;

    {** 获取证书验证结果描述
        @returns 验证结果的文字描述 *}
    function GetVerifyResultString: string;

    {** 获取当前会话（用于会话恢复）
        @returns 会话接口 *}
    function GetSession: ISSLSession;

    {** 设置要恢复的会话
        @param ASession 之前保存的会话 *}
    procedure SetSession(ASession: ISSLSession);

    {** 检查是否使用了会话恢复
        @returns True 如果会话被恢复 *}
    function IsSessionReused: Boolean;

    {** 获取 ALPN 协商结果
        @returns 协商的协议名称（如 "h2"），未协商返回空 *}
    function GetSelectedALPNProtocol: string;

    {** 检查连接是否活动
        @returns True 如果连接已建立且未关闭 *}
    function IsConnected: Boolean;

    {** 获取内部状态（调试用）
        @returns 状态字符串 *}
    function GetState: string;

    {** 获取状态描述字符串
        @returns 人类可读的状态描述 *}
    function GetStateString: string;

    {** 设置操作超时
        @param ATimeout 超时毫秒数 *}
    procedure SetTimeout(ATimeout: Integer);

    {** 获取当前超时设置
        @returns 超时毫秒数 *}
    function GetTimeout: Integer;

    {** 设置阻塞模式
        @param ABlocking True 为阻塞，False 为非阻塞 *}
    procedure SetBlocking(ABlocking: Boolean);

    {** 获取当前阻塞模式
        @returns True 如果是阻塞模式 *}
    function GetBlocking: Boolean;

    {** 获取原生 SSL 句柄
        @returns 平台相关的原生句柄 *}
    function GetNativeHandle: Pointer;

    {** 获取关联的上下文
        @returns 创建此连接的上下文接口 *}
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
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
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

  {**
   * ISSLCertificateStore - 证书存储接口
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
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

  {**
   * ISSLSession - 会话接口
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
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
  
  // TLS 1.2 及以下默认密码套件（排除不安全算法）
  SSL_DEFAULT_CIPHER_LIST = 'ECDHE+AESGCM:ECDHE+AES256:DHE+AESGCM:DHE+AES256:' +
                            '!aNULL:!eNULL:!NULL:!MD5:!DSS:!RC4:!3DES:!EXPORT';

  // Phase 3.8 P2-1: 网络端口常量
  SSL_DEFAULT_HTTPS_PORT = 443;
  SSL_DEFAULT_HTTP_PORT = 80;

  // HTTP 配置常量
  SSL_DEFAULT_MAX_REDIRECTS = 5;

  // I/O 缓冲区大小常量
  SSL_IO_STRING_BUFFER_SIZE = 4096;
  SSL_IO_STREAM_BUFFER_SIZE = 8192;

  // 环形缓冲区默认容量幂次 (2^16 = 65536)
  SSL_RINGBUFFER_DEFAULT_CAPACITY_POW2 = 16;

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

