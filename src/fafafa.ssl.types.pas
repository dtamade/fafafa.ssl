{
  fafafa.ssl.types - SSL/TLS 库通用类型定义
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    定义 fafafa.ssl 库使用的所有基础类型、常量、枚举和异常类。
    这些类型在所有后端实现中通用。
}

unit fafafa.ssl.types;

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
    sslErrUnsupported,       // 不支持的功能
    sslErrLibraryNotFound,   // 库文件未找到
    sslErrFunctionNotFound,  // 函数未找到
    sslErrVersionMismatch    // 版本不匹配
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
    sslHashNone,
    sslHashMD5,
    sslHashSHA1,
    sslHashSHA256,
    sslHashSHA384,
    sslHashSHA512
  );

  // ============================================================================
  // 记录类型定义
  // ============================================================================

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

const
  // ============================================================================
  // 常量定义
  // ============================================================================

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
    '握手失败',
    '证书错误',
    '证书已过期',
    '证书已被撤销',
    '未知证书',
    '连接错误',
    '操作超时',
    'I/O错误',
    '操作会阻塞',
    '不支持的功能',
    '库文件未找到',
    '函数未找到',
    '版本不匹配'
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

end.