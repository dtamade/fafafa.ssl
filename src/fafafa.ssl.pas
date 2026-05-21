{
  fafafa.ssl - 统一SSL/TLS库主单元
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  
  描述:
    fafafa.ssl 库的主入口单元，导出所有公共接口和类型。
    
  使用示例:
    uses fafafa.ssl;
    
    var
      LContext: ISSLContext;
      LConnector: TSSLConnector;
      LStream: TSSLStream;
    begin
      // 创建客户端上下文
      LContext := TSSLFactory.CreateContext(sslCtxClient);
      
      // 配置上下文
      LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LContext.SetVerifyMode([sslVerifyPeer]);
      
      // 推荐入口：通过 facade connector 建立 TLS
      LConnector := TSSLConnector.FromContext(LContext);
      LStream := LConnector.ConnectSocket(YourConnectedSocket, 'example.com');
      try
        // 通过 LStream 进行读写
      finally
        LStream.Free;
      end;
    end;
}

unit fafafa.ssl;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.safety,
  fafafa.ssl.exceptions,
  fafafa.ssl.factory,
  fafafa.ssl.tls,
  fafafa.ssl.cert.advanced;

// ============================================================================
// 重新导出所有公共类型
// ============================================================================

type
  // 从 fafafa.ssl.base 导出
  TSSLLibraryType = fafafa.ssl.base.TSSLLibraryType;
  TSSLLibraryTypes = fafafa.ssl.base.TSSLLibraryTypes;
  TSSLBackendImplType = fafafa.ssl.base.TSSLBackendImplType;
  TSSLFeatureSupportLevel = fafafa.ssl.base.TSSLFeatureSupportLevel;
  TSSLFeature = fafafa.ssl.base.TSSLFeature;
  TSSLFeatures = fafafa.ssl.base.TSSLFeatures;
  TSSLProtocolVersion = fafafa.ssl.base.TSSLProtocolVersion;
  TSSLProtocolVersions = fafafa.ssl.base.TSSLProtocolVersions;
  TSSLVerifyMode = fafafa.ssl.base.TSSLVerifyMode;
  TSSLVerifyModes = fafafa.ssl.base.TSSLVerifyModes;
  TSSLContextType = fafafa.ssl.base.TSSLContextType;
  TSSLHandshakeState = fafafa.ssl.base.TSSLHandshakeState;
  TSSLEarlyDataStatus = fafafa.ssl.base.TSSLEarlyDataStatus;
  TSSLEarlyDataServerPolicy = fafafa.ssl.base.TSSLEarlyDataServerPolicy;
  TSSLErrorCode = fafafa.ssl.base.TSSLErrorCode;
  TSSLLogLevel = fafafa.ssl.base.TSSLLogLevel;
  TSSLKeyExchange = fafafa.ssl.base.TSSLKeyExchange;
  TSSLKeyExchangeSupport = fafafa.ssl.base.TSSLKeyExchangeSupport;
  TSSLCipher = fafafa.ssl.base.TSSLCipher;
  TSSLCipherSupport = fafafa.ssl.base.TSSLCipherSupport;
  TSSLHash = fafafa.ssl.base.TSSLHash;
  TSSLHashSupport = fafafa.ssl.base.TSSLHashSupport;
  
  TSSLStringArray = fafafa.ssl.base.TSSLStringArray;
  TSSLCertificateInfo = fafafa.ssl.base.TSSLCertificateInfo;
  PSSLCertificateInfo = fafafa.ssl.base.PSSLCertificateInfo;
  TSSLCertificateArray = fafafa.ssl.base.TSSLCertificateArray;
  TSSLCertVerifyResult = fafafa.ssl.base.TSSLCertVerifyResult;
  TSSLConnectionInfo = fafafa.ssl.base.TSSLConnectionInfo;
  PSSLConnectionInfo = fafafa.ssl.base.PSSLConnectionInfo;
  TSSLConfig = fafafa.ssl.base.TSSLConfig;
  PSSLConfig = fafafa.ssl.base.PSSLConfig;
  TSSLStatistics = fafafa.ssl.base.TSSLStatistics;
  PSSLStatistics = fafafa.ssl.base.PSSLStatistics;
  TSSLHealthStatus = fafafa.ssl.base.TSSLHealthStatus;
  TSSLPerformanceMetrics = fafafa.ssl.base.TSSLPerformanceMetrics;
  TSSLDiagnosticInfo = fafafa.ssl.base.TSSLDiagnosticInfo;
  TSSLBackendCapabilities = fafafa.ssl.base.TSSLBackendCapabilities;
  
  // Result types (Rust-like error handling)
  TSSLOperationResult = fafafa.ssl.base.TSSLOperationResult;
  TSSLDataResult = fafafa.ssl.base.TSSLDataResult;
  TSSLStringResult = fafafa.ssl.base.TSSLStringResult;

  // 从 fafafa.ssl.safety 导出 - type-safety supporting surface
  TSSLVersion = fafafa.ssl.safety.TSSLVersion;
  TSSLVersions = fafafa.ssl.safety.TSSLVersions;
  TKeyType = fafafa.ssl.safety.TKeyType;
  TCertificateFormat = fafafa.ssl.safety.TCertificateFormat;
  TCipherMode = fafafa.ssl.safety.TCipherMode;
  TVerificationMode = fafafa.ssl.safety.TVerificationMode;
  TSessionCacheMode = fafafa.ssl.safety.TSessionCacheMode;
  TCertificatePurpose = fafafa.ssl.safety.TCertificatePurpose;
  TSignatureAlgorithm = fafafa.ssl.safety.TSignatureAlgorithm;
  TEllipticCurve = fafafa.ssl.safety.TEllipticCurve;
  TKeySize = fafafa.ssl.safety.TKeySize;
  TTimeoutDuration = fafafa.ssl.safety.TTimeoutDuration;
  TBufferSize = fafafa.ssl.safety.TBufferSize;
  
  ESSLException = fafafa.ssl.exceptions.ESSLException;
  ESSLHandshakeException = fafafa.ssl.exceptions.ESSLHandshakeException;
  ESSLCertificateException = fafafa.ssl.exceptions.ESSLCertificateException;
  ESSLProtocolException = fafafa.ssl.exceptions.ESSLProtocolException;
  ESSLConnectionException = fafafa.ssl.exceptions.ESSLConnectionException;
  ESSLTimeoutException = fafafa.ssl.exceptions.ESSLTimeoutException;
  ESSLLibraryException = fafafa.ssl.exceptions.ESSLLibraryException;
  
  TSSLVerifyCallback = fafafa.ssl.base.TSSLVerifyCallback;
  TSSLPasswordCallback = fafafa.ssl.base.TSSLPasswordCallback;
  TSSLInfoCallback = fafafa.ssl.base.TSSLInfoCallback;
  TSSLDataCallback = fafafa.ssl.base.TSSLDataCallback;
  TSSLHTTPGetCallback = fafafa.ssl.base.TSSLHTTPGetCallback;
  TSSLHTTPPostCallback = fafafa.ssl.base.TSSLHTTPPostCallback;
  
  // 从 fafafa.ssl.base 导出
  ISSLLibrary = fafafa.ssl.base.ISSLLibrary;
  ISSLContext = fafafa.ssl.base.ISSLContext;
  ISSLConnection = fafafa.ssl.base.ISSLConnection;
  ISSLClientConnection = fafafa.ssl.base.ISSLClientConnection;
  ISSLNativeHandleAccess = fafafa.ssl.base.ISSLNativeHandleAccess;
  ISSLConnectionInfo = fafafa.ssl.base.ISSLConnectionInfo;
  ISSLDiagnostics = fafafa.ssl.base.ISSLDiagnostics;
  ISSLSessionResumption = fafafa.ssl.base.ISSLSessionResumption;
  ISSLCertificateVerification = fafafa.ssl.base.ISSLCertificateVerification;
  ISSLOCSPStapling = fafafa.ssl.base.ISSLOCSPStapling;
  ISSLCertificateTransparency = fafafa.ssl.base.ISSLCertificateTransparency;
  ISSLCertificateTransparencyValidation = fafafa.ssl.base.ISSLCertificateTransparencyValidation;
  ISSLEarlyDataContext = fafafa.ssl.base.ISSLEarlyDataContext;
  ISSLEarlyDataConnection = fafafa.ssl.base.ISSLEarlyDataConnection;
  ISSLCertificate = fafafa.ssl.base.ISSLCertificate;
  ISSLCertificateStore = fafafa.ssl.base.ISSLCertificateStore;
  ISSLSession = fafafa.ssl.base.ISSLSession;
  ISSLHttpHooksAccess = fafafa.ssl.base.ISSLHttpHooksAccess;
  ISSLServerOCSPStaplingContext = fafafa.ssl.base.ISSLServerOCSPStaplingContext;
  
  // 从 fafafa.ssl.factory 导出
  TSSLFactory = fafafa.ssl.factory.TSSLFactory;
  TSSLHelper = fafafa.ssl.factory.TSSLHelper;

  // Rust 风格门面（Connector/Acceptor + Stream）
  TSSLConnector = fafafa.ssl.tls.TSSLConnector;
  TSSLAcceptor = fafafa.ssl.tls.TSSLAcceptor;
  TSSLStream = fafafa.ssl.tls.TSSLStream;

  // 从 fafafa.ssl.base 导出 - 证书验证标志
  TSSLCertVerifyFlag = fafafa.ssl.base.TSSLCertVerifyFlag;
  TSSLCertVerifyFlags = fafafa.ssl.base.TSSLCertVerifyFlags;

  // 从 fafafa.ssl.cert.advanced 导出 - OCSP/CRL 接口
  TOCSPStatus = fafafa.ssl.cert.advanced.TOCSPStatus;
  TOCSPResponse = fafafa.ssl.cert.advanced.TOCSPResponse;
  IOCSPClient = fafafa.ssl.cert.advanced.IOCSPClient;
  ICRLManager = fafafa.ssl.cert.advanced.ICRLManager;
  TPKCS12Options = fafafa.ssl.cert.advanced.TPKCS12Options;
  TPKCS12Manager = fafafa.ssl.cert.advanced.TPKCS12Manager;

const
  // 库类型常量
  sslAutoDetect = fafafa.ssl.base.sslAutoDetect;
  sslOpenSSL = fafafa.ssl.base.sslOpenSSL;
  sslWolfSSL = fafafa.ssl.base.sslWolfSSL;
  sslMbedTLS = fafafa.ssl.base.sslMbedTLS;
  sslWinSSL = fafafa.ssl.base.sslWinSSL;
  sslFreePascal = fafafa.ssl.base.sslFreePascal;

  // 后端实现类型常量
  sslImplNative = fafafa.ssl.base.sslImplNative;
  sslImplCLibrary = fafafa.ssl.base.sslImplCLibrary;
  sslImplOSNative = fafafa.ssl.base.sslImplOSNative;
  sslImplHybrid = fafafa.ssl.base.sslImplHybrid;

  // 功能支持级别常量
  sslSupportNone = fafafa.ssl.base.sslSupportNone;
  sslSupportExperimental = fafafa.ssl.base.sslSupportExperimental;
  sslSupportStable = fafafa.ssl.base.sslSupportStable;
  sslSupportDeprecated = fafafa.ssl.base.sslSupportDeprecated;

  // 功能枚举常量
  sslFeatSNI = fafafa.ssl.base.sslFeatSNI;
  sslFeatALPN = fafafa.ssl.base.sslFeatALPN;
  sslFeatSessionCache = fafafa.ssl.base.sslFeatSessionCache;
  sslFeatSessionTickets = fafafa.ssl.base.sslFeatSessionTickets;
  sslFeatRenegotiation = fafafa.ssl.base.sslFeatRenegotiation;
  sslFeatOCSPStapling = fafafa.ssl.base.sslFeatOCSPStapling;
  sslFeatCertificateTransparency = fafafa.ssl.base.sslFeatCertificateTransparency;

  // 密钥交换算法常量
  sslKexRSA = fafafa.ssl.base.sslKexRSA;
  sslKexDHE_RSA = fafafa.ssl.base.sslKexDHE_RSA;
  sslKexECDHE_RSA = fafafa.ssl.base.sslKexECDHE_RSA;
  sslKexDHE_DSS = fafafa.ssl.base.sslKexDHE_DSS;
  sslKexECDHE_ECDSA = fafafa.ssl.base.sslKexECDHE_ECDSA;
  sslKexPSK = fafafa.ssl.base.sslKexPSK;
  sslKexDHE_PSK = fafafa.ssl.base.sslKexDHE_PSK;
  sslKexRSA_PSK = fafafa.ssl.base.sslKexRSA_PSK;

  // 加密算法常量
  sslCipherNone = fafafa.ssl.base.sslCipherNone;
  sslCipherAES128 = fafafa.ssl.base.sslCipherAES128;
  sslCipherAES256 = fafafa.ssl.base.sslCipherAES256;
  sslCipherAES128GCM = fafafa.ssl.base.sslCipherAES128GCM;
  sslCipherAES256GCM = fafafa.ssl.base.sslCipherAES256GCM;
  sslCipherCHACHA20_POLY1305 = fafafa.ssl.base.sslCipherCHACHA20_POLY1305;
  sslCipher3DES = fafafa.ssl.base.sslCipher3DES;
  sslCipherDES = fafafa.ssl.base.sslCipherDES;
  sslCipherRC4 = fafafa.ssl.base.sslCipherRC4;

  // 哈希算法常量
  sslHashMD5 = fafafa.ssl.base.sslHashMD5;
  sslHashSHA1 = fafafa.ssl.base.sslHashSHA1;
  sslHashSHA224 = fafafa.ssl.base.sslHashSHA224;
  sslHashSHA256 = fafafa.ssl.base.sslHashSHA256;
  sslHashSHA384 = fafafa.ssl.base.sslHashSHA384;
  sslHashSHA512 = fafafa.ssl.base.sslHashSHA512;
  sslHashSHA3_256 = fafafa.ssl.base.sslHashSHA3_256;
  sslHashSHA3_512 = fafafa.ssl.base.sslHashSHA3_512;
  sslHashBLAKE2b = fafafa.ssl.base.sslHashBLAKE2b;

  // type-safety enum constants
  sslv_TLS10 = fafafa.ssl.safety.sslv_TLS10;
  sslv_TLS11 = fafafa.ssl.safety.sslv_TLS11;
  sslv_TLS12 = fafafa.ssl.safety.sslv_TLS12;
  sslv_TLS13 = fafafa.ssl.safety.sslv_TLS13;

  kt_RSA = fafafa.ssl.safety.kt_RSA;
  kt_EC = fafafa.ssl.safety.kt_EC;
  kt_DSA = fafafa.ssl.safety.kt_DSA;
  kt_Ed25519 = fafafa.ssl.safety.kt_Ed25519;
  kt_Ed448 = fafafa.ssl.safety.kt_Ed448;
  kt_X25519 = fafafa.ssl.safety.kt_X25519;
  kt_X448 = fafafa.ssl.safety.kt_X448;

  cf_PEM = fafafa.ssl.safety.cf_PEM;
  cf_DER = fafafa.ssl.safety.cf_DER;
  cf_PKCS12 = fafafa.ssl.safety.cf_PKCS12;
  cf_PKCS7 = fafafa.ssl.safety.cf_PKCS7;

  cm_GCM = fafafa.ssl.safety.cm_GCM;
  cm_CBC = fafafa.ssl.safety.cm_CBC;
  cm_CTR = fafafa.ssl.safety.cm_CTR;
  cm_CCM = fafafa.ssl.safety.cm_CCM;
  cm_OCB = fafafa.ssl.safety.cm_OCB;

  vm_None = fafafa.ssl.safety.vm_None;
  vm_Peer = fafafa.ssl.safety.vm_Peer;
  vm_FailIfNoPeerCert = fafafa.ssl.safety.vm_FailIfNoPeerCert;
  vm_ClientOnce = fafafa.ssl.safety.vm_ClientOnce;
  vm_PostHandshake = fafafa.ssl.safety.vm_PostHandshake;

  scm_Off = fafafa.ssl.safety.scm_Off;
  scm_Client = fafafa.ssl.safety.scm_Client;
  scm_Server = fafafa.ssl.safety.scm_Server;
  scm_Both = fafafa.ssl.safety.scm_Both;

  cp_Any = fafafa.ssl.safety.cp_Any;
  cp_ServerAuth = fafafa.ssl.safety.cp_ServerAuth;
  cp_ClientAuth = fafafa.ssl.safety.cp_ClientAuth;
  cp_CodeSigning = fafafa.ssl.safety.cp_CodeSigning;
  cp_EmailProtection = fafafa.ssl.safety.cp_EmailProtection;
  cp_TimeStamping = fafafa.ssl.safety.cp_TimeStamping;
  cp_OCSPSigning = fafafa.ssl.safety.cp_OCSPSigning;

  sa_RSA_PKCS1_SHA1 = fafafa.ssl.safety.sa_RSA_PKCS1_SHA1;
  sa_RSA_PKCS1_SHA256 = fafafa.ssl.safety.sa_RSA_PKCS1_SHA256;
  sa_RSA_PKCS1_SHA384 = fafafa.ssl.safety.sa_RSA_PKCS1_SHA384;
  sa_RSA_PKCS1_SHA512 = fafafa.ssl.safety.sa_RSA_PKCS1_SHA512;
  sa_RSA_PSS_SHA256 = fafafa.ssl.safety.sa_RSA_PSS_SHA256;
  sa_RSA_PSS_SHA384 = fafafa.ssl.safety.sa_RSA_PSS_SHA384;
  sa_RSA_PSS_SHA512 = fafafa.ssl.safety.sa_RSA_PSS_SHA512;
  sa_ECDSA_SHA256 = fafafa.ssl.safety.sa_ECDSA_SHA256;
  sa_ECDSA_SHA384 = fafafa.ssl.safety.sa_ECDSA_SHA384;
  sa_ECDSA_SHA512 = fafafa.ssl.safety.sa_ECDSA_SHA512;
  sa_Ed25519 = fafafa.ssl.safety.sa_Ed25519;
  sa_Ed448 = fafafa.ssl.safety.sa_Ed448;

  ec_P256 = fafafa.ssl.safety.ec_P256;
  ec_P384 = fafafa.ssl.safety.ec_P384;
  ec_P521 = fafafa.ssl.safety.ec_P521;
  ec_X25519 = fafafa.ssl.safety.ec_X25519;
  ec_X448 = fafafa.ssl.safety.ec_X448;
  ec_BrainpoolP256 = fafafa.ssl.safety.ec_BrainpoolP256;
  ec_BrainpoolP384 = fafafa.ssl.safety.ec_BrainpoolP384;
  ec_BrainpoolP512 = fafafa.ssl.safety.ec_BrainpoolP512;
  
  // 协议版本常量
  sslProtocolSSL2 = fafafa.ssl.base.sslProtocolSSL2;
  sslProtocolSSL3 = fafafa.ssl.base.sslProtocolSSL3;
  sslProtocolTLS10 = fafafa.ssl.base.sslProtocolTLS10;
  sslProtocolTLS11 = fafafa.ssl.base.sslProtocolTLS11;
  sslProtocolTLS12 = fafafa.ssl.base.sslProtocolTLS12;
  sslProtocolTLS13 = fafafa.ssl.base.sslProtocolTLS13;
  sslProtocolDTLS10 = fafafa.ssl.base.sslProtocolDTLS10;
  sslProtocolDTLS12 = fafafa.ssl.base.sslProtocolDTLS12;
  
  // 验证模式常量
  sslVerifyNone = fafafa.ssl.base.sslVerifyNone;
  sslVerifyPeer = fafafa.ssl.base.sslVerifyPeer;
  sslVerifyFailIfNoPeerCert = fafafa.ssl.base.sslVerifyFailIfNoPeerCert;
  sslVerifyClientOnce = fafafa.ssl.base.sslVerifyClientOnce;
  sslVerifyPostHandshake = fafafa.ssl.base.sslVerifyPostHandshake;
  
  // 上下文类型常量
  sslCtxClient = fafafa.ssl.base.sslCtxClient;
  sslCtxServer = fafafa.ssl.base.sslCtxServer;
  sslCtxBoth = fafafa.ssl.base.sslCtxBoth;

  // Early-data 状态与服务端策略常量
  sslEarlyDataNone = fafafa.ssl.base.sslEarlyDataNone;
  sslEarlyDataQueued = fafafa.ssl.base.sslEarlyDataQueued;
  sslEarlyDataAccepted = fafafa.ssl.base.sslEarlyDataAccepted;
  sslEarlyDataRejected = fafafa.ssl.base.sslEarlyDataRejected;
  sslEarlyDataServerReject = fafafa.ssl.base.sslEarlyDataServerReject;
  sslEarlyDataServerAccept = fafafa.ssl.base.sslEarlyDataServerAccept;
  sslEarlyDataServerIssueOnly = fafafa.ssl.base.sslEarlyDataServerIssueOnly;
  
  // 错误代码常量
  sslErrNone = fafafa.ssl.base.sslErrNone;
  sslErrGeneral = fafafa.ssl.base.sslErrGeneral;
  sslErrMemory = fafafa.ssl.base.sslErrMemory;
  sslErrInvalidParam = fafafa.ssl.base.sslErrInvalidParam;
  sslErrNotInitialized = fafafa.ssl.base.sslErrNotInitialized;
  sslErrProtocol = fafafa.ssl.base.sslErrProtocol;
  sslErrHandshake = fafafa.ssl.base.sslErrHandshake;
  sslErrCertificate = fafafa.ssl.base.sslErrCertificate;
  sslErrCertificateExpired = fafafa.ssl.base.sslErrCertificateExpired;
  sslErrCertificateRevoked = fafafa.ssl.base.sslErrCertificateRevoked;
  sslErrCertificateUnknown = fafafa.ssl.base.sslErrCertificateUnknown;
  sslErrConnection = fafafa.ssl.base.sslErrConnection;
  sslErrTimeout = fafafa.ssl.base.sslErrTimeout;
  sslErrIO = fafafa.ssl.base.sslErrIO;
  sslErrWouldBlock = fafafa.ssl.base.sslErrWouldBlock;
  sslErrUnsupported = fafafa.ssl.base.sslErrUnsupported;
  sslErrLibraryNotFound = fafafa.ssl.base.sslErrLibraryNotFound;
  sslErrFunctionNotFound = fafafa.ssl.base.sslErrFunctionNotFound;
  sslErrVersionMismatch = fafafa.ssl.base.sslErrVersionMismatch;
  
  // 默认值常量
  SSL_DEFAULT_BUFFER_SIZE = fafafa.ssl.base.SSL_DEFAULT_BUFFER_SIZE;
  SSL_DEFAULT_HANDSHAKE_TIMEOUT = fafafa.ssl.base.SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  SSL_DEFAULT_SESSION_CACHE_SIZE = fafafa.ssl.base.SSL_DEFAULT_SESSION_CACHE_SIZE;
  SSL_DEFAULT_SESSION_TIMEOUT = fafafa.ssl.base.SSL_DEFAULT_SESSION_TIMEOUT;
  SSL_DEFAULT_VERIFY_DEPTH = fafafa.ssl.base.SSL_DEFAULT_VERIFY_DEPTH;
  SSL_DEFAULT_TLS13_CIPHERSUITES = fafafa.ssl.base.SSL_DEFAULT_TLS13_CIPHERSUITES;
  SSL_DEFAULT_CIPHER_LIST = fafafa.ssl.base.SSL_DEFAULT_CIPHER_LIST;

  // 证书验证标志常量
  sslCertVerifyDefault = fafafa.ssl.base.sslCertVerifyDefault;
  sslCertVerifyCheckRevocation = fafafa.ssl.base.sslCertVerifyCheckRevocation;
  sslCertVerifyCheckOCSP = fafafa.ssl.base.sslCertVerifyCheckOCSP;
  sslCertVerifyCheckCRL = fafafa.ssl.base.sslCertVerifyCheckCRL;
  sslCertVerifyIgnoreExpiry = fafafa.ssl.base.sslCertVerifyIgnoreExpiry;
  sslCertVerifyIgnoreHostname = fafafa.ssl.base.sslCertVerifyIgnoreHostname;
  sslCertVerifyAllowSelfSigned = fafafa.ssl.base.sslCertVerifyAllowSelfSigned;
  sslCertVerifyStrictChain = fafafa.ssl.base.sslCertVerifyStrictChain;

  // OCSP 状态常量
  ocspGood = fafafa.ssl.cert.advanced.ocspGood;
  ocspRevoked = fafafa.ssl.cert.advanced.ocspRevoked;
  ocspUnknown = fafafa.ssl.cert.advanced.ocspUnknown;
  ocspError = fafafa.ssl.cert.advanced.ocspError;

// ============================================================================
// 重新导出辅助函数
// ============================================================================

// 辅助函数
function SSLErrorToString(AError: TSSLErrorCode): string;
function ProtocolVersionToString(AVersion: TSSLProtocolVersion): string;
function LibraryTypeToString(ALibType: TSSLLibraryType): string;

// type-safety supporting helper surface
function SSLVersionToString(AVersion: TSSLVersion): string;
function StringToSSLVersion(const AStr: string): TSSLVersion;
function KeyTypeToString(AType: TKeyType): string;
function CertificateFormatToString(AFormat: TCertificateFormat): string;
function CipherModeToString(AMode: TCipherMode): string;
function EllipticCurveToNID(ACurve: TEllipticCurve): Integer;
function EllipticCurveToString(ACurve: TEllipticCurve): string;

// capability / native-handle public helper surface
function IsCipherSupported(const ACaps: TSSLBackendCapabilities;
                          ACipher: TSSLCipher): Boolean;
function IsHashSupported(const ACaps: TSSLBackendCapabilities;
                        AHash: TSSLHash): Boolean;
function IsKeyExchangeSupported(const ACaps: TSSLBackendCapabilities;
                                AKex: TSSLKeyExchange): Boolean;
function IsFeatureStable(ASupport: TSSLFeatureSupportLevel): Boolean;
function IsFeatureUsable(ASupport: TSSLFeatureSupportLevel): Boolean;
function IsFeatureDeprecated(ASupport: TSSLFeatureSupportLevel): Boolean;
procedure NormalizeLegacyCapabilityBooleans(var ACaps: TSSLBackendCapabilities);
function IsNativeBackend(const ACaps: TSSLBackendCapabilities): Boolean;
function IsCLibraryBackend(const ACaps: TSSLBackendCapabilities): Boolean;
function RequiresExternalDependencies(const ACaps: TSSLBackendCapabilities): Boolean;
function GetSecurityScore(const ACaps: TSSLBackendCapabilities): Integer;
function GetPerformanceScore(const ACaps: TSSLBackendCapabilities): Integer;
function GetCapabilitiesDescription(const ACaps: TSSLBackendCapabilities): string;

// ============================================================================
// 便捷API（仍然 shipped，但不替代 TSSLFactory / TSSLConnector 主入口）
// ============================================================================

{ 初始化默认配置（fresh default-config convenience helper；若需要 library-owned defaults，请优先走 ISSLLibrary.GetDefaultConfig/SetDefaultConfig） }
function CreateDefaultConfig(AContextType: TSSLContextType = sslCtxClient): TSSLConfig;

{ 快速创建服务端 context（只返回配置好的 ISSLContext；socket bind/listen/accept 仍由应用层负责） }
function QuickServer(const ACertFile, AKeyFile: string;
  APort: Integer = 443): ISSLContext;

{ 检查SSL支持 }
function CheckSSLSupport: Boolean;
function GetSSLSupportInfo: string;

{ 证书工具 convenience helpers }
function LoadCertificate(const AFileName: string): ISSLCertificate;
function ValidateCertificate(const AFileName: string): Boolean;
function GetCertificateDetails(const AFileName: string): TSSLCertificateInfo;

{ OCSP/CRL 证书工具 facade re-export（非 TLS bootstrap 入口） }
function CreateOCSPClient: IOCSPClient;
function CreateCRLManager: ICRLManager;
function DefaultPKCS12Options: TPKCS12Options;

implementation

uses
  fafafa.ssl.openssl.backed,
  fafafa.ssl.freepascal.lib
  {$IFDEF WINDOWS}
  , fafafa.ssl.winssl.lib
  {$ENDIF}
  {$IFDEF ENABLE_MBEDTLS}
  , fafafa.ssl.mbedtls.lib
  {$ENDIF}
  {$IFDEF ENABLE_WOLFSSL}
  , fafafa.ssl.wolfssl.lib
  {$ENDIF}
  ;

// 从 fafafa.ssl.base 导入实现
function SSLErrorToString(AError: TSSLErrorCode): string;
begin
  Result := fafafa.ssl.base.SSLErrorToString(AError);
end;

function ProtocolVersionToString(AVersion: TSSLProtocolVersion): string;
begin
  Result := fafafa.ssl.base.ProtocolVersionToString(AVersion);
end;

function LibraryTypeToString(ALibType: TSSLLibraryType): string;
begin
  Result := fafafa.ssl.base.LibraryTypeToString(ALibType);
end;

function SSLVersionToString(AVersion: TSSLVersion): string;
begin
  Result := fafafa.ssl.safety.SSLVersionToString(AVersion);
end;

function StringToSSLVersion(const AStr: string): TSSLVersion;
begin
  Result := fafafa.ssl.safety.StringToSSLVersion(AStr);
end;

function KeyTypeToString(AType: TKeyType): string;
begin
  Result := fafafa.ssl.safety.KeyTypeToString(AType);
end;

function CertificateFormatToString(AFormat: TCertificateFormat): string;
begin
  Result := fafafa.ssl.safety.CertificateFormatToString(AFormat);
end;

function CipherModeToString(AMode: TCipherMode): string;
begin
  Result := fafafa.ssl.safety.CipherModeToString(AMode);
end;

function EllipticCurveToNID(ACurve: TEllipticCurve): Integer;
begin
  Result := fafafa.ssl.safety.EllipticCurveToNID(ACurve);
end;

function EllipticCurveToString(ACurve: TEllipticCurve): string;
begin
  Result := fafafa.ssl.safety.EllipticCurveToString(ACurve);
end;

function IsCipherSupported(const ACaps: TSSLBackendCapabilities;
  ACipher: TSSLCipher): Boolean;
begin
  Result := fafafa.ssl.base.IsCipherSupported(ACaps, ACipher);
end;

function IsHashSupported(const ACaps: TSSLBackendCapabilities;
  AHash: TSSLHash): Boolean;
begin
  Result := fafafa.ssl.base.IsHashSupported(ACaps, AHash);
end;

function IsKeyExchangeSupported(const ACaps: TSSLBackendCapabilities;
  AKex: TSSLKeyExchange): Boolean;
begin
  Result := fafafa.ssl.base.IsKeyExchangeSupported(ACaps, AKex);
end;

function IsFeatureStable(ASupport: TSSLFeatureSupportLevel): Boolean;
begin
  Result := fafafa.ssl.base.IsFeatureStable(ASupport);
end;

function IsFeatureUsable(ASupport: TSSLFeatureSupportLevel): Boolean;
begin
  Result := fafafa.ssl.base.IsFeatureUsable(ASupport);
end;

function IsFeatureDeprecated(ASupport: TSSLFeatureSupportLevel): Boolean;
begin
  Result := fafafa.ssl.base.IsFeatureDeprecated(ASupport);
end;

procedure NormalizeLegacyCapabilityBooleans(var ACaps: TSSLBackendCapabilities);
begin
  fafafa.ssl.base.NormalizeLegacyCapabilityBooleans(ACaps);
end;

function IsNativeBackend(const ACaps: TSSLBackendCapabilities): Boolean;
begin
  Result := fafafa.ssl.base.IsNativeBackend(ACaps);
end;

function IsCLibraryBackend(const ACaps: TSSLBackendCapabilities): Boolean;
begin
  Result := fafafa.ssl.base.IsCLibraryBackend(ACaps);
end;

function RequiresExternalDependencies(const ACaps: TSSLBackendCapabilities): Boolean;
begin
  Result := fafafa.ssl.base.RequiresExternalDependencies(ACaps);
end;

function GetSecurityScore(const ACaps: TSSLBackendCapabilities): Integer;
begin
  Result := fafafa.ssl.base.GetSecurityScore(ACaps);
end;

function GetPerformanceScore(const ACaps: TSSLBackendCapabilities): Integer;
begin
  Result := fafafa.ssl.base.GetPerformanceScore(ACaps);
end;

function GetCapabilitiesDescription(const ACaps: TSSLBackendCapabilities): string;
begin
  Result := fafafa.ssl.base.GetCapabilitiesDescription(ACaps);
end;

// 便捷API实现
function CreateDefaultConfig(AContextType: TSSLContextType): TSSLConfig;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  try
    LLib := TSSLFactory.GetLibrary(sslAutoDetect);
    LConfig := LLib.GetDefaultConfig;
    Result := LConfig;
    Result.LibraryType := sslAutoDetect;
    Result.ContextType := AContextType;
    if (AContextType = sslCtxServer) and
      (Result.VerifyMode = [sslVerifyPeer]) and
      (Trim(Result.CAFile) = '') and
      (Trim(Result.CAPath) = '') then
      Result.VerifyMode := [];
    TSSLFactory.NormalizeConfig(Result);
  except
    Result := Default(TSSLConfig);
    Result.LibraryType := sslAutoDetect;
    Result.ContextType := AContextType;
    Result.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    if AContextType = sslCtxServer then
      Result.VerifyMode := []
    else
      Result.VerifyMode := [sslVerifyPeer];
    Result.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
    Result.BufferSize := SSL_DEFAULT_BUFFER_SIZE;
    Result.HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
    Result.SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
    Result.SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
    Result.CipherList := SSL_DEFAULT_CIPHER_LIST;
    Result.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
    Result.EnableSessionTickets := True;
    Result.ClientEarlyDataEnabled := False;
    Result.ServerEarlyDataPolicy := sslEarlyDataServerReject;
    Result.ServerMaxEarlyDataSize := 0;
    Result.ServerEarlyDataReplayStoreFile := '';
    Result.ServerEarlyDataReplayStoreDirectory := '';
    Result.LogLevel := sslLogError;
  end;

  Result.LogLevel := sslLogError;
  Result.LogCallback := nil;
  TSSLFactory.NormalizeConfig(Result);
end;


function QuickServer(const ACertFile, AKeyFile: string;
  APort: Integer): ISSLContext;
begin
  // APort is provided for API consistency but socket binding is done at app layer
  if APort < 0 then; // Suppress unused parameter hint
  Result := TSSLFactory.CreateServerContext(ACertFile, AKeyFile);
  // 注意: Socket绑定和监听需要在应用层实现
  // 这个函数只创建配置好的SSL上下文
  // 使用者需要自己创建服务端socket，然后用Result.CreateConnection创建连接
end;

function CheckSSLSupport: Boolean;
var
  LLibs: TSSLLibraryTypes;
begin
  LLibs := TSSLFactory.GetAvailableLibraries;
  Result := LLibs <> [];
end;

function GetSSLSupportInfo: string;
begin
  Result := TSSLFactory.GetVersionInfo + LineEnding +
            TSSLFactory.GetSystemInfo;
end;

function LoadCertificate(const AFileName: string): ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificate;
  if not Result.LoadFromFile(AFileName) then
    raise ESSLCertificateException.Create(
      Format('无法加载证书文件: %s', [AFileName]),
      sslErrCertificate
    );
end;

function ValidateCertificate(const AFileName: string): Boolean;
begin
  Result := TSSLHelper.VerifyCertificateFile(AFileName);
end;

function GetCertificateDetails(const AFileName: string): TSSLCertificateInfo;
begin
  Result := TSSLHelper.GetCertificateInfo(AFileName);
end;

function CreateOCSPClient: IOCSPClient;
begin
  Result := fafafa.ssl.cert.advanced.CreateOCSPClient;
end;

function CreateCRLManager: ICRLManager;
begin
  Result := fafafa.ssl.cert.advanced.CreateCRLManager;
end;

function DefaultPKCS12Options: TPKCS12Options;
begin
  Result := fafafa.ssl.cert.advanced.DefaultPKCS12Options;
end;

end.
