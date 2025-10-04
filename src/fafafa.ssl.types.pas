{ 
  fafafa.ssl.types - SSL/TLS 类型定义（向后兼容层）
  
  版本: 2.0
  作者: fafafa.ssl 开发团队
  创建: 2025-09-28
  更新: 2025-10-04
  
  描述:
    此模块是向后兼容层,重新导出 fafafa.ssl.abstract.types 中的所有类型。
    保留此模块是为了不破坏现有代码,所有新代码应直接使用 fafafa.ssl.abstract.types。
    
  架构位置:
    兼容层 - 导出抽象类型以保持向后兼容
    
  迁移说明:
    为了使用新架构,建议将:
      uses fafafa.ssl.types;
    改为:
      uses fafafa.ssl.abstract.types;
}

unit fafafa.ssl.types;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  fafafa.ssl.abstract.types;

// 重新导出所有类型
type
  // 基础类型
  TSSLProc = fafafa.ssl.abstract.types.TSSLProc;
  TSSLProcString = fafafa.ssl.abstract.types.TSSLProcString;
  
  // 枚举类型
  TSSLLibraryType = fafafa.ssl.abstract.types.TSSLLibraryType;
  TSSLLibraryTypes = fafafa.ssl.abstract.types.TSSLLibraryTypes;
  TSSLProtocolVersion = fafafa.ssl.abstract.types.TSSLProtocolVersion;
  TSSLProtocolVersions = fafafa.ssl.abstract.types.TSSLProtocolVersions;
  TSSLVerifyMode = fafafa.ssl.abstract.types.TSSLVerifyMode;
  TSSLVerifyModes = fafafa.ssl.abstract.types.TSSLVerifyModes;
  TSSLContextType = fafafa.ssl.abstract.types.TSSLContextType;
  TSSLOption = fafafa.ssl.abstract.types.TSSLOption;
  TSSLOptions = fafafa.ssl.abstract.types.TSSLOptions;
  TSSLHandshakeState = fafafa.ssl.abstract.types.TSSLHandshakeState;
  TSSLErrorCode = fafafa.ssl.abstract.types.TSSLErrorCode;
  TSSLLogLevel = fafafa.ssl.abstract.types.TSSLLogLevel;
  TSSLKeyExchange = fafafa.ssl.abstract.types.TSSLKeyExchange;
  TSSLCipher = fafafa.ssl.abstract.types.TSSLCipher;
  TSSLHash = fafafa.ssl.abstract.types.TSSLHash;
  
  // 记录类型
  TSSLCertificateInfo = fafafa.ssl.abstract.types.TSSLCertificateInfo;
  PSSLCertificateInfo = fafafa.ssl.abstract.types.PSSLCertificateInfo;
  TSSLConnectionInfo = fafafa.ssl.abstract.types.TSSLConnectionInfo;
  PSSLConnectionInfo = fafafa.ssl.abstract.types.PSSLConnectionInfo;
  TSSLConfig = fafafa.ssl.abstract.types.TSSLConfig;
  PSSLConfig = fafafa.ssl.abstract.types.PSSLConfig;
  TSSLStatistics = fafafa.ssl.abstract.types.TSSLStatistics;
  PSSLStatistics = fafafa.ssl.abstract.types.PSSLStatistics;
  
  // 异常类型
  ESSLException = fafafa.ssl.abstract.types.ESSLException;
  ESSLHandshakeException = fafafa.ssl.abstract.types.ESSLHandshakeException;
  ESSLCertificateException = fafafa.ssl.abstract.types.ESSLCertificateException;
  ESSLProtocolException = fafafa.ssl.abstract.types.ESSLProtocolException;
  ESSLConnectionException = fafafa.ssl.abstract.types.ESSLConnectionException;
  ESSLTimeoutException = fafafa.ssl.abstract.types.ESSLTimeoutException;
  ESSLLibraryException = fafafa.ssl.abstract.types.ESSLLibraryException;
  
  // 回调类型
  TSSLLogCallback = fafafa.ssl.abstract.types.TSSLLogCallback;
  TSSLVerifyCallback = fafafa.ssl.abstract.types.TSSLVerifyCallback;
  TSSLPasswordCallback = fafafa.ssl.abstract.types.TSSLPasswordCallback;
  TSSLInfoCallback = fafafa.ssl.abstract.types.TSSLInfoCallback;
  TSSLDataCallback = fafafa.ssl.abstract.types.TSSLDataCallback;

const
  // 重新导出所有枚举常量 - TSSLLibraryType
  sslAutoDetect = fafafa.ssl.abstract.types.sslAutoDetect;
  sslOpenSSL = fafafa.ssl.abstract.types.sslOpenSSL;
  sslWolfSSL = fafafa.ssl.abstract.types.sslWolfSSL;
  sslMbedTLS = fafafa.ssl.abstract.types.sslMbedTLS;
  sslWinSSL = fafafa.ssl.abstract.types.sslWinSSL;
  
  // TSSLProtocolVersion
  sslProtocolSSL2 = fafafa.ssl.abstract.types.sslProtocolSSL2;
  sslProtocolSSL3 = fafafa.ssl.abstract.types.sslProtocolSSL3;
  sslProtocolTLS10 = fafafa.ssl.abstract.types.sslProtocolTLS10;
  sslProtocolTLS11 = fafafa.ssl.abstract.types.sslProtocolTLS11;
  sslProtocolTLS12 = fafafa.ssl.abstract.types.sslProtocolTLS12;
  sslProtocolTLS13 = fafafa.ssl.abstract.types.sslProtocolTLS13;
  sslProtocolDTLS10 = fafafa.ssl.abstract.types.sslProtocolDTLS10;
  sslProtocolDTLS12 = fafafa.ssl.abstract.types.sslProtocolDTLS12;
  
  // TSSLVerifyMode
  sslVerifyNone = fafafa.ssl.abstract.types.sslVerifyNone;
  sslVerifyPeer = fafafa.ssl.abstract.types.sslVerifyPeer;
  sslVerifyFailIfNoPeerCert = fafafa.ssl.abstract.types.sslVerifyFailIfNoPeerCert;
  sslVerifyClientOnce = fafafa.ssl.abstract.types.sslVerifyClientOnce;
  sslVerifyPostHandshake = fafafa.ssl.abstract.types.sslVerifyPostHandshake;
  
  // TSSLContextType
  sslCtxClient = fafafa.ssl.abstract.types.sslCtxClient;
  sslCtxServer = fafafa.ssl.abstract.types.sslCtxServer;
  sslCtxBoth = fafafa.ssl.abstract.types.sslCtxBoth;
  
  // TSSLOption - 所有选项
  ssoEnableSNI = fafafa.ssl.abstract.types.ssoEnableSNI;
  ssoEnableALPN = fafafa.ssl.abstract.types.ssoEnableALPN;
  ssoEnableSessionCache = fafafa.ssl.abstract.types.ssoEnableSessionCache;
  ssoEnableSessionTickets = fafafa.ssl.abstract.types.ssoEnableSessionTickets;
  ssoDisableCompression = fafafa.ssl.abstract.types.ssoDisableCompression;
  ssoDisableRenegotiation = fafafa.ssl.abstract.types.ssoDisableRenegotiation;
  ssoEnableOCSPStapling = fafafa.ssl.abstract.types.ssoEnableOCSPStapling;
  ssoSingleDHUse = fafafa.ssl.abstract.types.ssoSingleDHUse;
  ssoSingleECDHUse = fafafa.ssl.abstract.types.ssoSingleECDHUse;
  ssoCipherServerPreference = fafafa.ssl.abstract.types.ssoCipherServerPreference;
  ssoNoSSLv2 = fafafa.ssl.abstract.types.ssoNoSSLv2;
  ssoNoSSLv3 = fafafa.ssl.abstract.types.ssoNoSSLv3;
  ssoNoTLSv1 = fafafa.ssl.abstract.types.ssoNoTLSv1;
  ssoNoTLSv1_1 = fafafa.ssl.abstract.types.ssoNoTLSv1_1;
  ssoNoTLSv1_2 = fafafa.ssl.abstract.types.ssoNoTLSv1_2;
  ssoNoTLSv1_3 = fafafa.ssl.abstract.types.ssoNoTLSv1_3;
  
  // TSSLHandshakeState
  sslHsNotStarted = fafafa.ssl.abstract.types.sslHsNotStarted;
  sslHsInProgress = fafafa.ssl.abstract.types.sslHsInProgress;
  sslHsCompleted = fafafa.ssl.abstract.types.sslHsCompleted;
  sslHsFailed = fafafa.ssl.abstract.types.sslHsFailed;
  sslHsRenegotiating = fafafa.ssl.abstract.types.sslHsRenegotiating;
  
  // TSSLErrorCode - 所有错误码
  sslErrNone = fafafa.ssl.abstract.types.sslErrNone;
  sslErrGeneral = fafafa.ssl.abstract.types.sslErrGeneral;
  sslErrMemory = fafafa.ssl.abstract.types.sslErrMemory;
  sslErrInvalidParam = fafafa.ssl.abstract.types.sslErrInvalidParam;
  sslErrNotInitialized = fafafa.ssl.abstract.types.sslErrNotInitialized;
  sslErrProtocol = fafafa.ssl.abstract.types.sslErrProtocol;
  sslErrHandshake = fafafa.ssl.abstract.types.sslErrHandshake;
  sslErrCertificate = fafafa.ssl.abstract.types.sslErrCertificate;
  sslErrCertificateExpired = fafafa.ssl.abstract.types.sslErrCertificateExpired;
  sslErrCertificateRevoked = fafafa.ssl.abstract.types.sslErrCertificateRevoked;
  sslErrCertificateUnknown = fafafa.ssl.abstract.types.sslErrCertificateUnknown;
  sslErrConnection = fafafa.ssl.abstract.types.sslErrConnection;
  sslErrTimeout = fafafa.ssl.abstract.types.sslErrTimeout;
  sslErrIO = fafafa.ssl.abstract.types.sslErrIO;
  sslErrWouldBlock = fafafa.ssl.abstract.types.sslErrWouldBlock;
  sslErrUnsupported = fafafa.ssl.abstract.types.sslErrUnsupported;
  sslErrLibraryNotFound = fafafa.ssl.abstract.types.sslErrLibraryNotFound;
  sslErrFunctionNotFound = fafafa.ssl.abstract.types.sslErrFunctionNotFound;
  sslErrVersionMismatch = fafafa.ssl.abstract.types.sslErrVersionMismatch;
  
  // TSSLLogLevel
  sslLogNone = fafafa.ssl.abstract.types.sslLogNone;
  sslLogError = fafafa.ssl.abstract.types.sslLogError;
  sslLogWarning = fafafa.ssl.abstract.types.sslLogWarning;
  sslLogInfo = fafafa.ssl.abstract.types.sslLogInfo;
  sslLogDebug = fafafa.ssl.abstract.types.sslLogDebug;
  sslLogTrace = fafafa.ssl.abstract.types.sslLogTrace;
  
  // TSSLKeyExchange
  sslKexRSA = fafafa.ssl.abstract.types.sslKexRSA;
  sslKexDHE_RSA = fafafa.ssl.abstract.types.sslKexDHE_RSA;
  sslKexECDHE_RSA = fafafa.ssl.abstract.types.sslKexECDHE_RSA;
  sslKexDHE_DSS = fafafa.ssl.abstract.types.sslKexDHE_DSS;
  sslKexECDHE_ECDSA = fafafa.ssl.abstract.types.sslKexECDHE_ECDSA;
  sslKexPSK = fafafa.ssl.abstract.types.sslKexPSK;
  sslKexDHE_PSK = fafafa.ssl.abstract.types.sslKexDHE_PSK;
  sslKexRSA_PSK = fafafa.ssl.abstract.types.sslKexRSA_PSK;
  
  // TSSLCipher
  sslCipherNone = fafafa.ssl.abstract.types.sslCipherNone;
  sslCipherAES128 = fafafa.ssl.abstract.types.sslCipherAES128;
  sslCipherAES256 = fafafa.ssl.abstract.types.sslCipherAES256;
  sslCipherAES128GCM = fafafa.ssl.abstract.types.sslCipherAES128GCM;
  sslCipherAES256GCM = fafafa.ssl.abstract.types.sslCipherAES256GCM;
  sslCipherCHACHA20_POLY1305 = fafafa.ssl.abstract.types.sslCipherCHACHA20_POLY1305;
  sslCipher3DES = fafafa.ssl.abstract.types.sslCipher3DES;
  sslCipherDES = fafafa.ssl.abstract.types.sslCipherDES;
  sslCipherRC4 = fafafa.ssl.abstract.types.sslCipherRC4;
  
  // TSSLHash
  sslHashNone = fafafa.ssl.abstract.types.sslHashNone;
  sslHashMD5 = fafafa.ssl.abstract.types.sslHashMD5;
  sslHashSHA1 = fafafa.ssl.abstract.types.sslHashSHA1;
  sslHashSHA256 = fafafa.ssl.abstract.types.sslHashSHA256;
  sslHashSHA384 = fafafa.ssl.abstract.types.sslHashSHA384;
  sslHashSHA512 = fafafa.ssl.abstract.types.sslHashSHA512;
  
  // 注意: 数组常量无法直接重导出,请直接使用 fafafa.ssl.abstract.types 中的常量
  // SSL_LIBRARY_NAMES, SSL_PROTOCOL_NAMES, SSL_ERROR_MESSAGES
  
  SSL_DEFAULT_BUFFER_SIZE = fafafa.ssl.abstract.types.SSL_DEFAULT_BUFFER_SIZE;
  SSL_DEFAULT_HANDSHAKE_TIMEOUT = fafafa.ssl.abstract.types.SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  SSL_DEFAULT_SESSION_CACHE_SIZE = fafafa.ssl.abstract.types.SSL_DEFAULT_SESSION_CACHE_SIZE;
  SSL_DEFAULT_SESSION_TIMEOUT = fafafa.ssl.abstract.types.SSL_DEFAULT_SESSION_TIMEOUT;
  SSL_DEFAULT_VERIFY_DEPTH = fafafa.ssl.abstract.types.SSL_DEFAULT_VERIFY_DEPTH;
  
  SSL_DEFAULT_TLS13_CIPHERSUITES = fafafa.ssl.abstract.types.SSL_DEFAULT_TLS13_CIPHERSUITES;
  SSL_DEFAULT_CIPHER_LIST = fafafa.ssl.abstract.types.SSL_DEFAULT_CIPHER_LIST;

implementation

end.
