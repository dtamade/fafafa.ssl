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
      LConnection: ISSLConnection;
    begin
      // 创建客户端上下文
      LContext := CreateSSLContext(sslCtxClient);
      
      // 配置上下文
      LContext.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);
      LContext.SetVerifyMode([sslVerifyPeer]);
      
      // 创建连接...
    end;
}

unit fafafa.ssl;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.factory;

// ============================================================================
// 重新导出所有公共类型
// ============================================================================

type
  // 从 fafafa.ssl.base 导出
  TSSLLibraryType = fafafa.ssl.base.TSSLLibraryType;
  TSSLLibraryTypes = fafafa.ssl.base.TSSLLibraryTypes;
  TSSLProtocolVersion = fafafa.ssl.base.TSSLProtocolVersion;
  TSSLProtocolVersions = fafafa.ssl.base.TSSLProtocolVersions;
  TSSLVerifyMode = fafafa.ssl.base.TSSLVerifyMode;
  TSSLVerifyModes = fafafa.ssl.base.TSSLVerifyModes;
  TSSLContextType = fafafa.ssl.base.TSSLContextType;
  TSSLHandshakeState = fafafa.ssl.base.TSSLHandshakeState;
  TSSLErrorCode = fafafa.ssl.base.TSSLErrorCode;
  TSSLLogLevel = fafafa.ssl.base.TSSLLogLevel;
  TSSLKeyExchange = fafafa.ssl.base.TSSLKeyExchange;
  TSSLCipher = fafafa.ssl.base.TSSLCipher;
  TSSLHash = fafafa.ssl.base.TSSLHash;
  
  TSSLCertificateInfo = fafafa.ssl.base.TSSLCertificateInfo;
  PSSLCertificateInfo = fafafa.ssl.base.PSSLCertificateInfo;
  TSSLConnectionInfo = fafafa.ssl.base.TSSLConnectionInfo;
  PSSLConnectionInfo = fafafa.ssl.base.PSSLConnectionInfo;
  TSSLConfig = fafafa.ssl.base.TSSLConfig;
  PSSLConfig = fafafa.ssl.base.PSSLConfig;
  TSSLStatistics = fafafa.ssl.base.TSSLStatistics;
  PSSLStatistics = fafafa.ssl.base.PSSLStatistics;
  
  // Result types (Rust-like error handling)
  TSSLOperationResult = fafafa.ssl.base.TSSLOperationResult;
  TSSLDataResult = fafafa.ssl.base.TSSLDataResult;
  TSSLStringResult = fafafa.ssl.base.TSSLStringResult;
  
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
  
  // 从 fafafa.ssl.base 导出
  ISSLLibrary = fafafa.ssl.base.ISSLLibrary;
  ISSLContext = fafafa.ssl.base.ISSLContext;
  ISSLConnection = fafafa.ssl.base.ISSLConnection;
  ISSLCertificate = fafafa.ssl.base.ISSLCertificate;
  ISSLCertificateStore = fafafa.ssl.base.ISSLCertificateStore;
  ISSLSession = fafafa.ssl.base.ISSLSession;
  
  // 从 fafafa.ssl.factory 导出
  TSSLFactory = fafafa.ssl.factory.TSSLFactory;
  TSSLHelper = fafafa.ssl.factory.TSSLHelper;

const
  // 库类型常量
  sslAutoDetect = fafafa.ssl.base.sslAutoDetect;
  sslOpenSSL = fafafa.ssl.base.sslOpenSSL;
  sslWolfSSL = fafafa.ssl.base.sslWolfSSL;
  sslMbedTLS = fafafa.ssl.base.sslMbedTLS;
  sslWinSSL = fafafa.ssl.base.sslWinSSL;
  
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

// ============================================================================
// 重新导出全局函数
// ============================================================================

// 工厂函数
function SSLFactory: TSSLFactory;
function SSLHelper: TSSLHelper;

// 快速创建函数
function CreateSSLLibrary(aLibType: TSSLLibraryType = sslAutoDetect): ISSLLibrary;
function CreateSSLContext(aType: TSSLContextType = sslCtxClient): ISSLContext;
function CreateSSLCertificate: ISSLCertificate;
function CreateSSLConnection(aContext: ISSLContext; aSocket: THandle): ISSLConnection;

// 辅助函数
function SSLErrorToString(aError: TSSLErrorCode): string;
function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
function LibraryTypeToString(aLibType: TSSLLibraryType): string;

// ============================================================================
// 便捷API
// ============================================================================

{ 初始化默认配置 }
function CreateDefaultConfig(aContextType: TSSLContextType = sslCtxClient): TSSLConfig;

{ 快速创建服务端 }
function QuickServer(const aCertFile, aKeyFile: string;
  aPort: Integer = 443): ISSLContext;

{ 检查SSL支持 }
function CheckSSLSupport: Boolean;
function GetSSLSupportInfo: string;

{ 证书工具 }
function LoadCertificate(const aFileName: string): ISSLCertificate;
function ValidateCertificate(const aFileName: string): Boolean;
function GetCertificateDetails(const aFileName: string): TSSLCertificateInfo;

implementation

uses
  fafafa.ssl.openssl.backed
  {$IFDEF WINDOWS}
  , fafafa.ssl.winssl.lib
  {$ENDIF}
  ;

// 从 fafafa.ssl.factory 导入实现
function SSLFactory: TSSLFactory;
begin
  Result := fafafa.ssl.factory.SSLFactory;
end;

function SSLHelper: TSSLHelper;
begin
  Result := fafafa.ssl.factory.SSLHelper;
end;

function CreateSSLLibrary(aLibType: TSSLLibraryType): ISSLLibrary;
begin
  Result := fafafa.ssl.factory.CreateSSLLibrary(aLibType);
end;

function CreateSSLContext(aType: TSSLContextType): ISSLContext;
begin
  Result := fafafa.ssl.factory.CreateSSLContext(aType);
end;

function CreateSSLCertificate: ISSLCertificate;
begin
  Result := fafafa.ssl.factory.CreateSSLCertificate;
end;

function CreateSSLConnection(aContext: ISSLContext; aSocket: THandle): ISSLConnection;
begin
  Result := fafafa.ssl.factory.CreateSSLConnection(aContext, aSocket);
end;

// 从 fafafa.ssl.base 导入实现
function SSLErrorToString(aError: TSSLErrorCode): string;
begin
  Result := fafafa.ssl.base.SSLErrorToString(aError);
end;

function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
begin
  Result := fafafa.ssl.base.ProtocolVersionToString(aVersion);
end;

function LibraryTypeToString(aLibType: TSSLLibraryType): string;
begin
  Result := fafafa.ssl.base.LibraryTypeToString(aLibType);
end;

// 便捷API实现
function CreateDefaultConfig(aContextType: TSSLContextType): TSSLConfig;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  try
    LLib := TSSLFactory.GetLibrary(sslAutoDetect);
    LConfig := LLib.GetDefaultConfig;
    Result := LConfig;
    Result.LibraryType := sslAutoDetect;
    Result.ContextType := aContextType;
    TSSLFactory.NormalizeConfig(Result);
  except
    FillChar(Result, SizeOf(Result), 0);
    Result.LibraryType := sslAutoDetect;
    Result.ContextType := aContextType;
    Result.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    Result.VerifyMode := [sslVerifyPeer];
    Result.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
    Result.BufferSize := SSL_DEFAULT_BUFFER_SIZE;
    Result.HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
    Result.SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
    Result.SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
    Result.CipherList := SSL_DEFAULT_CIPHER_LIST;
    Result.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
    Result.EnableSessionTickets := True;
    Result.LogLevel := sslLogError;
    TSSLFactory.NormalizeConfig(Result);
  end;
end;


function QuickServer(const aCertFile, aKeyFile: string;
  aPort: Integer): ISSLContext;
begin
  Result := TSSLFactory.CreateServerContext(aCertFile, aKeyFile);
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

function LoadCertificate(const aFileName: string): ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificate;
  if not Result.LoadFromFile(aFileName) then
    raise ESSLCertificateException.Create(
      Format('无法加载证书文件: %s', [aFileName]),
      sslErrCertificate
    );
end;

function ValidateCertificate(const aFileName: string): Boolean;
begin
  Result := TSSLHelper.VerifyCertificateFile(aFileName);
end;

function GetCertificateDetails(const aFileName: string): TSSLCertificateInfo;
begin
  Result := TSSLHelper.GetCertificateInfo(aFileName);
end;

end.