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
  fafafa.ssl.types,
  fafafa.ssl.intf,
  fafafa.ssl.factory;

// ============================================================================
// 重新导出所有公共类型
// ============================================================================

type
  // 从 fafafa.ssl.types 导出
  TSSLLibraryType = fafafa.ssl.types.TSSLLibraryType;
  TSSLLibraryTypes = fafafa.ssl.types.TSSLLibraryTypes;
  TSSLProtocolVersion = fafafa.ssl.types.TSSLProtocolVersion;
  TSSLProtocolVersions = fafafa.ssl.types.TSSLProtocolVersions;
  TSSLVerifyMode = fafafa.ssl.types.TSSLVerifyMode;
  TSSLVerifyModes = fafafa.ssl.types.TSSLVerifyModes;
  TSSLContextType = fafafa.ssl.types.TSSLContextType;
  TSSLHandshakeState = fafafa.ssl.types.TSSLHandshakeState;
  TSSLErrorCode = fafafa.ssl.types.TSSLErrorCode;
  TSSLLogLevel = fafafa.ssl.types.TSSLLogLevel;
  TSSLKeyExchange = fafafa.ssl.types.TSSLKeyExchange;
  TSSLCipher = fafafa.ssl.types.TSSLCipher;
  TSSLHash = fafafa.ssl.types.TSSLHash;
  
  TSSLCertificateInfo = fafafa.ssl.types.TSSLCertificateInfo;
  PSSLCertificateInfo = fafafa.ssl.types.PSSLCertificateInfo;
  TSSLConnectionInfo = fafafa.ssl.types.TSSLConnectionInfo;
  PSSLConnectionInfo = fafafa.ssl.types.PSSLConnectionInfo;
  TSSLConfig = fafafa.ssl.types.TSSLConfig;
  PSSLConfig = fafafa.ssl.types.PSSLConfig;
  TSSLStatistics = fafafa.ssl.types.TSSLStatistics;
  PSSLStatistics = fafafa.ssl.types.PSSLStatistics;
  
  ESSLException = fafafa.ssl.types.ESSLException;
  ESSLHandshakeException = fafafa.ssl.types.ESSLHandshakeException;
  ESSLCertificateException = fafafa.ssl.types.ESSLCertificateException;
  ESSLProtocolException = fafafa.ssl.types.ESSLProtocolException;
  ESSLConnectionException = fafafa.ssl.types.ESSLConnectionException;
  ESSLTimeoutException = fafafa.ssl.types.ESSLTimeoutException;
  ESSLLibraryException = fafafa.ssl.types.ESSLLibraryException;
  
  TSSLVerifyCallback = fafafa.ssl.types.TSSLVerifyCallback;
  TSSLPasswordCallback = fafafa.ssl.types.TSSLPasswordCallback;
  TSSLInfoCallback = fafafa.ssl.types.TSSLInfoCallback;
  TSSLDataCallback = fafafa.ssl.types.TSSLDataCallback;
  
  // 从 fafafa.ssl.intf 导出
  ISSLLibrary = fafafa.ssl.intf.ISSLLibrary;
  ISSLContext = fafafa.ssl.intf.ISSLContext;
  ISSLConnection = fafafa.ssl.intf.ISSLConnection;
  ISSLCertificate = fafafa.ssl.intf.ISSLCertificate;
  ISSLCertificateStore = fafafa.ssl.intf.ISSLCertificateStore;
  ISSLSession = fafafa.ssl.intf.ISSLSession;
  
  // 从 fafafa.ssl.factory 导出
  TSSLFactory = fafafa.ssl.factory.TSSLFactory;
  TSSLHelper = fafafa.ssl.factory.TSSLHelper;

const
  // 库类型常量
  sslAutoDetect = fafafa.ssl.types.sslAutoDetect;
  sslOpenSSL = fafafa.ssl.types.sslOpenSSL;
  sslWolfSSL = fafafa.ssl.types.sslWolfSSL;
  sslMbedTLS = fafafa.ssl.types.sslMbedTLS;
  sslWinSSL = fafafa.ssl.types.sslWinSSL;
  
  // 协议版本常量
  sslProtocolSSL2 = fafafa.ssl.types.sslProtocolSSL2;
  sslProtocolSSL3 = fafafa.ssl.types.sslProtocolSSL3;
  sslProtocolTLS10 = fafafa.ssl.types.sslProtocolTLS10;
  sslProtocolTLS11 = fafafa.ssl.types.sslProtocolTLS11;
  sslProtocolTLS12 = fafafa.ssl.types.sslProtocolTLS12;
  sslProtocolTLS13 = fafafa.ssl.types.sslProtocolTLS13;
  sslProtocolDTLS10 = fafafa.ssl.types.sslProtocolDTLS10;
  sslProtocolDTLS12 = fafafa.ssl.types.sslProtocolDTLS12;
  
  // 验证模式常量
  sslVerifyNone = fafafa.ssl.types.sslVerifyNone;
  sslVerifyPeer = fafafa.ssl.types.sslVerifyPeer;
  sslVerifyFailIfNoPeerCert = fafafa.ssl.types.sslVerifyFailIfNoPeerCert;
  sslVerifyClientOnce = fafafa.ssl.types.sslVerifyClientOnce;
  sslVerifyPostHandshake = fafafa.ssl.types.sslVerifyPostHandshake;
  
  // 上下文类型常量
  sslCtxClient = fafafa.ssl.types.sslCtxClient;
  sslCtxServer = fafafa.ssl.types.sslCtxServer;
  sslCtxBoth = fafafa.ssl.types.sslCtxBoth;
  
  // 错误代码常量
  sslErrNone = fafafa.ssl.types.sslErrNone;
  sslErrGeneral = fafafa.ssl.types.sslErrGeneral;
  sslErrMemory = fafafa.ssl.types.sslErrMemory;
  sslErrInvalidParam = fafafa.ssl.types.sslErrInvalidParam;
  sslErrNotInitialized = fafafa.ssl.types.sslErrNotInitialized;
  sslErrProtocol = fafafa.ssl.types.sslErrProtocol;
  sslErrHandshake = fafafa.ssl.types.sslErrHandshake;
  sslErrCertificate = fafafa.ssl.types.sslErrCertificate;
  sslErrCertificateExpired = fafafa.ssl.types.sslErrCertificateExpired;
  sslErrCertificateRevoked = fafafa.ssl.types.sslErrCertificateRevoked;
  sslErrCertificateUnknown = fafafa.ssl.types.sslErrCertificateUnknown;
  sslErrConnection = fafafa.ssl.types.sslErrConnection;
  sslErrTimeout = fafafa.ssl.types.sslErrTimeout;
  sslErrIO = fafafa.ssl.types.sslErrIO;
  sslErrWouldBlock = fafafa.ssl.types.sslErrWouldBlock;
  sslErrUnsupported = fafafa.ssl.types.sslErrUnsupported;
  sslErrLibraryNotFound = fafafa.ssl.types.sslErrLibraryNotFound;
  sslErrFunctionNotFound = fafafa.ssl.types.sslErrFunctionNotFound;
  sslErrVersionMismatch = fafafa.ssl.types.sslErrVersionMismatch;
  
  // 默认值常量
  SSL_DEFAULT_BUFFER_SIZE = fafafa.ssl.types.SSL_DEFAULT_BUFFER_SIZE;
  SSL_DEFAULT_HANDSHAKE_TIMEOUT = fafafa.ssl.types.SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  SSL_DEFAULT_SESSION_CACHE_SIZE = fafafa.ssl.types.SSL_DEFAULT_SESSION_CACHE_SIZE;
  SSL_DEFAULT_SESSION_TIMEOUT = fafafa.ssl.types.SSL_DEFAULT_SESSION_TIMEOUT;
  SSL_DEFAULT_VERIFY_DEPTH = fafafa.ssl.types.SSL_DEFAULT_VERIFY_DEPTH;
  SSL_DEFAULT_TLS13_CIPHERSUITES = fafafa.ssl.types.SSL_DEFAULT_TLS13_CIPHERSUITES;
  SSL_DEFAULT_CIPHER_LIST = fafafa.ssl.types.SSL_DEFAULT_CIPHER_LIST;

// ============================================================================
// 重新导出全局函数
// ============================================================================

// 工厂函数
function SSLFactory: TSSLFactory;
function SSLHelper: TSSLHelper;

// 快速创建函数
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

{ 快速创建客户端连接 }
function QuickConnect(const aHost: string; aPort: Integer;
  aVerifyMode: TSSLVerifyModes = [sslVerifyPeer]): ISSLConnection;

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

// 从 fafafa.ssl.factory 导入实现
function SSLFactory: TSSLFactory;
begin
  Result := fafafa.ssl.factory.SSLFactory;
end;

function SSLHelper: TSSLHelper;
begin
  Result := fafafa.ssl.factory.SSLHelper;
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

// 从 fafafa.ssl.intf 导入实现
function SSLErrorToString(aError: TSSLErrorCode): string;
begin
  Result := fafafa.ssl.intf.SSLErrorToString(aError);
end;

function ProtocolVersionToString(aVersion: TSSLProtocolVersion): string;
begin
  Result := fafafa.ssl.intf.ProtocolVersionToString(aVersion);
end;

function LibraryTypeToString(aLibType: TSSLLibraryType): string;
begin
  Result := fafafa.ssl.intf.LibraryTypeToString(aLibType);
end;

// 便捷API实现
function CreateDefaultConfig(aContextType: TSSLContextType): TSSLConfig;
begin
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
end;

function QuickConnect(const aHost: string; aPort: Integer;
  aVerifyMode: TSSLVerifyModes): ISSLConnection;
begin
  Result := TSSLFactory.CreateClientConnection(aHost, aPort, aVerifyMode);
end;

function QuickServer(const aCertFile, aKeyFile: string;
  aPort: Integer): ISSLContext;
begin
  Result := TSSLFactory.CreateServerContext(aCertFile, aKeyFile);
  // TODO: 绑定到指定端口
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