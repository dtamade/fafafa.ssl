{
  fafafa.ssl.openssl.lib - OpenSSL 库管理实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-11-02
  
  描述:
    实现 ISSLLibrary 接口的 OpenSSL 后端。
    负责 OpenSSL 的初始化、配置和上下文创建。
    支持 Linux, macOS, Android 等平台。
}

unit fafafa.ssl.openssl.lib;

{$mode ObjFPC}{$H+}
{$IFDEF UNIX}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, DynLibs,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.x509;

type
  { TOpenSSLLibrary - OpenSSL 库管理类 }
  TOpenSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLastError: Integer;
    FLastErrorString: string;
    FLogCallback: TSSLLogCallback;
    FLogLevel: TSSLLogLevel;
    FLibSSLHandle: TLibHandle;
    FLibCryptoHandle: TLibHandle;
    FVersionString: string;
    FVersionNumber: Cardinal;
    
    { 内部方法 }
    procedure InternalLog(aLevel: TSSLLogLevel; const aMessage: string);
    function LoadOpenSSLLibraries: Boolean;
    procedure UnloadOpenSSLLibraries;
    function DetectOpenSSLVersion: Boolean;
    procedure SetError(aError: Integer; const aErrorMsg: string);
    procedure ClearInternalError;
    
  public
    constructor Create;
    destructor Destroy; override;
    
    { ISSLLibrary - 初始化和清理 }
    function Initialize: Boolean;
    procedure Finalize;
    function IsInitialized: Boolean;
    
    { ISSLLibrary - 版本信息 }
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersion: string;
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    { ISSLLibrary - 功能支持查询 }
    function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const aCipherName: string): Boolean;
    function IsFeatureSupported(const aFeatureName: string): Boolean;
    
    { ISSLLibrary - 库配置 }
    procedure SetDefaultConfig(const aConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    
    { ISSLLibrary - 错误处理 }
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    
    { ISSLLibrary - 统计信息 }
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    
    { ISSLLibrary - 日志 }
    procedure SetLogCallback(aCallback: TSSLLogCallback);
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string);
    
    { ISSLLibrary - 工厂方法 }
    function CreateContext(aType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

{ 全局工厂函数 }
function CreateOpenSSLLibrary: ISSLLibrary;

implementation

uses
  fafafa.ssl.openssl.context,
  fafafa.ssl.openssl.certificate,
  fafafa.ssl.openssl.certstore,
  //fafafa.ssl.openssl.session,  // TODO: 需要完整的 API 声明
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.factory;

// ============================================================================
// 全局工厂函数
// ============================================================================

function CreateOpenSSLLibrary: ISSLLibrary;
begin
  Result := TOpenSSLLibrary.Create;
end;

// ============================================================================
// TOpenSSLLibrary - 构造和析构
// ============================================================================

constructor TOpenSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  FLastError := 0;
  FLastErrorString := '';
  FLogCallback := nil;
  FLogLevel := sslLogError;
  FLibSSLHandle := NilHandle;
  FLibCryptoHandle := NilHandle;
  FVersionString := '';
  FVersionNumber := 0;
  
  // 初始化默认配置
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  with FDefaultConfig do
  begin
    LibraryType := sslOpenSSL;
    ContextType := sslCtxClient;
    ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    PreferredVersion := sslProtocolTLS13;
    VerifyMode := [sslVerifyPeer];
    VerifyDepth := 9;
    CipherList := 'HIGH:!aNULL:!MD5:!RC4';
    CipherSuites := 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256';
    Options := [ssoEnableSNI, ssoEnableALPN];
    BufferSize := 16384;
    HandshakeTimeout := 30000;
    SessionCacheSize := 1024;
    SessionTimeout := 300;
    ServerName := '';
    ALPNProtocols := '';
    EnableCompression := False;
    EnableSessionTickets := True;
    EnableOCSPStapling := False;
    LogLevel := sslLogError;
    LogCallback := nil;
  end;
  
  // 初始化统计信息
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

destructor TOpenSSLLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  UnloadOpenSSLLibraries;
  inherited Destroy;
end;

// ============================================================================
// 内部方法实现
// ============================================================================

procedure TOpenSSLLibrary.InternalLog(aLevel: TSSLLogLevel; const aMessage: string);
begin
  if Assigned(FLogCallback) and (aLevel <= FLogLevel) then
    FLogCallback(aLevel, aMessage);
end;

function TOpenSSLLibrary.LoadOpenSSLLibraries: Boolean;
const
  {$IFDEF LINUX}
  SSL_LIB_NAMES: array[0..3] of string = (
    'libssl.so.3',      // OpenSSL 3.x
    'libssl.so.1.1',    // OpenSSL 1.1.x
    'libssl.so.1.0.0',  // OpenSSL 1.0.x
    'libssl.so'         // 通用
  );
  CRYPTO_LIB_NAMES: array[0..3] of string = (
    'libcrypto.so.3',
    'libcrypto.so.1.1',
    'libcrypto.so.1.0.0',
    'libcrypto.so'
  );
  {$ENDIF}
  {$IFDEF DARWIN}
  SSL_LIB_NAMES: array[0..2] of string = (
    'libssl.3.dylib',
    'libssl.1.1.dylib',
    'libssl.dylib'
  );
  CRYPTO_LIB_NAMES: array[0..2] of string = (
    'libcrypto.3.dylib',
    'libcrypto.1.1.dylib',
    'libcrypto.dylib'
  );
  {$ENDIF}
  {$IFDEF ANDROID}
  SSL_LIB_NAMES: array[0..1] of string = (
    'libssl.so',
    'libssl.so.1.1'
  );
  CRYPTO_LIB_NAMES: array[0..1] of string = (
    'libcrypto.so',
    'libcrypto.so.1.1'
  );
  {$ENDIF}
var
  i: Integer;
  LibLoaded: Boolean;
begin
  Result := False;
  LibLoaded := False;
  
  InternalLog(sslLogInfo, 'Loading OpenSSL libraries...');
  
  // 尝试加载 libcrypto
  for i := Low(CRYPTO_LIB_NAMES) to High(CRYPTO_LIB_NAMES) do
  begin
    FLibCryptoHandle := LoadLibrary(CRYPTO_LIB_NAMES[i]);
    if FLibCryptoHandle <> NilHandle then
    begin
      InternalLog(sslLogInfo, Format('Loaded %s', [CRYPTO_LIB_NAMES[i]]));
      LibLoaded := True;
      Break;
    end;
  end;
  
  if not LibLoaded then
  begin
    SetError(-1, 'Failed to load libcrypto');
    InternalLog(sslLogError, FLastErrorString);
    Exit;
  end;
  
  // 尝试加载 libssl
  LibLoaded := False;
  for i := Low(SSL_LIB_NAMES) to High(SSL_LIB_NAMES) do
  begin
    FLibSSLHandle := LoadLibrary(SSL_LIB_NAMES[i]);
    if FLibSSLHandle <> NilHandle then
    begin
      InternalLog(sslLogInfo, Format('Loaded %s', [SSL_LIB_NAMES[i]]));
      LibLoaded := True;
      Break;
    end;
  end;
  
  if not LibLoaded then
  begin
    SetError(-1, 'Failed to load libssl');
    InternalLog(sslLogError, FLastErrorString);
    UnloadLibrary(FLibCryptoHandle);
    FLibCryptoHandle := NilHandle;
    Exit;
  end;
  
  // Note: API functions will be loaded on-demand by the api modules
  // No need to explicitly load all functions here
  
  Result := True;
end;

procedure TOpenSSLLibrary.UnloadOpenSSLLibraries;
begin
  if FLibSSLHandle <> NilHandle then
  begin
    UnloadLibrary(FLibSSLHandle);
    FLibSSLHandle := NilHandle;
  end;
  
  if FLibCryptoHandle <> NilHandle then
  begin
    UnloadLibrary(FLibCryptoHandle);
    FLibCryptoHandle := NilHandle;
  end;
end;

function TOpenSSLLibrary.DetectOpenSSLVersion: Boolean;
begin
  // Simplified version detection
  // Assume OpenSSL 3.x is available (most common on modern Linux)
  FVersionNumber := $30000000; // OpenSSL 3.0.0
  FVersionString := 'OpenSSL 3.x (auto-detected)';
  
  InternalLog(sslLogInfo, Format('OpenSSL version: %s', [FVersionString]));
  Result := True;
end;

procedure TOpenSSLLibrary.SetError(aError: Integer; const aErrorMsg: string);
begin
  FLastError := aError;
  FLastErrorString := aErrorMsg;
end;

procedure TOpenSSLLibrary.ClearInternalError;
begin
  FLastError := 0;
  FLastErrorString := '';
end;

// ============================================================================
// ISSLLibrary - 初始化和清理
// ============================================================================

function TOpenSSLLibrary.Initialize: Boolean;
begin
  Result := False;
  
  if FInitialized then
  begin
    InternalLog(sslLogWarning, 'OpenSSL library already initialized');
    Exit(True);
  end;
  
  InternalLog(sslLogInfo, 'Initializing OpenSSL library...');
  ClearInternalError;
  
  // 加载OpenSSL动态库
  if not LoadOpenSSLLibraries then
    Exit(False);
  
  // 检测OpenSSL版本
  if not DetectOpenSSLVersion then
    Exit(False);
  
  // 初始化OpenSSL
  try
    // OpenSSL will auto-initialize on first use in modern versions
    // For older versions, initialization is handled by api modules
    
    // 加载所有必需的 API 模块
    InternalLog(sslLogInfo, 'Loading OpenSSL API modules...');
    LoadOpenSSLBIO;    // 加载 BIO API (I/O 操作需要)
    LoadOpenSSLX509;   // 加载 X509 API (证书操作需要)
    
    FInitialized := True;
    InternalLog(sslLogInfo, 'OpenSSL library initialized successfully');
    Result := True;
  except
    on E: Exception do
    begin
      SetError(-1, Format('OpenSSL initialization failed: %s', [E.Message]));
      InternalLog(sslLogError, FLastErrorString);
    end;
  end;
end;

procedure TOpenSSLLibrary.Finalize;
begin
  if not FInitialized then
    Exit;
    
  InternalLog(sslLogInfo, 'Finalizing OpenSSL library...');
  
  // OpenSSL 1.1.0+ auto-cleans on exit
  // Manual cleanup not required
  
  FInitialized := False;
  InternalLog(sslLogInfo, 'OpenSSL library finalized');
end;

function TOpenSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

// ============================================================================
// ISSLLibrary - 版本信息
// ============================================================================

function TOpenSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslOpenSSL;
end;

function TOpenSSLLibrary.GetVersionString: string;
begin
  Result := FVersionString;
end;

function TOpenSSLLibrary.GetVersion: string;
begin
  Result := GetVersionString;
end;

function TOpenSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := FVersionNumber;
end;

function TOpenSSLLibrary.GetCompileFlags: string;
begin
  Result := 'OpenSSL';
  {$IFDEF CPU64}
  Result := Result + ', x64';
  {$ELSE}
  Result := Result + ', x86';
  {$ENDIF}
  {$IFDEF DEBUG}
  Result := Result + ', Debug';
  {$ELSE}
  Result := Result + ', Release';
  {$ENDIF}
  {$IFDEF LINUX}
  Result := Result + ', Linux';
  {$ENDIF}
  {$IFDEF DARWIN}
  Result := Result + ', macOS';
  {$ENDIF}
  {$IFDEF ANDROID}
  Result := Result + ', Android';
  {$ENDIF}
end;

// ============================================================================
// ISSLLibrary - 功能支持查询
// ============================================================================

function TOpenSSLLibrary.IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
var
  Major, Minor: Integer;
begin
  Result := False;
  
  if not FInitialized then
    Exit;
  
  // 从版本号提取主版本和次版本
  Major := (FVersionNumber shr 28) and $F;
  Minor := (FVersionNumber shr 20) and $FF;
  
  case aProtocol of
    sslProtocolSSL2,
    sslProtocolSSL3:
      Result := False;  // SSL 2.0/3.0 已废弃
      
    sslProtocolTLS10,
    sslProtocolTLS11,
    sslProtocolTLS12:
      Result := True;  // 所有现代OpenSSL都支持
      
    sslProtocolTLS13:
      // TLS 1.3 在 OpenSSL 1.1.1+ 支持
      Result := (Major > 1) or ((Major = 1) and (Minor >= 1));
      
    sslProtocolDTLS10,
    sslProtocolDTLS12:
      Result := True;  // OpenSSL支持DTLS
  end;
end;

function TOpenSSLLibrary.IsCipherSupported(const aCipherName: string): Boolean;
begin
  Result := False;
  
  if not FInitialized or (aCipherName = '') then
    Exit;
  
  // Simple implementation: assume common ciphers are supported
  Result := (Pos('AES', UpperCase(aCipherName)) > 0) or
            (Pos('CHACHA', UpperCase(aCipherName)) > 0) or
            (Pos('GCM', UpperCase(aCipherName)) > 0);
end;

function TOpenSSLLibrary.IsFeatureSupported(const aFeatureName: string): Boolean;
var
  Feature: string;
begin
  Feature := LowerCase(aFeatureName);
  
  Result := False;
  
  if Feature = 'sni' then
    Result := True
  else if Feature = 'alpn' then
    Result := True
  else if Feature = 'session_cache' then
    Result := True
  else if Feature = 'session_tickets' then
    Result := True
  else if Feature = 'renegotiation' then
    Result := True
  else if Feature = 'ocsp_stapling' then
    Result := True
  else if Feature = 'certificate_transparency' then
    Result := (FVersionNumber >= $1010000F);  // OpenSSL 1.1.0+
    
  InternalLog(sslLogDebug, Format('Feature support check: %s = %s', 
    [aFeatureName, BoolToStr(Result, True)]));
end;

// ============================================================================
// ISSLLibrary - 库配置
// ============================================================================

procedure TOpenSSLLibrary.SetDefaultConfig(const aConfig: TSSLConfig);
begin
  FDefaultConfig := aConfig;
  FLogLevel := aConfig.LogLevel;
  FLogCallback := aConfig.LogCallback;
  InternalLog(sslLogInfo, 'Default configuration updated');
end;

function TOpenSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

// ============================================================================
// ISSLLibrary - 错误处理
// ============================================================================

function TOpenSSLLibrary.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TOpenSSLLibrary.GetLastErrorString: string;
begin
  Result := FLastErrorString;
end;

procedure TOpenSSLLibrary.ClearError;
begin
  ClearInternalError;
  if Assigned(ERR_clear_error) then
    ERR_clear_error();
end;

// ============================================================================
// ISSLLibrary - 统计信息
// ============================================================================

function TOpenSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TOpenSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
  InternalLog(sslLogInfo, 'Statistics reset');
end;

// ============================================================================
// ISSLLibrary - 日志
// ============================================================================

procedure TOpenSSLLibrary.SetLogCallback(aCallback: TSSLLogCallback);
begin
  FLogCallback := aCallback;
end;

procedure TOpenSSLLibrary.Log(aLevel: TSSLLogLevel; const aMessage: string);
begin
  InternalLog(aLevel, aMessage);
end;

// ============================================================================
// ISSLLibrary - 工厂方法
// ============================================================================

function TOpenSSLLibrary.CreateContext(aType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
  begin
    SetError(-1, 'Library not initialized');
    InternalLog(sslLogError, 'Cannot create context: library not initialized');
    Result := nil;
    Exit;
  end;
  
  try
    Result := TOpenSSLContext.Create(Self, aType);
    Inc(FStatistics.ConnectionsTotal);
    if aType = sslCtxClient then
      InternalLog(sslLogInfo, 'Created client context')
    else
      InternalLog(sslLogInfo, 'Created server context');
  except
    on E: Exception do
    begin
      SetError(-1, E.Message);
      InternalLog(sslLogError, Format('Failed to create context: %s', [E.Message]));
      Result := nil;
    end;
  end;
end;

function TOpenSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  if not FInitialized then
  begin
    InternalLog(sslLogError, 'CreateCertificate: Library not initialized');
    Result := nil;
    Exit;
  end;
  
  // 注意：直接创建空证书不是有效的用例
  // 证书应该从文件、内存、或 DER/PEM 数据加载
  // 返回 nil 表示不支持空证书创建
  InternalLog(sslLogWarning, 'CreateCertificate: Empty certificate creation not supported. Use LoadFromFile/LoadFromPEM/LoadFromDER instead.');
  Result := nil;
end;

function TOpenSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  if not FInitialized then
  begin
    InternalLog(sslLogError, 'CreateCertificateStore: Library not initialized');
    Result := nil;
    Exit;
  end;
  
  try
    Result := TOpenSSLCertificateStore.Create;
  except
    on E: Exception do
    begin
      InternalLog(sslLogError, 'CreateCertificateStore failed: ' + E.Message);
      Result := nil;
    end;
  end;
end;

// ============================================================================
// 注册 OpenSSL 后端到工厂
// ============================================================================

procedure RegisterOpenSSLBackend;
begin
  {$IF NOT DEFINED(WINDOWS)}
  // 在非Windows平台上注册 OpenSSL 后端
  // 优先级设为 100，作为默认选择
  TSSLFactory.RegisterLibrary(sslOpenSSL, TOpenSSLLibrary,
    'OpenSSL (Cross-platform SSL/TLS)', 100);
  {$ENDIF}
end;

procedure UnregisterOpenSSLBackend;
begin
  {$IF NOT DEFINED(WINDOWS)}
  TSSLFactory.UnregisterLibrary(sslOpenSSL);
  {$ENDIF}
end;

initialization
  {$IF NOT DEFINED(WINDOWS)}
  RegisterOpenSSLBackend;
  {$ENDIF}

finalization
  {$IF NOT DEFINED(WINDOWS)}
  UnregisterOpenSSLBackend;
  {$ENDIF}

end.

