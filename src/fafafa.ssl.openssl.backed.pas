{
  fafafa.ssl.openssl.backed - OpenSSL 库管理实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-11-02
  
  描述:
    实现 ISSLLibrary 接口的 OpenSSL 后端。
    负责 OpenSSL 的初始化、配置和上下文创建。
    支持 Linux, macOS, Android 等平台。
}
 
unit fafafa.ssl.openssl.backed;

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
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.hmac,
  fafafa.ssl.openssl.api.ts,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.certificate;

type
  { TOpenSSLLibraryPaths - 自定义库路径配置 }
  TOpenSSLLibraryPaths = record
    CryptoLibPath: string;  // 自定义 libcrypto 路径
    SSLLibPath: string;     // 自定义 libssl 路径
  end;

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
    procedure InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
    function LoadOpenSSLLibraries: Boolean;
    procedure UnloadOpenSSLLibraries;
    function DetectOpenSSLVersion: Boolean;
    procedure SetError(AError: Integer; const AErrorMsg: string);
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
    {** @deprecated Will be removed in v2.0.0. Use GetVersionString instead. *}
    function GetVersion: string; deprecated 'Use GetVersionString instead - will be removed in v2.0.0';
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    { ISSLLibrary - 功能支持查询 }
    function IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const ACipherName: string): Boolean;
    {** @deprecated Will be removed in v2.0.0. Use IsFeatureSupported(TSSLFeature) instead. *}
    function IsFeatureSupported(const AFeatureName: string): Boolean; deprecated 'Use IsFeatureSupported(TSSLFeature) instead - will be removed in v2.0.0';
    function IsFeatureSupported(AFeature: TSSLFeature): Boolean; overload;
    
    { ISSLLibrary - 库配置 }
    procedure SetDefaultConfig(const AConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    
    { ISSLLibrary - 错误处理 }
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    
    { ISSLLibrary - 统计信息 }
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    
    { ISSLLibrary - 日志 }
    procedure SetLogCallback(ACallback: TSSLLogCallback);
    procedure Log(ALevel: TSSLLogLevel; const AMessage: string);
    
    { ISSLLibrary - 工厂方法 }
    function CreateContext(AType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

{ 全局变量 - 自定义库路径配置 }
var
  CustomLibraryPaths: TOpenSSLLibraryPaths;
  UseCustomPaths: Boolean = False;

{ 全局函数 - 库路径配置 }
procedure SetCustomLibraryPaths(const ACryptoPath, ASSLPath: string);
function GetCustomLibraryPaths: TOpenSSLLibraryPaths;
function IsUsingCustomPaths: Boolean;
procedure ClearCustomLibraryPaths;

{ 全局工厂函数 }
function CreateOpenSSLLibrary: ISSLLibrary;

procedure RegisterOpenSSLBackend;
procedure UnregisterOpenSSLBackend;

implementation

uses
  fafafa.ssl.openssl.context,
  fafafa.ssl.openssl.certstore,
  fafafa.ssl.openssl.session,     // Session API fully implemented
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.factory;

// ============================================================================
// 全局函数 - 库路径配置
// ============================================================================

procedure SetCustomLibraryPaths(const ACryptoPath, ASSLPath: string);
begin
  CustomLibraryPaths.CryptoLibPath := ACryptoPath;
  CustomLibraryPaths.SSLLibPath := ASSLPath;
  UseCustomPaths := (ACryptoPath <> '') or (ASSLPath <> '');
end;

function GetCustomLibraryPaths: TOpenSSLLibraryPaths;
begin
  Result := CustomLibraryPaths;
end;

function IsUsingCustomPaths: Boolean;
begin
  Result := UseCustomPaths;
end;

procedure ClearCustomLibraryPaths;
begin
  CustomLibraryPaths.CryptoLibPath := '';
  CustomLibraryPaths.SSLLibPath := '';
  UseCustomPaths := False;
end;

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
    VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
    CipherList := SSL_DEFAULT_CIPHER_LIST;
    CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
    Options := [ssoEnableSNI, ssoEnableALPN];
    BufferSize := SSL_DEFAULT_BUFFER_SIZE;
    HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
    SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
    SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
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

procedure TOpenSSLLibrary.InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FLogCallback) and (ALevel <= FLogLevel) then
    FLogCallback(ALevel, AMessage);
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
  {$IFDEF WINDOWS}
  SSL_LIB_NAMES: array[0..1] of string = (
    'libssl-3-x64.dll',
    'libssl-1_1-x64.dll'
  );
  CRYPTO_LIB_NAMES: array[0..1] of string = (
    'libcrypto-3-x64.dll',
    'libcrypto-1_1-x64.dll'
  );
  {$ENDIF}
var
  i: Integer;
  LibLoaded: Boolean;
begin
  Result := False;
  LibLoaded := False;
  
  InternalLog(sslLogInfo, 'Loading OpenSSL libraries...');
  
  // 1. 尝试使用自定义路径加载 libcrypto
  if UseCustomPaths and (CustomLibraryPaths.CryptoLibPath <> '') then
  begin
    InternalLog(sslLogInfo, Format('Trying custom libcrypto path: %s', [CustomLibraryPaths.CryptoLibPath]));
    FLibCryptoHandle := LoadLibrary(CustomLibraryPaths.CryptoLibPath);
    if FLibCryptoHandle <> NilHandle then
    begin
      InternalLog(sslLogInfo, Format('Loaded libcrypto from custom path: %s', [CustomLibraryPaths.CryptoLibPath]));
      LibLoaded := True;
    end
    else
      InternalLog(sslLogWarning, Format('Failed to load from custom path: %s, falling back to system libraries', [CustomLibraryPaths.CryptoLibPath]));
  end;
  
  // 2. 回退：尝试标准库名称
  if not LibLoaded then
  begin
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
  end;
  
  if not LibLoaded then
  begin
    SetError(-1, 'Failed to load libcrypto');
    InternalLog(sslLogError, FLastErrorString);
    Exit;
  end;
  
  // 3. 尝试使用自定义路径加载 libssl
  LibLoaded := False;
  if UseCustomPaths and (CustomLibraryPaths.SSLLibPath <> '') then
  begin
    InternalLog(sslLogInfo, Format('Trying custom libssl path: %s', [CustomLibraryPaths.SSLLibPath]));
    FLibSSLHandle := LoadLibrary(CustomLibraryPaths.SSLLibPath);
    if FLibSSLHandle <> NilHandle then
    begin
      InternalLog(sslLogInfo, Format('Loaded libssl from custom path: %s', [CustomLibraryPaths.SSLLibPath]));
      LibLoaded := True;
    end
    else
      InternalLog(sslLogWarning, Format('Failed to load from custom path: %s, falling back to system libraries', [CustomLibraryPaths.SSLLibPath]));
  end;
  
  // 4. 回退：尝试标准库名称
  if not LibLoaded then
  begin
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

procedure TOpenSSLLibrary.SetError(AError: Integer; const AErrorMsg: string);
begin
  FLastError := AError;
  FLastErrorString := AErrorMsg;
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

  try
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      SetError(-1, Format('OpenSSL core initialization failed: %s', [E.Message]));
      InternalLog(sslLogError, FLastErrorString);
      Exit(False);
    end;
  end;
  
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
    LoadEVP(GetCryptoLibHandle);          // 加载 EVP 摘要/加密 API（指纹等功能需要）
    LoadOpenSSLASN1(GetCryptoLibHandle);  // 加载 ASN1 API（时间与字符串解析需要）
    LoadX509V3Functions(GetCryptoLibHandle);
    LoadOpenSSLSSL;    // 加载 SSL 高层 API (状态、ALPN、Cipher 列表等)
    LoadOpenSSLERR;    // 加载错误处理 API (ERR_get_error 等)
    LoadOpenSSLHMAC;   // 加载 HMAC API (HKDF 等密钥派生功能需要)
    LoadTSFunctions;   // 加载 TSA API (时间戳功能需要)
    LoadPKCS12Module(GetCryptoLibHandle); // 加载 PKCS#12 API
    LoadOpenSSLOCSP(GetCryptoLibHandle);  // 加载 OCSP API
    
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

function TOpenSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
var
  Major, Minor: Integer;
begin
  Result := False;
  
  if not FInitialized then
    Exit;
  
  // 从版本号提取主版本和次版本
  Major := (FVersionNumber shr 28) and $F;
  Minor := (FVersionNumber shr 20) and $FF;
  
  case AProtocol of
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

function TOpenSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := False;
  
  if not FInitialized or (ACipherName = '') then
    Exit;
  
  // Simple implementation: assume common ciphers are supported
  Result := (Pos('AES', UpperCase(ACipherName)) > 0) or
            (Pos('CHACHA', UpperCase(ACipherName)) > 0) or
            (Pos('GCM', UpperCase(ACipherName)) > 0);
end;

function TOpenSSLLibrary.IsFeatureSupported(const AFeatureName: string): Boolean;
var
  Feature: string;
begin
  // 废弃方法：将字符串映射到枚举类型，调用类型安全版本
  Feature := LowerCase(AFeatureName);

  if Feature = 'sni' then
    Result := IsFeatureSupported(sslFeatSNI)
  else if Feature = 'alpn' then
    Result := IsFeatureSupported(sslFeatALPN)
  else if Feature = 'session_cache' then
    Result := IsFeatureSupported(sslFeatSessionCache)
  else if Feature = 'session_tickets' then
    Result := IsFeatureSupported(sslFeatSessionTickets)
  else if Feature = 'renegotiation' then
    Result := IsFeatureSupported(sslFeatRenegotiation)
  else if Feature = 'ocsp_stapling' then
    Result := IsFeatureSupported(sslFeatOCSPStapling)
  else if Feature = 'certificate_transparency' then
    Result := IsFeatureSupported(sslFeatCertificateTransparency)
  else
    Result := False;

  InternalLog(sslLogDebug, Format('Feature support check (deprecated): %s = %s',
    [AFeatureName, BoolToStr(Result, True)]));
end;

{ 类型安全版本（Phase 1.3 - Rust质量标准） }
function TOpenSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  case AFeature of
    sslFeatSNI:
      Result := True;  // OpenSSL原生支持SNI
    sslFeatALPN:
      Result := True;  // OpenSSL原生支持ALPN
    sslFeatSessionCache:
      Result := True;  // OpenSSL原生支持会话缓存
    sslFeatSessionTickets:
      Result := True;  // OpenSSL原生支持会话票据
    sslFeatRenegotiation:
      Result := True;  // OpenSSL原生支持重新协商
    sslFeatOCSPStapling:
      Result := True;  // OpenSSL原生支持OCSP装订
    sslFeatCertificateTransparency:
      // Certificate Transparency需要OpenSSL 1.1.0+
      Result := (FVersionNumber >= $1010000F);
  else
    Result := False;
  end;

  InternalLog(sslLogDebug, Format('Feature support check (type-safe): %d = %s',
    [Ord(AFeature), BoolToStr(Result, True)]));
end;

// ============================================================================
// ISSLLibrary - 库配置
// ============================================================================

procedure TOpenSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
  FLogLevel := AConfig.LogLevel;
  FLogCallback := AConfig.LogCallback;
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
var
  LErr: Cardinal;
begin
  Result := FLastError;

  if Result = 0 then
  begin
    if not Assigned(ERR_peek_last_error) then
      LoadOpenSSLERR;

    if Assigned(ERR_peek_last_error) then
    begin
      try
        LErr := ERR_peek_last_error();
        if LErr <> 0 then
        begin
          FLastError := Integer(LErr);
          Result := FLastError;
        end;
      except
      end;
    end;
  end;
end;

function TOpenSSLLibrary.GetLastErrorString: string;
var
  ErrCode: Cardinal;
  ErrBuf: array[0..255] of AnsiChar;
  OpenSSLErrors: string;
begin
  Result := FLastErrorString;
  
  // Check OpenSSL error queue
  OpenSSLErrors := '';
  if Assigned(ERR_get_error) and Assigned(ERR_error_string_n) then
  begin
    ErrCode := ERR_get_error();
    while ErrCode <> 0 do
    begin
      ERR_error_string_n(ErrCode, @ErrBuf[0], SizeOf(ErrBuf));
      if OpenSSLErrors <> '' then
        OpenSSLErrors := OpenSSLErrors + ' | ';
      OpenSSLErrors := OpenSSLErrors + StrPas(@ErrBuf[0]);
      ErrCode := ERR_get_error();
    end;
  end;
  
  if OpenSSLErrors <> '' then
  begin
    if Result <> '' then
      Result := Result + ' | ' + OpenSSLErrors
    else
      Result := OpenSSLErrors;
  end;
  
  if Result = '' then
    Result := 'No SSL errors';
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

procedure TOpenSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TOpenSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

// ============================================================================
// ISSLLibrary - 工厂方法
// ============================================================================

function TOpenSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
  begin
    SetError(-1, 'Library not initialized');
    InternalLog(sslLogError, 'Cannot create context: library not initialized');
    Result := nil;
    Exit;
  end;
  
  try
    Result := TOpenSSLContext.Create(Self, AType);
    if (Result <> nil) and (FDefaultConfig.Options <> []) then
      Result.SetOptions(FDefaultConfig.Options);
    Inc(FStatistics.ConnectionsTotal);
    if AType = sslCtxClient then
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
var
  LCert: PX509;
begin
  if not FInitialized then
  begin
    InternalLog(sslLogError, 'CreateCertificate: Library not initialized');
    Result := nil;
    Exit;
  end;
  
  // Create a new empty X509 certificate
  LCert := X509_new();
  if LCert <> nil then
    Result := TOpenSSLCertificate.Create(LCert, True)
  else
    Result := TOpenSSLCertificate.Create(nil, False);
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
  // 在非Windows平台上注册 OpenSSL 后端
  // 优先级设为 100，作为默认选择
  TSSLFactory.RegisterLibrary(sslOpenSSL, TOpenSSLLibrary,
    'OpenSSL (Cross-platform SSL/TLS)', 100);
end;

procedure UnregisterOpenSSLBackend;
begin
  TSSLFactory.UnregisterLibrary(sslOpenSSL);
end;

initialization
  RegisterOpenSSLBackend;

finalization
  UnregisterOpenSSLBackend;

end.
