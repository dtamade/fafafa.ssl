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
  fafafa.ssl.errors,            // Rust-quality: Raise helpers
  fafafa.ssl.exceptions,        // Rust-quality: Typed exceptions
  fafafa.ssl.openssl.errors,    // OpenSSL-specific raise helpers
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,    // P0-1.1: 使用统一的加载器
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
    // P0-1.1: 移除 FLibSSLHandle 和 FLibCryptoHandle
    // 现在通过 TOpenSSLLoader 获取
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
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    
    { ISSLLibrary - 功能支持查询 }
    function IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const ACipherName: string): Boolean;
    function IsFeatureSupported(AFeature: TSSLFeature): Boolean;
    function GetCapabilities: TSSLBackendCapabilities;  // P2-2

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
  // P0-1.1: 移除 FLibSSLHandle 和 FLibCryptoHandle 初始化
  // 现在通过 TOpenSSLLoader 管理
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

  // Normalize options derived from booleans (EnableSessionTickets, EnableCompression, etc.)
  // to keep ISSLLibrary.CreateContext consistent with TSSLFactory.NormalizeConfig.
  TSSLFactory.NormalizeConfig(FDefaultConfig);

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
var
  CryptoHandle, SSLHandle: TLibHandle;
begin
  // P0-1.1: 委托给 TOpenSSLLoader 进行加载
  Result := False;

  InternalLog(sslLogInfo, 'Loading OpenSSL libraries via TOpenSSLLoader...');

  // 触发 TOpenSSLLoader 加载库
  CryptoHandle := TOpenSSLLoader.GetLibraryHandle(osslLibCrypto);
  SSLHandle := TOpenSSLLoader.GetLibraryHandle(osslLibSSL);

  if CryptoHandle = 0 then
  begin
    SetError(-1, 'Failed to load libcrypto via TOpenSSLLoader');
    InternalLog(sslLogError, FLastErrorString);
    Exit;
  end;

  if SSLHandle = 0 then
  begin
    SetError(-1, 'Failed to load libssl via TOpenSSLLoader');
    InternalLog(sslLogError, FLastErrorString);
    Exit;
  end;

  InternalLog(sslLogInfo, 'OpenSSL libraries loaded successfully via TOpenSSLLoader');

  // Note: API functions will be loaded on-demand by the api modules
  // No need to explicitly load all functions here

  Result := True;
end;

procedure TOpenSSLLibrary.UnloadOpenSSLLibraries;
begin
  // P0-1.1: 委托给 TOpenSSLLoader 卸载库
  // 注意: 不在此处卸载，因为 TOpenSSLLoader 是全局共享的
  // 卸载应该由 TOpenSSLLoader.UnloadLibraries 处理
  InternalLog(sslLogInfo, 'UnloadOpenSSLLibraries called (delegated to TOpenSSLLoader)');
end;

function TOpenSSLLibrary.DetectOpenSSLVersion: Boolean;
var
  LInfo: TOpenSSLVersionInfo;
  LVersionStr: PAnsiChar;
begin
  Result := False;
  FVersionNumber := 0;
  FVersionString := '';

  try
    // Ensure core bindings are loaded (OpenSSL_version_num/OpenSSL_version).
    if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
      LoadOpenSSLCore;

    // Prefer OpenSSL runtime version functions.
    if Assigned(OpenSSL_version_num) then
      FVersionNumber := OpenSSL_version_num();

    if Assigned(OpenSSL_version) then
    begin
      // OPENSSL_VERSION = 0 (avoid name collision with OpenSSL_version symbol in Pascal)
      LVersionStr := OpenSSL_version(0);
      if LVersionStr <> nil then
        FVersionString := string(LVersionStr);
    end;

    // Fallback to loader-detected version string when core API doesn't provide it.
    if FVersionString = '' then
    begin
      LInfo := TOpenSSLLoader.GetVersionInfo;
      FVersionString := LInfo.VersionString;
    end;

    if FVersionString = '' then
      FVersionString := GetOpenSSLVersionString;

    if FVersionNumber <> 0 then
      InternalLog(sslLogInfo,
        Format('OpenSSL version: %s (0x%s)', [FVersionString, IntToHex(FVersionNumber, 8)]))
    else
      InternalLog(sslLogInfo, Format('OpenSSL version: %s', [FVersionString]));

    Result := True;
  except
    on E: Exception do
    begin
      SetError(-1, Format('OpenSSL version detection failed: %s', [E.Message]));
      InternalLog(sslLogWarning, FLastErrorString);
    end;
  end;
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
begin
  Result := False;

  if not FInitialized then
    Exit;

  case AProtocol of
    sslProtocolSSL2,
    sslProtocolSSL3:
      Result := False;  // SSL 2.0/3.0 已废弃

    sslProtocolTLS10,
    sslProtocolTLS11,
    sslProtocolTLS12:
      Result := True;  // 所有现代 OpenSSL 都支持

    sslProtocolTLS13:
      // TLS 1.3 在 OpenSSL 1.1.1+ 支持
      Result := (FVersionNumber >= $1010100F);

    sslProtocolDTLS10,
    sslProtocolDTLS12:
      Result := True;  // OpenSSL 支持 DTLS
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

function TOpenSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  // P2-2: 返回 OpenSSL 后端能力矩阵
  FillChar(Result, SizeOf(Result), 0);

  // 检测 TLS 1.3 支持 (OpenSSL 1.1.1+)
  Result.SupportsTLS13 := (FVersionNumber >= $1010100F);

  // OpenSSL 原生支持的特性
  Result.SupportsALPN := True;
  Result.SupportsSNI := True;
  Result.SupportsSessionTickets := True;
  Result.SupportsECDHE := True;

  // OCSP 装订支持
  Result.SupportsOCSPStapling := True;

  // Certificate Transparency 需要 OpenSSL 1.1.0+
  Result.SupportsCertificateTransparency := (FVersionNumber >= $1010000F);

  // ChaCha20-Poly1305 需要 OpenSSL 1.1.0+
  Result.SupportsChaChaPoly := (FVersionNumber >= $1010000F);

  // OpenSSL 原生支持 PEM 格式私钥
  Result.SupportsPEMPrivateKey := True;

  // 支持的协议版本范围
  Result.MinTLSVersion := sslProtocolTLS10;  // 可通过配置禁用
  if Result.SupportsTLS13 then
    Result.MaxTLSVersion := sslProtocolTLS13
  else
    Result.MaxTLSVersion := sslProtocolTLS12;

  InternalLog(sslLogDebug, Format('GetCapabilities: TLS1.3=%s, ALPN=%s, SNI=%s',
    [
      BoolToStr(Result.SupportsTLS13, True),
      BoolToStr(Result.SupportsALPN, True),
      BoolToStr(Result.SupportsSNI, True)
    ]));
end;

// ============================================================================
// ISSLLibrary - 库配置
// ============================================================================

procedure TOpenSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
var
  LConfig: TSSLConfig;
begin
  LConfig := AConfig;
  TSSLFactory.NormalizeConfig(LConfig);

  FDefaultConfig := LConfig;
  FLogLevel := LConfig.LogLevel;
  FLogCallback := LConfig.LogCallback;
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
        on E: Exception do
          InternalLog(sslLogWarning, Format('Exception in GetLastError: %s', [E.Message]));
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
var
  LConfig: TSSLConfig;
begin
  // Rust-quality: Explicit error handling instead of returning nil
  if not FInitialized then
    RaiseSSLInitError(
      'Cannot create context: OpenSSL library not initialized',
      'TOpenSSLLibrary.CreateContext'
    );

  // Let exceptions propagate - caller must handle errors explicitly
  Result := TOpenSSLContext.Create(Self, AType);

  // Apply default config (already normalized in constructor/SetDefaultConfig)
  if Result <> nil then
  begin
    LConfig := FDefaultConfig;
    LConfig.ContextType := AType;

    if (LConfig.ProtocolVersions <> []) and (LConfig.ProtocolVersions <> Result.GetProtocolVersions) then
      Result.SetProtocolVersions(LConfig.ProtocolVersions);

    if (LConfig.VerifyMode <> []) and (LConfig.VerifyMode <> Result.GetVerifyMode) then
      Result.SetVerifyMode(LConfig.VerifyMode);

    if (LConfig.VerifyDepth > 0) and (LConfig.VerifyDepth <> Result.GetVerifyDepth) then
      Result.SetVerifyDepth(LConfig.VerifyDepth);

    if (LConfig.CipherList <> '') and (LConfig.CipherList <> Result.GetCipherList) then
      Result.SetCipherList(LConfig.CipherList);

    if (LConfig.CipherSuites <> '') and (LConfig.CipherSuites <> Result.GetCipherSuites) then
      Result.SetCipherSuites(LConfig.CipherSuites);

    if (LConfig.Options <> []) and (LConfig.Options <> Result.GetOptions) then
      Result.SetOptions(LConfig.Options);

    if (LConfig.SessionCacheSize > 0) and (LConfig.SessionCacheSize <> Result.GetSessionCacheSize) then
      Result.SetSessionCacheSize(LConfig.SessionCacheSize);

    if (LConfig.SessionTimeout > 0) and (LConfig.SessionTimeout <> Result.GetSessionTimeout) then
      Result.SetSessionTimeout(LConfig.SessionTimeout);

    if (ssoEnableSessionCache in LConfig.Options) <> Result.GetSessionCacheMode then
      Result.SetSessionCacheMode(ssoEnableSessionCache in LConfig.Options);

    if (LConfig.ServerName <> '') and (LConfig.ServerName <> Result.GetServerName) then
      Result.SetServerName(LConfig.ServerName);

    if (LConfig.ALPNProtocols <> '') and (LConfig.ALPNProtocols <> Result.GetALPNProtocols) then
      Result.SetALPNProtocols(LConfig.ALPNProtocols);
  end;

  Inc(FStatistics.ConnectionsTotal);
  if AType = sslCtxClient then
    InternalLog(sslLogInfo, 'Created client context')
  else
    InternalLog(sslLogInfo, 'Created server context');
end;

function TOpenSSLLibrary.CreateCertificate: ISSLCertificate;
var
  LCert: PX509;
begin
  // Rust-quality: Explicit error handling
  if not FInitialized then
    RaiseSSLInitError(
      'Cannot create certificate: OpenSSL library not initialized',
      'TOpenSSLLibrary.CreateCertificate'
    );

  // Create a new empty X509 certificate
  LCert := X509_new();
  if LCert = nil then
    RaiseMemoryError('create X509 certificate');

  Result := TOpenSSLCertificate.Create(LCert, True);
end;

function TOpenSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  // Rust-quality: Explicit error handling
  if not FInitialized then
    RaiseSSLInitError(
      'Cannot create certificate store: OpenSSL library not initialized',
      'TOpenSSLLibrary.CreateCertificateStore'
    );

  // Let exceptions propagate - caller must handle errors explicitly
  Result := TOpenSSLCertificateStore.Create;
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
