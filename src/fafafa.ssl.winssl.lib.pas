{
  fafafa.ssl.winssl.library - WinSSL 库管理实现
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-06
  
  描述:
    实现 ISSLLibrary 接口的 WinSSL 后端。
    负责 Windows Schannel 的初始化、配置和上下文创建。
}

unit fafafa.ssl.winssl.lib;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  Windows, SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.base,
  fafafa.ssl.winssl.api,
  fafafa.ssl.winssl.utils;

type
  { TWinSSLLibrary - Windows Schannel 库管理类 }
  TWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLastError: Integer;
    FLastErrorString: string;
    FLogCallback: TSSLLogCallback;
    FLogLevel: TSSLLogLevel;
    FWindowsVersion: record
      Major: DWORD;
      Minor: DWORD;
      Build: DWORD;
      IsServer: Boolean;
    end;
    
    { 内部方法 }
    procedure InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
    function DetectWindowsVersion: Boolean;
    function CheckSchannelSupport: Boolean;
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

{ 全局工厂函数 }
function CreateWinSSLLibrary: ISSLLibrary;

implementation

uses
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.factory;  // 需要引用 factory 以调用 RegisterLibrary

// ============================================================================
// 全局工厂函数
// ============================================================================

function CreateWinSSLLibrary: ISSLLibrary;
begin
  Result := TWinSSLLibrary.Create;
end;

// ============================================================================
// TWinSSLLibrary - 构造和析构
// ============================================================================

constructor TWinSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  FLastError := 0;
  FLastErrorString := '';
  FLogCallback := nil;
  FLogLevel := sslLogError;
  
  // 初始化默认配置
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  with FDefaultConfig do
  begin
    LibraryType := sslWinSSL;
    ContextType := sslCtxClient;
    ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    PreferredVersion := sslProtocolTLS13;
    VerifyMode := [sslVerifyPeer];
    VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
    CipherList := '';  // 使用 Windows 默认
    CipherSuites := ''; // 使用 Windows 默认
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
  
  // 初始化 Windows 版本信息
  FillChar(FWindowsVersion, SizeOf(FWindowsVersion), 0);
end;

destructor TWinSSLLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited Destroy;
end;

// ============================================================================
// 内部方法实现
// ============================================================================

procedure TWinSSLLibrary.InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FLogCallback) and (ALevel <= FLogLevel) then
    FLogCallback(ALevel, AMessage);
end;

function TWinSSLLibrary.DetectWindowsVersion: Boolean;
var
  VersionInfo: OSVERSIONINFO;
begin
  Result := False;
  FillChar(VersionInfo, SizeOf(VersionInfo), 0);
  VersionInfo.dwOSVersionInfoSize := SizeOf(OSVERSIONINFO);
  
  if GetVersionEx(VersionInfo) then
  begin
    FWindowsVersion.Major := VersionInfo.dwMajorVersion;
    FWindowsVersion.Minor := VersionInfo.dwMinorVersion;
    FWindowsVersion.Build := VersionInfo.dwBuildNumber and $FFFF;
    FWindowsVersion.IsServer := False; // Simplified: can't detect with basic OSVERSIONINFO
    Result := True;
    
    InternalLog(sslLogInfo, Format('Windows version detected: %d.%d Build %d (%s)',
      [FWindowsVersion.Major, FWindowsVersion.Minor, FWindowsVersion.Build,
      'Workstation'])); // Simplified log
  end
  else
  begin
    SetError(GetLastError, 'Failed to detect Windows version');
    InternalLog(sslLogError, 'Failed to detect Windows version');
  end;
end;

function TWinSSLLibrary.CheckSchannelSupport: Boolean;
var
  CredHandle: TSecHandle;
  TimeStamp: TTimeStamp;
  Status: SECURITY_STATUS;
  SchannelCred: SCHANNEL_CRED;
begin
  Result := False;
  
  // 尝试获取一个简单的 Schannel 凭据
  FillChar(SchannelCred, SizeOf(SchannelCred), 0);
  SchannelCred.dwVersion := SCHANNEL_CRED_VERSION;
  SchannelCred.dwFlags := SCH_CRED_NO_DEFAULT_CREDS or SCH_CRED_MANUAL_CRED_VALIDATION;
  
  InitSecHandle(CredHandle);
  
  Status := AcquireCredentialsHandleW(
    nil,
    PWideChar(WideString('Microsoft Unified Security Protocol Provider')),
    SECPKG_CRED_OUTBOUND,
    nil,
    @SchannelCred,
    nil,
    nil,
    @CredHandle,
    @TimeStamp
  );
  
  if IsSuccess(Status) then
  begin
    Result := True;
    FreeCredentialsHandle(@CredHandle);
    InternalLog(sslLogInfo, 'Schannel support verified');
  end
  else
  begin
    SetError(Status, Format('Schannel not available: %s', 
      [GetSchannelErrorString(Status)]));
    InternalLog(sslLogError, Format('Schannel support check failed: %s',
      [GetSchannelErrorString(Status)]));
  end;
end;

procedure TWinSSLLibrary.SetError(AError: Integer; const AErrorMsg: string);
begin
  FLastError := AError;
  FLastErrorString := AErrorMsg;
end;

procedure TWinSSLLibrary.ClearInternalError;
begin
  FLastError := 0;
  FLastErrorString := '';
end;

// ============================================================================
// ISSLLibrary - 初始化和清理
// ============================================================================

function TWinSSLLibrary.Initialize: Boolean;
begin
  Result := False;
  
  if FInitialized then
  begin
    InternalLog(sslLogWarning, 'WinSSL library already initialized');
    Exit(True);
  end;
  
  InternalLog(sslLogInfo, 'Initializing WinSSL library...');
  ClearInternalError;
  
  // 检测 Windows 版本
  if not DetectWindowsVersion then
    Exit(False);
    
  // 检查 Windows 版本是否支持 Schannel
  // Windows Vista (6.0) 及以上支持 Schannel
  if (FWindowsVersion.Major < 6) then
  begin
    SetError(-1, 'Windows version too old. Schannel requires Windows Vista or later.');
    InternalLog(sslLogError, FLastErrorString);
    Exit(False);
  end;
  
  // 检查 Schannel 支持
  if not CheckSchannelSupport then
    Exit(False);
  
  FInitialized := True;
  InternalLog(sslLogInfo, 'WinSSL library initialized successfully');
  Result := True;
end;

procedure TWinSSLLibrary.Finalize;
begin
  if not FInitialized then
    Exit;
    
  InternalLog(sslLogInfo, 'Finalizing WinSSL library...');
  
  // WinSSL 不需要特殊的清理操作
  // Schannel 是 Windows 系统组件，由操作系统管理
  
  FInitialized := False;
  InternalLog(sslLogInfo, 'WinSSL library finalized');
end;

function TWinSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

// ============================================================================
// ISSLLibrary - 版本信息
// ============================================================================

function TWinSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TWinSSLLibrary.GetVersionString: string;
begin
  // Windows Schannel 版本与 Windows 版本对应
  if FInitialized then
    Result := Format('Windows Schannel %d.%d (Build %d)',
      [FWindowsVersion.Major, FWindowsVersion.Minor, FWindowsVersion.Build])
  else
    Result := 'Windows Schannel (not initialized)';
end;

function TWinSSLLibrary.GetVersionNumber: Cardinal;
begin
  // 返回 Windows 版本号
  // 格式: Major.Minor.Build -> 转换为 Cardinal
  if FInitialized then
    Result := (FWindowsVersion.Major shl 16) or FWindowsVersion.Minor
  else
    Result := 0;
end;

function TWinSSLLibrary.GetCompileFlags: string;
begin
  // Schannel 编译标志由 Windows 系统决定
  Result := 'Native Windows Schannel';
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
end;

// ============================================================================
// ISSLLibrary - 功能支持查询
// ============================================================================

function TWinSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
  
  if not FInitialized then
    Exit;
  
  // Windows 版本与 TLS 支持对应关系
  case AProtocol of
    sslProtocolSSL2,
    sslProtocolSSL3:
      Result := False;  // SSL 2.0/3.0 已废弃
      
    sslProtocolTLS10:
      Result := (FWindowsVersion.Major >= 6);  // Vista+
      
    sslProtocolTLS11:
      Result := (FWindowsVersion.Major > 6) or 
                ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 1));  // Win 7+
      
    sslProtocolTLS12:
      Result := (FWindowsVersion.Major > 6) or
                ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 1));  // Win 7+
      
    sslProtocolTLS13:
      // TLS 1.3 在 Windows 10 Build 20348+ 和 Windows 11 支持
      Result := (FWindowsVersion.Major >= 10) and (FWindowsVersion.Build >= 20348);
      
    sslProtocolDTLS10,
    sslProtocolDTLS12:
      Result := False;  // Schannel 不直接支持 DTLS
  end;
end;

function TWinSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  // Windows Schannel 的密码套件支持由系统策略决定
  // 这里简单返回 True，实际支持在握手时由系统确定
  Result := True;
  InternalLog(sslLogDebug, Format('Cipher support check: %s (deferred to system)', [ACipherName]));
end;

{ 类型安全版本（Phase 1.3 - Rust质量标准） }
function TWinSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  case AFeature of
    sslFeatSNI:
      Result := True;  // Windows Schannel原生支持SNI
    sslFeatALPN:
      // ALPN需要Windows 8+或Windows 10+
      Result := (FWindowsVersion.Major >= 10) or
                ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 2));
    sslFeatSessionCache:
      Result := True;  // Windows Schannel原生支持会话缓存
    sslFeatSessionTickets:
      Result := True;  // Windows Schannel原生支持会话票据
    sslFeatRenegotiation:
      Result := True;  // Windows Schannel原生支持重新协商
    sslFeatOCSPStapling:
      Result := False;  // 需要手动实现OCSP装订
    sslFeatCertificateTransparency:
      Result := False;  // Windows Schannel不原生支持证书透明度
  else
    Result := False;
  end;

  InternalLog(sslLogDebug, Format('Feature support check (type-safe): %d = %s',
    [Ord(AFeature), BoolToStr(Result, True)]));
end;

function TWinSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  // P2-2: 返回 WinSSL (Schannel) 后端能力矩阵
  FillChar(Result, SizeOf(Result), 0);

  // TLS 1.3 支持需要 Windows 10 版本 1903+ 或 Windows Server 2022+
  Result.SupportsTLS13 := (FWindowsVersion.Major >= 10) and (FWindowsVersion.Build >= 18362);

  // ALPN 需要 Windows 8+ (版本 6.2+) 或 Windows 10+
  Result.SupportsALPN := (FWindowsVersion.Major >= 10) or
                         ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 2));

  // SNI 是 Schannel 原生支持的
  Result.SupportsSNI := True;

  // OCSP 装订需要手动实现，Schannel 不原生支持
  Result.SupportsOCSPStapling := False;

  // Certificate Transparency Schannel 不原生支持
  Result.SupportsCertificateTransparency := False;

  // 会话票据是 Schannel 原生支持的
  Result.SupportsSessionTickets := True;

  // ECDHE 需要 Windows Vista+ (版本 6.0+)
  Result.SupportsECDHE := (FWindowsVersion.Major >= 6);

  // ChaCha20-Poly1305 需要 Windows 10 版本 1903+
  Result.SupportsChaChaPoly := (FWindowsVersion.Major >= 10) and (FWindowsVersion.Build >= 18362);

  // WinSSL/Schannel 不支持直接加载 PEM 格式私钥
  // 需要使用 PKCS#12/PFX 格式，或先转换为 DER 格式
  Result.SupportsPEMPrivateKey := False;

  // 支持的协议版本范围
  Result.MinTLSVersion := sslProtocolTLS10;  // Windows Vista+
  if Result.SupportsTLS13 then
    Result.MaxTLSVersion := sslProtocolTLS13
  else
    Result.MaxTLSVersion := sslProtocolTLS12;

  InternalLog(sslLogDebug, Format('GetCapabilities: TLS1.3=%s, ALPN=%s, SNI=%s (Win %d.%d.%d)',
    [BoolToStr(Result.SupportsTLS13, True),
     BoolToStr(Result.SupportsALPN, True),
     BoolToStr(Result.SupportsSNI, True),
     FWindowsVersion.Major,
     FWindowsVersion.Minor,
     FWindowsVersion.Build]));
end;

// ============================================================================
// ISSLLibrary - 库配置
// ============================================================================

procedure TWinSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
  FLogLevel := AConfig.LogLevel;
  FLogCallback := AConfig.LogCallback;
  InternalLog(sslLogInfo, 'Default configuration updated');
end;

function TWinSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

// ============================================================================
// ISSLLibrary - 错误处理
// ============================================================================

function TWinSSLLibrary.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TWinSSLLibrary.GetLastErrorString: string;
begin
  Result := FLastErrorString;
end;

procedure TWinSSLLibrary.ClearError;
begin
  ClearInternalError;
end;

// ============================================================================
// ISSLLibrary - 统计信息
// ============================================================================

function TWinSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TWinSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
  InternalLog(sslLogInfo, 'Statistics reset');
end;

// ============================================================================
// ISSLLibrary - 日志
// ============================================================================

procedure TWinSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TWinSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

// ============================================================================
// ISSLLibrary - 工厂方法
// ============================================================================

function TWinSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
  begin
    SetError(-1, 'Library not initialized');
    InternalLog(sslLogError, 'Cannot create context: library not initialized');
    Result := nil;
    Exit;
  end;
  
  try
    Result := TWinSSLContext.Create(Self, AType);
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

function TWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  if not FInitialized then
  begin
    SetError(-1, 'Library not initialized');
    InternalLog(sslLogError, 'Cannot create certificate: library not initialized');
    Result := nil;
    Exit;
  end;
  
  // 创建空证书对象，调用方可通过 LoadFromFile/LoadFromStream 等方法加载实际证书
  Result := TWinSSLCertificate.Create(nil, False);
end;

function TWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  if not FInitialized then
  begin
    SetError(-1, 'Library not initialized');
    InternalLog(sslLogError, 'Cannot create certificate store: library not initialized');
    Result := nil;
    Exit;
  end;
  
  // 默认创建受信任根证书存储，调用方可根据需要重新打开其他系统存储
  Result := TWinSSLCertificateStore.Create(SSL_STORE_ROOT);
end;

// ============================================================================
// 注册 WinSSL 后端到工厂
// ============================================================================

procedure RegisterWinSSLBackend;
begin
  {$IFDEF WINDOWS}
  // 在 Windows 平台上注册 WinSSL 后端
  // 优先级设为 200，高于 OpenSSL 的 100，使其成为 Windows 上的默认选择
  TSSLFactory.RegisterLibrary(sslWinSSL, TWinSSLLibrary,
    'Windows Schannel (Native SSL/TLS)', 200);
  {$ENDIF}
end;

procedure UnregisterWinSSLBackend;
begin
  {$IFDEF WINDOWS}
  TSSLFactory.UnregisterLibrary(sslWinSSL);
  {$ENDIF}
end;

initialization
  {$IFDEF WINDOWS}
  RegisterWinSSLBackend;
  {$ENDIF}

finalization
  {$IFDEF WINDOWS}
  UnregisterWinSSLBackend;
  {$ENDIF}

end.
