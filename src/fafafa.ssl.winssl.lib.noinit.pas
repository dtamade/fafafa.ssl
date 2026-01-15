{
  fafafa.ssl.winssl.library - WinSSL 库管理实现 (无自动初始化版本)
  
  版本: 1.0
  作者: fafafa.ssl 开发团队
  创建: 2025-10-06
  
  描述:
    实现 ISSLLibrary 接口的 WinSSL 后端。
    负责 Windows Schannel 的初始化、配置和上下文创建。
    
  注意:
    此版本不包含自动初始化，需要手动调用 RegisterWinSSLBackend。
}

unit fafafa.ssl.winssl.lib.noinit;

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
    function GetCapabilities: TSSLBackendCapabilities;

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

{ 手动注册/注销函数 }
procedure RegisterWinSSLBackend;
procedure UnregisterWinSSLBackend;

implementation

uses
  fafafa.ssl.winssl.context,
  fafafa.ssl.winssl.certificate,
  fafafa.ssl.winssl.certstore,
  fafafa.ssl.errors,   // P0 后端语义统一：引入统一的错误抛出函数
  fafafa.ssl.factory;

// 复制原始 fafafa.ssl.winssl.lib.pas 的所有实现代码...
// (为简洁起见，这里只是一个框架)

function CreateWinSSLLibrary: ISSLLibrary;
begin
  Result := TWinSSLLibrary.Create;
end;

constructor TWinSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  FLastError := 0;
  FLastErrorString := '';
  FLogCallback := nil;
  FLogLevel := sslLogError;
  
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  with FDefaultConfig do
  begin
    LibraryType := sslWinSSL;
    ContextType := sslCtxClient;
    ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    PreferredVersion := sslProtocolTLS13;
    VerifyMode := [sslVerifyPeer];
    VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
    CipherList := '';
    CipherSuites := '';
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
  
  FillChar(FStatistics, SizeOf(FStatistics), 0);
  FillChar(FWindowsVersion, SizeOf(FWindowsVersion), 0);
end;

destructor TWinSSLLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited Destroy;
end;

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
    FWindowsVersion.IsServer := False;
    Result := True;
  end
  else
    SetError(GetLastError, 'Failed to detect Windows version');
end;

function TWinSSLLibrary.CheckSchannelSupport: Boolean;
var
  CredHandle: TSecHandle;
  TimeStamp: TTimeStamp;
  Status: SECURITY_STATUS;
  SchannelCred: SCHANNEL_CRED;
begin
  Result := False;
  
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
  end
  else
    SetError(Status, Format('Schannel not available: %s', [GetSchannelErrorString(Status)]));
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

function TWinSSLLibrary.Initialize: Boolean;
begin
  Result := False;
  
  if FInitialized then
    Exit(True);
  
  ClearInternalError;
  
  if not DetectWindowsVersion then
    Exit(False);
    
  if (FWindowsVersion.Major < 6) then
  begin
    SetError(-1, 'Windows version too old. Schannel requires Windows Vista or later.');
    Exit(False);
  end;
  
  if not CheckSchannelSupport then
    Exit(False);
  
  FInitialized := True;
  Result := True;
end;

procedure TWinSSLLibrary.Finalize;
begin
  if not FInitialized then
    Exit;
  FInitialized := False;
end;

function TWinSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TWinSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWinSSL;
end;

function TWinSSLLibrary.GetVersionString: string;
begin
  if FInitialized then
    Result := Format('Windows Schannel %d.%d (Build %d)',
      [FWindowsVersion.Major, FWindowsVersion.Minor, FWindowsVersion.Build])
  else
    Result := 'Windows Schannel (not initialized)';
end;

function TWinSSLLibrary.GetVersionNumber: Cardinal;
begin
  if FInitialized then
    Result := (FWindowsVersion.Major shl 16) or FWindowsVersion.Minor
  else
    Result := 0;
end;

function TWinSSLLibrary.GetCompileFlags: string;
begin
  Result := 'Native Windows Schannel';
  {$IFDEF CPU64}
  Result := Result + ', x64';
  {$ELSE}
  Result := Result + ', x86';
  {$ENDIF}
end;

function TWinSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
  if not FInitialized then Exit;
  
  case AProtocol of
    sslProtocolSSL2, sslProtocolSSL3: Result := False;
    sslProtocolTLS10: Result := (FWindowsVersion.Major >= 6);
    sslProtocolTLS11: Result := (FWindowsVersion.Major > 6) or ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 1));
    sslProtocolTLS12: Result := (FWindowsVersion.Major > 6) or ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 1));
    sslProtocolTLS13: Result := (FWindowsVersion.Major >= 10) and (FWindowsVersion.Build >= 20348);
    sslProtocolDTLS10, sslProtocolDTLS12: Result := False;
  end;
end;

function TWinSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := True;
end;

function TWinSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  case AFeature of
    sslFeatSNI: Result := True;
    sslFeatALPN: Result := (FWindowsVersion.Major >= 10) or ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 2));
    sslFeatSessionCache: Result := True;
    sslFeatSessionTickets: Result := True;
    sslFeatRenegotiation: Result := True;
    sslFeatOCSPStapling: Result := False;
    sslFeatCertificateTransparency: Result := False;
  else
    Result := False;
  end;
end;

function TWinSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.SupportsTLS13 := (FWindowsVersion.Major >= 10) and (FWindowsVersion.Build >= 18362);
  Result.SupportsALPN := (FWindowsVersion.Major >= 10) or ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 2));
  Result.SupportsSNI := True;
  Result.SupportsOCSPStapling := False;
  Result.SupportsCertificateTransparency := False;
  Result.SupportsSessionTickets := True;
  Result.SupportsECDHE := (FWindowsVersion.Major >= 6);
  Result.SupportsChaChaPoly := (FWindowsVersion.Major >= 10) and (FWindowsVersion.Build >= 18362);
  Result.SupportsPEMPrivateKey := False;
  Result.MinTLSVersion := sslProtocolTLS10;
  if Result.SupportsTLS13 then
    Result.MaxTLSVersion := sslProtocolTLS13
  else
    Result.MaxTLSVersion := sslProtocolTLS12;
end;

procedure TWinSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
  FLogLevel := AConfig.LogLevel;
  FLogCallback := AConfig.LogCallback;
end;

function TWinSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

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

function TWinSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TWinSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

procedure TWinSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TWinSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

function TWinSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  // P0 后端语义统一：与 OpenSSL 后端保持一致的失败语义
  // 未初始化时抛出异常，而不是返回 nil
  if not FInitialized then
    RaiseSSLInitError(
      'Cannot create context: WinSSL library not initialized',
      'TWinSSLLibrary.CreateContext'
    );

  // 让异常传播 - 调用方必须显式处理错误
  Result := TWinSSLContext.Create(Self, AType);

  if (Result <> nil) and (FDefaultConfig.Options <> []) then
    Result.SetOptions(FDefaultConfig.Options);

  Inc(FStatistics.ConnectionsTotal);
end;

function TWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  // P0 后端语义统一：与 OpenSSL 后端保持一致的失败语义
  if not FInitialized then
    RaiseSSLInitError(
      'Cannot create certificate: WinSSL library not initialized',
      'TWinSSLLibrary.CreateCertificate'
    );

  Result := TWinSSLCertificate.Create(nil, False);
end;

function TWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  // P0 后端语义统一：与 OpenSSL 后端保持一致的失败语义
  if not FInitialized then
    RaiseSSLInitError(
      'Cannot create certificate store: WinSSL library not initialized',
      'TWinSSLLibrary.CreateCertificateStore'
    );

  Result := TWinSSLCertificateStore.Create(SSL_STORE_ROOT);
end;

procedure RegisterWinSSLBackend;
begin
  {$IFDEF WINDOWS}
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

// NO initialization section - must call RegisterWinSSLBackend manually

end.
