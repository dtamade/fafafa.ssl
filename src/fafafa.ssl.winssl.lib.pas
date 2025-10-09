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
  Windows, SysUtils, Classes, StrUtils,
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
  fafafa.ssl.winssl.types,
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
    procedure InternalLog(aLevel: TSSLLogLevel; const aMessage: string);
    function DetectWindowsVersion: Boolean;
    function CheckSchannelSupport: Boolean;
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
function CreateWinSSLLibrary: ISSLLibrary;

implementation

uses
  fafafa.ssl.winssl.context;

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
    VerifyDepth := 9;
    CipherList := '';  // 使用 Windows 默认
    CipherSuites := ''; // 使用 Windows 默认
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

procedure TWinSSLLibrary.InternalLog(aLevel: TSSLLogLevel; const aMessage: string);
begin
  if Assigned(FLogCallback) and (aLevel <= FLogLevel) then
    FLogCallback(aLevel, aMessage);
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
       IfThen(FWindowsVersion.IsServer, 'Server', 'Workstation')]));
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

procedure TWinSSLLibrary.SetError(aError: Integer; const aErrorMsg: string);
begin
  FLastError := aError;
  FLastErrorString := aErrorMsg;
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

function TWinSSLLibrary.IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
  
  if not FInitialized then
    Exit;
  
  // Windows 版本与 TLS 支持对应关系
  case aProtocol of
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

function TWinSSLLibrary.IsCipherSupported(const aCipherName: string): Boolean;
begin
  // Windows Schannel 的密码套件支持由系统策略决定
  // 这里简单返回 True，实际支持在握手时由系统确定
  Result := True;
  InternalLog(sslLogDebug, Format('Cipher support check: %s (deferred to system)', [aCipherName]));
end;

function TWinSSLLibrary.IsFeatureSupported(const aFeatureName: string): Boolean;
var
  Feature: string;
begin
  Feature := LowerCase(aFeatureName);
  
  // 支持的功能列表
  Result := False;
  
  if Feature = 'sni' then
    Result := True
  else if Feature = 'alpn' then
    Result := (FWindowsVersion.Major >= 10) or  // Windows 10+
              ((FWindowsVersion.Major = 6) and (FWindowsVersion.Minor >= 2))  // Windows 8+
  else if Feature = 'session_cache' then
    Result := True
  else if Feature = 'session_tickets' then
    Result := True
  else if Feature = 'renegotiation' then
    Result := True
  else if Feature = 'ocsp_stapling' then
    Result := False  // 需要手动实现
  else if Feature = 'certificate_transparency' then
    Result := False;
    
    InternalLog(sslLogDebug, Format('Feature support check: %s = %s', 
    [aFeatureName, IfThen(Result, 'supported', 'not supported')]));
end;

// ============================================================================
// ISSLLibrary - 库配置
// ============================================================================

procedure TWinSSLLibrary.SetDefaultConfig(const aConfig: TSSLConfig);
begin
  FDefaultConfig := aConfig;
  FLogLevel := aConfig.LogLevel;
  FLogCallback := aConfig.LogCallback;
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

procedure TWinSSLLibrary.SetLogCallback(aCallback: TSSLLogCallback);
begin
  FLogCallback := aCallback;
end;

procedure TWinSSLLibrary.Log(aLevel: TSSLLogLevel; const aMessage: string);
begin
  InternalLog(aLevel, aMessage);
end;

// ============================================================================
// ISSLLibrary - 工厂方法
// ============================================================================

function TWinSSLLibrary.CreateContext(aType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
  begin
    SetError(-1, 'Library not initialized');
    InternalLog(sslLogError, 'Cannot create context: library not initialized');
    Result := nil;
    Exit;
  end;
  
  try
    Result := TWinSSLContext.Create(Self, aType);
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

function TWinSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  // TODO: 实现证书创建
  Result := nil;
  InternalLog(sslLogWarning, 'CreateCertificate not yet implemented');
end;

function TWinSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  // TODO: 实现证书存储创建
  Result := nil;
  InternalLog(sslLogWarning, 'CreateCertificateStore not yet implemented');
end;

end.
