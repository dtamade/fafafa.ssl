{**
 * Unit: fafafa.ssl.wolfssl.lib
 * Purpose: WolfSSL 后端库管理实现
 *
 * P2-7: WolfSSL 后端框架 - ISSLLibrary 实现
 *
 * 实现策略：
 * - 最小子集：仅实现 TLS Client/Server + 证书验证主链路
 * - 能力门控：不支持的功能通过 TSSLBackendCapabilities 显式标记
 * - 统一语义：与 OpenSSL/WinSSL 后端保持一致的失败语义（fail-fast）
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-09
 *}

unit fafafa.ssl.wolfssl.lib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.wolfssl.base,
  fafafa.ssl.wolfssl.api;

type
  { TWolfSSLLibrary - WolfSSL 库管理类 }
  TWolfSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLastError: Integer;
    FLastErrorString: string;
    FLogCallback: TSSLLogCallback;
    FLogLevel: TSSLLogLevel;
    FCapabilities: TWolfSSLCapabilities;

    procedure InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
    procedure SetError(AError: Integer; const AErrorMsg: string);
    procedure ClearInternalError;
    function DetectCapabilities: Boolean;

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
function CreateWolfSSLLibrary: ISSLLibrary;

{ 手动注册/注销函数 }
procedure RegisterWolfSSLBackend;
procedure UnregisterWolfSSLBackend;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.factory,
  fafafa.ssl.wolfssl.context,
  fafafa.ssl.wolfssl.certificate;

function CreateWolfSSLLibrary: ISSLLibrary;
begin
  Result := TWolfSSLLibrary.Create;
end;

{ TWolfSSLLibrary }

constructor TWolfSSLLibrary.Create;
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
    LibraryType := sslWolfSSL;
    ContextType := sslCtxClient;
    ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
    PreferredVersion := sslProtocolTLS13;
    VerifyMode := [sslVerifyPeer];
    VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
    Options := [ssoEnableSNI];
    BufferSize := SSL_DEFAULT_BUFFER_SIZE;
    HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
    SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
    SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
    LogLevel := sslLogError;
  end;

  FillChar(FStatistics, SizeOf(FStatistics), 0);
  FillChar(FCapabilities, SizeOf(FCapabilities), 0);
end;

destructor TWolfSSLLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited Destroy;
end;

procedure TWolfSSLLibrary.InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FLogCallback) and (ALevel <= FLogLevel) then
    FLogCallback(ALevel, '[WolfSSL] ' + AMessage);
end;

procedure TWolfSSLLibrary.SetError(AError: Integer; const AErrorMsg: string);
begin
  FLastError := AError;
  FLastErrorString := AErrorMsg;
  InternalLog(sslLogError, AErrorMsg);
end;

procedure TWolfSSLLibrary.ClearInternalError;
begin
  FLastError := 0;
  FLastErrorString := '';
end;

function TWolfSSLLibrary.DetectCapabilities: Boolean;
var
  LVer: string;
  LParts: array of string;
  LMajor, LMinor, LPatch: Integer;
begin
  Result := False;

  if not IsWolfSSLLoaded then
    Exit;

  // 检测版本
  if Assigned(wolfSSL_lib_version) then
    FCapabilities.VersionString := string(wolfSSL_lib_version())
  else
    FCapabilities.VersionString := 'Unknown';

  // 解析版本号 (例如 "5.7.2" -> 50702)
  LVer := FCapabilities.VersionString;
  LMajor := 0;
  LMinor := 0;
  LPatch := 0;
  SetLength(LParts, 0);

  // 简单解析 major.minor.patch
  if Pos('.', LVer) > 0 then
  begin
    LMajor := StrToIntDef(Copy(LVer, 1, Pos('.', LVer) - 1), 0);
    Delete(LVer, 1, Pos('.', LVer));
    if Pos('.', LVer) > 0 then
    begin
      LMinor := StrToIntDef(Copy(LVer, 1, Pos('.', LVer) - 1), 0);
      Delete(LVer, 1, Pos('.', LVer));
      LPatch := StrToIntDef(LVer, 0);
    end
    else
      LMinor := StrToIntDef(LVer, 0);
  end;
  FCapabilities.VersionNumber := LMajor * 10000 + LMinor * 100 + LPatch;

  // 检测 TLS 1.3 支持
  FCapabilities.HasTLS13 := Assigned(wolfTLSv1_3_client_method);

  // 检测 SNI 支持
  FCapabilities.HasSNI := Assigned(wolfSSL_UseSNI);

  // WolfSSL 默认支持的功能
  FCapabilities.HasALPN := True;  // 需要编译时启用
  FCapabilities.HasSessionTickets := True;
  FCapabilities.HasECDHE := True;
  FCapabilities.HasChaCha20 := True;
  FCapabilities.HasOCSP := False;  // 需要额外配置

  Result := True;
end;

function TWolfSSLLibrary.Initialize: Boolean;
begin
  Result := False;

  if FInitialized then
    Exit(True);

  ClearInternalError;

  // 加载 WolfSSL 库
  if not LoadWolfSSLLibrary then
  begin
    SetError(-1, 'Failed to load WolfSSL library: ' + WOLFSSL_LIB_NAME);
    Exit(False);
  end;

  // 初始化 WolfSSL
  if Assigned(wolfssl_init) then
  begin
    if wolfssl_init() <> WOLFSSL_SUCCESS then
    begin
      SetError(-2, 'wolfSSL_Init() failed');
      UnloadWolfSSLLibrary;
      Exit(False);
    end;
  end;

  // 检测能力
  if not DetectCapabilities then
  begin
    SetError(-3, 'Failed to detect WolfSSL capabilities');
    UnloadWolfSSLLibrary;
    Exit(False);
  end;

  FInitialized := True;
  InternalLog(sslLogInfo, Format('WolfSSL initialized: %s', [FCapabilities.VersionString]));
  Result := True;
end;

procedure TWolfSSLLibrary.Finalize;
begin
  if not FInitialized then
    Exit;

  if Assigned(wolfssl_cleanup) then
    wolfssl_cleanup();

  UnloadWolfSSLLibrary;
  FInitialized := False;
  InternalLog(sslLogInfo, 'WolfSSL finalized');
end;

function TWolfSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TWolfSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslWolfSSL;
end;

function TWolfSSLLibrary.GetVersionString: string;
begin
  if FInitialized then
    Result := 'WolfSSL ' + FCapabilities.VersionString
  else
    Result := 'WolfSSL (not initialized)';
end;

function TWolfSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := FCapabilities.VersionNumber;
end;

function TWolfSSLLibrary.GetCompileFlags: string;
begin
  Result := 'WolfSSL';
  {$IFDEF CPU64}
  Result := Result + ', x64';
  {$ELSE}
  Result := Result + ', x86';
  {$ENDIF}
end;

function TWolfSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
  if not FInitialized then Exit;

  case AProtocol of
    sslProtocolSSL2, sslProtocolSSL3: Result := False;  // 不支持废弃协议
    sslProtocolTLS10: Result := False;  // WolfSSL 默认禁用
    sslProtocolTLS11: Result := False;  // WolfSSL 默认禁用
    sslProtocolTLS12: Result := True;
    sslProtocolTLS13: Result := FCapabilities.HasTLS13;
    sslProtocolDTLS10, sslProtocolDTLS12: Result := False;  // 暂不支持
  end;
end;

function TWolfSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  // WolfSSL 支持大多数现代密码套件
  Result := FInitialized;
end;

function TWolfSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  if not FInitialized then
    Exit(False);

  case AFeature of
    sslFeatSNI: Result := FCapabilities.HasSNI;
    sslFeatALPN: Result := FCapabilities.HasALPN;
    sslFeatSessionCache: Result := True;
    sslFeatSessionTickets: Result := FCapabilities.HasSessionTickets;
    sslFeatRenegotiation: Result := False;  // 安全考虑，默认禁用
    sslFeatOCSPStapling: Result := FCapabilities.HasOCSP;
    sslFeatCertificateTransparency: Result := False;
  else
    Result := False;
  end;
end;

function TWolfSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);

  if not FInitialized then
    Exit;

  Result.SupportsTLS13 := FCapabilities.HasTLS13;
  Result.SupportsALPN := FCapabilities.HasALPN;
  Result.SupportsSNI := FCapabilities.HasSNI;
  Result.SupportsOCSPStapling := FCapabilities.HasOCSP;
  Result.SupportsCertificateTransparency := False;
  Result.SupportsSessionTickets := FCapabilities.HasSessionTickets;
  Result.SupportsECDHE := FCapabilities.HasECDHE;
  Result.SupportsChaChaPoly := FCapabilities.HasChaCha20;
  Result.SupportsPEMPrivateKey := True;
  Result.MinTLSVersion := sslProtocolTLS12;

  if FCapabilities.HasTLS13 then
    Result.MaxTLSVersion := sslProtocolTLS13
  else
    Result.MaxTLSVersion := sslProtocolTLS12;
end;

procedure TWolfSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
  FLogLevel := AConfig.LogLevel;
  FLogCallback := AConfig.LogCallback;
end;

function TWolfSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

function TWolfSSLLibrary.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TWolfSSLLibrary.GetLastErrorString: string;
begin
  Result := FLastErrorString;
end;

procedure TWolfSSLLibrary.ClearError;
begin
  ClearInternalError;
end;

function TWolfSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TWolfSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

procedure TWolfSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TWolfSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

function TWolfSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  // P0 后端语义统一：与 OpenSSL/WinSSL 后端保持一致的失败语义
  if not FInitialized then
    raise ESSLInitError.Create('Cannot create context: WolfSSL library not initialized');

  Result := TWolfSSLContext.Create(Self, AType);
end;

function TWolfSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  if not FInitialized then
    raise ESSLInitError.Create('Cannot create certificate: WolfSSL library not initialized');

  Result := TWolfSSLCertificate.Create;
end;

function TWolfSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  if not FInitialized then
    raise ESSLInitError.Create('Cannot create certificate store: WolfSSL library not initialized');

  Result := TWolfSSLCertificateStore.Create;
end;

{ 注册函数 }

procedure RegisterWolfSSLBackend;
begin
  try
    // 注册 WolfSSL 后端，优先级 150（介于 OpenSSL 100 和 WinSSL 200 之间）
    TSSLFactory.RegisterLibrary(sslWolfSSL, TWolfSSLLibrary,
      'WolfSSL (Lightweight TLS)', 150);
  except
    // 注册失败时静默处理
  end;
end;

procedure UnregisterWolfSSLBackend;
begin
  TSSLFactory.UnregisterLibrary(sslWolfSSL);
end;

initialization
  RegisterWolfSSLBackend;

finalization
  UnregisterWolfSSLBackend;

end.
