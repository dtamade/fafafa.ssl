{**
 * Unit: fafafa.ssl.mbedtls.lib
 * Purpose: MbedTLS 后端库管理实现
 *
 * P3-9: MbedTLS 后端框架 - ISSLLibrary 实现
 *
 * 实现策略：
 * - 最小子集：仅实现 TLS Client/Server + 证书验证主链路
 * - 能力门控：不支持的功能通过 TSSLBackendCapabilities 显式标记
 * - 统一语义：与 OpenSSL/WinSSL/WolfSSL 后端保持一致的失败语义（fail-fast）
 *
 * MbedTLS 特殊处理：
 * - 需要管理全局熵源和随机数生成器
 * - 这些资源在 Initialize 时创建，供所有上下文共享
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-01-09
 *}

unit fafafa.ssl.mbedtls.lib;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.mbedtls.base,
  fafafa.ssl.mbedtls.api;

type
  { TMbedTLSLibrary - MbedTLS 库管理类 }
  TMbedTLSLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLastError: Integer;
    FLastErrorString: string;
    FLogCallback: TSSLLogCallback;
    FLogLevel: TSSLLogLevel;
    FCapabilities: TMbedTLSCapabilities;

    // MbedTLS 特有：全局熵源和随机数生成器
    FEntropyContext: Pmbedtls_entropy_context;
    FCtrDrbgContext: Pmbedtls_ctr_drbg_context;

    procedure InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
    procedure SetError(AError: Integer; const AErrorMsg: string);
    procedure ClearInternalError;
    function DetectCapabilities: Boolean;
    function InitializeRNG: Boolean;
    procedure FinalizeRNG;

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

    { MbedTLS 特有：获取随机数生成器上下文 }
    function GetCtrDrbgContext: Pmbedtls_ctr_drbg_context;
  end;

{ 全局工厂函数 }
function CreateMbedTLSLibrary: ISSLLibrary;

{ 手动注册/注销函数 }
procedure RegisterMbedTLSBackend;
procedure UnregisterMbedTLSBackend;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.exceptions,
  fafafa.ssl.factory,
  fafafa.ssl.mbedtls.context,
  fafafa.ssl.mbedtls.certificate;

const
  // MbedTLS 上下文结构体大小（估算值，实际大小取决于编译配置）
  // Use large buffers for safety - MbedTLS 3.x structures can be quite large
  MBEDTLS_ENTROPY_CONTEXT_SIZE = 4096;
  MBEDTLS_CTR_DRBG_CONTEXT_SIZE = 2048;

function CreateMbedTLSLibrary: ISSLLibrary;
begin
  Result := TMbedTLSLibrary.Create;
end;

{ TMbedTLSLibrary }

constructor TMbedTLSLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  FLastError := 0;
  FLastErrorString := '';
  FLogCallback := nil;
  FLogLevel := sslLogError;
  FEntropyContext := nil;
  FCtrDrbgContext := nil;

  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  with FDefaultConfig do
  begin
    LibraryType := sslMbedTLS;
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

destructor TMbedTLSLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited Destroy;
end;

procedure TMbedTLSLibrary.InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FLogCallback) and (ALevel <= FLogLevel) then
    FLogCallback(ALevel, '[MbedTLS] ' + AMessage);
end;

procedure TMbedTLSLibrary.SetError(AError: Integer; const AErrorMsg: string);
begin
  FLastError := AError;
  FLastErrorString := AErrorMsg;
  InternalLog(sslLogError, AErrorMsg);
end;

procedure TMbedTLSLibrary.ClearInternalError;
begin
  FLastError := 0;
  FLastErrorString := '';
end;

function TMbedTLSLibrary.DetectCapabilities: Boolean;
var
  LVersionBuf: array[0..31] of AnsiChar;
begin
  Result := False;

  if not IsMbedTLSLoaded then
    Exit;

  // 检测版本
  if Assigned(mbedtls_version_get_string) then
  begin
    FillChar(LVersionBuf, SizeOf(LVersionBuf), 0);
    mbedtls_version_get_string(@LVersionBuf[0]);
    FCapabilities.VersionString := string(LVersionBuf);
  end
  else
    FCapabilities.VersionString := 'Unknown';

  if Assigned(mbedtls_version_get_number) then
    FCapabilities.VersionNumber := mbedtls_version_get_number()
  else
    FCapabilities.VersionNumber := 0;

  // MbedTLS 3.x 支持 TLS 1.3
  FCapabilities.HasTLS12 := True;
  FCapabilities.HasTLS13 := FCapabilities.VersionNumber >= MBEDTLS_MIN_VERSION;

  // MbedTLS 默认支持的功能
  FCapabilities.HasSNI := True;
  FCapabilities.HasALPN := True;
  FCapabilities.HasSessionTickets := True;
  FCapabilities.HasECDHE := True;
  FCapabilities.HasChaCha20 := True;
  FCapabilities.HasAESNI := True;

  Result := True;
end;

function TMbedTLSLibrary.InitializeRNG: Boolean;
var
  LRet: Integer;
begin
  Result := False;

  // 分配熵源上下文
  GetMem(FEntropyContext, MBEDTLS_ENTROPY_CONTEXT_SIZE);
  FillChar(FEntropyContext^, MBEDTLS_ENTROPY_CONTEXT_SIZE, 0);

  // 分配随机数生成器上下文
  GetMem(FCtrDrbgContext, MBEDTLS_CTR_DRBG_CONTEXT_SIZE);
  FillChar(FCtrDrbgContext^, MBEDTLS_CTR_DRBG_CONTEXT_SIZE, 0);

  // 初始化熵源
  if Assigned(mbedtls_entropy_init) then
    mbedtls_entropy_init(FEntropyContext);

  // 初始化随机数生成器
  if Assigned(mbedtls_ctr_drbg_init) then
    mbedtls_ctr_drbg_init(FCtrDrbgContext);

  // 种子随机数生成器
  if Assigned(mbedtls_ctr_drbg_seed) and Assigned(mbedtls_entropy_func) then
  begin
    LRet := mbedtls_ctr_drbg_seed(FCtrDrbgContext,
      mbedtls_entropy_func, FEntropyContext, nil, 0);
    if LRet <> 0 then
    begin
      SetError(LRet, Format('mbedtls_ctr_drbg_seed failed: 0x%04X', [-LRet]));
      FinalizeRNG;
      Exit(False);
    end;
  end
  else
  begin
    SetError(-1, 'Required RNG functions not available');
    FinalizeRNG;
    Exit(False);
  end;

  Result := True;
end;

procedure TMbedTLSLibrary.FinalizeRNG;
begin
  if FCtrDrbgContext <> nil then
  begin
    if Assigned(mbedtls_ctr_drbg_free) then
      mbedtls_ctr_drbg_free(FCtrDrbgContext);
    FreeMem(FCtrDrbgContext);
    FCtrDrbgContext := nil;
  end;

  if FEntropyContext <> nil then
  begin
    if Assigned(mbedtls_entropy_free) then
      mbedtls_entropy_free(FEntropyContext);
    FreeMem(FEntropyContext);
    FEntropyContext := nil;
  end;
end;

function TMbedTLSLibrary.Initialize: Boolean;
begin
  Result := False;

  if FInitialized then
    Exit(True);

  ClearInternalError;

  // 加载 MbedTLS 库
  if not LoadMbedTLSLibrary then
  begin
    SetError(-1, 'Failed to load MbedTLS libraries');
    Exit(False);
  end;

  // 检测能力
  if not DetectCapabilities then
  begin
    SetError(-2, 'Failed to detect MbedTLS capabilities');
    UnloadMbedTLSLibrary;
    Exit(False);
  end;

  // 初始化随机数生成器（MbedTLS 特有）
  if not InitializeRNG then
  begin
    UnloadMbedTLSLibrary;
    Exit(False);
  end;

  FInitialized := True;
  InternalLog(sslLogInfo, Format('MbedTLS initialized: %s', [FCapabilities.VersionString]));
  Result := True;
end;

procedure TMbedTLSLibrary.Finalize;
begin
  if not FInitialized then
    Exit;

  FinalizeRNG;
  UnloadMbedTLSLibrary;
  FInitialized := False;
  InternalLog(sslLogInfo, 'MbedTLS finalized');
end;

function TMbedTLSLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TMbedTLSLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TMbedTLSLibrary.GetVersionString: string;
begin
  if FInitialized then
    Result := 'MbedTLS ' + FCapabilities.VersionString
  else
    Result := 'MbedTLS (not initialized)';
end;

function TMbedTLSLibrary.GetVersionNumber: Cardinal;
begin
  Result := FCapabilities.VersionNumber;
end;

function TMbedTLSLibrary.GetCompileFlags: string;
begin
  Result := 'MbedTLS';
  {$IFDEF CPU64}
  Result := Result + ', x64';
  {$ELSE}
  Result := Result + ', x86';
  {$ENDIF}
end;

function TMbedTLSLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := False;
  if not FInitialized then Exit;

  case AProtocol of
    sslProtocolSSL2, sslProtocolSSL3: Result := False;  // 不支持废弃协议
    sslProtocolTLS10: Result := False;  // MbedTLS 3.x 默认禁用
    sslProtocolTLS11: Result := False;  // MbedTLS 3.x 默认禁用
    sslProtocolTLS12: Result := FCapabilities.HasTLS12;
    sslProtocolTLS13: Result := FCapabilities.HasTLS13;
    sslProtocolDTLS10, sslProtocolDTLS12: Result := False;  // 暂不支持
  end;
end;

function TMbedTLSLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := FInitialized;
end;

function TMbedTLSLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  if not FInitialized then
    Exit(False);

  case AFeature of
    sslFeatSNI: Result := FCapabilities.HasSNI;
    sslFeatALPN: Result := FCapabilities.HasALPN;
    sslFeatSessionCache: Result := True;
    sslFeatSessionTickets: Result := FCapabilities.HasSessionTickets;
    sslFeatRenegotiation: Result := False;  // 安全考虑，默认禁用
    sslFeatOCSPStapling: Result := False;   // 需要手动实现
    sslFeatCertificateTransparency: Result := False;
  else
    Result := False;
  end;
end;

function TMbedTLSLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  FillChar(Result, SizeOf(Result), 0);

  if not FInitialized then
    Exit;

  Result.SupportsTLS13 := FCapabilities.HasTLS13;
  Result.SupportsALPN := FCapabilities.HasALPN;
  Result.SupportsSNI := FCapabilities.HasSNI;
  Result.SupportsOCSPStapling := False;
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

procedure TMbedTLSLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
  FLogLevel := AConfig.LogLevel;
  FLogCallback := AConfig.LogCallback;
end;

function TMbedTLSLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

function TMbedTLSLibrary.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TMbedTLSLibrary.GetLastErrorString: string;
begin
  Result := FLastErrorString;
end;

procedure TMbedTLSLibrary.ClearError;
begin
  ClearInternalError;
end;

function TMbedTLSLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TMbedTLSLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

procedure TMbedTLSLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TMbedTLSLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

function TMbedTLSLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
    raise ESSLInitError.Create('Cannot create context: MbedTLS library not initialized');

  Result := TMbedTLSContext.Create(Self, AType);
  InternalLog(sslLogDebug, 'Created MbedTLS context');
end;

function TMbedTLSLibrary.CreateCertificate: ISSLCertificate;
begin
  if not FInitialized then
    raise ESSLInitError.Create('Cannot create certificate: MbedTLS library not initialized');

  Result := TMbedTLSCertificate.Create;
end;

function TMbedTLSLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  if not FInitialized then
    raise ESSLInitError.Create('Cannot create certificate store: MbedTLS library not initialized');

  Result := TMbedTLSCertificateStore.Create;
end;

function TMbedTLSLibrary.GetCtrDrbgContext: Pmbedtls_ctr_drbg_context;
begin
  Result := FCtrDrbgContext;
end;

{ 注册函数 }

procedure RegisterMbedTLSBackend;
begin
  try
    TSSLFactory.RegisterLibrary(sslMbedTLS, TMbedTLSLibrary,
      'MbedTLS (Embedded TLS)', 175);
  except
  end;
end;

procedure UnregisterMbedTLSBackend;
begin
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
end;

end.
