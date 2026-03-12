program test_factory_shared_config_and_init_race;

{$mode ObjFPC}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  SysUtils, Classes, SyncObjs,
  fafafa.ssl.base,
  fafafa.ssl.exceptions,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.context;

const
  INIT_RACE_THREADS = 16;
  INIT_RACE_SLEEP_MS = 75;

type
  TBaseFakeLibrary = class(TInterfacedObject, ISSLLibrary)
  protected
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    function BackendType: TSSLLibraryType; virtual; abstract;
    procedure InitBaselineConfig;
    procedure EnsureBaselineConfig;
  public
    constructor Create;
    function Initialize: Boolean; virtual;
    procedure Finalize;
    function IsInitialized: Boolean;
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    function IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const ACipherName: string): Boolean;
    function IsFeatureSupported(AFeature: TSSLFeature): Boolean;
    function GetCapabilities: TSSLBackendCapabilities;
    procedure SetDefaultConfig(const AConfig: TSSLConfig); virtual;
    function GetDefaultConfig: TSSLConfig;
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    procedure SetLogCallback(ACallback: TSSLLogCallback);
    procedure Log(ALevel: TSSLLogLevel; const AMessage: string);
    function CreateContext(AType: TSSLContextType): ISSLContext; virtual;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

  TConfigLeakFakeLibrary = class(TBaseFakeLibrary)
  protected
    function BackendType: TSSLLibraryType; override;
  end;

  TSlowInitFakeLibrary = class(TBaseFakeLibrary)
  protected
    function BackendType: TSSLLibraryType; override;
  public
    function Initialize: Boolean; override;
  end;

  TLogProbe = class
  public
    Count: Integer;
    LastLevel: TSSLLogLevel;
    LastMessage: string;
    procedure HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
  end;

  TGetLibraryThread = class(TThread)
  private
    FBackend: TSSLLibraryType;
    FStartEvent: TEvent;
    FSuccess: Boolean;
    FError: string;
  protected
    procedure Execute; override;
  public
    constructor Create(AStartEvent: TEvent; ABackend: TSSLLibraryType);
    property Success: Boolean read FSuccess;
    property Error: string read FError;
  end;

var
  GSlowInitCalls: Integer = 0;

procedure TLogProbe.HandleLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  Inc(Count);
  LastLevel := ALevel;
  LastMessage := AMessage;
end;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    raise Exception.Create(AMessage);
end;

procedure RequireEquals(const AName, AExpected, AActual: string);
begin
  if AExpected <> AActual then
    raise Exception.CreateFmt('%s mismatch: expected="%s" actual="%s"', [AName, AExpected, AActual]);
end;

procedure RequireEquals(const AName: string; AExpected, AActual: Integer);
begin
  if AExpected <> AActual then
    raise Exception.CreateFmt('%s mismatch: expected=%d actual=%d', [AName, AExpected, AActual]);
end;

procedure InitCustomConfig(out AConfig: TSSLConfig; ALibType: TSSLLibraryType);
begin
  FillChar(AConfig, SizeOf(AConfig), 0);
  AConfig.LibraryType := ALibType;
  AConfig.ContextType := sslCtxClient;
  AConfig.ProtocolVersions := [sslProtocolTLS13];
  AConfig.PreferredVersion := sslProtocolTLS13;
  AConfig.VerifyMode := [sslVerifyNone];
  AConfig.VerifyDepth := 9;
  AConfig.CipherList := 'CUSTOM-CIPHER';
  AConfig.CipherSuites := 'TLS_AES_256_GCM_SHA384';
  AConfig.SessionTimeout := 123;
  AConfig.SessionCacheSize := 7;
  AConfig.Options := [ssoEnableSessionTickets, ssoEnableSNI];
  AConfig.ServerName := 'custom.example';
  AConfig.ALPNProtocols := 'h2,http/1.1';
end;

function MismatchedLibraryType(AOwner: TSSLLibraryType): TSSLLibraryType;
begin
  if AOwner = sslFreePascal then
    Result := sslMbedTLS
  else
    Result := sslFreePascal;
end;

procedure ExpectLibraryDefaultConfigError(const AName: string; ALib: ISSLLibrary;
  const AConfig: TSSLConfig; const AExpectedText: string);
var
  LRaised: Boolean;
begin
  LRaised := False;
  try
    ALib.SetDefaultConfig(AConfig);
  except
    on E: ESSLConfigurationException do
    begin
      LRaised := True;
      Require(Pos(AExpectedText, E.Message) > 0,
        AName + ' should mention ' + AExpectedText + ', actual=' + E.Message);
    end;
  end;

  Require(LRaised, AName + ' should raise ESSLConfigurationException');
end;

{ TBaseFakeLibrary }

constructor TBaseFakeLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  InitBaselineConfig;
end;

procedure TBaseFakeLibrary.InitBaselineConfig;
begin
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  FDefaultConfig.LibraryType := BackendType;
  FDefaultConfig.ContextType := sslCtxClient;
  FDefaultConfig.ProtocolVersions := [sslProtocolTLS12];
  FDefaultConfig.PreferredVersion := sslProtocolTLS12;
  FDefaultConfig.VerifyMode := [sslVerifyPeer];
  FDefaultConfig.VerifyDepth := 3;
  FDefaultConfig.CipherList := 'BASELINE-CIPHER';
  FDefaultConfig.CipherSuites := 'TLS_AES_128_GCM_SHA256';
  FDefaultConfig.SessionTimeout := 777;
  FDefaultConfig.SessionCacheSize := 33;
  FDefaultConfig.Options := [ssoEnableSNI];
  FDefaultConfig.ServerName := 'baseline.local';
  FDefaultConfig.ALPNProtocols := 'http/1.1';
end;

procedure TBaseFakeLibrary.EnsureBaselineConfig;
begin
  if FDefaultConfig.SessionTimeout <= 0 then
    InitBaselineConfig;
end;

function TBaseFakeLibrary.Initialize: Boolean;
begin
  FInitialized := True;
  Result := True;
end;

procedure TBaseFakeLibrary.Finalize;
begin
  FInitialized := False;
  InitBaselineConfig;
end;

function TBaseFakeLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TBaseFakeLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := BackendType;
end;

function TBaseFakeLibrary.GetVersionString: string;
begin
  Result := 'fake';
end;

function TBaseFakeLibrary.GetVersionNumber: Cardinal;
begin
  Result := 1;
end;

function TBaseFakeLibrary.GetCompileFlags: string;
begin
  Result := '';
end;

function TBaseFakeLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := AProtocol in [sslProtocolTLS12, sslProtocolTLS13];
end;

function TBaseFakeLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := True;
end;

function TBaseFakeLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  Result := True;
end;

function TBaseFakeLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  Result := Default(TSSLBackendCapabilities);
  Result.BackendType := BackendType;
  Result.MinTLSVersion := sslProtocolTLS12;
  Result.MaxTLSVersion := sslProtocolTLS13;
  Result.SupportsTLS13 := True;
  Result.SupportsSystemCertStore := True;
end;

procedure TBaseFakeLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
var
  LConfig: TSSLConfig;
begin
  LConfig := AConfig;
  TSSLFactory.NormalizeLibraryDefaultOwnerFields(LConfig, BackendType);
  TSSLFactory.ValidateLibraryDefaultConfigFields(LConfig, 'TBaseFakeLibrary.SetDefaultConfig');
  TSSLFactory.NormalizeConfig(LConfig);
  FDefaultConfig := LConfig;
end;

function TBaseFakeLibrary.GetDefaultConfig: TSSLConfig;
begin
  EnsureBaselineConfig;
  Result := FDefaultConfig;
end;

function TBaseFakeLibrary.GetLastError: Integer;
begin
  Result := 0;
end;

function TBaseFakeLibrary.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TBaseFakeLibrary.ClearError;
begin
end;

function TBaseFakeLibrary.GetStatistics: TSSLStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TBaseFakeLibrary.ResetStatistics;
begin
end;

procedure TBaseFakeLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FDefaultConfig.LogCallback := ACallback;
end;

procedure TBaseFakeLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FDefaultConfig.LogCallback) and (ALevel <= FDefaultConfig.LogLevel) then
    FDefaultConfig.LogCallback(ALevel, AMessage);
end;

function TBaseFakeLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
var
  LConfig: TSSLConfig;
begin
  EnsureBaselineConfig;
  Result := TFreePascalContext.Create(Self as ISSLLibrary, AType);
  if Result <> nil then
  begin
    LConfig := FDefaultConfig;
    LConfig.ContextType := AType;
    TSSLFactory.ApplyConfigToContext(Result, LConfig);
  end;
end;

function TBaseFakeLibrary.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TBaseFakeLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

function TConfigLeakFakeLibrary.BackendType: TSSLLibraryType;
begin
  Result := sslFreePascal;
end;

function TSlowInitFakeLibrary.BackendType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TSlowInitFakeLibrary.Initialize: Boolean;
begin
  InterlockedIncrement(GSlowInitCalls);
  Sleep(INIT_RACE_SLEEP_MS);
  Result := inherited Initialize;
end;

{ TGetLibraryThread }

constructor TGetLibraryThread.Create(AStartEvent: TEvent; ABackend: TSSLLibraryType);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FBackend := ABackend;
  FStartEvent := AStartEvent;
  FSuccess := False;
  FError := '';
end;

procedure TGetLibraryThread.Execute;
var
  LLib: ISSLLibrary;
begin
  try
    FStartEvent.WaitFor(INFINITE);
    LLib := TSSLFactory.GetLibrary(FBackend);
    FSuccess := LLib <> nil;
  except
    on E: Exception do
    begin
      FSuccess := False;
      FError := E.Message;
    end;
  end;
end;

function CreateConfigLeakFakeLibrary: ISSLLibrary;
begin
  Result := TConfigLeakFakeLibrary.Create;
end;

function CreateSlowInitFakeLibrary: ISSLLibrary;
begin
  Result := TSlowInitFakeLibrary.Create;
end;

procedure RegisterConfigLeakLibrary;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslFreePascal);
  TSSLFactory.RegisterLibrary(
    sslFreePascal,
    TConfigLeakFakeLibrary,
    'fake-config-leak',
    100,
    @CreateConfigLeakFakeLibrary
  );
  TSSLFactory.SetDefaultLibrary(sslFreePascal);
end;

procedure CleanupConfigLeakLibrary;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslFreePascal);
end;

procedure RegisterSlowInitLibrary;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
  TSSLFactory.RegisterLibrary(
    sslMbedTLS,
    TSlowInitFakeLibrary,
    'fake-slow-init',
    100,
    @CreateSlowInitFakeLibrary
  );
end;

procedure CleanupSlowInitLibrary;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
end;

procedure TestPerRequestConfigDoesNotMutateLibraryDefaults;
var
  LLib: ISSLLibrary;
  LContext: ISSLContext;
  LDefaultContext: ISSLContext;
  LBaselineConfig: TSSLConfig;
  LAfterConfig: TSSLConfig;
  LCustomConfig: TSSLConfig;
begin
  WriteLn('--- Test: per-request config must not mutate library defaults');

  RegisterConfigLeakLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    LBaselineConfig := LLib.GetDefaultConfig;

    InitCustomConfig(LCustomConfig, sslFreePascal);
    LContext := TSSLFactory.CreateContext(LCustomConfig);
    Require(LContext <> nil, 'CreateContext(custom config) returned nil');

    RequireEquals('custom session timeout', 123, LContext.GetSessionTimeout);
    RequireEquals('custom session cache size', 7, LContext.GetSessionCacheSize);
    RequireEquals('custom verify depth', 9, LContext.GetVerifyDepth);
    RequireEquals('custom ALPN', 'h2,http/1.1', LContext.GetALPNProtocols);
    RequireEquals('custom cipher list', 'CUSTOM-CIPHER', LContext.GetCipherList);
    RequireEquals('custom cipher suites', 'TLS_AES_256_GCM_SHA384', LContext.GetCipherSuites);
    Require(LContext.GetVerifyMode = [sslVerifyNone], 'custom verify mode should be sslVerifyNone');
    Require(LContext.GetProtocolVersions = [sslProtocolTLS13], 'custom protocol versions should be TLS13 only');

    LAfterConfig := LLib.GetDefaultConfig;
    RequireEquals('baseline session timeout', LBaselineConfig.SessionTimeout, LAfterConfig.SessionTimeout);
    RequireEquals('baseline session cache size', LBaselineConfig.SessionCacheSize, LAfterConfig.SessionCacheSize);
    RequireEquals('baseline verify depth', LBaselineConfig.VerifyDepth, LAfterConfig.VerifyDepth);
    RequireEquals('baseline ALPN', LBaselineConfig.ALPNProtocols, LAfterConfig.ALPNProtocols);
    RequireEquals('baseline cipher list', LBaselineConfig.CipherList, LAfterConfig.CipherList);
    RequireEquals('baseline cipher suites', LBaselineConfig.CipherSuites, LAfterConfig.CipherSuites);
    Require(LAfterConfig.VerifyMode = LBaselineConfig.VerifyMode, 'baseline verify mode must stay unchanged');

    LDefaultContext := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    Require(LDefaultContext <> nil, 'CreateContext(default) returned nil');
    RequireEquals('default context session timeout', LBaselineConfig.SessionTimeout, LDefaultContext.GetSessionTimeout);
    RequireEquals('default context session cache size', LBaselineConfig.SessionCacheSize, LDefaultContext.GetSessionCacheSize);
    RequireEquals('default context verify depth', LBaselineConfig.VerifyDepth, LDefaultContext.GetVerifyDepth);
    RequireEquals('default context ALPN', LBaselineConfig.ALPNProtocols, LDefaultContext.GetALPNProtocols);
    RequireEquals('default context cipher list', LBaselineConfig.CipherList, LDefaultContext.GetCipherList);
    RequireEquals('default context cipher suites', LBaselineConfig.CipherSuites, LDefaultContext.GetCipherSuites);
  finally
    CleanupConfigLeakLibrary;
  end;
end;

procedure TestFakeLibrarySetDefaultConfigValidatesAndNormalizes;
var
  LLib: ISSLLibrary;
  LConfig: TSSLConfig;
begin
  WriteLn('--- Test: fake backend SetDefaultConfig should validate and normalize library defaults');

  RegisterConfigLeakLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    Require(LLib <> nil, 'fake library instance should not be nil');

    InitCustomConfig(LConfig, sslFreePascal);
    LConfig.HandshakeTimeout := 100;
    ExpectLibraryDefaultConfigError('fake backend HandshakeTimeout', LLib, LConfig, 'HandshakeTimeout');

    InitCustomConfig(LConfig, sslFreePascal);
    LConfig.CertificateFile := '/tmp/request-only-cert.pem';
    ExpectLibraryDefaultConfigError('fake backend CertificateFile', LLib, LConfig, 'CertificateFile');

    InitCustomConfig(LConfig, sslFreePascal);
    LConfig.LibraryType := MismatchedLibraryType(sslFreePascal);
    LConfig.ContextType := sslCtxServer;
    LConfig.SessionTimeout := 456;
    LLib.SetDefaultConfig(LConfig);

    Require(LLib.GetDefaultConfig.LibraryType = sslFreePascal,
      'fake backend should normalize LibraryType back to owner');
    Require(LLib.GetDefaultConfig.ContextType = sslCtxClient,
      'fake backend should normalize ContextType back to stable client baseline');
    RequireEquals('fake backend normalized config preserves session timeout',
      456,
      LLib.GetDefaultConfig.SessionTimeout);
  finally
    CleanupConfigLeakLibrary;
  end;
end;

procedure TestFakeLibraryLogCallbackVisibleizesDefaultConfig;
var
  LLib: ISSLLibrary;
  LProbe: TLogProbe;
begin
  WriteLn('--- Test: fake backend log callback should visibleize default config');

  RegisterConfigLeakLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    Require(LLib <> nil, 'fake library instance should not be nil');

    LProbe := TLogProbe.Create;
    try
      LLib.SetLogCallback(@LProbe.HandleLog);
      Require(Assigned(LLib.GetDefaultConfig.LogCallback),
        'fake backend SetLogCallback should visibleize in GetDefaultConfig');

      LLib.SetLogCallback(nil);
      Require(not Assigned(LLib.GetDefaultConfig.LogCallback),
        'fake backend clearing SetLogCallback should clear GetDefaultConfig snapshot');
    finally
      LProbe.Free;
    end;
  finally
    CleanupConfigLeakLibrary;
  end;
end;

procedure TestFakeLibraryLogDispatchRespectsSnapshot;
var
  LLib: ISSLLibrary;
  LProbe: TLogProbe;
  LConfig: TSSLConfig;
begin
  WriteLn('--- Test: fake backend Log should dispatch callback with level gating');

  RegisterConfigLeakLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    Require(LLib <> nil, 'fake library instance should not be nil');

    InitCustomConfig(LConfig, sslFreePascal);
    LConfig.LogLevel := sslLogInfo;
    LLib.SetDefaultConfig(LConfig);

    LProbe := TLogProbe.Create;
    try
      LLib.SetLogCallback(@LProbe.HandleLog);

      LLib.Log(sslLogInfo, 'visible');
      RequireEquals('info log dispatch count', 1, LProbe.Count);
      Require(LProbe.LastLevel = sslLogInfo, 'info log should preserve level');
      RequireEquals('info log message', 'visible', LProbe.LastMessage);

      LLib.Log(sslLogDebug, 'hidden');
      RequireEquals('debug log filtered count', 1, LProbe.Count);

      LLib.SetLogCallback(nil);
      LLib.Log(sslLogError, 'cleared');
      RequireEquals('cleared callback suppresses dispatch', 1, LProbe.Count);
    finally
      LProbe.Free;
    end;
  finally
    CleanupConfigLeakLibrary;
  end;
end;

procedure TestFakeLibraryCreateContextDoesNotMutateDefaultSnapshot;
var
  LLib: ISSLLibrary;
  LBefore: TSSLConfig;
  LAfter: TSSLConfig;
  LServerContext: ISSLContext;
begin
  WriteLn('--- Test: fake backend CreateContext should not mutate default snapshot');

  RegisterConfigLeakLibrary;
  try
    LLib := TSSLFactory.GetLibrary(sslFreePascal);
    Require(LLib <> nil, 'fake library instance should not be nil');

    LBefore := LLib.GetDefaultConfig;
    LServerContext := LLib.CreateContext(sslCtxServer);
    Require(LServerContext <> nil, 'fake backend CreateContext(server) returned nil');
    Require(LServerContext.GetContextType = sslCtxServer, 'fake backend server context type should be server');
    RequireEquals('fake backend server context session timeout', LBefore.SessionTimeout, LServerContext.GetSessionTimeout);
    RequireEquals('fake backend server context session cache size', LBefore.SessionCacheSize, LServerContext.GetSessionCacheSize);

    LAfter := LLib.GetDefaultConfig;
    Require(LAfter.ContextType = LBefore.ContextType,
      'fake backend CreateContext(server) should not mutate default ContextType snapshot');
    Require(LAfter.LibraryType = LBefore.LibraryType,
      'fake backend CreateContext(server) should not mutate default LibraryType snapshot');
    RequireEquals('fake backend default session timeout after server create',
      LBefore.SessionTimeout, LAfter.SessionTimeout);
    RequireEquals('fake backend default ALPN after server create',
      LBefore.ALPNProtocols, LAfter.ALPNProtocols);
  finally
    CleanupConfigLeakLibrary;
  end;
end;

procedure TestConcurrentFirstAccessInitializesOnlyOnce;
var
  LStartEvent: TEvent;
  LThreads: array[0..INIT_RACE_THREADS - 1] of TGetLibraryThread;
  I: Integer;
begin
  WriteLn('--- Test: concurrent first access initializes only once');

  GSlowInitCalls := 0;
  RegisterSlowInitLibrary;
  LStartEvent := TEvent.Create(nil, True, False, '');
  try
    for I := 0 to High(LThreads) do
    begin
      LThreads[I] := TGetLibraryThread.Create(LStartEvent, sslMbedTLS);
      LThreads[I].Start;
    end;

    LStartEvent.SetEvent;

    for I := 0 to High(LThreads) do
    begin
      LThreads[I].WaitFor;
      Require(LThreads[I].Success, Format('GetLibrary thread %d failed: %s', [I, LThreads[I].Error]));
    end;

    RequireEquals('slow init call count', 1, GSlowInitCalls);
  finally
    for I := 0 to High(LThreads) do
      LThreads[I].Free;
    LStartEvent.Free;
    CleanupSlowInitLibrary;
  end;
end;

begin
  WriteLn('fafafa.ssl - factory shared config and init race contract');
  try
    TestPerRequestConfigDoesNotMutateLibraryDefaults;
    TestFakeLibrarySetDefaultConfigValidatesAndNormalizes;
    TestFakeLibraryLogCallbackVisibleizesDefaultConfig;
    TestFakeLibraryLogDispatchRespectsSnapshot;
    TestFakeLibraryCreateContextDoesNotMutateDefaultSnapshot;
    TestConcurrentFirstAccessInitializesOnlyOnce;
    WriteLn('✅ factory shared config and init race contract passed');
  except
    on E: Exception do
    begin
      WriteLn('❌ ', E.Message);
      Halt(1);
    end;
  end;
end.
