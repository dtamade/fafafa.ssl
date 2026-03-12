type
  TBackendCounters = record
    ContextCreates: Integer;
    StoreCreates: Integer;
  end;

  TFakeCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
  public
    function AddCertificate(ACert: ISSLCertificate): Boolean;
    function RemoveCertificate(ACert: ISSLCertificate): Boolean;
    function Contains(ACert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(AIndex: Integer): ISSLCertificate;
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromPath(const APath: string): Boolean;
    function LoadSystemStore: Boolean;
    function FindBySubject(const ASubject: string): ISSLCertificate;
    function FindByIssuer(const AIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const AFingerprint: string): ISSLCertificate;
    function VerifyCertificate(ACert: ISSLCertificate): Boolean;
    function BuildCertificateChain(ACert: ISSLCertificate): TSSLCertificateArray;
  end;

var
  GCounters: array[TSSLLibraryType] of TBackendCounters;

procedure ResetCounters;
var
  LBackend: TSSLLibraryType;
begin
  for LBackend := Low(TSSLLibraryType) to High(TSSLLibraryType) do
  begin
    GCounters[LBackend].ContextCreates := 0;
    GCounters[LBackend].StoreCreates := 0;
  end;
end;

function TFakeCertificateStore.AddCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := True;
end;

function TFakeCertificateStore.RemoveCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := True;
end;

function TFakeCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
end;

procedure TFakeCertificateStore.Clear;
begin
end;

function TFakeCertificateStore.GetCount: Integer;
begin
  Result := 0;
end;

function TFakeCertificateStore.GetCertificate(AIndex: Integer): ISSLCertificate;
begin
  Result := nil;
end;

function TFakeCertificateStore.LoadFromFile(const AFileName: string): Boolean;
begin
  Result := True;
end;

function TFakeCertificateStore.LoadFromPath(const APath: string): Boolean;
begin
  Result := True;
end;

function TFakeCertificateStore.LoadSystemStore: Boolean;
begin
  Result := True;
end;

function TFakeCertificateStore.FindBySubject(const ASubject: string): ISSLCertificate;
begin
  Result := nil;
end;

function TFakeCertificateStore.FindByIssuer(const AIssuer: string): ISSLCertificate;
begin
  Result := nil;
end;

function TFakeCertificateStore.FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
begin
  Result := nil;
end;

function TFakeCertificateStore.FindByFingerprint(const AFingerprint: string): ISSLCertificate;
begin
  Result := nil;
end;

function TFakeCertificateStore.VerifyCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := True;
end;

function TFakeCertificateStore.BuildCertificateChain(
  ACert: ISSLCertificate): TSSLCertificateArray;
begin
  Result := nil;
end;

type
  TBaseFakeLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FDefaultConfig: TSSLConfig;
  protected
    function BackendType: TSSLLibraryType; virtual; abstract;
  public
    constructor Create;
    function Initialize: Boolean;
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
    procedure SetDefaultConfig(const AConfig: TSSLConfig);
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

  TExplicitFakeLibrary = class(TBaseFakeLibrary)
  protected
    function BackendType: TSSLLibraryType; override;
  end;

  TDefaultFakeLibrary = class(TBaseFakeLibrary)
  protected
    function BackendType: TSSLLibraryType; override;
  end;

  TDriftingDefaultFakeLibrary = class(TBaseFakeLibrary)
  protected
    function BackendType: TSSLLibraryType; override;
  public
    function CreateContext(AType: TSSLContextType): ISSLContext; override;
  end;

constructor TBaseFakeLibrary.Create;
begin
  inherited Create;
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  FDefaultConfig.LibraryType := BackendType;
  FDefaultConfig.ContextType := sslCtxClient;
  FDefaultConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FDefaultConfig.VerifyMode := [sslVerifyPeer];
  FDefaultConfig.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FDefaultConfig.BufferSize := SSL_DEFAULT_BUFFER_SIZE;
  FDefaultConfig.HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  FDefaultConfig.SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FDefaultConfig.SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FDefaultConfig.CipherList := SSL_DEFAULT_CIPHER_LIST;
  FDefaultConfig.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
  FDefaultConfig.EnableSessionTickets := True;
  FDefaultConfig.LogLevel := sslLogError;
  TSSLFactory.NormalizeConfig(FDefaultConfig);
end;

function TBaseFakeLibrary.Initialize: Boolean;
begin
  Result := True;
end;

procedure TBaseFakeLibrary.Finalize;
begin
end;

function TBaseFakeLibrary.IsInitialized: Boolean;
begin
  Result := True;
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
  Inc(GCounters[BackendType].ContextCreates);
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
  Inc(GCounters[BackendType].StoreCreates);
  Result := TFakeCertificateStore.Create;
end;

function TExplicitFakeLibrary.BackendType: TSSLLibraryType;
begin
  Result := sslFreePascal;
end;

function TDefaultFakeLibrary.BackendType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TDriftingDefaultFakeLibrary.BackendType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TDriftingDefaultFakeLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  Result := inherited CreateContext(AType);
  TSSLFactory.SetDefaultLibrary(sslFreePascal);
end;

function CreateExplicitFakeLibrary: ISSLLibrary;
begin
  Result := TExplicitFakeLibrary.Create;
end;

function CreateDefaultFakeLibrary: ISSLLibrary;
begin
  Result := TDefaultFakeLibrary.Create;
end;

function CreateDriftingDefaultFakeLibrary: ISSLLibrary;
begin
  Result := TDriftingDefaultFakeLibrary.Create;
end;

procedure RegisterFakeLibraries;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslFreePascal);
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
  TSSLFactory.RegisterLibrary(sslFreePascal, TExplicitFakeLibrary, 'fake-explicit', 1, @CreateExplicitFakeLibrary);
  TSSLFactory.RegisterLibrary(sslMbedTLS, TDefaultFakeLibrary, 'fake-default', 10, @CreateDefaultFakeLibrary);
  TSSLFactory.SetDefaultLibrary(sslMbedTLS);
end;

procedure CleanupFakeLibraries;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslFreePascal);
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
end;

procedure RegisterDriftingFakeLibraries;
begin
  TSSLFactory.ReleaseAllLibraries;
  TSSLFactory.UnregisterLibrary(sslFreePascal);
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
  TSSLFactory.RegisterLibrary(sslFreePascal, TExplicitFakeLibrary, 'fake-explicit', 1, @CreateExplicitFakeLibrary);
  TSSLFactory.RegisterLibrary(sslMbedTLS, TDriftingDefaultFakeLibrary, 'fake-default-drifting', 10, @CreateDriftingDefaultFakeLibrary);
  TSSLFactory.SetDefaultLibrary(sslMbedTLS);
end;
