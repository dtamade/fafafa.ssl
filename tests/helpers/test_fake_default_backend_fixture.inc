type
  TTestDefaultFakeLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FDefaultConfig: TSSLConfig;
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
    function CreateContext(AType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

constructor TTestDefaultFakeLibrary.Create;
begin
  inherited Create;
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  FDefaultConfig.LibraryType := sslFreePascal;
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

function TTestDefaultFakeLibrary.Initialize: Boolean;
begin
  Result := True;
end;

procedure TTestDefaultFakeLibrary.Finalize;
begin
end;

function TTestDefaultFakeLibrary.IsInitialized: Boolean;
begin
  Result := True;
end;

function TTestDefaultFakeLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslFreePascal;
end;

function TTestDefaultFakeLibrary.GetVersionString: string;
begin
  Result := 'test-default-fake';
end;

function TTestDefaultFakeLibrary.GetVersionNumber: Cardinal;
begin
  Result := 1;
end;

function TTestDefaultFakeLibrary.GetCompileFlags: string;
begin
  Result := '';
end;

function TTestDefaultFakeLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := AProtocol in [sslProtocolTLS12, sslProtocolTLS13];
end;

function TTestDefaultFakeLibrary.IsCipherSupported(const ACipherName: string): Boolean;
begin
  Result := True;
end;

function TTestDefaultFakeLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  Result := True;
end;

function TTestDefaultFakeLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  Result := Default(TSSLBackendCapabilities);
  Result.BackendType := sslFreePascal;
  Result.MinTLSVersion := sslProtocolTLS12;
  Result.MaxTLSVersion := sslProtocolTLS13;
end;

procedure TTestDefaultFakeLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
var
  LConfig: TSSLConfig;
begin
  LConfig := AConfig;
  TSSLFactory.NormalizeLibraryDefaultOwnerFields(LConfig, sslFreePascal);
  TSSLFactory.ValidateLibraryDefaultConfigFields(LConfig, 'TTestDefaultFakeLibrary.SetDefaultConfig');
  TSSLFactory.NormalizeConfig(LConfig);
  FDefaultConfig := LConfig;
end;

function TTestDefaultFakeLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

function TTestDefaultFakeLibrary.GetLastError: Integer;
begin
  Result := 0;
end;

function TTestDefaultFakeLibrary.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TTestDefaultFakeLibrary.ClearError;
begin
end;

function TTestDefaultFakeLibrary.GetStatistics: TSSLStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TTestDefaultFakeLibrary.ResetStatistics;
begin
end;

procedure TTestDefaultFakeLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FDefaultConfig.LogCallback := ACallback;
end;

procedure TTestDefaultFakeLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FDefaultConfig.LogCallback) and (ALevel <= FDefaultConfig.LogLevel) then
    FDefaultConfig.LogCallback(ALevel, AMessage);
end;

function TTestDefaultFakeLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
var
  LConfig: TSSLConfig;
begin
  Result := TFreePascalContext.Create(Self as ISSLLibrary, AType);
  if Result <> nil then
  begin
    LConfig := FDefaultConfig;
    LConfig.ContextType := AType;
    TSSLFactory.ApplyConfigToContext(Result, LConfig);
  end;
end;

function TTestDefaultFakeLibrary.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TTestDefaultFakeLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

function CreateTestDefaultFakeLibrary: ISSLLibrary;
begin
  Result := TTestDefaultFakeLibrary.Create;
end;

procedure RegisterTestDefaultFakeLibrary;
begin
  TSSLFactory.ReleaseLibrary(sslFreePascal);
  TSSLFactory.UnregisterLibrary(sslFreePascal);
  TSSLFactory.RegisterLibrary(
    sslFreePascal,
    TTestDefaultFakeLibrary,
    'test-default-fake',
    100,
    @CreateTestDefaultFakeLibrary
  );
  TSSLFactory.SetDefaultLibrary(sslFreePascal);
end;

procedure CleanupTestDefaultFakeLibrary;
begin
  TSSLFactory.ReleaseLibrary(sslFreePascal);
  TSSLFactory.SetDefaultLibrary(sslAutoDetect);
  TSSLFactory.UnregisterLibrary(sslFreePascal);
end;
