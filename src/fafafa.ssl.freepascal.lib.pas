{**
 * Unit: fafafa.ssl.freepascal.lib
 * Purpose: 纯 FreePascal 后端库管理实现
 *}

unit fafafa.ssl.freepascal.lib;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils,
  fafafa.ssl.base;

type
  TFreePascalSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FDefaultConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLastError: Integer;
    FLastErrorString: string;
    FLogCallback: TSSLLogCallback;
    FLogLevel: TSSLLogLevel;
    FCapabilitiesCache: TSSLBackendCapabilities;
    FCapabilitiesCached: Boolean;

    procedure InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
    procedure InvalidateCapabilitiesCache;
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

function CreateFreePascalSSLLibrary: ISSLLibrary;

procedure RegisterFreePascalBackend;
procedure UnregisterFreePascalBackend;

implementation

uses
  fafafa.ssl.exceptions,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.context;

constructor TFreePascalSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
  FLastError := 0;
  FLastErrorString := '';
  FLogCallback := nil;
  FLogLevel := sslLogError;

  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  FDefaultConfig.LibraryType := sslFreePascal;
  FDefaultConfig.ContextType := sslCtxClient;
  FDefaultConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FDefaultConfig.PreferredVersion := sslProtocolTLS13;
  FDefaultConfig.VerifyMode := [sslVerifyPeer];
  FDefaultConfig.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FDefaultConfig.CipherList := SSL_DEFAULT_CIPHER_LIST;
  FDefaultConfig.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
  FDefaultConfig.Options := [ssoEnableSessionCache, ssoEnableSessionTickets, ssoEnableSNI, ssoEnableALPN];
  FDefaultConfig.BufferSize := SSL_DEFAULT_BUFFER_SIZE;
  FDefaultConfig.HandshakeTimeout := SSL_DEFAULT_HANDSHAKE_TIMEOUT;
  FDefaultConfig.SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FDefaultConfig.SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FDefaultConfig.LogLevel := sslLogError;

  FillChar(FStatistics, SizeOf(FStatistics), 0);
  FillChar(FCapabilitiesCache, SizeOf(FCapabilitiesCache), 0);
  FCapabilitiesCached := False;
end;

procedure TFreePascalSSLLibrary.InternalLog(ALevel: TSSLLogLevel; const AMessage: string);
begin
  if Assigned(FLogCallback) and (ALevel <= FLogLevel) then
    FLogCallback(ALevel, '[FreePascal] ' + AMessage);
end;

procedure TFreePascalSSLLibrary.InvalidateCapabilitiesCache;
begin
  FCapabilitiesCached := False;
  FillChar(FCapabilitiesCache, SizeOf(FCapabilitiesCache), 0);
end;

function TFreePascalSSLLibrary.Initialize: Boolean;
begin
  FInitialized := True;
  Result := True;
end;

procedure TFreePascalSSLLibrary.Finalize;
begin
  FInitialized := False;
end;

function TFreePascalSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TFreePascalSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslFreePascal;
end;

function TFreePascalSSLLibrary.GetVersionString: string;
begin
  Result := 'FreePascal Native Backend (skeleton)';
end;

function TFreePascalSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := 10000;
end;

function TFreePascalSSLLibrary.GetCompileFlags: string;
begin
  Result := 'PurePascal;NoExternalTLSLibrary';
end;

function TFreePascalSSLLibrary.IsProtocolSupported(AProtocol: TSSLProtocolVersion): Boolean;
begin
  case AProtocol of
    sslProtocolTLS12,
    sslProtocolTLS13:
      Result := True;
  else
    Result := False;
  end;
end;

function TFreePascalSSLLibrary.IsCipherSupported(const ACipherName: string): Boolean;
var
  LUpper: string;
begin
  LUpper := UpperCase(Trim(ACipherName));
  Result :=
    (LUpper = 'TLS_AES_128_GCM_SHA256') or
    (LUpper = 'TLS_AES_256_GCM_SHA384') or
    (LUpper = 'TLS_CHACHA20_POLY1305_SHA256');
end;

function TFreePascalSSLLibrary.IsFeatureSupported(AFeature: TSSLFeature): Boolean;
begin
  case AFeature of
    sslFeatSNI,
    sslFeatALPN,
    sslFeatSessionTickets,
    sslFeatSessionCache:
      Result := True;
  else
    Result := False;
  end;
end;

function TFreePascalSSLLibrary.GetCapabilities: TSSLBackendCapabilities;
begin
  if FCapabilitiesCached then
    Exit(FCapabilitiesCache);

  FillChar(Result, SizeOf(Result), 0);
  Result.SupportsTLS13 := True;
  Result.SupportsALPN := True;
  Result.SupportsSNI := True;
  Result.SupportsOCSPStapling := False;
  Result.SupportsCertificateTransparency := False;
  Result.SupportsSessionTickets := True;
  Result.SupportsECDHE := True;
  Result.SupportsChaChaPoly := True;
  Result.SupportsPEMPrivateKey := True;
  Result.MinTLSVersion := sslProtocolTLS12;
  Result.MaxTLSVersion := sslProtocolTLS13;

  Result.BackendType := sslFreePascal;
  Result.BackendImplType := sslImplNative;
  Result.BackendVersion := GetVersionString;
  Result.SupportsDTLS := False;

  Result.SNISupport := sslSupportExperimental;
  Result.ALPNSupport := sslSupportExperimental;
  Result.OCSPStaplingSupport := sslSupportNone;
  Result.CertTransparencySupport := sslSupportNone;
  Result.SessionTicketsSupport := sslSupportExperimental;
  Result.SessionCacheSupport := sslSupportExperimental;
  Result.ZeroRTTSupport := sslSupportNone;
  Result.EarlyDataSupport := sslSupportNone;
  Result.RenegotiationSupport := sslSupportNone;
  Result.PostHandshakeAuthSupport := sslSupportNone;

  Result.SupportedCiphers := [sslCipherAES128GCM, sslCipherAES256GCM, sslCipherCHACHA20_POLY1305];
  Result.SupportedHashes := [sslHashSHA256, sslHashSHA384, sslHashSHA512];
  Result.SupportedKeyExchanges := [sslKexECDHE_RSA, sslKexECDHE_ECDSA];

  Result.HasHardwareAcceleration := False;
  Result.HasSIMDOptimization := False;
  Result.HasAssemblyOptimization := False;

  Result.RequiresExternalLibrary := False;
  Result.SupportsSystemCertStore := False;
  Result.SupportsPKCS11 := False;
  Result.SupportsTPM := False;

  Result.HasConstantTimeOperations := True;
  Result.SupportsFIPSMode := False;
  Result.HasSecureMemoryWipe := True;

  Result.SupportsDERPrivateKey := True;
  Result.SupportsPKCS8PrivateKey := True;
  Result.SupportsPKCS12 := False;
  Result.SupportsPasswordProtectedKeys := True;

  Result.SupportsCustomCipherSuites := True;
  Result.SupportsCallbacks := True;

  Result.CompatibilityLevel := 35;
  Result.KnownIssues := 'TLS 1.3 client path works (CHACHA20); server path now includes EncryptedExtensions+Certificate+CertificateVerify+Finished flight framing with pure-Pas RSA signer. ECDSA CertificateVerify signer remains pending.';

  FCapabilitiesCache := Result;
  FCapabilitiesCached := True;
end;

procedure TFreePascalSSLLibrary.SetDefaultConfig(const AConfig: TSSLConfig);
begin
  FDefaultConfig := AConfig;
  FLogLevel := AConfig.LogLevel;
  FLogCallback := AConfig.LogCallback;
  InvalidateCapabilitiesCache;
end;

function TFreePascalSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
end;

function TFreePascalSSLLibrary.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TFreePascalSSLLibrary.GetLastErrorString: string;
begin
  Result := FLastErrorString;
end;

procedure TFreePascalSSLLibrary.ClearError;
begin
  FLastError := 0;
  FLastErrorString := '';
end;

function TFreePascalSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TFreePascalSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

procedure TFreePascalSSLLibrary.SetLogCallback(ACallback: TSSLLogCallback);
begin
  FLogCallback := ACallback;
end;

procedure TFreePascalSSLLibrary.Log(ALevel: TSSLLogLevel; const AMessage: string);
begin
  InternalLog(ALevel, AMessage);
end;

function TFreePascalSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
    raise ESSLInitializationException.CreateWithContext(
      'Cannot create context: FreePascal library not initialized',
      sslErrNotInitialized,
      'TFreePascalSSLLibrary.CreateContext',
      0,
      sslFreePascal
    );

  Result := TFreePascalContext.Create(Self, AType);
end;

function TFreePascalSSLLibrary.CreateCertificate: ISSLCertificate;
begin
  raise ESSLConfigurationException.CreateWithContext(
    'CreateCertificate is not implemented in FreePascal backend yet',
    sslErrUnsupported,
    'TFreePascalSSLLibrary.CreateCertificate',
    0,
    sslFreePascal
  );
end;

function TFreePascalSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  raise ESSLConfigurationException.CreateWithContext(
    'CreateCertificateStore is not implemented in FreePascal backend yet',
    sslErrUnsupported,
    'TFreePascalSSLLibrary.CreateCertificateStore',
    0,
    sslFreePascal
  );
end;

function CreateFreePascalSSLLibrary: ISSLLibrary;
begin
  Result := TFreePascalSSLLibrary.Create;
end;

procedure RegisterFreePascalBackend;
begin
  try
    TSSLFactory.RegisterLibrary(sslFreePascal, TFreePascalSSLLibrary,
      'FreePascal Native TLS Backend (in progress)', 50);
  except
  end;
end;

procedure UnregisterFreePascalBackend;
begin
  TSSLFactory.UnregisterLibrary(sslFreePascal);
end;

initialization
  RegisterFreePascalBackend;

finalization
  UnregisterFreePascalBackend;

end.
