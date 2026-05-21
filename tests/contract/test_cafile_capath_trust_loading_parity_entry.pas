program test_cafile_capath_trust_loading_parity_entry;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory;

type
  IInspectableContext = interface
    ['{1E513724-FA95-4CD5-A2FA-5C9272CD1187}']
    function GetBackendType: TSSLLibraryType;
    function GetLoadCAFileCount: Integer;
    function GetLoadCAPathCount: Integer;
    function GetLastCAFile: string;
    function GetLastCAPath: string;
  end;

  TMockContext = class(TInterfacedObject, ISSLContext, IInspectableContext)
  private
    FBackendType: TSSLLibraryType;
    FContextType: TSSLContextType;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FOptions: TSSLOptions;
    FSessionCacheMode: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FProtocolVersions: TSSLProtocolVersions;
    FPreferredVersion: TSSLProtocolVersion;
    FCipherList: string;
    FCipherSuites: string;
    FServerName: string;
    FALPNProtocols: string;
    FCertVerifyFlags: TSSLCertVerifyFlags;
    FLoadCAFileCount: Integer;
    FLoadCAPathCount: Integer;
    FLastCAFile: string;
    FLastCAPath: string;
  public
    constructor Create(ABackendType: TSSLLibraryType; AContextType: TSSLContextType);

    function GetBackendType: TSSLLibraryType;
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(AVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    procedure SetPreferredVersion(AVersion: TSSLProtocolVersion);
    function GetPreferredVersion: TSSLProtocolVersion;

    procedure LoadCertificate(const AFileName: string); overload;
    procedure LoadCertificate(AStream: TStream); overload;
    procedure LoadCertificate(ACert: ISSLCertificate); overload;

    procedure LoadPrivateKey(const AFileName: string; const APassword: string = ''); overload;
    procedure LoadPrivateKey(AStream: TStream; const APassword: string = ''); overload;

    procedure LoadCertificatePEM(const APEM: string);
    procedure LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');

    procedure LoadCAFile(const AFileName: string);
    procedure LoadCAPath(const APath: string);
    procedure SetCertificateStore(AStore: ISSLCertificateStore);

    procedure SetVerifyMode(AMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(ADepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(ACallback: TSSLVerifyCallback);

    procedure SetCipherList(const ACipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const ACipherSuites: string);
    function GetCipherSuites: string;

    procedure SetSessionCacheMode(AEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(ATimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(ASize: Integer);
    function GetSessionCacheSize: Integer;

    procedure SetOptions(const AOptions: TSSLOptions);
    function GetOptions: TSSLOptions;

    procedure SetServerName(const AServerName: string);
    function GetServerName: string;

    procedure SetALPNProtocols(const AProtocols: string);
    function GetALPNProtocols: string;

    procedure SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
    function GetCertVerifyFlags: TSSLCertVerifyFlags;

    procedure SetPasswordCallback(ACallback: TSSLPasswordCallback);
    procedure SetInfoCallback(ACallback: TSSLInfoCallback);

    procedure AddCertificatePin(const AHash: TBytes; APinType: Integer;
      const ADescription: string; AIsBackup: Boolean = False);
    procedure AddCertificatePinBase64(const ABase64Hash: string; APinType: Integer;
      const ADescription: string; AIsBackup: Boolean = False);
    procedure SetCertificatePinningEnabled(AEnabled: Boolean);
    function GetCertificatePinningEnabled: Boolean;
    procedure ClearCertificatePins;

    function CreateConnection(ASocket: THandle): ISSLConnection; overload;
    function CreateConnection(AStream: TStream): ISSLConnection; overload;
    function IsValid: Boolean;

    function GetLoadCAFileCount: Integer;
    function GetLoadCAPathCount: Integer;
    function GetLastCAFile: string;
    function GetLastCAPath: string;
  end;

  TMockLibraryBase = class(TInterfacedObject, ISSLLibrary)
  private
    FDefaultConfig: TSSLConfig;
  public
    constructor Create;

    function Initialize: Boolean; virtual;
    procedure Finalize;
    function IsInitialized: Boolean; virtual;

    function GetLibraryType: TSSLLibraryType; virtual;
    function GetVersionString: string; virtual;
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;

    function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const aCipherName: string): Boolean;
    function IsFeatureSupported(aFeature: TSSLFeature): Boolean;
    function GetCapabilities: TSSLBackendCapabilities; virtual;

    procedure SetDefaultConfig(const aConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;

    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;

    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;

    procedure SetLogCallback(aCallback: TSSLLogCallback);
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string);

    function CreateContext(aType: TSSLContextType): ISSLContext; virtual;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;

  TMockOpenSSLLibrary = class(TMockLibraryBase)
  public
    function GetLibraryType: TSSLLibraryType; override;
    function GetVersionString: string; override;
  end;

  TMockMbedTLSLibrary = class(TMockLibraryBase)
  public
    function GetLibraryType: TSSLLibraryType; override;
    function GetVersionString: string; override;
  end;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure Check(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GTestsPassed);
    WriteLn('[PASS] ', AMessage);
  end
  else
  begin
    Inc(GTestsFailed);
    WriteLn('[FAIL] ', AMessage);
  end;
end;

procedure CheckEqualsInt(const AMessage: string; AExpected, AActual: Integer);
begin
  Check(AExpected = AActual,
    AMessage + ' (expected=' + IntToStr(AExpected) + ', actual=' + IntToStr(AActual) + ')');
end;

procedure CheckAtLeastInt(const AMessage: string; AExpectedMin, AActual: Integer);
begin
  Check(AActual >= AExpectedMin,
    AMessage + ' (expected>=' + IntToStr(AExpectedMin) + ', actual=' + IntToStr(AActual) + ')');
end;

procedure CheckEqualsBackend(const AMessage: string; AExpected, AActual: TSSLLibraryType);
begin
  Check(AExpected = AActual,
    AMessage + ' (expected=' + SSL_LIBRARY_NAMES[AExpected] + ', actual=' + SSL_LIBRARY_NAMES[AActual] + ')');
end;

procedure CheckEqualsString(const AMessage, AExpected, AActual: string);
begin
  Check(AExpected = AActual,
    AMessage + ' (expected=' + AExpected + ', actual=' + AActual + ')');
end;

procedure CheckEqualsVerifyMode(const AMessage: string;
  const AExpected, AActual: TSSLVerifyModes);
begin
  Check(AExpected = AActual, AMessage);
end;

constructor TMockContext.Create(ABackendType: TSSLLibraryType;
  AContextType: TSSLContextType);
begin
  inherited Create;
  FBackendType := ABackendType;
  FContextType := AContextType;
  FVerifyMode := [];
  FVerifyDepth := 0;
  FOptions := [];
  FSessionCacheMode := False;
  FSessionTimeout := 0;
  FSessionCacheSize := 0;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FPreferredVersion := sslProtocolUnknown;
  FCipherList := '';
  FCipherSuites := '';
  FServerName := '';
  FALPNProtocols := '';
  FCertVerifyFlags := [];
  FLoadCAFileCount := 0;
  FLoadCAPathCount := 0;
  FLastCAFile := '';
  FLastCAPath := '';
end;

function TMockContext.GetBackendType: TSSLLibraryType;
begin
  Result := FBackendType;
end;

function TMockContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TMockContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;
end;

function TMockContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TMockContext.SetPreferredVersion(AVersion: TSSLProtocolVersion);
begin
  FPreferredVersion := AVersion;
end;

function TMockContext.GetPreferredVersion: TSSLProtocolVersion;
begin
  Result := FPreferredVersion;
end;

procedure TMockContext.LoadCertificate(const AFileName: string);
begin
  if AFileName = '' then;
end;

procedure TMockContext.LoadCertificate(AStream: TStream);
begin
  if AStream <> nil then;
end;

procedure TMockContext.LoadCertificate(ACert: ISSLCertificate);
begin
  if ACert <> nil then;
end;

procedure TMockContext.LoadPrivateKey(const AFileName: string; const APassword: string);
begin
  if (AFileName = '') and (APassword = '') then;
end;

procedure TMockContext.LoadPrivateKey(AStream: TStream; const APassword: string);
begin
  if (AStream <> nil) and (APassword = '') then;
end;

procedure TMockContext.LoadCertificatePEM(const APEM: string);
begin
  if APEM = '' then;
end;

procedure TMockContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string);
begin
  if (APEM = '') and (APassword = '') then;
end;

procedure TMockContext.LoadCAFile(const AFileName: string);
begin
  Inc(FLoadCAFileCount);
  FLastCAFile := AFileName;
end;

procedure TMockContext.LoadCAPath(const APath: string);
begin
  Inc(FLoadCAPathCount);
  FLastCAPath := APath;
end;

procedure TMockContext.SetCertificateStore(AStore: ISSLCertificateStore);
begin
  if AStore <> nil then;
end;

procedure TMockContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
end;

function TMockContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TMockContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
end;

function TMockContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TMockContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
  if Assigned(ACallback) then;
end;

procedure TMockContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
end;

function TMockContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TMockContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TMockContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

procedure TMockContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheMode := AEnabled;
end;

function TMockContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheMode;
end;

procedure TMockContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
end;

function TMockContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TMockContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
end;

function TMockContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

procedure TMockContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TMockContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TMockContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TMockContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TMockContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TMockContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TMockContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TMockContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TMockContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
  if Assigned(ACallback) then;
end;

procedure TMockContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
  if Assigned(ACallback) then;
end;

procedure TMockContext.AddCertificatePin(const AHash: TBytes; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean);
begin
  if (Length(AHash) > 0) and (APinType >= 0) and (ADescription <> '') and AIsBackup then;
end;

procedure TMockContext.AddCertificatePinBase64(const ABase64Hash: string; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean);
begin
  if (ABase64Hash <> '') and (APinType >= 0) and (ADescription <> '') and AIsBackup then;
end;

procedure TMockContext.SetCertificatePinningEnabled(AEnabled: Boolean);
begin
  if AEnabled then;
end;

function TMockContext.GetCertificatePinningEnabled: Boolean;
begin
  Result := False;
end;

procedure TMockContext.ClearCertificatePins;
begin
end;

function TMockContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  if ASocket <> 0 then;
  Result := nil;
end;

function TMockContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  if AStream <> nil then;
  Result := nil;
end;

function TMockContext.IsValid: Boolean;
begin
  Result := True;
end;

function TMockContext.GetLoadCAFileCount: Integer;
begin
  Result := FLoadCAFileCount;
end;

function TMockContext.GetLoadCAPathCount: Integer;
begin
  Result := FLoadCAPathCount;
end;

function TMockContext.GetLastCAFile: string;
begin
  Result := FLastCAFile;
end;

function TMockContext.GetLastCAPath: string;
begin
  Result := FLastCAPath;
end;

constructor TMockLibraryBase.Create;
begin
  inherited Create;
  FillChar(FDefaultConfig, SizeOf(FDefaultConfig), 0);
  FDefaultConfig.LibraryType := GetLibraryType;
  FDefaultConfig.ContextType := sslCtxClient;
  FDefaultConfig.ProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FDefaultConfig.PreferredVersion := sslProtocolTLS13;
  FDefaultConfig.VerifyMode := [sslVerifyPeer];
  FDefaultConfig.VerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FDefaultConfig.CipherList := SSL_DEFAULT_CIPHER_LIST;
  FDefaultConfig.CipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
  FDefaultConfig.Options := [ssoEnableSessionCache];
  FDefaultConfig.SessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FDefaultConfig.SessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
end;

function TMockLibraryBase.Initialize: Boolean;
begin
  Result := True;
end;

procedure TMockLibraryBase.Finalize;
begin
end;

function TMockLibraryBase.IsInitialized: Boolean;
begin
  Result := True;
end;

function TMockLibraryBase.GetLibraryType: TSSLLibraryType;
begin
  Result := sslAutoDetect;
end;

function TMockLibraryBase.GetVersionString: string;
begin
  Result := 'MockLibrary';
end;

function TMockLibraryBase.GetVersionNumber: Cardinal;
begin
  Result := 1;
end;

function TMockLibraryBase.GetCompileFlags: string;
begin
  Result := '';
end;

function TMockLibraryBase.IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := aProtocol in [sslProtocolTLS12, sslProtocolTLS13];
end;

function TMockLibraryBase.IsCipherSupported(const aCipherName: string): Boolean;
begin
  Result := aCipherName <> '';
end;

function TMockLibraryBase.IsFeatureSupported(aFeature: TSSLFeature): Boolean;
begin
  Result := aFeature in [sslFeatSNI, sslFeatALPN, sslFeatSessionCache];
end;

function TMockLibraryBase.GetCapabilities: TSSLBackendCapabilities;
begin
  Result := Default(TSSLBackendCapabilities);
  Result.BackendType := GetLibraryType;
  Result.BackendVersion := 'mock-cafile-capath';
  Result.MinTLSVersion := sslProtocolTLS12;
  Result.MaxTLSVersion := sslProtocolTLS13;
  Result.SupportsTLS13 := True;
  Result.SNISupport := sslSupportStable;
  Result.ALPNSupport := sslSupportStable;
  Result.SessionCacheSupport := sslSupportStable;
  NormalizeLegacyCapabilityBooleans(Result);
end;

procedure TMockLibraryBase.SetDefaultConfig(const aConfig: TSSLConfig);
var
  LConfig: TSSLConfig;
begin
  LConfig := aConfig;
  TSSLFactory.NormalizeConfig(LConfig);
  LConfig.LibraryType := GetLibraryType;
  FDefaultConfig := LConfig;
end;

function TMockLibraryBase.GetDefaultConfig: TSSLConfig;
begin
  Result := FDefaultConfig;
  Result.LibraryType := GetLibraryType;
end;

function TMockLibraryBase.GetLastError: Integer;
begin
  Result := 0;
end;

function TMockLibraryBase.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TMockLibraryBase.ClearError;
begin
end;

function TMockLibraryBase.GetStatistics: TSSLStatistics;
begin
  FillChar(Result, SizeOf(Result), 0);
end;

procedure TMockLibraryBase.ResetStatistics;
begin
end;

procedure TMockLibraryBase.SetLogCallback(aCallback: TSSLLogCallback);
begin
  if Assigned(aCallback) then;
end;

procedure TMockLibraryBase.Log(aLevel: TSSLLogLevel; const aMessage: string);
begin
  if (aLevel = sslLogNone) and (aMessage = '') then;
end;

function TMockLibraryBase.CreateContext(aType: TSSLContextType): ISSLContext;
var
  LConfig: TSSLConfig;
  LVerifyMode: TSSLVerifyModes;
begin
  LConfig := FDefaultConfig;
  LConfig.ContextType := aType;
  LVerifyMode := LConfig.VerifyMode;
  if (aType = sslCtxServer) and
    (LVerifyMode = [sslVerifyPeer]) and
    (Trim(LConfig.CAFile) = '') and
    (Trim(LConfig.CAPath) = '') and
    (not LConfig.UseSystemRoots) then
    LVerifyMode := [];

  Result := TMockContext.Create(GetLibraryType, aType);
  Result.SetVerifyMode(LVerifyMode);

  if LConfig.CAFile <> '' then
    Result.LoadCAFile(LConfig.CAFile);

  if LConfig.CAPath <> '' then
    Result.LoadCAPath(LConfig.CAPath);
end;

function TMockLibraryBase.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMockLibraryBase.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil;
end;

function TMockOpenSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslOpenSSL;
end;

function TMockOpenSSLLibrary.GetVersionString: string;
begin
  Result := 'MockOpenSSL';
end;

function TMockMbedTLSLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslMbedTLS;
end;

function TMockMbedTLSLibrary.GetVersionString: string;
begin
  Result := 'MockMbedTLS';
end;

procedure ConfigureMockBackends;
begin
  TSSLFactory.ReleaseLibrary(sslOpenSSL);
  TSSLFactory.ReleaseLibrary(sslMbedTLS);
  TSSLFactory.UnregisterLibrary(sslOpenSSL);
  TSSLFactory.UnregisterLibrary(sslMbedTLS);
  TSSLFactory.RegisterLibrary(sslOpenSSL, TMockOpenSSLLibrary,
    'Mock OpenSSL backend for CAFile/CAPath trust-loading parity tests', 100000);
  TSSLFactory.RegisterLibrary(sslMbedTLS, TMockMbedTLSLibrary,
    'Mock MbedTLS backend for CAFile/CAPath trust-loading parity tests', 90000);
  TSSLFactory.SetDefaultLibrary(sslOpenSSL);
end;

function InspectContext(const AContext: ISSLContext): IInspectableContext;
begin
  Check(Supports(AContext, IInspectableContext, Result),
    'Context supports inspection');
end;

procedure Test_FactoryOneShotLoadsCAFileAndCAPath;
const
  CExpectedCAFile = 'tests/certificate/test_certs/ca_cert.pem';
  CExpectedCAPath = '/mock/ca/path';
var
  LConfig: TSSLConfig;
  LContext: ISSLContext;
  LInspectable: IInspectableContext;
begin
  WriteLn('=== Test 1: one-shot factory path loads CAFile and CAPath ===');
  ConfigureMockBackends;

  LConfig := CreateDefaultConfig(sslCtxClient);
  LConfig.LibraryType := sslMbedTLS;
  LConfig.ContextType := sslCtxClient;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.CAFile := CExpectedCAFile;
  LConfig.CAPath := CExpectedCAPath;

  LContext := TSSLFactory.CreateContext(LConfig);
  LInspectable := InspectContext(LContext);
  if LInspectable = nil then
    Exit;

  CheckEqualsBackend('one-shot factory keeps explicit backend',
    sslMbedTLS, LInspectable.GetBackendType);
  CheckEqualsInt('one-shot factory loads CAFile exactly once',
    1, LInspectable.GetLoadCAFileCount);
  CheckEqualsString('one-shot factory keeps CAFile value',
    CExpectedCAFile, LInspectable.GetLastCAFile);
  CheckEqualsInt('one-shot factory loads CAPath exactly once',
    1, LInspectable.GetLoadCAPathCount);
  CheckEqualsString('one-shot factory keeps CAPath value',
    CExpectedCAPath, LInspectable.GetLastCAPath);
end;

procedure Test_FactoryOneShotServerKeepsVerifyPeerWhenCAPathPresent;
const
  CExpectedCAPath = '/mock/ca/path';
var
  LConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 2: one-shot factory server keeps verify-peer when CAPath is configured ===');
  ConfigureMockBackends;

  LConfig := CreateDefaultConfig(sslCtxServer);
  LConfig.LibraryType := sslMbedTLS;
  LConfig.ContextType := sslCtxServer;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.CAPath := CExpectedCAPath;

  LContext := TSSLFactory.CreateContext(LConfig);
  CheckEqualsVerifyMode('one-shot factory server keeps VerifyMode with CAPath',
    [sslVerifyPeer], LContext.GetVerifyMode);
end;

procedure Test_FactoryRawDefaultConfigLoadsCAFileAndCAPath;
const
  CExpectedCAFile = 'tests/certificate/test_certs/ca_cert.pem';
  CExpectedCAPath = '/mock/raw/path';
var
  LLib: ISSLLibrary;
  LOriginalConfig: TSSLConfig;
  LConfig: TSSLConfig;
  LContext: ISSLContext;
  LInspectable: IInspectableContext;
begin
  WriteLn('=== Test 3: raw factory default-config path consumes CAFile and CAPath ===');
  ConfigureMockBackends;

  LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  LOriginalConfig := LLib.GetDefaultConfig;
  LConfig := LOriginalConfig;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.CAFile := CExpectedCAFile;
  LConfig.CAPath := CExpectedCAPath;
  LLib.SetDefaultConfig(LConfig);
  try
    LContext := TSSLFactory.CreateContext(sslCtxClient, sslOpenSSL);
    LInspectable := InspectContext(LContext);
    if LInspectable = nil then
      Exit;

    CheckEqualsBackend('raw factory keeps explicit backend',
      sslOpenSSL, LInspectable.GetBackendType);
    CheckAtLeastInt('raw factory consumes CAFile at least once',
      1, LInspectable.GetLoadCAFileCount);
    CheckEqualsString('raw factory keeps CAFile value',
      CExpectedCAFile, LInspectable.GetLastCAFile);
    CheckAtLeastInt('raw factory consumes CAPath at least once',
      1, LInspectable.GetLoadCAPathCount);
    CheckEqualsString('raw factory keeps CAPath value',
      CExpectedCAPath, LInspectable.GetLastCAPath);
  finally
    LLib.SetDefaultConfig(LOriginalConfig);
  end;
end;

procedure Test_FactoryRawServerKeepsVerifyPeerWhenCAFilePresent;
const
  CExpectedCAFile = 'tests/certificate/test_certs/ca_cert.pem';
var
  LLib: ISSLLibrary;
  LOriginalConfig: TSSLConfig;
  LConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 4: raw factory server keeps verify-peer when CAFile is configured ===');
  ConfigureMockBackends;

  LLib := TSSLFactory.GetLibraryInstance(sslOpenSSL);
  LOriginalConfig := LLib.GetDefaultConfig;
  LConfig := LOriginalConfig;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.CAFile := CExpectedCAFile;
  LLib.SetDefaultConfig(LConfig);
  try
    LContext := TSSLFactory.CreateContext(sslCtxServer, sslOpenSSL);
    CheckEqualsVerifyMode('raw factory server keeps VerifyMode with CAFile',
      [sslVerifyPeer], LContext.GetVerifyMode);
  finally
    LLib.SetDefaultConfig(LOriginalConfig);
  end;
end;

procedure Test_DirectLibraryDefaultConfigLoadsCAFileAndCAPath;
const
  CExpectedCAFile = 'tests/certificate/test_certs/ca_cert.pem';
  CExpectedCAPath = '/mock/direct/path';
var
  LLib: ISSLLibrary;
  LOriginalConfig: TSSLConfig;
  LConfig: TSSLConfig;
  LContext: ISSLContext;
  LInspectable: IInspectableContext;
begin
  WriteLn('=== Test 5: direct-library default-config path loads CAFile and CAPath ===');
  ConfigureMockBackends;

  LLib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  LOriginalConfig := LLib.GetDefaultConfig;
  LConfig := LOriginalConfig;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.CAFile := CExpectedCAFile;
  LConfig.CAPath := CExpectedCAPath;
  LLib.SetDefaultConfig(LConfig);
  try
    LContext := LLib.CreateContext(sslCtxClient);
    LInspectable := InspectContext(LContext);
    if LInspectable = nil then
      Exit;

    CheckEqualsBackend('direct-library keeps explicit backend',
      sslMbedTLS, LInspectable.GetBackendType);
    CheckEqualsInt('direct-library loads CAFile exactly once',
      1, LInspectable.GetLoadCAFileCount);
    CheckEqualsString('direct-library keeps CAFile value',
      CExpectedCAFile, LInspectable.GetLastCAFile);
    CheckEqualsInt('direct-library loads CAPath exactly once',
      1, LInspectable.GetLoadCAPathCount);
    CheckEqualsString('direct-library keeps CAPath value',
      CExpectedCAPath, LInspectable.GetLastCAPath);
  finally
    LLib.SetDefaultConfig(LOriginalConfig);
  end;
end;

procedure Test_DirectLibraryServerKeepsVerifyPeerWhenCAPathPresent;
const
  CExpectedCAPath = '/mock/direct/server/path';
var
  LLib: ISSLLibrary;
  LOriginalConfig: TSSLConfig;
  LConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 6: direct-library server keeps verify-peer when CAPath is configured ===');
  ConfigureMockBackends;

  LLib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  LOriginalConfig := LLib.GetDefaultConfig;
  LConfig := LOriginalConfig;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.CAPath := CExpectedCAPath;
  LLib.SetDefaultConfig(LConfig);
  try
    LContext := LLib.CreateContext(sslCtxServer);
    CheckEqualsVerifyMode('direct-library server keeps VerifyMode with CAPath',
      [sslVerifyPeer], LContext.GetVerifyMode);
  finally
    LLib.SetDefaultConfig(LOriginalConfig);
  end;
end;

begin
  Test_FactoryOneShotLoadsCAFileAndCAPath;
  Test_FactoryOneShotServerKeepsVerifyPeerWhenCAPathPresent;
  Test_FactoryRawDefaultConfigLoadsCAFileAndCAPath;
  Test_FactoryRawServerKeepsVerifyPeerWhenCAFilePresent;
  Test_DirectLibraryDefaultConfigLoadsCAFileAndCAPath;
  Test_DirectLibraryServerKeepsVerifyPeerWhenCAPathPresent;

  if GTestsFailed > 0 then
  begin
    WriteLn('FAILED: ', GTestsFailed, ' test(s) failed');
    Halt(1);
  end;

  WriteLn('PASSED: ', GTestsPassed, ' checks');
end.
