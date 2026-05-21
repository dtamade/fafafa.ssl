program test_system_roots_public_surface_entry;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory;

type
  IInspectableCertificateStore = interface
    ['{A294FD3E-795A-4C95-9A68-9C1A580C88F9}']
    function GetBackendType: TSSLLibraryType;
    function GetLoadSystemStoreCount: Integer;
  end;

  IInspectableContext = interface
    ['{3BD19789-E71F-4ECA-A750-40B8B4D33387}']
    function GetBackendType: TSSLLibraryType;
    function GetSetCertificateStoreCount: Integer;
    function GetLastStoreBackendType: TSSLLibraryType;
    function GetLastStoreLoadSystemStoreCount: Integer;
  end;

  TMockCertificateStore = class(TInterfacedObject, ISSLCertificateStore,
    IInspectableCertificateStore)
  private
    FBackendType: TSSLLibraryType;
    FLoadSystemStoreCount: Integer;
  public
    constructor Create(ABackendType: TSSLLibraryType);

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

    function GetBackendType: TSSLLibraryType;
    function GetLoadSystemStoreCount: Integer;
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
    FSetCertificateStoreCount: Integer;
    FLastStoreBackendType: TSSLLibraryType;
    FLastStoreLoadSystemStoreCount: Integer;
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

    function GetSetCertificateStoreCount: Integer;
    function GetLastStoreBackendType: TSSLLibraryType;
    function GetLastStoreLoadSystemStoreCount: Integer;
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

procedure CheckEqualsBackend(const AMessage: string; AExpected, AActual: TSSLLibraryType);
begin
  Check(AExpected = AActual,
    AMessage + ' (expected=' + SSL_LIBRARY_NAMES[AExpected] + ', actual=' + SSL_LIBRARY_NAMES[AActual] + ')');
end;

procedure CheckEqualsVerifyMode(const AMessage: string;
  const AExpected, AActual: TSSLVerifyModes);
begin
  Check(AExpected = AActual, AMessage);
end;

constructor TMockCertificateStore.Create(ABackendType: TSSLLibraryType);
begin
  inherited Create;
  FBackendType := ABackendType;
  FLoadSystemStoreCount := 0;
end;

function TMockCertificateStore.AddCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := True;
end;

function TMockCertificateStore.RemoveCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := True;
end;

function TMockCertificateStore.Contains(ACert: ISSLCertificate): Boolean;
begin
  Result := False;
end;

procedure TMockCertificateStore.Clear;
begin
end;

function TMockCertificateStore.GetCount: Integer;
begin
  Result := 0;
end;

function TMockCertificateStore.GetCertificate(AIndex: Integer): ISSLCertificate;
begin
  Result := nil;
end;

function TMockCertificateStore.LoadFromFile(const AFileName: string): Boolean;
begin
  if AFileName = '' then;
  Result := True;
end;

function TMockCertificateStore.LoadFromPath(const APath: string): Boolean;
begin
  if APath = '' then;
  Result := True;
end;

function TMockCertificateStore.LoadSystemStore: Boolean;
begin
  Inc(FLoadSystemStoreCount);
  Result := True;
end;

function TMockCertificateStore.FindBySubject(const ASubject: string): ISSLCertificate;
begin
  Result := nil;
end;

function TMockCertificateStore.FindByIssuer(const AIssuer: string): ISSLCertificate;
begin
  Result := nil;
end;

function TMockCertificateStore.FindBySerialNumber(const ASerialNumber: string): ISSLCertificate;
begin
  Result := nil;
end;

function TMockCertificateStore.FindByFingerprint(const AFingerprint: string): ISSLCertificate;
begin
  Result := nil;
end;

function TMockCertificateStore.VerifyCertificate(ACert: ISSLCertificate): Boolean;
begin
  Result := True;
end;

function TMockCertificateStore.BuildCertificateChain(
  ACert: ISSLCertificate): TSSLCertificateArray;
begin
  Result := nil;
end;

function TMockCertificateStore.GetBackendType: TSSLLibraryType;
begin
  Result := FBackendType;
end;

function TMockCertificateStore.GetLoadSystemStoreCount: Integer;
begin
  Result := FLoadSystemStoreCount;
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
  FSetCertificateStoreCount := 0;
  FLastStoreBackendType := sslAutoDetect;
  FLastStoreLoadSystemStoreCount := 0;
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
  if AFileName = '' then;
end;

procedure TMockContext.LoadCAPath(const APath: string);
begin
  if APath = '' then;
end;

procedure TMockContext.SetCertificateStore(AStore: ISSLCertificateStore);
var
  LInspectable: IInspectableCertificateStore;
begin
  Inc(FSetCertificateStoreCount);
  if Supports(AStore, IInspectableCertificateStore, LInspectable) then
  begin
    FLastStoreBackendType := LInspectable.GetBackendType;
    FLastStoreLoadSystemStoreCount := LInspectable.GetLoadSystemStoreCount;
  end
  else
  begin
    FLastStoreBackendType := sslAutoDetect;
    FLastStoreLoadSystemStoreCount := 0;
  end;
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

function TMockContext.GetSetCertificateStoreCount: Integer;
begin
  Result := FSetCertificateStoreCount;
end;

function TMockContext.GetLastStoreBackendType: TSSLLibraryType;
begin
  Result := FLastStoreBackendType;
end;

function TMockContext.GetLastStoreLoadSystemStoreCount: Integer;
begin
  Result := FLastStoreLoadSystemStoreCount;
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
  Result.BackendVersion := 'mock-system-roots';
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
begin
  Result := TMockContext.Create(GetLibraryType, aType);
end;

function TMockLibraryBase.CreateCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TMockLibraryBase.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := TMockCertificateStore.Create(GetLibraryType);
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
    'Mock OpenSSL backend for system-roots public surface tests', 100000);
  TSSLFactory.RegisterLibrary(sslMbedTLS, TMockMbedTLSLibrary,
    'Mock MbedTLS backend for system-roots public surface tests', 90000);
  TSSLFactory.SetDefaultLibrary(sslOpenSSL);
end;

function InspectContext(const AContext: ISSLContext): IInspectableContext;
begin
  Check(Supports(AContext, IInspectableContext, Result),
    'Context supports inspection');
end;

procedure CheckStoreInjection(const ALabel: string; const AContext: ISSLContext;
  AExpectedBackend: TSSLLibraryType; const AExpectedVerifyMode: TSSLVerifyModes);
var
  LInspectable: IInspectableContext;
begin
  if not Supports(AContext, IInspectableContext, LInspectable) then
  begin
    Check(False, ALabel + ' context exposes inspection seam');
    Exit;
  end;

  CheckEqualsBackend(ALabel + ' context keeps explicit backend',
    AExpectedBackend, LInspectable.GetBackendType);
  CheckEqualsInt(ALabel + ' injects one certificate store', 1,
    LInspectable.GetSetCertificateStoreCount);
  CheckEqualsInt(ALabel + ' loads system roots before injection', 1,
    LInspectable.GetLastStoreLoadSystemStoreCount);
  CheckEqualsBackend(ALabel + ' uses explicit backend store',
    AExpectedBackend, LInspectable.GetLastStoreBackendType);
  CheckEqualsVerifyMode(ALabel + ' keeps verify mode after system roots',
    AExpectedVerifyMode, AContext.GetVerifyMode);
end;

procedure Test_FactoryOneShotClientUsesSystemRoots;
var
  LConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 1: one-shot factory client path loads system roots ===');
  ConfigureMockBackends;

  LConfig := CreateDefaultConfig(sslCtxClient);
  LConfig.LibraryType := sslMbedTLS;
  LConfig.ContextType := sslCtxClient;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.UseSystemRoots := True;

  LContext := TSSLFactory.CreateContext(LConfig);
  CheckStoreInjection('One-shot client factory', LContext, sslMbedTLS,
    [sslVerifyPeer]);
end;

procedure Test_FactoryOneShotServerUsesSystemRoots;
var
  LConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 2: one-shot factory server path keeps verify-peer with system roots ===');
  ConfigureMockBackends;

  LConfig := CreateDefaultConfig(sslCtxServer);
  LConfig.LibraryType := sslMbedTLS;
  LConfig.ContextType := sslCtxServer;
  LConfig.VerifyMode := [sslVerifyPeer];
  LConfig.UseSystemRoots := True;

  LContext := TSSLFactory.CreateContext(LConfig);
  CheckStoreInjection('One-shot server factory', LContext, sslMbedTLS,
    [sslVerifyPeer]);
end;

procedure Test_FactoryDefaultConfigClientUsesSystemRoots;
var
  LLib: ISSLLibrary;
  LOriginalConfig: TSSLConfig;
  LDefaultConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 3: raw factory client path uses library default-config system roots ===');
  ConfigureMockBackends;

  LLib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  LOriginalConfig := LLib.GetDefaultConfig;
  try
    LDefaultConfig := LOriginalConfig;
    LDefaultConfig.VerifyMode := [sslVerifyPeer];
    LDefaultConfig.UseSystemRoots := True;
    LLib.SetDefaultConfig(LDefaultConfig);

    LContext := TSSLFactory.CreateContext(sslCtxClient, sslMbedTLS);
    CheckStoreInjection('Default-config client factory', LContext, sslMbedTLS,
      [sslVerifyPeer]);
  finally
    LLib.SetDefaultConfig(LOriginalConfig);
  end;
end;

procedure Test_FactoryDefaultConfigServerUsesSystemRoots;
var
  LLib: ISSLLibrary;
  LOriginalConfig: TSSLConfig;
  LDefaultConfig: TSSLConfig;
  LContext: ISSLContext;
begin
  WriteLn('=== Test 4: raw factory server path keeps verify-peer with system roots ===');
  ConfigureMockBackends;

  LLib := TSSLFactory.GetLibraryInstance(sslMbedTLS);
  LOriginalConfig := LLib.GetDefaultConfig;
  try
    LDefaultConfig := LOriginalConfig;
    LDefaultConfig.VerifyMode := [sslVerifyPeer];
    LDefaultConfig.UseSystemRoots := True;
    LLib.SetDefaultConfig(LDefaultConfig);

    LContext := TSSLFactory.CreateContext(sslCtxServer, sslMbedTLS);
    CheckStoreInjection('Default-config server factory', LContext, sslMbedTLS,
      [sslVerifyPeer]);
  finally
    LLib.SetDefaultConfig(LOriginalConfig);
  end;
end;

begin
  Test_FactoryOneShotClientUsesSystemRoots;
  Test_FactoryOneShotServerUsesSystemRoots;
  Test_FactoryDefaultConfigClientUsesSystemRoots;
  Test_FactoryDefaultConfigServerUsesSystemRoots;

  WriteLn('---');
  WriteLn('Passed: ', GTestsPassed);
  WriteLn('Failed: ', GTestsFailed);

  if GTestsFailed > 0 then
    Halt(1);
end.
