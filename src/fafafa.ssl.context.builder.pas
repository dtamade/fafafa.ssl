{
  fafafa.ssl.context.builder - Fluent SSL Context Builder
  
  Provides a modern, fluent API for SSL context configuration, inspired by
  Rust's rustls ConfigBuilder pattern.
  
  Features:
  - Method chaining for readable code
  - Type-safe configuration
  - Safe defaults built-in
  - Separate client/server building
}

unit fafafa.ssl.context.builder;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.pkcs11.types,
  fafafa.ssl.backend.selector;  // v1.3.0: 自动后端选择

type
  { Forward declarations }
  ISSLContextBuilder = interface;

  { Callback types for conditional configuration (Phase 2.2.1) }
  TBuilderConfigProc = procedure(ABuilder: ISSLContextBuilder);

  { Callback type for transformation (Phase 2.2.4) }
  TBuilderTransformFunc = function(ABuilder: ISSLContextBuilder): ISSLContextBuilder;

  {**
   * ISSLContextBuilder - Fluent API for SSL context configuration
   * @stable 1.0
   * @locked 2025-12-24
   * @breaking-change-policy Requires major version bump
   *}
  ISSLContextBuilder = interface
    ['{F6A7B8C9-D0E1-4F23-4567-890ABCDEF012}']

    // Protocol version configuration
    function WithTLS12: ISSLContextBuilder;
    function WithTLS13: ISSLContextBuilder;
    function WithTLS12And13: ISSLContextBuilder;
    function WithProtocols(AVersions: TSSLProtocolVersions): ISSLContextBuilder;

    // Verification mode
    function WithVerifyPeer: ISSLContextBuilder;
    function WithVerifyNone: ISSLContextBuilder;  // Warning: Insecure!
    function WithVerifyDepth(ADepth: Integer): ISSLContextBuilder;

    // Certificate configuration
    function WithCertificate(const AFile: string): ISSLContextBuilder;
    function WithCertificatePEM(const APEM: string): ISSLContextBuilder;
    function WithPrivateKey(const AFile: string; const APassword: string = ''): ISSLContextBuilder;
    function WithPrivateKeyPEM(const APEM: string; const APassword: string = ''): ISSLContextBuilder;
    function WithCAFile(const AFile: string): ISSLContextBuilder;
    function WithCAPath(const APath: string): ISSLContextBuilder;
    function WithSystemRoots: ISSLContextBuilder;

    // Cipher configuration
    function WithCipherList(const ACiphers: string): ISSLContextBuilder;
    function WithTLS13Ciphersuites(const ACiphers: string): ISSLContextBuilder;
    function WithSafeDefaults: ISSLContextBuilder;  // Modern secure defaults

    // Advanced options
    function WithSNI(const AServerName: string): ISSLContextBuilder;
    function WithALPN(const AProtocols: string): ISSLContextBuilder;
    function WithSessionCache(AEnabled: Boolean): ISSLContextBuilder;
    function WithSessionTimeout(ASeconds: Integer): ISSLContextBuilder;

    // PKCS#11 support
    function UsePKCS11(const AURI: string): ISSLContextBuilder;
    function WithPKCS11PIN(const APIN: string): ISSLContextBuilder;
    function WithPKCS11PINMethod(AMethod: TPKCS11PINMethod): ISSLContextBuilder;

    // OCSP Stapling support
    function WithOCSPStapling(AEnabled: Boolean = True): ISSLContextBuilder;
    function WithOCSPStaplingRequired(ARequired: Boolean = True): ISSLContextBuilder;
    function WithCertVerifyCache(AEnabled: Boolean = True): ISSLContextBuilder;
    function WithCertVerifyCacheSkipValidHitRefresh(AEnabled: Boolean = True): ISSLContextBuilder;

    // v1.3.0: Automatic backend selection
    function WithAutoBackendSelection(const ARequirements: TSSLRequirements): ISSLContextBuilder;
    function WithSecurityFirst: ISSLContextBuilder;
    function WithPerformanceFirst: ISSLContextBuilder;
    function WithCompatibilityFirst: ISSLContextBuilder;
    function WithBackend(ABackendType: TSSLLibraryType): ISSLContextBuilder;
    function RequireTLS13: ISSLContextBuilder;
    function RequireCipher(ACipher: TSSLCipher): ISSLContextBuilder;
    function RequirePKCS11Support: ISSLContextBuilder;
    function PreferOSNative: ISSLContextBuilder;

    // Options
    function WithOption(AOption: TSSLOption): ISSLContextBuilder;
    function WithOptions(AOptions: TSSLOptions): ISSLContextBuilder;
    function WithoutOption(AOption: TSSLOption): ISSLContextBuilder;

    // Build methods
    function BuildClient: ISSLContext;
    function BuildServer: ISSLContext;

    // Try-pattern build methods (non-throwing)
    function TryBuildClient(out AContext: ISSLContext): TSSLOperationResult;
    function TryBuildServer(out AContext: ISSLContext): TSSLOperationResult;

    // Configuration validation (Phase 2.1.2)
    function Validate: TBuildValidationResult;
    function ValidateClient: TBuildValidationResult;
    function ValidateServer: TBuildValidationResult;
    function BuildClientWithValidation(out AValidation: TBuildValidationResult): ISSLContext;
    function BuildServerWithValidation(out AValidation: TBuildValidationResult): ISSLContext;

    // Configuration import/export (Phase 2.1.3)
    function ExportToJSON: string;
    function ImportFromJSON(const AJSON: string): ISSLContextBuilder;
    function ExportToINI: string;
    function ImportFromINI(const AINI: string): ISSLContextBuilder;

    // Configuration snapshot and clone (Phase 2.1.4)
    function Clone: ISSLContextBuilder;
    function Reset: ISSLContextBuilder;
    function ResetToDefaults: ISSLContextBuilder;  // Alias for Reset
    function Merge(ASource: ISSLContextBuilder): ISSLContextBuilder;

    // Conditional configuration (Phase 2.2.1)
    function When(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function Unless(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function WhenDevelopment(AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function WhenProduction(AConfig: TBuilderConfigProc): ISSLContextBuilder;

    // Batch configuration (Phase 2.2.2)
    function Apply(AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function ApplyPreset(APreset: ISSLContextBuilder): ISSLContextBuilder;
    function Pipe(ATransform: TBuilderConfigProc): ISSLContextBuilder;

    // Convenience methods (Phase 2.2.3)
    function WithCertificateChain(const ACerts: array of string): ISSLContextBuilder;
    function WithMutualTLS(const ACAFile: string; ARequired: Boolean = True): ISSLContextBuilder;
    function WithHTTP2: ISSLContextBuilder;
    function WithModernDefaults: ISSLContextBuilder;

    // Configuration transformation (Phase 2.2.4)
    function Transform(ATransform: TBuilderTransformFunc): ISSLContextBuilder;
    function Extend(const AOptions: array of TSSLOption): ISSLContextBuilder;
    function Override(const AField, AValue: string): ISSLContextBuilder;
  end;

  { Factory class for creating builders }
  TSSLContextBuilder = class
  public
    class function Create: ISSLContextBuilder; static;
    class function CreateWithSafeDefaults: ISSLContextBuilder; static;

    // Preset configurations (Phase 2.1.1)
    class function Development: ISSLContextBuilder; static;
    class function Production: ISSLContextBuilder; static;
    class function StrictSecurity: ISSLContextBuilder; static;
    class function LegacyCompatibility: ISSLContextBuilder; static;
  end;

implementation

uses
  fafafa.ssl.factory,
  fafafa.ssl.exceptions,
  fpjson, jsonparser;  // JSON support for Phase 2.1.3

type
  { Internal builder implementation }
  TSSLContextBuilderImpl = class(TInterfacedObject, ISSLContextBuilder)
  private
    FProtocolVersions: TSSLProtocolVersions;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FCertificateFile: string;
    FCertificatePEM: string;
    FPrivateKeyFile: string;
    FPrivateKeyPassword: string;
    FPrivateKeyPEM: string;
    FCAFile: string;
    FCAPath: string;
    FUseSystemRoots: Boolean;
    FCipherList: string;
    FTLS13Ciphersuites: string;
    FServerName: string;
    FALPNProtocols: string;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FOptions: TSSLOptions;
    
    // PKCS#11 fields
    FPKCS11URI: string;
    FPKCS11PIN: string;
    FPKCS11PINMethod: TPKCS11PINMethod;
    
    // OCSP Stapling fields
    FOCSPStaplingEnabled: Boolean;
    FOCSPStaplingRequired: Boolean;

    // v1.3.0: Automatic backend selection fields
    FAutoSelectBackend: Boolean;
    FBackendRequirements: TSSLRequirements;
    FExplicitBackend: TSSLLibraryType;
    FExplicitBackendSet: Boolean;

    procedure SyncOCSPStaplingOptions;
    procedure SyncAdvancedOptionCoupledFields(
      const AHasServerName, AHasALPNProtocols, AHasSessionCacheEnabled: Boolean;
      AUseFieldOnlyImportSemantics: Boolean);
    procedure ApplyResolvedContextConfiguration(AContext: ISSLContext;
      ASelectedBackend: TSSLLibraryType; ARequireServerIdentity: Boolean);
    function ResolveBuildBackend(out ASelectedBackend: TSSLLibraryType): Boolean;
    function CreateResolvedContext(AContextType: TSSLContextType;
      const AFailureMessage: string; out ASelectedBackend: TSSLLibraryType): ISSLContext;
  public
    constructor Create;
    
    // ISSLContextBuilder
    function WithTLS12: ISSLContextBuilder;
    function WithTLS13: ISSLContextBuilder;
    function WithTLS12And13: ISSLContextBuilder;
    function WithProtocols(AVersions: TSSLProtocolVersions): ISSLContextBuilder;
    
    function WithVerifyPeer: ISSLContextBuilder;
    function WithVerifyNone: ISSLContextBuilder;
    function WithVerifyDepth(ADepth: Integer): ISSLContextBuilder;
    
    function WithCertificate(const AFile: string): ISSLContextBuilder;
    function WithCertificatePEM(const APEM: string): ISSLContextBuilder;
    function WithPrivateKey(const AFile: string; const APassword: string = ''): ISSLContextBuilder;
    function WithPrivateKeyPEM(const APEM: string; const APassword: string = ''): ISSLContextBuilder;
    function WithCAFile(const AFile: string): ISSLContextBuilder;
    function WithCAPath(const APath: string): ISSLContextBuilder;
    function WithSystemRoots: ISSLContextBuilder;
    
    function WithCipherList(const ACiphers: string): ISSLContextBuilder;
    function WithTLS13Ciphersuites(const ACiphers: string): ISSLContextBuilder;
    function WithSafeDefaults: ISSLContextBuilder;
    
    function WithSNI(const AServerName: string): ISSLContextBuilder;
    function WithALPN(const AProtocols: string): ISSLContextBuilder;
    function WithSessionCache(AEnabled: Boolean): ISSLContextBuilder;
    function WithSessionTimeout(ASeconds: Integer): ISSLContextBuilder;
    
    function WithOption(AOption: TSSLOption): ISSLContextBuilder;
    function WithOptions(AOptions: TSSLOptions): ISSLContextBuilder;
    function WithoutOption(AOption: TSSLOption): ISSLContextBuilder;
    
    // PKCS#11 support
    function UsePKCS11(const AURI: string): ISSLContextBuilder;
    function WithPKCS11PIN(const APIN: string): ISSLContextBuilder;
    function WithPKCS11PINMethod(AMethod: TPKCS11PINMethod): ISSLContextBuilder;

    // OCSP Stapling support
    function WithOCSPStapling(AEnabled: Boolean = True): ISSLContextBuilder;
    function WithOCSPStaplingRequired(ARequired: Boolean = True): ISSLContextBuilder;
    function WithCertVerifyCache(AEnabled: Boolean = True): ISSLContextBuilder;
    function WithCertVerifyCacheSkipValidHitRefresh(AEnabled: Boolean = True): ISSLContextBuilder;

    // v1.3.0: Automatic backend selection
    function WithAutoBackendSelection(const ARequirements: TSSLRequirements): ISSLContextBuilder;
    function WithSecurityFirst: ISSLContextBuilder;
    function WithPerformanceFirst: ISSLContextBuilder;
    function WithCompatibilityFirst: ISSLContextBuilder;
    function WithBackend(ABackendType: TSSLLibraryType): ISSLContextBuilder;
    function RequireTLS13: ISSLContextBuilder;
    function RequireCipher(ACipher: TSSLCipher): ISSLContextBuilder;
    function RequirePKCS11Support: ISSLContextBuilder;
    function PreferOSNative: ISSLContextBuilder;

    function BuildClient: ISSLContext;
    function BuildServer: ISSLContext;
    function TryBuildClient(out AContext: ISSLContext): TSSLOperationResult;
    function TryBuildServer(out AContext: ISSLContext): TSSLOperationResult;

    // Configuration validation (Phase 2.1.2)
    function Validate: TBuildValidationResult;
    function ValidateClient: TBuildValidationResult;
    function ValidateServer: TBuildValidationResult;
    function BuildClientWithValidation(out AValidation: TBuildValidationResult): ISSLContext;
    function BuildServerWithValidation(out AValidation: TBuildValidationResult): ISSLContext;

    // Configuration import/export (Phase 2.1.3)
    function ExportToJSON: string;
    function ImportFromJSON(const AJSON: string): ISSLContextBuilder;
    function ExportToINI: string;
    function ImportFromINI(const AINI: string): ISSLContextBuilder;

    // Configuration snapshot and clone (Phase 2.1.4)
    function Clone: ISSLContextBuilder;
    function Reset: ISSLContextBuilder;
    function ResetToDefaults: ISSLContextBuilder;
    function Merge(ASource: ISSLContextBuilder): ISSLContextBuilder;

    // Conditional configuration (Phase 2.2.1)
    function When(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function Unless(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function WhenDevelopment(AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function WhenProduction(AConfig: TBuilderConfigProc): ISSLContextBuilder;

    // Batch configuration (Phase 2.2.2)
    function Apply(AConfig: TBuilderConfigProc): ISSLContextBuilder;
    function ApplyPreset(APreset: ISSLContextBuilder): ISSLContextBuilder;
    function Pipe(ATransform: TBuilderConfigProc): ISSLContextBuilder;

    // Convenience methods (Phase 2.2.3)
    function WithCertificateChain(const ACerts: array of string): ISSLContextBuilder;
    function WithMutualTLS(const ACAFile: string; ARequired: Boolean = True): ISSLContextBuilder;
    function WithHTTP2: ISSLContextBuilder;
    function WithModernDefaults: ISSLContextBuilder;

    // Configuration transformation (Phase 2.2.4)
    function Transform(ATransform: TBuilderTransformFunc): ISSLContextBuilder;
    function Extend(const AOptions: array of TSSLOption): ISSLContextBuilder;
    function Override(const AField, AValue: string): ISSLContextBuilder;
  end;

{ TSSLContextBuilder }

class function TSSLContextBuilder.Create: ISSLContextBuilder;
begin
  Result := TSSLContextBuilderImpl.Create;
end;

class function TSSLContextBuilder.CreateWithSafeDefaults: ISSLContextBuilder;
begin
  Result := TSSLContextBuilderImpl.Create.WithSafeDefaults;
end;

{ Preset Configurations - Phase 2.1.1 }

class function TSSLContextBuilder.Development: ISSLContextBuilder;
begin
  {
    Development preset:
    - Relaxed verification for easier testing
    - Session cache disabled for easier debugging
    - TLS 1.2 and 1.3 support
    - Detailed logging enabled (via options)
  }
  Result := TSSLContextBuilderImpl.Create
    .WithTLS12And13
    .WithVerifyNone  // Relaxed for development - accept self-signed certs
    .WithSessionCache(False)  // Easier debugging without session cache
    .WithOption(ssoEnableSessionTickets);
end;

class function TSSLContextBuilder.Production: ISSLContextBuilder;
begin
  {
    Production preset:
    - Strict security settings
    - Performance optimizations (session cache enabled)
    - TLS 1.2 and 1.3 only
    - Safe defaults for cipher suites
  }
  Result := TSSLContextBuilderImpl.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSessionCache(True)  // Performance: enable session cache
    .WithSafeDefaults
    .WithOption(ssoEnableSessionTickets);
end;

class function TSSLContextBuilder.StrictSecurity: ISSLContextBuilder;
begin
  {
    StrictSecurity preset:
    - Maximum security level
    - TLS 1.3 only
    - Strict peer verification
    - Strong cipher suites only
    - All modern security features enabled
  }
  Result := TSSLContextBuilderImpl.Create
    .WithTLS13  // Only TLS 1.3 for maximum security
    .WithVerifyPeer
    .WithVerifyDepth(10)
    .WithSessionCache(True)
    .WithOptions([
      ssoEnableSNI,
      ssoDisableCompression,      // Prevent CRIME attack
      ssoDisableRenegotiation,    // Prevent renegotiation attacks
      ssoCipherServerPreference,  // Server chooses cipher
      ssoNoSSLv2,                 // Disable all insecure protocols
      ssoNoSSLv3,
      ssoNoTLSv1,
      ssoNoTLSv1_1,
      ssoNoTLSv1_2                // TLS 1.3 only
    ]);
end;

class function TSSLContextBuilder.LegacyCompatibility: ISSLContextBuilder;
begin
  {
    LegacyCompatibility preset:
    - Support for older protocols (TLS 1.0, 1.1, 1.2, 1.3)
    - Wider cipher suite support
    - More lenient verification
    - For interoperability with legacy systems
    WARNING: This preset is less secure, use only when necessary!
  }
  Result := TSSLContextBuilderImpl.Create
    .WithProtocols([sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12, sslProtocolTLS13])
    .WithVerifyPeer  // Still verify, but allow older protocols
    .WithSessionCache(True)
    .WithOptions([
      ssoEnableSNI,
      ssoEnableSessionTickets
      // Note: We intentionally don't disable compression or renegotiation
      // for maximum compatibility with legacy systems
    ]);
end;

{ TSSLContextBuilderImpl }

constructor TSSLContextBuilderImpl.Create;
begin
  inherited Create;
  // Initialize with sensible defaults
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FCertificateFile := '';
  FCertificatePEM := '';
  FPrivateKeyFile := '';
  FPrivateKeyPassword := '';
  FPrivateKeyPEM := '';
  FCAFile := '';
  FCAPath := '';
  FUseSystemRoots := False;
  FCipherList := '';
  FTLS13Ciphersuites := '';
  FServerName := '';
  FALPNProtocols := '';
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FOptions := [ssoEnableSNI, ssoDisableCompression, ssoDisableRenegotiation];
  
  // PKCS#11 defaults
  FPKCS11URI := '';
  FPKCS11PIN := '';
  FPKCS11PINMethod := pmNone;
  
  // OCSP Stapling defaults
  FOCSPStaplingEnabled := False;
  FOCSPStaplingRequired := False;

  // v1.3.0: Automatic backend selection defaults
  FAutoSelectBackend := False;
  FillChar(FBackendRequirements, SizeOf(FBackendRequirements), 0);
  FExplicitBackend := sslOpenSSL;  // 默认
  FExplicitBackendSet := False;
end;

procedure TSSLContextBuilderImpl.SyncOCSPStaplingOptions;
begin
  // Option-set can come from generic APIs/import; keep boolean flags and options aligned.
  if ssoRequireOCSPStapling in FOptions then
    FOCSPStaplingRequired := True;

  if FOCSPStaplingRequired then
    Include(FOptions, ssoRequireOCSPStapling)
  else
    Exclude(FOptions, ssoRequireOCSPStapling);

  if ssoEnableOCSPStapling in FOptions then
    FOCSPStaplingEnabled := True;

  if FOCSPStaplingRequired then
    FOCSPStaplingEnabled := True;

  if FOCSPStaplingEnabled then
    Include(FOptions, ssoEnableOCSPStapling)
  else
  begin
    Exclude(FOptions, ssoEnableOCSPStapling);
    Exclude(FOptions, ssoRequireOCSPStapling);
    FOCSPStaplingRequired := False;
  end;
end;

procedure TSSLContextBuilderImpl.SyncAdvancedOptionCoupledFields(
  const AHasServerName, AHasALPNProtocols, AHasSessionCacheEnabled: Boolean;
  AUseFieldOnlyImportSemantics: Boolean);
begin
  if AHasServerName then
    if (not AUseFieldOnlyImportSemantics) or (FServerName <> '') then
      Include(FOptions, ssoEnableSNI);

  if AHasALPNProtocols then
    if (not AUseFieldOnlyImportSemantics) or (FALPNProtocols <> '') then
      Include(FOptions, ssoEnableALPN);

  if AHasSessionCacheEnabled then
    if FSessionCacheEnabled then
      Include(FOptions, ssoEnableSessionCache)
    else
      Exclude(FOptions, ssoEnableSessionCache);
end;

function PKCS11PINMethodToText(AMethod: TPKCS11PINMethod): string;
begin
  case AMethod of
    pmValue: Result := 'value';
    pmEnvironment: Result := 'environment';
    pmFile: Result := 'file';
    pmCallback: Result := 'callback';
    pmInteractive: Result := 'interactive';
  else
    Result := 'none';
  end;
end;

function TryParsePKCS11PINMethodText(const AValue: string;
  out AMethod: TPKCS11PINMethod): Boolean;
var
  LValue: string;
begin
  LValue := LowerCase(Trim(AValue));

  if (LValue = 'none') or (LValue = 'pmnone') then
    AMethod := pmNone
  else if (LValue = 'value') or (LValue = 'pmvalue') then
    AMethod := pmValue
  else if (LValue = 'environment') or (LValue = 'env') or (LValue = 'pmenvironment') then
    AMethod := pmEnvironment
  else if (LValue = 'file') or (LValue = 'pmfile') then
    AMethod := pmFile
  else if (LValue = 'callback') or (LValue = 'pmcallback') then
    AMethod := pmCallback
  else if (LValue = 'interactive') or (LValue = 'prompt') or (LValue = 'pminteractive') then
    AMethod := pmInteractive
  else
    Exit(False);

  Result := True;
end;


procedure AddBackendSelectionToJSON(AObject: TJSONObject;
  AAutoSelectBackend: Boolean;
  const ARequirements: TSSLRequirements;
  AExplicitBackend: TSSLLibraryType;
  AExplicitBackendSet: Boolean);
var
  LProtocols: TJSONArray;
  LCiphers: TJSONArray;
  LHashes: TJSONArray;
  LKeyExchanges: TJSONArray;
  LFeatures: TJSONArray;
  LPreferredCiphers: TJSONArray;
  LPreferredHashes: TJSONArray;
  LProtocol: TSSLProtocolVersion;
  LCipher: TSSLCipher;
  LHash: TSSLHash;
  LKeyExchange: TSSLKeyExchange;
  LFeature: TSSLFeature;
begin
  AObject.Add('backend_auto_select', AAutoSelectBackend);
  AObject.Add('backend_explicit_library', Ord(AExplicitBackend));
  AObject.Add('backend_explicit_library_set', AExplicitBackendSet);

  LProtocols := TJSONArray.Create;
  for LProtocol := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
    if LProtocol in ARequirements.RequiredProtocols then
      LProtocols.Add(Ord(LProtocol));
  AObject.Add('backend_required_protocols', LProtocols);

  LCiphers := TJSONArray.Create;
  for LCipher := Low(TSSLCipher) to High(TSSLCipher) do
    if LCipher in ARequirements.RequiredCiphers then
      LCiphers.Add(Ord(LCipher));
  AObject.Add('backend_required_ciphers', LCiphers);

  LHashes := TJSONArray.Create;
  for LHash := Low(TSSLHash) to High(TSSLHash) do
    if LHash in ARequirements.RequiredHashes then
      LHashes.Add(Ord(LHash));
  AObject.Add('backend_required_hashes', LHashes);

  LKeyExchanges := TJSONArray.Create;
  for LKeyExchange := Low(TSSLKeyExchange) to High(TSSLKeyExchange) do
    if LKeyExchange in ARequirements.RequiredKeyExchanges then
      LKeyExchanges.Add(Ord(LKeyExchange));
  AObject.Add('backend_required_key_exchanges', LKeyExchanges);

  LFeatures := TJSONArray.Create;
  for LFeature := Low(TSSLFeature) to High(TSSLFeature) do
    if LFeature in ARequirements.RequiredFeatures then
      LFeatures.Add(Ord(LFeature));
  AObject.Add('backend_required_features', LFeatures);

  LPreferredCiphers := TJSONArray.Create;
  for LCipher := Low(TSSLCipher) to High(TSSLCipher) do
    if LCipher in ARequirements.PreferredCiphers then
      LPreferredCiphers.Add(Ord(LCipher));
  AObject.Add('backend_preferred_ciphers', LPreferredCiphers);

  LPreferredHashes := TJSONArray.Create;
  for LHash := Low(TSSLHash) to High(TSSLHash) do
    if LHash in ARequirements.PreferredHashes then
      LPreferredHashes.Add(Ord(LHash));
  AObject.Add('backend_preferred_hashes', LPreferredHashes);

  AObject.Add('backend_min_security_score', ARequirements.MinSecurityScore);
  AObject.Add('backend_min_performance_score', ARequirements.MinPerformanceScore);
  AObject.Add('backend_min_compatibility_level', ARequirements.MinCompatibilityLevel);
  AObject.Add('backend_prefer_os_native', ARequirements.PlatformPreferences.PreferOSNative);
  AObject.Add('backend_prefer_hardware_accel',
    ARequirements.PlatformPreferences.PreferHardwareAccel);
  AObject.Add('backend_prefer_fips_compliant',
    ARequirements.PlatformPreferences.PreferFIPSCompliant);
  AObject.Add('backend_require_pkcs11', ARequirements.PlatformPreferences.RequirePKCS11);
  AObject.Add('backend_require_tpm', ARequirements.PlatformPreferences.RequireTPM);
  AObject.Add('backend_require_system_cert_store',
    ARequirements.PlatformPreferences.RequireSystemCertStore);
  AObject.Add('backend_optimization_target', Ord(ARequirements.OptimizationTarget));
end;

procedure ResetBackendRequirements(var ARequirements: TSSLRequirements); forward;
procedure ResetExplicitBackendSelection(
  var AExplicitBackend: TSSLLibraryType;
  var AExplicitBackendSet: Boolean); forward;
function HasBackendSelectionRequirements(const ARequirements: TSSLRequirements): Boolean; forward;
procedure NormalizeBackendSelectionState(
  var AAutoSelectBackend: Boolean;
  var ARequirements: TSSLRequirements;
  var AExplicitBackend: TSSLLibraryType;
  var AExplicitBackendSet: Boolean); forward;

procedure LoadBackendSelectionFromJSONObject(AObject: TJSONObject;
  var AAutoSelectBackend: Boolean;
  var ARequirements: TSSLRequirements;
  var AExplicitBackend: TSSLLibraryType;
  var AExplicitBackendSet: Boolean);
var
  LValues: TJSONArray;
  I: Integer;
begin
  if AObject.IndexOfName('backend_auto_select') >= 0 then
    AAutoSelectBackend := AObject.Booleans['backend_auto_select'];

  if AObject.IndexOfName('backend_explicit_library') >= 0 then
    AExplicitBackend := TSSLLibraryType(AObject.Integers['backend_explicit_library']);

  if AObject.IndexOfName('backend_explicit_library_set') >= 0 then
    AExplicitBackendSet := AObject.Booleans['backend_explicit_library_set'];

  if AObject.IndexOfName('backend_required_protocols') >= 0 then
  begin
    LValues := AObject.Arrays['backend_required_protocols'];
    ARequirements.RequiredProtocols := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.RequiredProtocols, TSSLProtocolVersion(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_required_ciphers') >= 0 then
  begin
    LValues := AObject.Arrays['backend_required_ciphers'];
    ARequirements.RequiredCiphers := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.RequiredCiphers, TSSLCipher(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_required_hashes') >= 0 then
  begin
    LValues := AObject.Arrays['backend_required_hashes'];
    ARequirements.RequiredHashes := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.RequiredHashes, TSSLHash(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_required_key_exchanges') >= 0 then
  begin
    LValues := AObject.Arrays['backend_required_key_exchanges'];
    ARequirements.RequiredKeyExchanges := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.RequiredKeyExchanges, TSSLKeyExchange(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_required_features') >= 0 then
  begin
    LValues := AObject.Arrays['backend_required_features'];
    ARequirements.RequiredFeatures := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.RequiredFeatures, TSSLFeature(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_preferred_ciphers') >= 0 then
  begin
    LValues := AObject.Arrays['backend_preferred_ciphers'];
    ARequirements.PreferredCiphers := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.PreferredCiphers, TSSLCipher(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_preferred_hashes') >= 0 then
  begin
    LValues := AObject.Arrays['backend_preferred_hashes'];
    ARequirements.PreferredHashes := [];
    for I := 0 to LValues.Count - 1 do
      Include(ARequirements.PreferredHashes, TSSLHash(LValues.Integers[I]));
  end;

  if AObject.IndexOfName('backend_min_security_score') >= 0 then
    ARequirements.MinSecurityScore := AObject.Integers['backend_min_security_score'];

  if AObject.IndexOfName('backend_min_performance_score') >= 0 then
    ARequirements.MinPerformanceScore := AObject.Integers['backend_min_performance_score'];

  if AObject.IndexOfName('backend_min_compatibility_level') >= 0 then
    ARequirements.MinCompatibilityLevel := AObject.Integers['backend_min_compatibility_level'];

  if AObject.IndexOfName('backend_prefer_os_native') >= 0 then
    ARequirements.PlatformPreferences.PreferOSNative :=
      AObject.Booleans['backend_prefer_os_native'];

  if AObject.IndexOfName('backend_prefer_hardware_accel') >= 0 then
    ARequirements.PlatformPreferences.PreferHardwareAccel :=
      AObject.Booleans['backend_prefer_hardware_accel'];

  if AObject.IndexOfName('backend_prefer_fips_compliant') >= 0 then
    ARequirements.PlatformPreferences.PreferFIPSCompliant :=
      AObject.Booleans['backend_prefer_fips_compliant'];

  if AObject.IndexOfName('backend_require_pkcs11') >= 0 then
    ARequirements.PlatformPreferences.RequirePKCS11 :=
      AObject.Booleans['backend_require_pkcs11'];

  if AObject.IndexOfName('backend_require_tpm') >= 0 then
    ARequirements.PlatformPreferences.RequireTPM :=
      AObject.Booleans['backend_require_tpm'];

  if AObject.IndexOfName('backend_require_system_cert_store') >= 0 then
    ARequirements.PlatformPreferences.RequireSystemCertStore :=
      AObject.Booleans['backend_require_system_cert_store'];

  if AObject.IndexOfName('backend_optimization_target') >= 0 then
    ARequirements.OptimizationTarget :=
      TSSLOptimizationTarget(AObject.Integers['backend_optimization_target']);

  NormalizeBackendSelectionState(
    AAutoSelectBackend,
    ARequirements,
    AExplicitBackend,
    AExplicitBackendSet);
end;

procedure AddBackendSelectionToINI(ALines: TStringList;
  AAutoSelectBackend: Boolean;
  const ARequirements: TSSLRequirements;
  AExplicitBackend: TSSLLibraryType;
  AExplicitBackendSet: Boolean);
var
  LRequiredProtocols: string;
  LRequiredCiphers: string;
  LRequiredHashes: string;
  LRequiredKeyExchanges: string;
  LRequiredFeatures: string;
  LPreferredCiphers: string;
  LPreferredHashes: string;
  LProtocol: TSSLProtocolVersion;
  LCipher: TSSLCipher;
  LHash: TSSLHash;
  LKeyExchange: TSSLKeyExchange;
  LFeature: TSSLFeature;

  procedure AppendValue(var ATarget: string; AValue: Integer);
  begin
    if ATarget <> '' then
      ATarget := ATarget + ',';
    ATarget := ATarget + IntToStr(AValue);
  end;
begin
  LRequiredProtocols := '';
  for LProtocol := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
    if LProtocol in ARequirements.RequiredProtocols then
      AppendValue(LRequiredProtocols, Ord(LProtocol));

  LRequiredCiphers := '';
  for LCipher := Low(TSSLCipher) to High(TSSLCipher) do
    if LCipher in ARequirements.RequiredCiphers then
      AppendValue(LRequiredCiphers, Ord(LCipher));

  LRequiredHashes := '';
  for LHash := Low(TSSLHash) to High(TSSLHash) do
    if LHash in ARequirements.RequiredHashes then
      AppendValue(LRequiredHashes, Ord(LHash));

  LRequiredKeyExchanges := '';
  for LKeyExchange := Low(TSSLKeyExchange) to High(TSSLKeyExchange) do
    if LKeyExchange in ARequirements.RequiredKeyExchanges then
      AppendValue(LRequiredKeyExchanges, Ord(LKeyExchange));

  LRequiredFeatures := '';
  for LFeature := Low(TSSLFeature) to High(TSSLFeature) do
    if LFeature in ARequirements.RequiredFeatures then
      AppendValue(LRequiredFeatures, Ord(LFeature));

  LPreferredCiphers := '';
  for LCipher := Low(TSSLCipher) to High(TSSLCipher) do
    if LCipher in ARequirements.PreferredCiphers then
      AppendValue(LPreferredCiphers, Ord(LCipher));

  LPreferredHashes := '';
  for LHash := Low(TSSLHash) to High(TSSLHash) do
    if LHash in ARequirements.PreferredHashes then
      AppendValue(LPreferredHashes, Ord(LHash));

  ALines.Add('[Backend Selection]');
  if AAutoSelectBackend then
    ALines.Add('backend_auto_select=true')
  else
    ALines.Add('backend_auto_select=false');
  ALines.Add('backend_explicit_library=' + IntToStr(Ord(AExplicitBackend)));
  if AExplicitBackendSet then
    ALines.Add('backend_explicit_library_set=true')
  else
    ALines.Add('backend_explicit_library_set=false');
  ALines.Add('backend_required_protocols=' + LRequiredProtocols);
  ALines.Add('backend_required_ciphers=' + LRequiredCiphers);
  ALines.Add('backend_required_hashes=' + LRequiredHashes);
  ALines.Add('backend_required_key_exchanges=' + LRequiredKeyExchanges);
  ALines.Add('backend_required_features=' + LRequiredFeatures);
  ALines.Add('backend_preferred_ciphers=' + LPreferredCiphers);
  ALines.Add('backend_preferred_hashes=' + LPreferredHashes);
  ALines.Add('backend_min_security_score=' + IntToStr(ARequirements.MinSecurityScore));
  ALines.Add('backend_min_performance_score=' + IntToStr(ARequirements.MinPerformanceScore));
  ALines.Add('backend_min_compatibility_level=' + IntToStr(ARequirements.MinCompatibilityLevel));
  if ARequirements.PlatformPreferences.PreferOSNative then
    ALines.Add('backend_prefer_os_native=true')
  else
    ALines.Add('backend_prefer_os_native=false');
  if ARequirements.PlatformPreferences.PreferHardwareAccel then
    ALines.Add('backend_prefer_hardware_accel=true')
  else
    ALines.Add('backend_prefer_hardware_accel=false');
  if ARequirements.PlatformPreferences.PreferFIPSCompliant then
    ALines.Add('backend_prefer_fips_compliant=true')
  else
    ALines.Add('backend_prefer_fips_compliant=false');
  if ARequirements.PlatformPreferences.RequirePKCS11 then
    ALines.Add('backend_require_pkcs11=true')
  else
    ALines.Add('backend_require_pkcs11=false');
  if ARequirements.PlatformPreferences.RequireTPM then
    ALines.Add('backend_require_tpm=true')
  else
    ALines.Add('backend_require_tpm=false');
  if ARequirements.PlatformPreferences.RequireSystemCertStore then
    ALines.Add('backend_require_system_cert_store=true')
  else
    ALines.Add('backend_require_system_cert_store=false');
  ALines.Add('backend_optimization_target=' +
    IntToStr(Ord(ARequirements.OptimizationTarget)));
  ALines.Add('');
end;

function ApplyBackendSelectionINIKey(const AKey, AValue: string;
  AParts: TStringList;
  var AAutoSelectBackend: Boolean;
  var ARequirements: TSSLRequirements;
  var AExplicitBackend: TSSLLibraryType;
  var AExplicitBackendSet: Boolean): Boolean;
var
  I: Integer;
begin
  Result := True;

  if AKey = 'backend_auto_select' then
    AAutoSelectBackend := LowerCase(AValue) = 'true'
  else if AKey = 'backend_explicit_library' then
    AExplicitBackend := TSSLLibraryType(StrToIntDef(AValue, Ord(sslOpenSSL)))
  else if AKey = 'backend_explicit_library_set' then
    AExplicitBackendSet := LowerCase(AValue) = 'true'
  else if AKey = 'backend_required_protocols' then
  begin
    ARequirements.RequiredProtocols := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.RequiredProtocols,
          TSSLProtocolVersion(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_required_ciphers' then
  begin
    ARequirements.RequiredCiphers := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.RequiredCiphers,
          TSSLCipher(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_required_hashes' then
  begin
    ARequirements.RequiredHashes := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.RequiredHashes,
          TSSLHash(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_required_key_exchanges' then
  begin
    ARequirements.RequiredKeyExchanges := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.RequiredKeyExchanges,
          TSSLKeyExchange(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_required_features' then
  begin
    ARequirements.RequiredFeatures := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.RequiredFeatures,
          TSSLFeature(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_preferred_ciphers' then
  begin
    ARequirements.PreferredCiphers := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.PreferredCiphers,
          TSSLCipher(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_preferred_hashes' then
  begin
    ARequirements.PreferredHashes := [];
    if AValue <> '' then
    begin
      AParts.CommaText := AValue;
      for I := 0 to AParts.Count - 1 do
        Include(ARequirements.PreferredHashes,
          TSSLHash(StrToIntDef(AParts[I], 0)));
    end;
  end
  else if AKey = 'backend_min_security_score' then
    ARequirements.MinSecurityScore := StrToIntDef(AValue, 0)
  else if AKey = 'backend_min_performance_score' then
    ARequirements.MinPerformanceScore := StrToIntDef(AValue, 0)
  else if AKey = 'backend_min_compatibility_level' then
    ARequirements.MinCompatibilityLevel := StrToIntDef(AValue, 0)
  else if AKey = 'backend_prefer_os_native' then
    ARequirements.PlatformPreferences.PreferOSNative := LowerCase(AValue) = 'true'
  else if AKey = 'backend_prefer_hardware_accel' then
    ARequirements.PlatformPreferences.PreferHardwareAccel := LowerCase(AValue) = 'true'
  else if AKey = 'backend_prefer_fips_compliant' then
    ARequirements.PlatformPreferences.PreferFIPSCompliant := LowerCase(AValue) = 'true'
  else if AKey = 'backend_require_pkcs11' then
    ARequirements.PlatformPreferences.RequirePKCS11 := LowerCase(AValue) = 'true'
  else if AKey = 'backend_require_tpm' then
    ARequirements.PlatformPreferences.RequireTPM := LowerCase(AValue) = 'true'
  else if AKey = 'backend_require_system_cert_store' then
    ARequirements.PlatformPreferences.RequireSystemCertStore := LowerCase(AValue) = 'true'
  else if AKey = 'backend_optimization_target' then
    ARequirements.OptimizationTarget :=
      TSSLOptimizationTarget(StrToIntDef(AValue, Ord(optBalanced)))
  else
    Result := False;
end;


procedure ResetBackendRequirements(var ARequirements: TSSLRequirements);
begin
  FillChar(ARequirements, SizeOf(ARequirements), 0);
end;

procedure ResetExplicitBackendSelection(
  var AExplicitBackend: TSSLLibraryType;
  var AExplicitBackendSet: Boolean);
begin
  AExplicitBackend := sslOpenSSL;
  AExplicitBackendSet := False;
end;

function HasBackendSelectionRequirements(const ARequirements: TSSLRequirements): Boolean;
begin
  Result :=
    (ARequirements.RequiredProtocols <> []) or
    (ARequirements.RequiredCiphers <> []) or
    (ARequirements.RequiredHashes <> []) or
    (ARequirements.RequiredKeyExchanges <> []) or
    (ARequirements.RequiredFeatures <> []) or
    (ARequirements.PreferredCiphers <> []) or
    (ARequirements.PreferredHashes <> []) or
    (ARequirements.MinSecurityScore <> 0) or
    (ARequirements.MinPerformanceScore <> 0) or
    (ARequirements.MinCompatibilityLevel <> 0) or
    ARequirements.PlatformPreferences.PreferOSNative or
    ARequirements.PlatformPreferences.PreferHardwareAccel or
    ARequirements.PlatformPreferences.PreferFIPSCompliant or
    ARequirements.PlatformPreferences.RequirePKCS11 or
    ARequirements.PlatformPreferences.RequireTPM or
    ARequirements.PlatformPreferences.RequireSystemCertStore or
    (ARequirements.OptimizationTarget <> optBalanced);
end;

procedure NormalizeBackendSelectionState(
  var AAutoSelectBackend: Boolean;
  var ARequirements: TSSLRequirements;
  var AExplicitBackend: TSSLLibraryType;
  var AExplicitBackendSet: Boolean);
begin
  if AAutoSelectBackend then
  begin
    ResetExplicitBackendSelection(AExplicitBackend, AExplicitBackendSet);
    Exit;
  end;

  if AExplicitBackendSet then
  begin
    ResetBackendRequirements(ARequirements);
    Exit;
  end;

  if HasBackendSelectionRequirements(ARequirements) then
  begin
    AAutoSelectBackend := True;
    ResetExplicitBackendSelection(AExplicitBackend, AExplicitBackendSet);
    Exit;
  end;

  ResetBackendRequirements(ARequirements);
  ResetExplicitBackendSelection(AExplicitBackend, AExplicitBackendSet);
end;

function TSSLContextBuilderImpl.WithTLS12: ISSLContextBuilder;
begin
  FProtocolVersions := [sslProtocolTLS12];
  Result := Self;
end;

function TSSLContextBuilderImpl.WithTLS13: ISSLContextBuilder;
begin
  FProtocolVersions := [sslProtocolTLS13];
  Result := Self;
end;

function TSSLContextBuilderImpl.WithTLS12And13: ISSLContextBuilder;
begin
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  Result := Self;
end;

function TSSLContextBuilderImpl.WithProtocols(AVersions: TSSLProtocolVersions): ISSLContextBuilder;
begin
  FProtocolVersions := AVersions;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithVerifyPeer: ISSLContextBuilder;
begin
  FVerifyMode := [sslVerifyPeer];
  Result := Self;
end;

function TSSLContextBuilderImpl.WithVerifyNone: ISSLContextBuilder;
begin
  FVerifyMode := [sslVerifyNone];
  Result := Self;
end;

function TSSLContextBuilderImpl.WithVerifyDepth(ADepth: Integer): ISSLContextBuilder;
begin
  FVerifyDepth := ADepth;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCertificate(const AFile: string): ISSLContextBuilder;
begin
  FCertificateFile := AFile;
  FCertificatePEM := '';
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCertificatePEM(const APEM: string): ISSLContextBuilder;
begin
  FCertificatePEM := APEM;
  FCertificateFile := '';
  Result := Self;
end;

function TSSLContextBuilderImpl.WithPrivateKey(const AFile: string; const APassword: string): ISSLContextBuilder;
begin
  FPrivateKeyFile := AFile;
  FPrivateKeyPassword := APassword;
  FPrivateKeyPEM := '';
  Result := Self;
end;

function TSSLContextBuilderImpl.WithPrivateKeyPEM(const APEM: string; const APassword: string): ISSLContextBuilder;
begin
  FPrivateKeyPEM := APEM;
  FPrivateKeyPassword := APassword;
  FPrivateKeyFile := '';
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCAFile(const AFile: string): ISSLContextBuilder;
begin
  FCAFile := AFile;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCAPath(const APath: string): ISSLContextBuilder;
begin
  FCAPath := APath;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithSystemRoots: ISSLContextBuilder;
begin
  FUseSystemRoots := True;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCipherList(const ACiphers: string): ISSLContextBuilder;
begin
  FCipherList := ACiphers;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithTLS13Ciphersuites(const ACiphers: string): ISSLContextBuilder;
begin
  FTLS13Ciphersuites := ACiphers;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithSafeDefaults: ISSLContextBuilder;
begin
  // Apply modern, secure defaults
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FCipherList := SSL_DEFAULT_CIPHER_LIST;
  FTLS13Ciphersuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
  FOptions := [
    ssoEnableSNI,
    ssoDisableCompression,      // Prevent CRIME attack
    ssoDisableRenegotiation,    // Prevent renegotiation attacks
    ssoCipherServerPreference,  // Server chooses cipher
    ssoNoSSLv2,                 // Disable insecure protocols
    ssoNoSSLv3,
    ssoNoTLSv1,
    ssoNoTLSv1_1
  ];
  Result := Self;
end;

function TSSLContextBuilderImpl.WithSNI(const AServerName: string): ISSLContextBuilder;
begin
  FServerName := AServerName;
  Include(FOptions, ssoEnableSNI);
  Result := Self;
end;

function TSSLContextBuilderImpl.WithALPN(const AProtocols: string): ISSLContextBuilder;
begin
  FALPNProtocols := AProtocols;
  Include(FOptions, ssoEnableALPN);
  Result := Self;
end;

function TSSLContextBuilderImpl.WithSessionCache(AEnabled: Boolean): ISSLContextBuilder;
begin
  FSessionCacheEnabled := AEnabled;
  if AEnabled then
    Include(FOptions, ssoEnableSessionCache)
  else
    Exclude(FOptions, ssoEnableSessionCache);
  Result := Self;
end;

function TSSLContextBuilderImpl.WithSessionTimeout(ASeconds: Integer): ISSLContextBuilder;
begin
  FSessionTimeout := ASeconds;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithOption(AOption: TSSLOption): ISSLContextBuilder;
begin
  Include(FOptions, AOption);

  case AOption of
    ssoEnableOCSPStapling:
      FOCSPStaplingEnabled := True;
    ssoRequireOCSPStapling:
      FOCSPStaplingRequired := True;
  else
    // Other options have no coupled builder state.
  end;

  SyncOCSPStaplingOptions;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithOptions(AOptions: TSSLOptions): ISSLContextBuilder;
begin
  FOptions := FOptions + AOptions;
  SyncOCSPStaplingOptions;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithoutOption(AOption: TSSLOption): ISSLContextBuilder;
begin
  Exclude(FOptions, AOption);

  case AOption of
    ssoEnableOCSPStapling:
      begin
        FOCSPStaplingEnabled := False;
        FOCSPStaplingRequired := False;
      end;
    ssoRequireOCSPStapling:
      FOCSPStaplingRequired := False;
  else
    // Other options have no coupled builder state.
  end;

  SyncOCSPStaplingOptions;
  Result := Self;
end;

{ PKCS#11 Support }

function TSSLContextBuilderImpl.UsePKCS11(const AURI: string): ISSLContextBuilder;
begin
  FPKCS11URI := AURI;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithPKCS11PIN(const APIN: string): ISSLContextBuilder;
begin
  FPKCS11PIN := APIN;
  FPKCS11PINMethod := pmValue;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithPKCS11PINMethod(AMethod: TPKCS11PINMethod): ISSLContextBuilder;
begin
  FPKCS11PINMethod := AMethod;
  Result := Self;
end;

{ OCSP Stapling Support }

function TSSLContextBuilderImpl.WithOCSPStapling(AEnabled: Boolean): ISSLContextBuilder;
begin
  FOCSPStaplingEnabled := AEnabled;
  if AEnabled then
    Include(FOptions, ssoEnableOCSPStapling)
  else
    Exclude(FOptions, ssoEnableOCSPStapling);

  SyncOCSPStaplingOptions;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithOCSPStaplingRequired(ARequired: Boolean): ISSLContextBuilder;
begin
  FOCSPStaplingRequired := ARequired;

  if ARequired then
    Include(FOptions, ssoRequireOCSPStapling)
  else
    Exclude(FOptions, ssoRequireOCSPStapling);

  SyncOCSPStaplingOptions;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCertVerifyCache(AEnabled: Boolean): ISSLContextBuilder;
begin
  if AEnabled then
    Include(FOptions, ssoEnableCertVerifyCache)
  else
    Exclude(FOptions, ssoEnableCertVerifyCache);
  Result := Self;
end;

function TSSLContextBuilderImpl.WithCertVerifyCacheSkipValidHitRefresh(AEnabled: Boolean): ISSLContextBuilder;
begin
  if AEnabled then
    Include(FOptions, ssoSkipCertVerifyCacheValidHitRefresh)
  else
    Exclude(FOptions, ssoSkipCertVerifyCacheValidHitRefresh);
  Result := Self;
end;

function TSSLContextBuilderImpl.ResolveBuildBackend(
  out ASelectedBackend: TSSLLibraryType): Boolean;
var
  LMatchScore: Integer;
begin
  ASelectedBackend := sslAutoDetect;

  if FAutoSelectBackend then
  begin
    Result := SelectBestBackend(FBackendRequirements, ASelectedBackend, LMatchScore);
    Exit;
  end;

  Result := True;
  if FExplicitBackendSet then
  begin
    if FExplicitBackend = sslAutoDetect then
      ASelectedBackend := TSSLFactory.GetDefaultLibrary
    else
      ASelectedBackend := FExplicitBackend;
    Exit;
  end;

  ASelectedBackend := TSSLFactory.GetDefaultLibrary;
end;

function TSSLContextBuilderImpl.CreateResolvedContext(
  AContextType: TSSLContextType;
  const AFailureMessage: string;
  out ASelectedBackend: TSSLLibraryType): ISSLContext;
begin
  if not ResolveBuildBackend(ASelectedBackend) then
    raise ESSLException.Create('No suitable SSL backend found for requirements');

  Result := TSSLFactory.CreateContext(AContextType, ASelectedBackend);
  if Result = nil then
    raise ESSLException.Create(AFailureMessage);
end;

procedure TSSLContextBuilderImpl.ApplyResolvedContextConfiguration(AContext: ISSLContext;
  ASelectedBackend: TSSLLibraryType; ARequireServerIdentity: Boolean);
var
  Store: ISSLCertificateStore;
begin
  Store := nil;

  SyncOCSPStaplingOptions;
  AContext.SetProtocolVersions(FProtocolVersions);
  AContext.SetVerifyMode(FVerifyMode);
  AContext.SetVerifyDepth(FVerifyDepth);
  AContext.SetOptions(FOptions);

  if ARequireServerIdentity then
  begin
    if (FCertificateFile = '') and (FCertificatePEM = '') then
      raise ESSLException.Create('Server context requires a certificate');

    if (FPrivateKeyFile = '') and (FPrivateKeyPEM = '') and (FPKCS11URI = '') then
      raise ESSLException.Create('Server context requires a private key');
  end;

  if FCertificatePEM <> '' then
    AContext.LoadCertificatePEM(FCertificatePEM)
  else if FCertificateFile <> '' then
    AContext.LoadCertificate(FCertificateFile);

  if FPKCS11URI <> '' then
  begin
    AContext.LoadPrivateKey(FPKCS11URI, FPKCS11PIN);
  end
  else if FPrivateKeyPEM <> '' then
    AContext.LoadPrivateKeyPEM(FPrivateKeyPEM, FPrivateKeyPassword)
  else if FPrivateKeyFile <> '' then
    AContext.LoadPrivateKey(FPrivateKeyFile, FPrivateKeyPassword);

  if FUseSystemRoots then
  begin
    Store := TSSLFactory.CreateCertificateStore(ASelectedBackend);
    if Store <> nil then
    begin
      Store.LoadSystemStore;
      AContext.SetCertificateStore(Store);
    end;
  end;

  if FCAFile <> '' then
    AContext.LoadCAFile(FCAFile);

  if FCAPath <> '' then
    AContext.LoadCAPath(FCAPath);

  if FCipherList <> '' then
    AContext.SetCipherList(FCipherList);

  if FTLS13Ciphersuites <> '' then
    AContext.SetCipherSuites(FTLS13Ciphersuites);

  if FServerName <> '' then
  begin
    {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
    AContext.SetServerName(FServerName);
    {$POP}
  end;

  if FALPNProtocols <> '' then
    AContext.SetALPNProtocols(FALPNProtocols);

  AContext.SetSessionCacheMode(FSessionCacheEnabled);
  AContext.SetSessionTimeout(FSessionTimeout);
end;

function TSSLContextBuilderImpl.BuildClient: ISSLContext;
var
  SelectedBackend: TSSLLibraryType;
begin
  SelectedBackend := sslAutoDetect;

  Result := CreateResolvedContext(
    sslCtxClient,
    'Failed to create SSL client context',
    SelectedBackend);

  ApplyResolvedContextConfiguration(Result, SelectedBackend, False);
end;

function TSSLContextBuilderImpl.BuildServer: ISSLContext;
var
  SelectedBackend: TSSLLibraryType;
begin
  SelectedBackend := sslAutoDetect;

  Result := CreateResolvedContext(
    sslCtxServer,
    'Failed to create SSL server context',
    SelectedBackend);

  ApplyResolvedContextConfiguration(Result, SelectedBackend, True);
end;

function TSSLContextBuilderImpl.TryBuildClient(out AContext: ISSLContext): TSSLOperationResult;
begin
  AContext := nil;

  try
    AContext := BuildClient;
    if AContext = nil then
    begin
      Result := TSSLOperationResult.Err(sslErrConfiguration, 'Failed to create SSL client context');
      Exit;
    end;

    Result := TSSLOperationResult.Ok;
  except
    on E: ESSLException do
    begin
      AContext := nil;
      Result := TSSLOperationResult.Err(sslErrConfiguration, 'SSL error: ' + E.Message);
    end;
    on E: Exception do
    begin
      AContext := nil;
      Result := TSSLOperationResult.Err(sslErrConfiguration, E.Message);
    end;
  end;
end;

function TSSLContextBuilderImpl.TryBuildServer(out AContext: ISSLContext): TSSLOperationResult;
begin
  AContext := nil;

  try
    AContext := BuildServer;
    if AContext = nil then
    begin
      Result := TSSLOperationResult.Err(sslErrConfiguration, 'Failed to create SSL server context');
      Exit;
    end;

    Result := TSSLOperationResult.Ok;
  except
    on E: ESSLException do
    begin
      AContext := nil;
      Result := TSSLOperationResult.Err(sslErrConfiguration, 'SSL error: ' + E.Message);
    end;
    on E: Exception do
    begin
      AContext := nil;
      Result := TSSLOperationResult.Err(sslErrConfiguration, E.Message);
    end;
  end;
end;

{ Configuration Validation - Phase 2.1.2 }

function TSSLContextBuilderImpl.ValidateClient: TBuildValidationResult;
begin
  Result := TBuildValidationResult.Ok;

  // Check protocol versions
  if FProtocolVersions = [] then
    Result.AddWarning('No protocol versions specified, will use default');

  // Check for insecure protocols
  if sslProtocolSSL2 in FProtocolVersions then
    Result.AddError('SSL 2.0 is insecure and should not be used');
  if sslProtocolSSL3 in FProtocolVersions then
    Result.AddError('SSL 3.0 is insecure and should not be used');

  // Warn about old TLS versions
  if sslProtocolTLS10 in FProtocolVersions then
    Result.AddWarning('TLS 1.0 is deprecated and should be avoided');
  if sslProtocolTLS11 in FProtocolVersions then
    Result.AddWarning('TLS 1.1 is deprecated and should be avoided');

  // Check verification settings
  if sslVerifyNone in FVerifyMode then
    Result.AddWarning('Certificate verification is disabled - insecure for production');

  // Check CA configuration when verification is enabled
  if (sslVerifyPeer in FVerifyMode) and
    (FCAFile = '') and (FCAPath = '') and (not FUseSystemRoots) then
    Result.AddWarning('Peer verification enabled but no CA certificates configured');

  // Check cipher configuration
  if (FCipherList <> '') and (Pos('NULL-', UpperCase(FCipherList)) > 0) then
    Result.AddError('NULL cipher detected in cipher list - provides no encryption');
  if (FCipherList <> '') and (Pos('EXPORT', UpperCase(FCipherList)) > 0) and
    (Pos('!EXPORT', UpperCase(FCipherList)) = 0) then
    Result.AddWarning('EXPORT cipher detected - uses weak encryption');
  if (FCipherList <> '') and (Pos('RC4', UpperCase(FCipherList)) > 0) and
    (Pos('!RC4', UpperCase(FCipherList)) = 0) then
    Result.AddWarning('RC4 cipher detected - considered insecure');

  // Check session configuration
  if FSessionTimeout < 0 then
    Result.AddError('Session timeout cannot be negative');
  if FSessionTimeout > 86400 then  // 24 hours
    Result.AddWarning('Session timeout is very long (> 24 hours)');
end;

function TSSLContextBuilderImpl.ValidateServer: TBuildValidationResult;
begin
  Result := ValidateClient;  // Start with client validation

  // Server-specific validations

  // Check certificate configuration (REQUIRED for server)
  if (FCertificateFile = '') and (FCertificatePEM = '') then
    Result.AddError('Server context requires a certificate (use WithCertificate or WithCertificatePEM)');

  // Check private key configuration (REQUIRED for server)
  if (FPrivateKeyFile = '') and (FPrivateKeyPEM = '') and (FPKCS11URI = '') then
    Result.AddError('Server context requires a private key (use WithPrivateKey, WithPrivateKeyPEM, or UsePKCS11)');

  // Check if both file and PEM are set for certificate (potentially confusing)
  if (FCertificateFile <> '') and (FCertificatePEM <> '') then
    Result.AddWarning('Both certificate file and PEM are set - PEM will be used');

  // Check if both file and PEM are set for private key
  if (FPrivateKeyFile <> '') and (FPrivateKeyPEM <> '') then
    Result.AddWarning('Both private key file and PEM are set - PEM will be used');

  if FPKCS11URI <> '' then
  begin
    if FPrivateKeyFile <> '' then
      Result.AddWarning('PKCS#11 URI and private key file are both set - PKCS#11 will be used');
    if FPrivateKeyPEM <> '' then
      Result.AddWarning('PKCS#11 URI and private key PEM are both set - PKCS#11 will be used');
  end;

  // Warn if client verification is enabled without CA
  if (sslVerifyPeer in FVerifyMode) and
    (FCAFile = '') and (FCAPath = '') and (not FUseSystemRoots) then
    Result.AddWarning('Client verification enabled but no CA certificates configured');
end;

function TSSLContextBuilderImpl.Validate: TBuildValidationResult;
begin
  // Generic validation (works for both client and server)
  Result := ValidateClient;
end;

function TSSLContextBuilderImpl.BuildClientWithValidation(out AValidation: TBuildValidationResult): ISSLContext;
begin
  AValidation := ValidateClient;

  if not AValidation.IsValid then
    raise ESSLConfigurationException.Create(
      'Configuration validation failed: ' + AValidation.Errors[0]
    );

  Result := BuildClient;
end;

function TSSLContextBuilderImpl.BuildServerWithValidation(out AValidation: TBuildValidationResult): ISSLContext;
begin
  AValidation := ValidateServer;

  if not AValidation.IsValid then
    raise ESSLConfigurationException.Create(
      'Configuration validation failed: ' + AValidation.Errors[0]
    );

  Result := BuildServer;
end;

{ Configuration Import/Export - Phase 2.1.3 }

function TSSLContextBuilderImpl.ExportToJSON: string;
var
  LRoot: TJSONObject;
  LProtocols: TJSONArray;
  LVerify: TJSONArray;
  LOptions: TJSONArray;
  LProto: TSSLProtocolVersion;
  LVerifyMode: TSSLVerifyMode;
  LOption: TSSLOption;
begin
  LRoot := TJSONObject.Create;
  try
    // Protocol versions
    LProtocols := TJSONArray.Create;
    for LProto := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
      if LProto in FProtocolVersions then
        LProtocols.Add(Ord(LProto));
    LRoot.Add('protocols', LProtocols);

    // Verification mode
    LVerify := TJSONArray.Create;
    for LVerifyMode := Low(TSSLVerifyMode) to High(TSSLVerifyMode) do
      if LVerifyMode in FVerifyMode then
        LVerify.Add(Ord(LVerifyMode));
    LRoot.Add('verify_modes', LVerify);
    LRoot.Add('verify_depth', FVerifyDepth);

    // Certificate configuration
    LRoot.Add('certificate_file', FCertificateFile);
    LRoot.Add('certificate_pem', FCertificatePEM);
    LRoot.Add('private_key_file', FPrivateKeyFile);
    LRoot.Add('private_key_password', FPrivateKeyPassword);
    LRoot.Add('private_key_pem', FPrivateKeyPEM);
    LRoot.Add('ca_file', FCAFile);
    LRoot.Add('ca_path', FCAPath);
    LRoot.Add('use_system_roots', FUseSystemRoots);
    LRoot.Add('pkcs11_uri', FPKCS11URI);
    LRoot.Add('pkcs11_pin', FPKCS11PIN);
    LRoot.Add('pkcs11_pin_method', PKCS11PINMethodToText(FPKCS11PINMethod));

    // Cipher configuration
    LRoot.Add('cipher_list', FCipherList);
    LRoot.Add('tls13_ciphersuites', FTLS13Ciphersuites);

    // Advanced options
    LRoot.Add('server_name', FServerName);
    LRoot.Add('alpn_protocols', FALPNProtocols);
    LRoot.Add('session_cache_enabled', FSessionCacheEnabled);
    LRoot.Add('session_timeout', FSessionTimeout);

    AddBackendSelectionToJSON(
      LRoot,
      FAutoSelectBackend,
      FBackendRequirements,
      FExplicitBackend,
      FExplicitBackendSet);

    // Options
    LOptions := TJSONArray.Create;
    for LOption := Low(TSSLOption) to High(TSSLOption) do
      if LOption in FOptions then
        LOptions.Add(Ord(LOption));
    LRoot.Add('options', LOptions);
    
    // OCSP Stapling
    LRoot.Add('ocsp_stapling_enabled', FOCSPStaplingEnabled);
    LRoot.Add('ocsp_stapling_required', FOCSPStaplingRequired);

    Result := LRoot.FormatJSON;
  finally
    LRoot.Free;
  end;
end;

function TSSLContextBuilderImpl.ImportFromJSON(const AJSON: string): ISSLContextBuilder;
var
  LRoot: TJSONData;
  LProtocols, LVerify, LOptions: TJSONArray;
  LPKCS11PINMethod: TPKCS11PINMethod;
  LHasOptions: Boolean;
  LHasServerName: Boolean;
  LHasALPNProtocols: Boolean;
  LHasSessionCacheEnabled: Boolean;
  I: Integer;
begin
  Result := Self;

  if AJSON = '' then
    Exit;

  LRoot := GetJSON(AJSON);
  try
    if not (LRoot is TJSONObject) then
      Exit;

    with TJSONObject(LRoot) do
    begin
      // Protocol versions
      if IndexOfName('protocols') >= 0 then
      begin
        LProtocols := Arrays['protocols'];
        FProtocolVersions := [];
        for I := 0 to LProtocols.Count - 1 do
          Include(FProtocolVersions, TSSLProtocolVersion(LProtocols.Integers[I]));
      end;

      // Verification mode
      if IndexOfName('verify_modes') >= 0 then
      begin
        LVerify := Arrays['verify_modes'];
        FVerifyMode := [];
        for I := 0 to LVerify.Count - 1 do
          Include(FVerifyMode, TSSLVerifyMode(LVerify.Integers[I]));
      end;

      if IndexOfName('verify_depth') >= 0 then
        FVerifyDepth := Integers['verify_depth'];

      // Certificate configuration
      if IndexOfName('certificate_file') >= 0 then
        FCertificateFile := Strings['certificate_file'];
      if IndexOfName('certificate_pem') >= 0 then
        FCertificatePEM := Strings['certificate_pem'];
      if IndexOfName('private_key_file') >= 0 then
        FPrivateKeyFile := Strings['private_key_file'];
      if IndexOfName('private_key_password') >= 0 then
        FPrivateKeyPassword := Strings['private_key_password'];
      if IndexOfName('private_key_pem') >= 0 then
        FPrivateKeyPEM := Strings['private_key_pem'];
      if IndexOfName('ca_file') >= 0 then
        FCAFile := Strings['ca_file'];
      if IndexOfName('ca_path') >= 0 then
        FCAPath := Strings['ca_path'];
      if IndexOfName('use_system_roots') >= 0 then
        FUseSystemRoots := Booleans['use_system_roots'];
      if IndexOfName('pkcs11_uri') >= 0 then
        FPKCS11URI := Strings['pkcs11_uri'];
      if IndexOfName('pkcs11_pin') >= 0 then
        FPKCS11PIN := Strings['pkcs11_pin'];
      if (IndexOfName('pkcs11_pin_method') >= 0) and
        TryParsePKCS11PINMethodText(Strings['pkcs11_pin_method'], LPKCS11PINMethod) then
        FPKCS11PINMethod := LPKCS11PINMethod;

      // Cipher configuration
      if IndexOfName('cipher_list') >= 0 then
        FCipherList := Strings['cipher_list'];
      if IndexOfName('tls13_ciphersuites') >= 0 then
        FTLS13Ciphersuites := Strings['tls13_ciphersuites'];

      // Advanced options
      LHasServerName := IndexOfName('server_name') >= 0;
      if LHasServerName then
        FServerName := Strings['server_name'];
      LHasALPNProtocols := IndexOfName('alpn_protocols') >= 0;
      if LHasALPNProtocols then
        FALPNProtocols := Strings['alpn_protocols'];
      LHasSessionCacheEnabled := IndexOfName('session_cache_enabled') >= 0;
      if LHasSessionCacheEnabled then
        FSessionCacheEnabled := Booleans['session_cache_enabled'];
      if IndexOfName('session_timeout') >= 0 then
        FSessionTimeout := Integers['session_timeout'];

      LoadBackendSelectionFromJSONObject(
        TJSONObject(LRoot),
        FAutoSelectBackend,
        FBackendRequirements,
        FExplicitBackend,
        FExplicitBackendSet);

      // Options
      LHasOptions := IndexOfName('options') >= 0;
      if LHasOptions then
      begin
        LOptions := Arrays['options'];
        FOptions := [];
        for I := 0 to LOptions.Count - 1 do
          Include(FOptions, TSSLOption(LOptions.Integers[I]));
      end
      else
        SyncAdvancedOptionCoupledFields(
          LHasServerName, LHasALPNProtocols, LHasSessionCacheEnabled, True);
      
      // OCSP Stapling
      if IndexOfName('ocsp_stapling_enabled') >= 0 then
        FOCSPStaplingEnabled := Booleans['ocsp_stapling_enabled'];
      if IndexOfName('ocsp_stapling_required') >= 0 then
        FOCSPStaplingRequired := Booleans['ocsp_stapling_required'];

      NormalizeBackendSelectionState(
        FAutoSelectBackend,
        FBackendRequirements,
        FExplicitBackend,
        FExplicitBackendSet);
      SyncOCSPStaplingOptions;
    end;
  finally
    LRoot.Free;
  end;
end;

function TSSLContextBuilderImpl.ExportToINI: string;
var
  LLines: TStringList;
  LProto: TSSLProtocolVersion;
  LVerifyMode: TSSLVerifyMode;
  LOption: TSSLOption;
  LProtocolStr, LVerifyStr, LOptionsStr: string;
begin
  LLines := TStringList.Create;
  try
    LLines.Add('[SSL Context Configuration]');
    LLines.Add('');

    // Protocol versions
    LProtocolStr := '';
    for LProto := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
      if LProto in FProtocolVersions then
      begin
        if LProtocolStr <> '' then
          LProtocolStr := LProtocolStr + ',';
        LProtocolStr := LProtocolStr + IntToStr(Ord(LProto));
      end;
    LLines.Add('protocols=' + LProtocolStr);

    // Verification
    LVerifyStr := '';
    for LVerifyMode := Low(TSSLVerifyMode) to High(TSSLVerifyMode) do
      if LVerifyMode in FVerifyMode then
      begin
        if LVerifyStr <> '' then
          LVerifyStr := LVerifyStr + ',';
        LVerifyStr := LVerifyStr + IntToStr(Ord(LVerifyMode));
      end;
    LLines.Add('verify_modes=' + LVerifyStr);
    LLines.Add('verify_depth=' + IntToStr(FVerifyDepth));
    LLines.Add('');

    // Certificate configuration
    LLines.Add('[Certificates]');
    LLines.Add('certificate_file=' + FCertificateFile);
    LLines.Add('private_key_file=' + FPrivateKeyFile);
    LLines.Add('private_key_password=' + FPrivateKeyPassword);
    LLines.Add('ca_file=' + FCAFile);
    LLines.Add('ca_path=' + FCAPath);
    if FUseSystemRoots then
      LLines.Add('use_system_roots=true')
    else
      LLines.Add('use_system_roots=false');
    LLines.Add('');

    // PKCS#11 configuration
    LLines.Add('[PKCS11]');
    LLines.Add('pkcs11_uri=' + FPKCS11URI);
    LLines.Add('pkcs11_pin=' + FPKCS11PIN);
    LLines.Add('pkcs11_pin_method=' + PKCS11PINMethodToText(FPKCS11PINMethod));
    LLines.Add('');

    // Cipher configuration
    LLines.Add('[Ciphers]');
    LLines.Add('cipher_list=' + FCipherList);
    LLines.Add('tls13_ciphersuites=' + FTLS13Ciphersuites);
    LLines.Add('');

    // Advanced options
    LLines.Add('[Advanced]');
    LLines.Add('server_name=' + FServerName);
    LLines.Add('alpn_protocols=' + FALPNProtocols);
    if FSessionCacheEnabled then
      LLines.Add('session_cache_enabled=true')
    else
      LLines.Add('session_cache_enabled=false');
    LLines.Add('session_timeout=' + IntToStr(FSessionTimeout));
    LLines.Add('');

    AddBackendSelectionToINI(
      LLines,
      FAutoSelectBackend,
      FBackendRequirements,
      FExplicitBackend,
      FExplicitBackendSet);

    // Options
    LOptionsStr := '';
    for LOption := Low(TSSLOption) to High(TSSLOption) do
      if LOption in FOptions then
      begin
        if LOptionsStr <> '' then
          LOptionsStr := LOptionsStr + ',';
        LOptionsStr := LOptionsStr + IntToStr(Ord(LOption));
      end;
    LLines.Add('[Options]');
    LLines.Add('options=' + LOptionsStr);
    LLines.Add('');
    
    // OCSP Stapling
    LLines.Add('[OCSP Stapling]');
    if FOCSPStaplingEnabled then
      LLines.Add('ocsp_stapling_enabled=true')
    else
      LLines.Add('ocsp_stapling_enabled=false');
    if FOCSPStaplingRequired then
      LLines.Add('ocsp_stapling_required=true')
    else
      LLines.Add('ocsp_stapling_required=false');

    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function TSSLContextBuilderImpl.ImportFromINI(const AINI: string): ISSLContextBuilder;
var
  LLines: TStringList;
  I: Integer;
  LLine, LKey, LValue: string;
  LPos: Integer;
  LParts: TStringList;
  LPKCS11PINMethod: TPKCS11PINMethod;
  LHasOptions: Boolean;
  LHasServerName: Boolean;
  LHasALPNProtocols: Boolean;
  LHasSessionCacheEnabled: Boolean;
  J: Integer;
begin
  Result := Self;

  if AINI = '' then
    Exit;

  LHasOptions := False;
  LHasServerName := False;
  LHasALPNProtocols := False;
  LHasSessionCacheEnabled := False;

  LLines := TStringList.Create;
  LParts := TStringList.Create;
  try
    LLines.Text := AINI;

    for I := 0 to LLines.Count - 1 do
    begin
      LLine := Trim(LLines[I]);

      // Skip empty lines and section headers
      if (LLine = '') or (LLine[1] = '[') then
        Continue;

      // Parse key=value
      LPos := Pos('=', LLine);
      if LPos > 0 then
      begin
        LKey := Trim(Copy(LLine, 1, LPos - 1));
        LValue := Trim(Copy(LLine, LPos + 1, Length(LLine)));

        // Parse based on key
        if LKey = 'protocols' then
        begin
          LParts.CommaText := LValue;
          FProtocolVersions := [];
          for J := 0 to LParts.Count - 1 do
            Include(FProtocolVersions, TSSLProtocolVersion(StrToIntDef(LParts[J], 0)));
        end
        else if LKey = 'verify_modes' then
        begin
          LParts.CommaText := LValue;
          FVerifyMode := [];
          for J := 0 to LParts.Count - 1 do
            Include(FVerifyMode, TSSLVerifyMode(StrToIntDef(LParts[J], 0)));
        end
        else if LKey = 'verify_depth' then
          FVerifyDepth := StrToIntDef(LValue, SSL_DEFAULT_VERIFY_DEPTH)
        else if LKey = 'certificate_file' then
          FCertificateFile := LValue
        else if LKey = 'private_key_file' then
          FPrivateKeyFile := LValue
        else if LKey = 'private_key_password' then
          FPrivateKeyPassword := LValue
        else if LKey = 'ca_file' then
          FCAFile := LValue
        else if LKey = 'ca_path' then
          FCAPath := LValue
        else if LKey = 'use_system_roots' then
          FUseSystemRoots := (LowerCase(LValue) = 'true')
        else if LKey = 'pkcs11_uri' then
          FPKCS11URI := LValue
        else if LKey = 'pkcs11_pin' then
          FPKCS11PIN := LValue
        else if (LKey = 'pkcs11_pin_method') and
          TryParsePKCS11PINMethodText(LValue, LPKCS11PINMethod) then
          FPKCS11PINMethod := LPKCS11PINMethod
        else if LKey = 'cipher_list' then
          FCipherList := LValue
        else if LKey = 'tls13_ciphersuites' then
          FTLS13Ciphersuites := LValue
        else if ApplyBackendSelectionINIKey(
          LKey, LValue, LParts,
          FAutoSelectBackend,
          FBackendRequirements,
          FExplicitBackend,
          FExplicitBackendSet) then
        begin
        end
        else if LKey = 'server_name' then
        begin
          FServerName := LValue;
          LHasServerName := True;
        end
        else if LKey = 'alpn_protocols' then
        begin
          FALPNProtocols := LValue;
          LHasALPNProtocols := True;
        end
        else if LKey = 'session_cache_enabled' then
        begin
          FSessionCacheEnabled := (LowerCase(LValue) = 'true');
          LHasSessionCacheEnabled := True;
        end
        else if LKey = 'session_timeout' then
          FSessionTimeout := StrToIntDef(LValue, SSL_DEFAULT_SESSION_TIMEOUT)
        else if LKey = 'options' then
        begin
          LHasOptions := True;
          LParts.CommaText := LValue;
          FOptions := [];
          for J := 0 to LParts.Count - 1 do
            Include(FOptions, TSSLOption(StrToIntDef(LParts[J], 0)));
        end
        else if LKey = 'ocsp_stapling_enabled' then
          FOCSPStaplingEnabled := (LowerCase(LValue) = 'true')
        else if LKey = 'ocsp_stapling_required' then
          FOCSPStaplingRequired := (LowerCase(LValue) = 'true');
      end;
    end;

    if not LHasOptions then
      SyncAdvancedOptionCoupledFields(
        LHasServerName, LHasALPNProtocols, LHasSessionCacheEnabled, True);

    NormalizeBackendSelectionState(
      FAutoSelectBackend,
      FBackendRequirements,
      FExplicitBackend,
      FExplicitBackendSet);
    SyncOCSPStaplingOptions;
  finally
    LParts.Free;
    LLines.Free;
  end;
end;

{ Configuration Snapshot and Clone - Phase 2.1.4 }

function TSSLContextBuilderImpl.Clone: ISSLContextBuilder;
var
  LClone: TSSLContextBuilderImpl;
begin
  // Create new instance and copy all fields
  LClone := TSSLContextBuilderImpl.Create;

  // Copy all configuration fields
  LClone.FProtocolVersions := FProtocolVersions;
  LClone.FVerifyMode := FVerifyMode;
  LClone.FVerifyDepth := FVerifyDepth;
  LClone.FCertificateFile := FCertificateFile;
  LClone.FCertificatePEM := FCertificatePEM;
  LClone.FPrivateKeyFile := FPrivateKeyFile;
  LClone.FPrivateKeyPassword := FPrivateKeyPassword;
  LClone.FPrivateKeyPEM := FPrivateKeyPEM;
  LClone.FCAFile := FCAFile;
  LClone.FCAPath := FCAPath;
  LClone.FUseSystemRoots := FUseSystemRoots;
  LClone.FCipherList := FCipherList;
  LClone.FTLS13Ciphersuites := FTLS13Ciphersuites;
  LClone.FServerName := FServerName;
  LClone.FALPNProtocols := FALPNProtocols;
  LClone.FSessionCacheEnabled := FSessionCacheEnabled;
  LClone.FSessionTimeout := FSessionTimeout;
  LClone.FOptions := FOptions;
  
  // Copy PKCS#11 fields
  LClone.FPKCS11URI := FPKCS11URI;
  LClone.FPKCS11PIN := FPKCS11PIN;
  LClone.FPKCS11PINMethod := FPKCS11PINMethod;
  
  // Copy OCSP Stapling fields
  LClone.FOCSPStaplingEnabled := FOCSPStaplingEnabled;
  LClone.FOCSPStaplingRequired := FOCSPStaplingRequired;

  // Copy backend-selection fields
  LClone.FAutoSelectBackend := FAutoSelectBackend;
  LClone.FBackendRequirements := FBackendRequirements;
  LClone.FExplicitBackend := FExplicitBackend;
  LClone.FExplicitBackendSet := FExplicitBackendSet;
  LClone.SyncOCSPStaplingOptions;

  Result := LClone;
end;

function TSSLContextBuilderImpl.Reset: ISSLContextBuilder;
begin
  // Reset all fields to default values (same as constructor)
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FCertificateFile := '';
  FCertificatePEM := '';
  FPrivateKeyFile := '';
  FPrivateKeyPassword := '';
  FPrivateKeyPEM := '';
  FCAFile := '';
  FCAPath := '';
  FUseSystemRoots := False;
  FCipherList := '';
  FTLS13Ciphersuites := '';
  FServerName := '';
  FALPNProtocols := '';
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FOptions := [ssoEnableSNI, ssoDisableCompression, ssoDisableRenegotiation];
  
  // Reset PKCS#11 fields
  FPKCS11URI := '';
  FPKCS11PIN := '';
  FPKCS11PINMethod := pmNone;
  
  // Reset OCSP Stapling fields
  FOCSPStaplingEnabled := False;
  FOCSPStaplingRequired := False;

  // Reset backend-selection fields
  FAutoSelectBackend := False;
  FillChar(FBackendRequirements, SizeOf(FBackendRequirements), 0);
  FExplicitBackend := sslOpenSSL;
  FExplicitBackendSet := False;
  SyncOCSPStaplingOptions;

  Result := Self;
end;

function TSSLContextBuilderImpl.ResetToDefaults: ISSLContextBuilder;
begin
  // Alias for Reset
  Result := Reset;
end;

function TSSLContextBuilderImpl.Merge(ASource: ISSLContextBuilder): ISSLContextBuilder;
var
  LSourceJSON: string;
  LData: TJSONData;
  LObj: TJSONObject;
  LProtocols, LVerify, LOptions: TJSONArray;
  LPKCS11PINMethod: TPKCS11PINMethod;
  I: Integer;
begin
  Result := Self;

  if ASource = nil then
    Exit;

  // Export source to JSON and merge non-empty fields
  LSourceJSON := ASource.ExportToJSON;
  if LSourceJSON = '' then
    Exit;

  LData := GetJSON(LSourceJSON);
  try
    if not (LData is TJSONObject) then
      Exit;

    LObj := TJSONObject(LData);

    // Merge protocols if specified
    if LObj.IndexOfName('protocols') >= 0 then
    begin
      LProtocols := LObj.Arrays['protocols'];
      if LProtocols.Count > 0 then
      begin
        FProtocolVersions := [];
        for I := 0 to LProtocols.Count - 1 do
          Include(FProtocolVersions, TSSLProtocolVersion(LProtocols.Integers[I]));
      end;
    end;

    // Merge verify modes if specified
    if LObj.IndexOfName('verify_modes') >= 0 then
    begin
      LVerify := LObj.Arrays['verify_modes'];
      if LVerify.Count > 0 then
      begin
        FVerifyMode := [];
        for I := 0 to LVerify.Count - 1 do
          Include(FVerifyMode, TSSLVerifyMode(LVerify.Integers[I]));
      end;
    end;

    // Merge other fields if non-empty
    if LObj.IndexOfName('verify_depth') >= 0 then
      FVerifyDepth := LObj.Integers['verify_depth'];

    if LObj.IndexOfName('certificate_file') >= 0 then
      FCertificateFile := LObj.Strings['certificate_file'];

    if LObj.IndexOfName('certificate_pem') >= 0 then
      FCertificatePEM := LObj.Strings['certificate_pem'];

    if LObj.IndexOfName('private_key_file') >= 0 then
      FPrivateKeyFile := LObj.Strings['private_key_file'];

    if LObj.IndexOfName('private_key_password') >= 0 then
      FPrivateKeyPassword := LObj.Strings['private_key_password'];

    if LObj.IndexOfName('private_key_pem') >= 0 then
      FPrivateKeyPEM := LObj.Strings['private_key_pem'];

    if LObj.IndexOfName('ca_file') >= 0 then
      FCAFile := LObj.Strings['ca_file'];

    if LObj.IndexOfName('ca_path') >= 0 then
      FCAPath := LObj.Strings['ca_path'];

    if LObj.IndexOfName('use_system_roots') >= 0 then
      FUseSystemRoots := LObj.Booleans['use_system_roots'];

    if LObj.IndexOfName('pkcs11_uri') >= 0 then
      FPKCS11URI := LObj.Strings['pkcs11_uri'];

    if LObj.IndexOfName('pkcs11_pin') >= 0 then
      FPKCS11PIN := LObj.Strings['pkcs11_pin'];

    if (LObj.IndexOfName('pkcs11_pin_method') >= 0) and
      TryParsePKCS11PINMethodText(LObj.Strings['pkcs11_pin_method'], LPKCS11PINMethod) then
      FPKCS11PINMethod := LPKCS11PINMethod;

    if LObj.IndexOfName('cipher_list') >= 0 then
      FCipherList := LObj.Strings['cipher_list'];

    if LObj.IndexOfName('tls13_ciphersuites') >= 0 then
      FTLS13Ciphersuites := LObj.Strings['tls13_ciphersuites'];

    if LObj.IndexOfName('server_name') >= 0 then
      FServerName := LObj.Strings['server_name'];

    if LObj.IndexOfName('alpn_protocols') >= 0 then
      FALPNProtocols := LObj.Strings['alpn_protocols'];

    if LObj.IndexOfName('session_cache_enabled') >= 0 then
      FSessionCacheEnabled := LObj.Booleans['session_cache_enabled'];

    if LObj.IndexOfName('session_timeout') >= 0 then
      FSessionTimeout := LObj.Integers['session_timeout'];

    LoadBackendSelectionFromJSONObject(
      LObj,
      FAutoSelectBackend,
      FBackendRequirements,
      FExplicitBackend,
      FExplicitBackendSet);

    if LObj.IndexOfName('ocsp_stapling_enabled') >= 0 then
      FOCSPStaplingEnabled := LObj.Booleans['ocsp_stapling_enabled'];

    if LObj.IndexOfName('ocsp_stapling_required') >= 0 then
      FOCSPStaplingRequired := LObj.Booleans['ocsp_stapling_required'];

    // Merge options
    if LObj.IndexOfName('options') >= 0 then
    begin
      LOptions := LObj.Arrays['options'];
      FOptions := [];
      for I := 0 to LOptions.Count - 1 do
        Include(FOptions, TSSLOption(LOptions.Integers[I]));
    end;
  finally
    LData.Free;
  end;
end;

{ Conditional Configuration - Phase 2.2.1 }

function TSSLContextBuilderImpl.When(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
begin
  Result := Self;

  if not ACondition then
    Exit;

  if Assigned(AConfig) then
    AConfig(Self);
end;

function TSSLContextBuilderImpl.Unless(ACondition: Boolean; AConfig: TBuilderConfigProc): ISSLContextBuilder;
begin
  Result := Self;

  if ACondition then
    Exit;

  if Assigned(AConfig) then
    AConfig(Self);
end;

function TSSLContextBuilderImpl.WhenDevelopment(AConfig: TBuilderConfigProc): ISSLContextBuilder;
begin
  {$IFDEF DEBUG}
  Result := When(True, AConfig);
  {$ELSE}
  Result := Self;
  {$ENDIF}
end;

function TSSLContextBuilderImpl.WhenProduction(AConfig: TBuilderConfigProc): ISSLContextBuilder;
begin
  {$IFNDEF DEBUG}
  Result := When(True, AConfig);
  {$ELSE}
  Result := Self;
  {$ENDIF}
end;

{ Batch Configuration - Phase 2.2.2 }

function TSSLContextBuilderImpl.Apply(AConfig: TBuilderConfigProc): ISSLContextBuilder;
begin
  Result := Self;

  if Assigned(AConfig) then
    AConfig(Self);
end;

function TSSLContextBuilderImpl.ApplyPreset(APreset: ISSLContextBuilder): ISSLContextBuilder;
begin
  Result := Self;

  if APreset = nil then
    Exit;

  // Merge the preset configuration into current builder
  Merge(APreset);
end;

function TSSLContextBuilderImpl.Pipe(ATransform: TBuilderConfigProc): ISSLContextBuilder;
begin
  // Pipe is an alias for Apply - functional programming style
  Result := Apply(ATransform);
end;

{ Convenience Methods - Phase 2.2.3 }

function TSSLContextBuilderImpl.WithCertificateChain(const ACerts: array of string): ISSLContextBuilder;
var
  I: Integer;
begin
  Result := Self;

  // Load all certificates in the chain
  // The first certificate is typically the end-entity certificate
  // Followed by intermediate certificates up to the root
  for I := Low(ACerts) to High(ACerts) do
  begin
    if I = Low(ACerts) then
      // First cert is the primary certificate
      FCertificatePEM := ACerts[I]
    else
      // Additional certs are part of the chain
      // Note: Current implementation only stores one cert
      // In a full implementation, we'd store the chain separately
      FCertificatePEM := FCertificatePEM + #10 + ACerts[I];
  end;
end;

function TSSLContextBuilderImpl.WithMutualTLS(const ACAFile: string; ARequired: Boolean): ISSLContextBuilder;
begin
  Result := Self;

  // Enable client certificate verification
  FVerifyMode := [sslVerifyPeer];

  if ARequired then
    // Fail if client doesn't provide certificate
    Include(FVerifyMode, sslVerifyFailIfNoPeerCert);

  // Set CA file for verifying client certificates
  FCAFile := ACAFile;
end;

function TSSLContextBuilderImpl.WithHTTP2: ISSLContextBuilder;
begin
  Result := Self;

  // Configure ALPN for HTTP/2
  // Include both h2 and http/1.1 for compatibility
  FALPNProtocols := 'h2,http/1.1';
  Include(FOptions, ssoEnableALPN);
end;

function TSSLContextBuilderImpl.WithModernDefaults: ISSLContextBuilder;
begin
  // Modern defaults focus on security and performance
  Result := Self;

  // Only modern TLS versions
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];

  // Strong cipher suites
  FCipherList := 'ECDHE+AESGCM:ECDHE+CHACHA20:DHE+AESGCM';
  FTLS13Ciphersuites := 'TLS_AES_256_GCM_SHA384:TLS_CHACHA20_POLY1305_SHA256:TLS_AES_128_GCM_SHA256';

  // Modern security options
  FOptions := [
    ssoEnableSNI,                // SNI support
    ssoDisableCompression,       // Prevent CRIME attack
    ssoDisableRenegotiation,     // Prevent renegotiation attacks
    ssoCipherServerPreference,   // Server chooses cipher
    ssoNoSSLv2,                  // Disable all old protocols
    ssoNoSSLv3,
    ssoNoTLSv1,
    ssoNoTLSv1_1,
    ssoEnableSessionTickets,     // Session resumption
    ssoEnableALPN                // ALPN support
  ];

  // Reasonable session settings
  FSessionCacheEnabled := True;
  FSessionTimeout := 7200;  // 2 hours

  // Strict verification by default
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := 10;
end;

{ Configuration Transformation - Phase 2.2.4 }

function TSSLContextBuilderImpl.Transform(ATransform: TBuilderTransformFunc): ISSLContextBuilder;
begin
  Result := Self;

  if not Assigned(ATransform) then
    Exit;

  // Apply transformation and return the result
  Result := ATransform(Self);
end;

function TSSLContextBuilderImpl.Extend(const AOptions: array of TSSLOption): ISSLContextBuilder;
var
  I: Integer;
begin
  Result := Self;

  // Add all options to the current option set
  for I := Low(AOptions) to High(AOptions) do
    Include(FOptions, AOptions[I]);

  SyncOCSPStaplingOptions;
end;

function TSSLContextBuilderImpl.Override(const AField, AValue: string): ISSLContextBuilder;
var
  LFieldLower: string;
  LPKCS11PINMethod: TPKCS11PINMethod;
  LSyncOCSP: Boolean;
begin
  Result := Self;

  LFieldLower := LowerCase(AField);
  LSyncOCSP := False;

  // Override specific configuration fields based on field name
  if LFieldLower = 'cipher_list' then
    FCipherList := AValue
  else if LFieldLower = 'tls13_ciphersuites' then
    FTLS13Ciphersuites := AValue
  else if LFieldLower = 'server_name' then
  begin
    FServerName := AValue;
    SyncAdvancedOptionCoupledFields(True, False, False, False);
  end
  else if LFieldLower = 'alpn_protocols' then
  begin
    FALPNProtocols := AValue;
    SyncAdvancedOptionCoupledFields(False, True, False, False);
  end
  else if LFieldLower = 'ca_file' then
    FCAFile := AValue
  else if LFieldLower = 'ca_path' then
    FCAPath := AValue
  else if LFieldLower = 'use_system_roots' then
    FUseSystemRoots := (LowerCase(AValue) = 'true')
  else if LFieldLower = 'cert_verify_cache' then
  begin
    if LowerCase(AValue) = 'true' then
      Include(FOptions, ssoEnableCertVerifyCache)
    else
      Exclude(FOptions, ssoEnableCertVerifyCache);
  end
  else if LFieldLower = 'cert_verify_cache_skip_valid_hit_refresh' then
  begin
    if LowerCase(AValue) = 'true' then
      Include(FOptions, ssoSkipCertVerifyCacheValidHitRefresh)
    else
      Exclude(FOptions, ssoSkipCertVerifyCacheValidHitRefresh);
  end
  else if LFieldLower = 'ocsp_stapling_enabled' then
  begin
    FOCSPStaplingEnabled := (LowerCase(AValue) = 'true');
    if FOCSPStaplingEnabled then
      Include(FOptions, ssoEnableOCSPStapling)
    else
      Exclude(FOptions, ssoEnableOCSPStapling);
    LSyncOCSP := True;
  end
  else if LFieldLower = 'ocsp_stapling_required' then
  begin
    FOCSPStaplingRequired := (LowerCase(AValue) = 'true');
    if FOCSPStaplingRequired then
      Include(FOptions, ssoRequireOCSPStapling)
    else
      Exclude(FOptions, ssoRequireOCSPStapling);
    LSyncOCSP := True;
  end
  else if LFieldLower = 'pkcs11_uri' then
    FPKCS11URI := AValue
  else if LFieldLower = 'pkcs11_pin' then
  begin
    FPKCS11PIN := AValue;
    FPKCS11PINMethod := pmValue;
  end
  else if LFieldLower = 'pkcs11_pin_method' then
  begin
    if TryParsePKCS11PINMethodText(AValue, LPKCS11PINMethod) then
      FPKCS11PINMethod := LPKCS11PINMethod;
  end
  else if LFieldLower = 'certificate_file' then
  begin
    FCertificateFile := AValue;
    if AValue <> '' then
      FCertificatePEM := '';
  end
  else if LFieldLower = 'certificate_pem' then
  begin
    FCertificatePEM := AValue;
    if AValue <> '' then
      FCertificateFile := '';
  end
  else if LFieldLower = 'private_key_file' then
  begin
    FPrivateKeyFile := AValue;
    if AValue <> '' then
      FPrivateKeyPEM := '';
  end
  else if LFieldLower = 'private_key_pem' then
  begin
    FPrivateKeyPEM := AValue;
    if AValue <> '' then
      FPrivateKeyFile := '';
  end
  else if LFieldLower = 'private_key_password' then
    FPrivateKeyPassword := AValue
  else if LFieldLower = 'session_timeout' then
    FSessionTimeout := StrToIntDef(AValue, FSessionTimeout)
  else if LFieldLower = 'verify_depth' then
    FVerifyDepth := StrToIntDef(AValue, FVerifyDepth)
  else if LFieldLower = 'session_cache_enabled' then
  begin
    FSessionCacheEnabled := (LowerCase(AValue) = 'true');
    SyncAdvancedOptionCoupledFields(False, False, True, False);
  end;

  if LSyncOCSP then
    SyncOCSPStaplingOptions;
  // If field not recognized, silently ignore (defensive programming)
end;

{ v1.3.0: Automatic backend selection methods }

function TSSLContextBuilderImpl.WithAutoBackendSelection(
  const ARequirements: TSSLRequirements): ISSLContextBuilder;
begin
  Result := Self;
  FAutoSelectBackend := True;
  FBackendRequirements := ARequirements;
  NormalizeBackendSelectionState(
    FAutoSelectBackend,
    FBackendRequirements,
    FExplicitBackend,
    FExplicitBackendSet);
end;

function TSSLContextBuilderImpl.WithSecurityFirst: ISSLContextBuilder;
begin
  Result := WithAutoBackendSelection(CreateSecurityFirstRequirements);
end;

function TSSLContextBuilderImpl.WithPerformanceFirst: ISSLContextBuilder;
begin
  Result := WithAutoBackendSelection(CreatePerformanceFirstRequirements);
end;

function TSSLContextBuilderImpl.WithCompatibilityFirst: ISSLContextBuilder;
begin
  Result := WithAutoBackendSelection(CreateCompatibilityFirstRequirements);
end;

function TSSLContextBuilderImpl.WithBackend(ABackendType: TSSLLibraryType): ISSLContextBuilder;
begin
  Result := Self;
  FExplicitBackend := ABackendType;
  FExplicitBackendSet := True;
  FAutoSelectBackend := False;  // 显式指定后端时禁用自动选择
  NormalizeBackendSelectionState(
    FAutoSelectBackend,
    FBackendRequirements,
    FExplicitBackend,
    FExplicitBackendSet);
end;

function TSSLContextBuilderImpl.RequireTLS13: ISSLContextBuilder;
begin
  Result := Self;
  if not FAutoSelectBackend then
  begin
    // 如果还没有启用自动选择，则创建默认需求
    FBackendRequirements := CreateDefaultRequirements;
    FAutoSelectBackend := True;
  end;
  // 添加 TLS 1.3 要求
  FBackendRequirements.RequiredProtocols := [sslProtocolTLS13];
  NormalizeBackendSelectionState(
    FAutoSelectBackend,
    FBackendRequirements,
    FExplicitBackend,
    FExplicitBackendSet);
end;

function TSSLContextBuilderImpl.RequireCipher(ACipher: TSSLCipher): ISSLContextBuilder;
begin
  Result := Self;
  if not FAutoSelectBackend then
  begin
    FBackendRequirements := CreateDefaultRequirements;
    FAutoSelectBackend := True;
  end;
  // 添加密码算法要求
  Include(FBackendRequirements.RequiredCiphers, ACipher);
  NormalizeBackendSelectionState(
    FAutoSelectBackend,
    FBackendRequirements,
    FExplicitBackend,
    FExplicitBackendSet);
end;

function TSSLContextBuilderImpl.RequirePKCS11Support: ISSLContextBuilder;
begin
  Result := Self;
  if not FAutoSelectBackend then
  begin
    FBackendRequirements := CreateDefaultRequirements;
    FAutoSelectBackend := True;
  end;
  // 要求 PKCS#11 支持
  FBackendRequirements.PlatformPreferences.RequirePKCS11 := True;
  NormalizeBackendSelectionState(
    FAutoSelectBackend,
    FBackendRequirements,
    FExplicitBackend,
    FExplicitBackendSet);
end;

function TSSLContextBuilderImpl.PreferOSNative: ISSLContextBuilder;
begin
  Result := Self;
  if not FAutoSelectBackend then
  begin
    FBackendRequirements := CreateDefaultRequirements;
    FAutoSelectBackend := True;
  end;
  // 优先 OS 原生实现
  FBackendRequirements.PlatformPreferences.PreferOSNative := True;
  NormalizeBackendSelectionState(
    FAutoSelectBackend,
    FBackendRequirements,
    FExplicitBackend,
    FExplicitBackendSet);
end;

end.
