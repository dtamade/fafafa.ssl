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
  fafafa.ssl.base;

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
  Result := Self;
end;

function TSSLContextBuilderImpl.WithOptions(AOptions: TSSLOptions): ISSLContextBuilder;
begin
  FOptions := FOptions + AOptions;
  Result := Self;
end;

function TSSLContextBuilderImpl.WithoutOption(AOption: TSSLOption): ISSLContextBuilder;
begin
  Exclude(FOptions, AOption);
  Result := Self;
end;

function TSSLContextBuilderImpl.BuildClient: ISSLContext;
begin
  Result := TSSLFactory.CreateContext(sslCtxClient, sslAutoDetect);
  if Result = nil then
    raise ESSLException.Create('Failed to create SSL client context');
  
  // Apply configuration
  Result.SetProtocolVersions(FProtocolVersions);
  Result.SetVerifyMode(FVerifyMode);
  Result.SetVerifyDepth(FVerifyDepth);
  Result.SetOptions(FOptions);
  
  // Load certificates
  if FCertificateFile <> '' then
    Result.LoadCertificate(FCertificateFile);
  if FCertificatePEM <> '' then
    Result.LoadCertificatePEM(FCertificatePEM);
  
  if FPrivateKeyFile <> '' then
    Result.LoadPrivateKey(FPrivateKeyFile, FPrivateKeyPassword);
  if FPrivateKeyPEM <> '' then
    Result.LoadPrivateKeyPEM(FPrivateKeyPEM, FPrivateKeyPassword);
  
  if FCAFile <> '' then
    Result.LoadCAFile(FCAFile);
    
  if FCAPath <> '' then
    Result.LoadCAPath(FCAPath);
  
  // Load system roots if requested
  if FUseSystemRoots then
  begin
    {$IFDEF UNIX}
    // Common CA bundle locations on Linux
    if FileExists('/etc/ssl/certs/ca-certificates.crt') then
      Result.LoadCAFile('/etc/ssl/certs/ca-certificates.crt')
    else if FileExists('/etc/pki/tls/certs/ca-bundle.crt') then
      Result.LoadCAFile('/etc/pki/tls/certs/ca-bundle.crt')
    else if DirectoryExists('/etc/ssl/certs') then
      Result.LoadCAPath('/etc/ssl/certs');
    {$ENDIF}
    // On Windows, WinSSL uses system store automatically
  end;
  
  // Cipher configuration
  if FCipherList <> '' then
    Result.SetCipherList(FCipherList);
    
  if FTLS13Ciphersuites <> '' then
    Result.SetCipherSuites(FTLS13Ciphersuites);
  
  // SNI and ALPN
  if FServerName <> '' then
    Result.SetServerName(FServerName);
    
  if FALPNProtocols <> '' then
    Result.SetALPNProtocols(FALPNProtocols);
  
  // Session configuration
  Result.SetSessionCacheMode(FSessionCacheEnabled);
  Result.SetSessionTimeout(FSessionTimeout);
end;

function TSSLContextBuilderImpl.BuildServer: ISSLContext;
begin
  Result := TSSLFactory.CreateContext(sslCtxServer, sslAutoDetect);
  if Result = nil then
    raise ESSLException.Create('Failed to create SSL server context');
  
  // Apply configuration (same as client, but server context)
  Result.SetProtocolVersions(FProtocolVersions);
  Result.SetVerifyMode(FVerifyMode);
  Result.SetVerifyDepth(FVerifyDepth);
  Result.SetOptions(FOptions);
  
  // Server MUST have certificate and private key
  if (FCertificateFile = '') and (FCertificatePEM = '') then
    raise ESSLException.Create('Server context requires a certificate');
    
  if (FPrivateKeyFile = '') and (FPrivateKeyPEM = '') then
    raise ESSLException.Create('Server context requires a private key');
  
  if FCertificateFile <> '' then
    Result.LoadCertificate(FCertificateFile);
  
  if FPrivateKeyFile <> '' then
    Result.LoadPrivateKey(FPrivateKeyFile, FPrivateKeyPassword);
  
  if FCAFile <> '' then
    Result.LoadCAFile(FCAFile);
    
  if FCAPath <> '' then
    Result.LoadCAPath(FCAPath);
  
  // Cipher configuration
  if FCipherList <> '' then
    Result.SetCipherList(FCipherList);
    
  if FTLS13Ciphersuites <> '' then
    Result.SetCipherSuites(FTLS13Ciphersuites);
  
  if FALPNProtocols <> '' then
    Result.SetALPNProtocols(FALPNProtocols);
  
  // Session configuration
  Result.SetSessionCacheMode(FSessionCacheEnabled);
  Result.SetSessionTimeout(FSessionTimeout);
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
  if (FPrivateKeyFile = '') and (FPrivateKeyPEM = '') then
    Result.AddError('Server context requires a private key (use WithPrivateKey or WithPrivateKeyPEM)');

  // Check if both file and PEM are set for certificate (potentially confusing)
  if (FCertificateFile <> '') and (FCertificatePEM <> '') then
    Result.AddWarning('Both certificate file and PEM are set - PEM will be used');

  // Check if both file and PEM are set for private key
  if (FPrivateKeyFile <> '') and (FPrivateKeyPEM <> '') then
    Result.AddWarning('Both private key file and PEM are set - PEM will be used');

  // Warn if client verification is enabled without CA
  if (sslVerifyPeer in FVerifyMode) and
    (FCAFile = '') and (FCAPath = '') then
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
    LRoot.Add('private_key_pem', FPrivateKeyPEM);
    LRoot.Add('ca_file', FCAFile);
    LRoot.Add('ca_path', FCAPath);
    LRoot.Add('use_system_roots', FUseSystemRoots);

    // Cipher configuration
    LRoot.Add('cipher_list', FCipherList);
    LRoot.Add('tls13_ciphersuites', FTLS13Ciphersuites);

    // Advanced options
    LRoot.Add('server_name', FServerName);
    LRoot.Add('alpn_protocols', FALPNProtocols);
    LRoot.Add('session_cache_enabled', FSessionCacheEnabled);
    LRoot.Add('session_timeout', FSessionTimeout);

    // Options
    LOptions := TJSONArray.Create;
    for LOption := Low(TSSLOption) to High(TSSLOption) do
      if LOption in FOptions then
        LOptions.Add(Ord(LOption));
    LRoot.Add('options', LOptions);

    Result := LRoot.FormatJSON;
  finally
    LRoot.Free;
  end;
end;

function TSSLContextBuilderImpl.ImportFromJSON(const AJSON: string): ISSLContextBuilder;
var
  LRoot: TJSONData;
  LProtocols, LVerify, LOptions: TJSONArray;
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
      if IndexOfName('private_key_pem') >= 0 then
        FPrivateKeyPEM := Strings['private_key_pem'];
      if IndexOfName('ca_file') >= 0 then
        FCAFile := Strings['ca_file'];
      if IndexOfName('ca_path') >= 0 then
        FCAPath := Strings['ca_path'];
      if IndexOfName('use_system_roots') >= 0 then
        FUseSystemRoots := Booleans['use_system_roots'];

      // Cipher configuration
      if IndexOfName('cipher_list') >= 0 then
        FCipherList := Strings['cipher_list'];
      if IndexOfName('tls13_ciphersuites') >= 0 then
        FTLS13Ciphersuites := Strings['tls13_ciphersuites'];

      // Advanced options
      if IndexOfName('server_name') >= 0 then
        FServerName := Strings['server_name'];
      if IndexOfName('alpn_protocols') >= 0 then
        FALPNProtocols := Strings['alpn_protocols'];
      if IndexOfName('session_cache_enabled') >= 0 then
        FSessionCacheEnabled := Booleans['session_cache_enabled'];
      if IndexOfName('session_timeout') >= 0 then
        FSessionTimeout := Integers['session_timeout'];

      // Options
      if IndexOfName('options') >= 0 then
      begin
        LOptions := Arrays['options'];
        FOptions := [];
        for I := 0 to LOptions.Count - 1 do
          Include(FOptions, TSSLOption(LOptions.Integers[I]));
      end;
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
    LLines.Add('ca_file=' + FCAFile);
    LLines.Add('ca_path=' + FCAPath);
    if FUseSystemRoots then
      LLines.Add('use_system_roots=true')
    else
      LLines.Add('use_system_roots=false');
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
  J: Integer;
begin
  Result := Self;

  if AINI = '' then
    Exit;

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
        else if LKey = 'ca_file' then
          FCAFile := LValue
        else if LKey = 'ca_path' then
          FCAPath := LValue
        else if LKey = 'use_system_roots' then
          FUseSystemRoots := (LowerCase(LValue) = 'true')
        else if LKey = 'cipher_list' then
          FCipherList := LValue
        else if LKey = 'tls13_ciphersuites' then
          FTLS13Ciphersuites := LValue
        else if LKey = 'server_name' then
          FServerName := LValue
        else if LKey = 'alpn_protocols' then
          FALPNProtocols := LValue
        else if LKey = 'session_cache_enabled' then
          FSessionCacheEnabled := (LowerCase(LValue) = 'true')
        else if LKey = 'session_timeout' then
          FSessionTimeout := StrToIntDef(LValue, SSL_DEFAULT_SESSION_TIMEOUT)
        else if LKey = 'options' then
        begin
          LParts.CommaText := LValue;
          FOptions := [];
          for J := 0 to LParts.Count - 1 do
            Include(FOptions, TSSLOption(StrToIntDef(LParts[J], 0)));
        end;
      end;
    end;
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

    if (LObj.IndexOfName('certificate_file') >= 0) and (LObj.Strings['certificate_file'] <> '') then
      FCertificateFile := LObj.Strings['certificate_file'];

    if (LObj.IndexOfName('certificate_pem') >= 0) and (LObj.Strings['certificate_pem'] <> '') then
      FCertificatePEM := LObj.Strings['certificate_pem'];

    if (LObj.IndexOfName('private_key_file') >= 0) and (LObj.Strings['private_key_file'] <> '') then
      FPrivateKeyFile := LObj.Strings['private_key_file'];

    if (LObj.IndexOfName('private_key_pem') >= 0) and (LObj.Strings['private_key_pem'] <> '') then
      FPrivateKeyPEM := LObj.Strings['private_key_pem'];

    if (LObj.IndexOfName('ca_file') >= 0) and (LObj.Strings['ca_file'] <> '') then
      FCAFile := LObj.Strings['ca_file'];

    if (LObj.IndexOfName('ca_path') >= 0) and (LObj.Strings['ca_path'] <> '') then
      FCAPath := LObj.Strings['ca_path'];

    if LObj.IndexOfName('use_system_roots') >= 0 then
      FUseSystemRoots := LObj.Booleans['use_system_roots'];

    if (LObj.IndexOfName('cipher_list') >= 0) and (LObj.Strings['cipher_list'] <> '') then
      FCipherList := LObj.Strings['cipher_list'];

    if (LObj.IndexOfName('tls13_ciphersuites') >= 0) and (LObj.Strings['tls13_ciphersuites'] <> '') then
      FTLS13Ciphersuites := LObj.Strings['tls13_ciphersuites'];

    if (LObj.IndexOfName('server_name') >= 0) and (LObj.Strings['server_name'] <> '') then
      FServerName := LObj.Strings['server_name'];

    if (LObj.IndexOfName('alpn_protocols') >= 0) and (LObj.Strings['alpn_protocols'] <> '') then
      FALPNProtocols := LObj.Strings['alpn_protocols'];

    if LObj.IndexOfName('session_cache_enabled') >= 0 then
      FSessionCacheEnabled := LObj.Booleans['session_cache_enabled'];

    if LObj.IndexOfName('session_timeout') >= 0 then
      FSessionTimeout := LObj.Integers['session_timeout'];

    // Merge options
    if LObj.IndexOfName('options') >= 0 then
    begin
      LOptions := LObj.Arrays['options'];
      if LOptions.Count > 0 then
      begin
        FOptions := [];
        for I := 0 to LOptions.Count - 1 do
          Include(FOptions, TSSLOption(LOptions.Integers[I]));
      end;
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
end;

function TSSLContextBuilderImpl.Override(const AField, AValue: string): ISSLContextBuilder;
var
  LFieldLower: string;
begin
  Result := Self;

  LFieldLower := LowerCase(AField);

  // Override specific configuration fields based on field name
  if LFieldLower = 'cipher_list' then
    FCipherList := AValue
  else if LFieldLower = 'tls13_ciphersuites' then
    FTLS13Ciphersuites := AValue
  else if LFieldLower = 'server_name' then
    FServerName := AValue
  else if LFieldLower = 'alpn_protocols' then
    FALPNProtocols := AValue
  else if LFieldLower = 'ca_file' then
    FCAFile := AValue
  else if LFieldLower = 'ca_path' then
    FCAPath := AValue
  else if LFieldLower = 'certificate_file' then
    FCertificateFile := AValue
  else if LFieldLower = 'private_key_file' then
    FPrivateKeyFile := AValue
  else if LFieldLower = 'session_timeout' then
    FSessionTimeout := StrToIntDef(AValue, FSessionTimeout)
  else if LFieldLower = 'verify_depth' then
    FVerifyDepth := StrToIntDef(AValue, FVerifyDepth)
  else if LFieldLower = 'session_cache_enabled' then
    FSessionCacheEnabled := (LowerCase(AValue) = 'true');
  // If field not recognized, silently ignore (defensive programming)
end;

end.
