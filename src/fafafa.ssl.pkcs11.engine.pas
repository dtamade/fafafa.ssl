unit fafafa.ssl.pkcs11.engine;

{******************************************************************************}
{                                                                              }
{  fafafa.ssl - PKCS#11 ENGINE Backend (OpenSSL 1.1.1)                        }
{                                                                              }
{  Purpose: Load PKCS#11 keys using OpenSSL 1.1.1 ENGINE API                  }
{                                                                              }
{  Architecture:                                                               }
{    - Uses ENGINE API for PKCS#11 integration                                }
{    - Loads pkcs11 engine dynamically                                        }
{    - Supports RFC 7512 pkcs11: URIs                                         }
{    - Fallback backend for OpenSSL 1.1.1                                     }
{                                                                              }
{  Requirements:                                                               }
{    - OpenSSL 1.1.1 or later (but < 3.0)                                     }
{    - pkcs11 engine (libp11)                                                 }
{    - PKCS#11 module (.so, .dll, .dylib)                                     }
{                                                                              }
{******************************************************************************}

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.pkcs11.types,
  fafafa.ssl.pkcs11.backend,
  fafafa.ssl.pkcs11.uri,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.engine;

type
  { TEngineBackend - OpenSSL 1.1.1 ENGINE-based PKCS#11 backend }
  TEngineBackend = class(TBasePKCS11Backend)
  private
    FEngine: PENGINE;
    FEngineLoaded: Boolean;
    
    { Load pkcs11 engine }
    procedure LoadEngine(const AModulePath: string);
    
    { Unload pkcs11 engine }
    procedure UnloadEngine;
    
    { Build ENGINE key ID from config }
    function BuildEngineKeyID(const AConfig: TPKCS11Config): string;
    
    { Load key using ENGINE API }
    function LoadKeyFromEngine(const AKeyID: string; const APIN: string): PEVP_PKEY;
  protected
    function FindToken(const AConfig: TPKCS11Config): CK_SLOT_ID; override;
    function FindKey(ASession: CK_SESSION_HANDLE; const AConfig: TPKCS11Config): CK_OBJECT_HANDLE; override;
  public
    constructor Create;
    destructor Destroy; override;
    
    { IPKCS11Backend interface }
    function LoadPrivateKey(const AConfig: TPKCS11Config): PEVP_PKEY; override;
    function LoadCertificate(const AConfig: TPKCS11Config): PX509; override;
    function IsAvailable: Boolean; override;
    function GetName: string; override;
    function GetVersion: string; override;
  end;

implementation

uses
  fafafa.ssl.openssl.api.x509;

{ TEngineBackend }

constructor TEngineBackend.Create;
begin
  inherited Create;
  FEngine := nil;
  FEngineLoaded := False;
end;

destructor TEngineBackend.Destroy;
begin
  UnloadEngine;
  inherited Destroy;
end;

procedure TEngineBackend.LoadEngine(const AModulePath: string);
var
  EngineID: AnsiString;
begin
  if FEngineLoaded then
    Exit;
  
  // Load dynamic engine
  EngineID := 'pkcs11';
  FEngine := ENGINE_by_id(PAnsiChar(EngineID));
  
  if FEngine = nil then
  begin
    // Try to load dynamic engine
    FEngine := ENGINE_by_id('dynamic');
    if FEngine = nil then
      raise EPKCS11Exception.Create(
        'Failed to load dynamic ENGINE',
        CKR_GENERAL_ERROR);
    
    // Set engine parameters
    if ENGINE_ctrl_cmd_string(FEngine, 'SO_PATH', PAnsiChar(AnsiString('/usr/lib/engines-1.1/pkcs11.so')), 0) = 0 then
      raise EPKCS11Exception.Create(
        'Failed to set ENGINE SO_PATH',
        CKR_GENERAL_ERROR);
    
    if ENGINE_ctrl_cmd_string(FEngine, 'ID', PAnsiChar(EngineID), 0) = 0 then
      raise EPKCS11Exception.Create(
        'Failed to set ENGINE ID',
        CKR_GENERAL_ERROR);
    
    if ENGINE_ctrl_cmd_string(FEngine, 'LOAD', nil, 0) = 0 then
      raise EPKCS11Exception.Create(
        'Failed to LOAD ENGINE',
        CKR_GENERAL_ERROR);
  end;
  
  // Set PKCS#11 module path
  if ENGINE_ctrl_cmd_string(FEngine, 'MODULE_PATH', PAnsiChar(AnsiString(AModulePath)), 0) = 0 then
    raise EPKCS11Exception.Create(
      'Failed to set ENGINE MODULE_PATH: ' + AModulePath,
      CKR_GENERAL_ERROR);
  
  // Initialize engine
  if ENGINE_init(FEngine) = 0 then
  begin
    ENGINE_free(FEngine);
    FEngine := nil;
    raise EPKCS11Exception.Create(
      'Failed to initialize ENGINE',
      CKR_GENERAL_ERROR);
  end;
  
  FEngineLoaded := True;
end;

procedure TEngineBackend.UnloadEngine;
begin
  if FEngineLoaded and (FEngine <> nil) then
  begin
    ENGINE_finish(FEngine);
    ENGINE_free(FEngine);
    FEngine := nil;
    FEngineLoaded := False;
  end;
end;

function TEngineBackend.BuildEngineKeyID(const AConfig: TPKCS11Config): string;
var
  URI: TPKCS11URI;
begin
  // Build RFC 7512 URI from config
  // ENGINE expects URI format for key identification
  FillChar(URI, SizeOf(URI), 0);
  
  URI.Token := AConfig.TokenLabel;
  URI.ObjectLabel := AConfig.KeyLabel;
  
  if AConfig.SlotID >= 0 then
    URI.SlotID := IntToStr(AConfig.SlotID);
  
  // Don't include module path in URI (already set in ENGINE)
  // Don't include PIN in URI (will be provided separately)
  
  Result := TPKCS11URIParser.Generate(URI);
end;

function TEngineBackend.LoadKeyFromEngine(const AKeyID: string; const APIN: string): PEVP_PKEY;
var
  KeyIDAnsi: AnsiString;
  PINAnsi: AnsiString;
  UIMethod: PUI_METHOD;
begin
  Result := nil;
  
  KeyIDAnsi := AnsiString(AKeyID);
  
  // Set PIN if provided
  if APIN <> '' then
  begin
    PINAnsi := AnsiString(APIN);
    // Set PIN via ENGINE control command
    if ENGINE_ctrl_cmd_string(FEngine, 'PIN', PAnsiChar(PINAnsi), 0) = 0 then
      raise EPKCS11Exception.Create(
        'Failed to set ENGINE PIN',
        CKR_PIN_INCORRECT);
  end;
  
  // Load private key from engine
  UIMethod := nil; // TODO: Implement UI method for PIN callback
  Result := ENGINE_load_private_key(FEngine, PAnsiChar(KeyIDAnsi), UIMethod, nil);
  
  if Result = nil then
    raise EPKCS11Exception.Create(
      'Failed to load private key from ENGINE with ID: ' + AKeyID,
      CKR_KEY_HANDLE_INVALID);
end;

function TEngineBackend.FindToken(const AConfig: TPKCS11Config): CK_SLOT_ID;
begin
  // Not used in ENGINE backend (ENGINE handles token selection)
  Result := 0;
end;

function TEngineBackend.FindKey(ASession: CK_SESSION_HANDLE; const AConfig: TPKCS11Config): CK_OBJECT_HANDLE;
begin
  // Not used in ENGINE backend (ENGINE handles key selection)
  Result := 0;
end;

function TEngineBackend.LoadPrivateKey(const AConfig: TPKCS11Config): PEVP_PKEY;
var
  KeyID: string;
  PIN: string;
begin
  // Validate configuration
  ValidateConfig(AConfig);
  
  // Load engine if not already loaded
  LoadEngine(AConfig.ModulePath);
  
  // Resolve PIN
  PIN := ResolvePIN(AConfig);
  
  // Build ENGINE key ID
  KeyID := BuildEngineKeyID(AConfig);
  
  // Load key from engine
  Result := LoadKeyFromEngine(KeyID, PIN);
end;

function TEngineBackend.LoadCertificate(const AConfig: TPKCS11Config): PX509;
var
  KeyID: string;
  PIN: string;
  KeyIDAnsi: AnsiString;
  PINAnsi: AnsiString;
  Cert: PX509;
begin
  Result := nil;
  
  // Validate configuration
  ValidateConfig(AConfig);
  
  // Load engine if not already loaded
  LoadEngine(AConfig.ModulePath);
  
  // Resolve PIN
  PIN := ResolvePIN(AConfig);
  
  // Build ENGINE key ID
  KeyID := BuildEngineKeyID(AConfig);
  KeyIDAnsi := AnsiString(KeyID);
  
  // Set PIN if provided
  if PIN <> '' then
  begin
    PINAnsi := AnsiString(PIN);
    if ENGINE_ctrl_cmd_string(FEngine, 'PIN', PAnsiChar(PINAnsi), 0) = 0 then
      raise EPKCS11Exception.Create(
        'Failed to set ENGINE PIN',
        CKR_PIN_INCORRECT);
  end;
  
  // Load certificate from engine
  // Note: ENGINE_load_certificate might not be available in all ENGINE implementations
  // This is a simplified implementation
  Cert := ENGINE_load_public_key(FEngine, PAnsiChar(KeyIDAnsi), nil, nil) as PX509;
  
  if Cert = nil then
    raise EPKCS11Exception.Create(
      'Failed to load certificate from ENGINE with ID: ' + KeyID,
      CKR_GENERAL_ERROR);
  
  Result := Cert;
end;

function TEngineBackend.IsAvailable: Boolean;
begin
  // Check if OpenSSL 1.1.1 ENGINE API is available
  Result := Assigned(ENGINE_by_id) and
            Assigned(ENGINE_init) and
            Assigned(ENGINE_finish) and
            Assigned(ENGINE_free) and
            Assigned(ENGINE_ctrl_cmd_string) and
            Assigned(ENGINE_load_private_key) and
            Assigned(ENGINE_load_public_key);
end;

function TEngineBackend.GetName: string;
begin
  Result := 'ENGINE (OpenSSL 1.1.1)';
end;

function TEngineBackend.GetVersion: string;
begin
  Result := '1.1.1+';
end;

end.
