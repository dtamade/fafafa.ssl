unit fafafa.ssl.openssl;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.asn1,
  fafafa.ssl.openssl.api.bn,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.obj,
  fafafa.ssl.openssl.api.crypto,
  fafafa.ssl.openssl.api.x509v3,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.ocsp,
  fafafa.ssl.openssl.lib;

type
  { TOpenSSLLibrary }
  TOpenSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    FInitialized: Boolean;
    FConfig: TSSLConfig;
    FStatistics: TSSLStatistics;
    FLogCallback: TSSLLogCallback;
  protected
    { ISSLLibrary implementation }
    function Initialize: Boolean;
    procedure Finalize;
    function IsInitialized: Boolean;
    function GetLibraryType: TSSLLibraryType;
    function GetVersionString: string;
    function GetVersion: string;  // 便捷方法
    function GetVersionNumber: Cardinal;
    function GetCompileFlags: string;
    function IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
    function IsCipherSupported(const aCipherName: string): Boolean;
    function IsFeatureSupported(const aFeatureName: string): Boolean;
    procedure SetDefaultConfig(const aConfig: TSSLConfig);
    function GetDefaultConfig: TSSLConfig;
    function GetLastError: Integer;
    function GetLastErrorString: string;
    procedure ClearError;
    function GetStatistics: TSSLStatistics;
    procedure ResetStatistics;
    procedure SetLogCallback(aCallback: TSSLLogCallback);
    procedure Log(aLevel: TSSLLogLevel; const aMessage: string);
    function CreateContext(aType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TOpenSSLContext }
  TOpenSSLContext = class(TInterfacedObject, ISSLContext)
  private
    FLibrary: TOpenSSLLibrary;
    FContextType: TSSLContextType;
    FSSLCtx: PSSL_CTX;
    FProtocolVersions: TSSLProtocolVersions;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FOptions: TSSLOptions;
    FServerName: string;
    FALPNProtocols: string;
    FALPNWireData: TBytes;
    FCipherList: string;
    FCipherSuites: string;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;
    FSystemCAsLoaded: Boolean;
    
    procedure EnsureSystemCAsLoaded;
    procedure SetupContext;
    procedure SetupSSLCore;
    procedure SetupSecurityDefaults;
    procedure SetupDefaultVerification;
  protected
    { ISSLContext implementation }
    function GetContextType: TSSLContextType;
    procedure SetProtocolVersions(aVersions: TSSLProtocolVersions);
    function GetProtocolVersions: TSSLProtocolVersions;
    procedure LoadCertificate(const aFileName: string); overload;
    procedure LoadCertificate(aStream: TStream); overload;
    procedure LoadCertificate(aCert: ISSLCertificate); overload;
    procedure LoadPrivateKey(const aFileName: string; const aPassword: string = ''); overload;
    procedure LoadPrivateKey(aStream: TStream; const aPassword: string = ''); overload;
    
    // PEM 字符串直接加载
    procedure LoadCertificatePEM(const aPEM: string);
    procedure LoadPrivateKeyPEM(const aPEM: string; const aPassword: string = '');
    procedure LoadCAFile(const aFileName: string);
    procedure LoadCAPath(const aPath: string);
    procedure SetCertificateStore(aStore: ISSLCertificateStore);
    procedure SetVerifyMode(aMode: TSSLVerifyModes);
    function GetVerifyMode: TSSLVerifyModes;
    procedure SetVerifyDepth(aDepth: Integer);
    function GetVerifyDepth: Integer;
    procedure SetVerifyCallback(aCallback: TSSLVerifyCallback);
    procedure SetCipherList(const aCipherList: string);
    function GetCipherList: string;
    procedure SetCipherSuites(const aCipherSuites: string);
    function GetCipherSuites: string;
    procedure SetSessionCacheMode(aEnabled: Boolean);
    function GetSessionCacheMode: Boolean;
    procedure SetSessionTimeout(aTimeout: Integer);
    function GetSessionTimeout: Integer;
    procedure SetSessionCacheSize(aSize: Integer);
    function GetSessionCacheSize: Integer;
    procedure SetOptions(const aOptions: TSSLOptions);
    function GetOptions: TSSLOptions;
    procedure SetServerName(const aServerName: string);
    function GetServerName: string;
    procedure SetALPNProtocols(const aProtocols: string);
    function GetALPNProtocols: string;
    procedure SetPasswordCallback(aCallback: TSSLPasswordCallback);
    procedure SetInfoCallback(aCallback: TSSLInfoCallback);
    function CreateConnection(aSocket: THandle): ISSLConnection; overload;
    function CreateConnection(aStream: TStream): ISSLConnection; overload;
    function IsValid: Boolean;
    function GetNativeHandle: Pointer;
  public
    constructor Create(ALibrary: TOpenSSLLibrary; AType: TSSLContextType);
    destructor Destroy; override;
  end;

  { TOpenSSLSession }
  TOpenSSLSession = class(TInterfacedObject, ISSLSession)
  private
    FSession: PSSL_SESSION;
    FOwned: Boolean;
    function EnsureSession: Boolean;
    class function SecondsSinceUnixEpochToDateTime(const ASeconds: LongInt): TDateTime; static;
    function FetchCipher: PSSL_CIPHER;
  protected
    { ISSLSession implementation }
    function GetID: string;
    function GetCreationTime: TDateTime;
    function GetTimeout: Integer;
    procedure SetTimeout(aTimeout: Integer);
    function IsValid: Boolean;
    function IsResumable: Boolean;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function Serialize: TBytes;
    function Deserialize(const aData: TBytes): Boolean;
    function GetNativeHandle: Pointer;
    function Clone: ISSLSession;
  public
    constructor Create(ASession: PSSL_SESSION; AOwned: Boolean);
    destructor Destroy; override;
  end;

  { TOpenSSLCertificate }
  TOpenSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FCert: PX509;
    FOwned: Boolean;
    FIssuerCert: ISSLCertificate;
  protected
    { ISSLCertificate implementation - Load/Save }
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromStream(aStream: TStream): Boolean;
    function LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
    function LoadFromPEM(const aPEM: string): Boolean;
    function LoadFromDER(const aDER: TBytes): Boolean;
    function SaveToFile(const aFileName: string): Boolean;
    function SaveToStream(aStream: TStream): Boolean;
    function SaveToPEM: string;
    function SaveToDER: TBytes;
    
    { ISSLCertificate implementation - Info }
    function GetInfo: TSSLCertificateInfo;
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetPublicKey: string;
    function GetPublicKeyAlgorithm: string;
    function GetSignatureAlgorithm: string;
    function GetVersion: Integer;
    
    { ISSLCertificate implementation - Verification }
    function Verify(aCAStore: ISSLCertificateStore): Boolean;
    function VerifyEx(aCAStore: ISSLCertificateStore; 
      aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
    function VerifyHostname(const aHostname: string): Boolean;
    function IsExpired: Boolean;
    function IsSelfSigned: Boolean;
    function IsCA: Boolean;
    
    { ISSLCertificate implementation - Extensions }
    function GetExtension(const aOID: string): string;
    function GetSubjectAltNames: TStringList;
    function GetKeyUsage: TStringList;
    function GetExtendedKeyUsage: TStringList;
    
    { ISSLCertificate implementation - Fingerprints }
    function GetFingerprint(aHashType: TSSLHash): string;
    function GetFingerprintSHA1: string;
    function GetFingerprintSHA256: string;
    
    { ISSLCertificate implementation - Chain }
    procedure SetIssuerCertificate(aCert: ISSLCertificate);
    function GetIssuerCertificate: ISSLCertificate;
    
    { ISSLCertificate implementation - Native/Clone }
    function GetNativeHandle: Pointer;
    function Clone: ISSLCertificate;
  public
    constructor Create(ACert: PX509; AOwned: Boolean = True);
    destructor Destroy; override;
  end;

  { TOpenSSLCertificateStore }
  TOpenSSLCertificateStore = class(TInterfacedObject, ISSLCertificateStore)
  private
    FStore: PX509_STORE;
    FCertificates: TList; // List of PX509 pointers
    FOwned: Boolean;
  protected
    { ISSLCertificateStore implementation - Certificate Management }
    function AddCertificate(aCert: ISSLCertificate): Boolean;
    function RemoveCertificate(aCert: ISSLCertificate): Boolean;
    function Contains(aCert: ISSLCertificate): Boolean;
    procedure Clear;
    function GetCount: Integer;
    function GetCertificate(aIndex: Integer): ISSLCertificate;
    
    { ISSLCertificateStore implementation - Loading }
    function LoadFromFile(const aFileName: string): Boolean;
    function LoadFromPath(const aPath: string): Boolean;
    function LoadSystemStore: Boolean;
    
    { ISSLCertificateStore implementation - Finding }
    function FindBySubject(const aSubject: string): ISSLCertificate;
    function FindByIssuer(const aIssuer: string): ISSLCertificate;
    function FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
    function FindByFingerprint(const aFingerprint: string): ISSLCertificate;
    
    { ISSLCertificateStore implementation - Verification }
    function VerifyCertificate(aCert: ISSLCertificate): Boolean;
    function BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
    
    { ISSLCertificateStore implementation - Native Handle }
    function GetNativeHandle: Pointer;
  public
    constructor Create; overload;
    constructor Create(AStore: PX509_STORE; AOwned: Boolean = True); overload;
    destructor Destroy; override;
  end;

  { TOpenSSLConnection }
  TOpenSSLConnection = class(TInterfacedObject, ISSLConnection)
  private
    FContext: TOpenSSLContext;
    FSSL: PSSL;
    FBioRead: PBIO;
    FBioWrite: PBIO;
    FSocket: THandle;
    FStream: TStream;
    FHandshakeComplete: Boolean;
    FTimeout: Integer;
    FBlocking: Boolean;
    
    function TranslateError(ErrorCode: Integer): TSSLErrorCode;
  protected
    { ISSLConnection implementation }
    function Connect: Boolean;
    function Accept: Boolean;
    function Shutdown: Boolean;
    procedure Close;
    function DoHandshake: TSSLHandshakeState;
    function IsHandshakeComplete: Boolean;
    function Renegotiate: Boolean;
    function Read(var aBuffer; aCount: Integer): Integer;
    function Write(const aBuffer; aCount: Integer): Integer;
    function ReadString(out aStr: string): Boolean;
    function WriteString(const aStr: string): Boolean;
    function WantRead: Boolean;
    function WantWrite: Boolean;
    function GetError(aRet: Integer): TSSLErrorCode;
    function GetConnectionInfo: TSSLConnectionInfo;
    function GetProtocolVersion: TSSLProtocolVersion;
    function GetCipherName: string;
    function GetPeerCertificate: ISSLCertificate;
    function GetPeerCertificateChain: TSSLCertificateArray;
    function GetVerifyResult: Integer;
    function GetVerifyResultString: string;
    function GetSession: ISSLSession;
    procedure SetSession(aSession: ISSLSession);
    function IsSessionReused: Boolean;
    function GetSelectedALPNProtocol: string;
    function GetALPNProtocol: string;
    function GetSNI: string;
    function IsConnected: Boolean;
    function GetState: string;
    function GetStateString: string;
    procedure SetTimeout(aTimeout: Integer);
    function GetTimeout: Integer;
    procedure SetBlocking(aBlocking: Boolean);
    function GetBlocking: Boolean;
    function GetSocket: THandle;
    function GetStream: TStream;
    function GetNativeHandle: Pointer;
    function GetContext: ISSLContext;
  public
    constructor Create(AContext: TOpenSSLContext; ASocket: THandle); overload;
    constructor Create(AContext: TOpenSSLContext; AStream: TStream); overload;
    destructor Destroy; override;
  end;

{ Helper functions }
function OpenSSLAvailable: Boolean;
function LoadOpenSSL(const aLibraryPath: string = ''): Boolean;
procedure UnloadOpenSSL;
function GetOpenSSLVersion: string;
function GetOpenSSLVersionNumber: Cardinal;

{ Error handling }
function GetOpenSSLError: Cardinal;
function GetOpenSSLErrorString(aError: Cardinal = 0): string;
procedure ClearOpenSSLErrors;

{ Error classification }
function ClassifyOpenSSLError(aError: Cardinal): TSSLErrorCode;
function GetOpenSSLErrorCategory(aError: Cardinal): string;
function GetFriendlyErrorMessage(aError: Cardinal): string;

{ Certificate utilities }
function LoadCertificateFromFile(const aFileName: string): PX509;
function LoadCertificateFromMemory(const aData: Pointer; aSize: Integer): PX509;
function LoadPrivateKeyFromFile(const aFileName: string; const aPassword: string = ''): PEVP_PKEY;
function LoadPrivateKeyFromMemory(const aData: Pointer; aSize: Integer; const aPassword: string = ''): PEVP_PKEY;
function VerifyCertificate(aCert: PX509; aCAStore: PX509_STORE): Boolean;
function GetCertificateInfo(aCert: PX509): TSSLCertificateInfo;

{ Protocol utilities }
function ProtocolToOpenSSL(aProtocol: TSSLProtocolVersion): Integer;
function OpenSSLToProtocol(aVersion: Integer): TSSLProtocolVersion;
function GetProtocolName(aProtocol: TSSLProtocolVersion): string;

{ Context registry helpers }
procedure RegisterContextInstance(const AContext: TOpenSSLContext);
procedure UnregisterContextInstance(const AContext: TOpenSSLContext);
function LookupContext(AHandle: PSSL_CTX): TOpenSSLContext;

{ Callback bridges }
function ALPNSelectCallback(ssl: PSSL; const out_proto: PPByte; out_proto_len: PByte;
  const in_proto: PByte; in_proto_len: Cardinal; arg: Pointer): Integer; cdecl;
function VerifyCertificateCallback(ctx: PX509_STORE_CTX; arg: Pointer): Integer; cdecl;
function PasswordCallbackThunk(buf: PAnsiChar; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;
procedure InfoCallbackThunk(ssl: PSSL; where: Integer; ret: Integer); cdecl;

{ Initialization }
procedure RegisterOpenSSLBackend;
procedure UnregisterOpenSSLBackend;

implementation

uses
  Math,
  fafafa.ssl.factory,
  fafafa.ssl.errors;

var
  GContextRegistry: TStringList = nil;

const
  OPENSSL_NPN_NEGOTIATED = 1;
  OPENSSL_NPN_NO_OVERLAP = 2;

function ContextRegistryKey(AHandle: PSSL_CTX): string;
begin
  Result := IntToHex(PtrUInt(AHandle), SizeOf(Pointer) * 2);
end;

procedure EnsureContextRegistry;
begin
  if GContextRegistry = nil then
  begin
    GContextRegistry := TStringList.Create;
    GContextRegistry.Sorted := True; // Enable binary search
    GContextRegistry.Duplicates := dupError;
  end;
end;

procedure RegisterContextInstance(const AContext: TOpenSSLContext);
var
  Key: string;
  Index: Integer;
begin
  if (AContext = nil) or (AContext.FSSLCtx = nil) then
    Exit;

  EnsureContextRegistry;
  Key := ContextRegistryKey(AContext.FSSLCtx);
  if GContextRegistry.Find(Key, Index) then
    GContextRegistry.Objects[Index] := AContext
  else
    GContextRegistry.AddObject(Key, AContext);
end;

procedure UnregisterContextInstance(const AContext: TOpenSSLContext);
var
  Key: string;
  Index: Integer;
begin
  if (GContextRegistry = nil) or (AContext = nil) or (AContext.FSSLCtx = nil) then
    Exit;

  Key := ContextRegistryKey(AContext.FSSLCtx);
  if GContextRegistry.Find(Key, Index) then
    GContextRegistry.Delete(Index);

  if GContextRegistry.Count = 0 then
  begin
    GContextRegistry.Free;
    GContextRegistry := nil;
  end;
end;

function LookupContext(AHandle: PSSL_CTX): TOpenSSLContext;
var
  Key: string;
  Index: Integer;
begin
  Result := nil;
  if (GContextRegistry = nil) or (AHandle = nil) then
    Exit;

  Key := ContextRegistryKey(AHandle);
  if GContextRegistry.Find(Key, Index) then
    Result := TOpenSSLContext(GContextRegistry.Objects[Index]);
end;

function BuildALPNWireData(const aProtocols: string): TBytes;
var
  ProtoList: TStringArray;
  Proto: string;
  TrimmedProto: string;
  TotalLen: Integer;
  Offset: Integer;
  AnsiProto: AnsiString;
begin
  TotalLen := 0;
  ProtoList := aProtocols.Split([',']);
  for Proto in ProtoList do
  begin
    TrimmedProto := Trim(Proto);
    if TrimmedProto = '' then
      Continue;
    if Length(TrimmedProto) > 255 then
      RaiseInvalidParameter('ALPN protocol name length');
    Inc(TotalLen, 1 + Length(TrimmedProto));
  end;

  SetLength(Result, TotalLen);
  Offset := 0;
  if TotalLen = 0 then
    Exit;

  for Proto in ProtoList do
  begin
    TrimmedProto := Trim(Proto);
    if TrimmedProto = '' then
      Continue;
    Result[Offset] := Length(TrimmedProto);
    Inc(Offset);
    AnsiProto := AnsiString(TrimmedProto);
    if Length(TrimmedProto) > 0 then
    begin
      Move(AnsiProto[1], Result[Offset], Length(TrimmedProto));
      Inc(Offset, Length(TrimmedProto));
    end;
  end;
end;

function ALPNSelectCallback(ssl: PSSL; const out_proto: PPByte; out_proto_len: PByte;
  const in_proto: PByte; in_proto_len: Cardinal; arg: Pointer): Integer; cdecl;
var
  Context: TOpenSSLContext;
  Wire: TBytes;
  ResultCode: Integer;
begin
  Result := SSL_TLSEXT_ERR_NOACK;
  if (ssl = nil) or (out_proto = nil) or (out_proto_len = nil) then
    Exit;

  Context := TOpenSSLContext(arg);
  if Context = nil then
    Exit;

  Wire := Context.FALPNWireData;
  if (Length(Wire) = 0) or not Assigned(SSL_select_next_proto) then
    Exit;

  ResultCode := SSL_select_next_proto(out_proto, out_proto_len, @Wire[0], Length(Wire), in_proto, in_proto_len);
  case ResultCode of
    OPENSSL_NPN_NEGOTIATED:
      Result := SSL_TLSEXT_ERR_OK;
    OPENSSL_NPN_NO_OVERLAP:
      Result := SSL_TLSEXT_ERR_NOACK;
  else
    Result := SSL_TLSEXT_ERR_ALERT_FATAL;
  end;
end;

function VerifyCertificateCallback(ctx: PX509_STORE_CTX; arg: Pointer): Integer; cdecl;
var
  Context: TOpenSSLContext;
  DefaultResult: Integer;
  Cert: PX509;
  Info: TSSLCertificateInfo;
  ErrorCode: Integer;
  ErrorMsg: string;
begin
  Context := TOpenSSLContext(arg);
  if not Assigned(X509_verify_cert) then
  begin
    Result := 1;
    Exit;
  end;

  DefaultResult := X509_verify_cert(ctx);

  if (Context = nil) or not Assigned(Context.FVerifyCallback) then
  begin
    Result := DefaultResult;
    Exit;
  end;

  FillChar(Info, SizeOf(Info), 0);
  if Assigned(X509_STORE_CTX_get_current_cert) then
  begin
    Cert := X509_STORE_CTX_get_current_cert(ctx);
    if Cert <> nil then
      Info := GetCertificateInfo(Cert);
  end;

  ErrorCode := 0;
  if Assigned(X509_STORE_CTX_get_error) then
    ErrorCode := X509_STORE_CTX_get_error(ctx);

  if Assigned(X509_verify_cert_error_string) then
    ErrorMsg := string(X509_verify_cert_error_string(ErrorCode))
  else
    ErrorMsg := '';

  if Context.FVerifyCallback(Info, ErrorCode, ErrorMsg) then
  begin
    Result := DefaultResult;
    if (Result = 0) and Assigned(X509_STORE_CTX_set_error) then
      X509_STORE_CTX_set_error(ctx, X509_V_OK);
  end
  else
  begin
    Result := 0;
    if Assigned(X509_STORE_CTX_set_error) then
      X509_STORE_CTX_set_error(ctx, X509_V_ERR_APPLICATION_VERIFICATION);
  end;
end;

function PasswordCallbackThunk(buf: PAnsiChar; size: Integer; rwflag: Integer; userdata: Pointer): Integer; cdecl;
var
  Context: TOpenSSLContext;
  Password: string;
  AnsiPwd: AnsiString;
begin
  Result := 0;
  if (buf = nil) or (size <= 0) then
    Exit;

  Context := TOpenSSLContext(userdata);
  if (Context = nil) or not Assigned(Context.FPasswordCallback) then
    Exit;

  Password := '';
  if not Context.FPasswordCallback(Password, rwflag <> 0) then
    Exit;

  AnsiPwd := AnsiString(Password);
  if Length(AnsiPwd) >= size then
    Exit;

  if Length(AnsiPwd) > 0 then
    Move(AnsiPwd[1], buf^, Length(AnsiPwd));
  buf[Length(AnsiPwd)] := #0;
  Result := Length(AnsiPwd);
end;

procedure InfoCallbackThunk(ssl: PSSL; where: Integer; ret: Integer); cdecl;
var
  CtxHandle: PSSL_CTX;
  Context: TOpenSSLContext;
  StatePtr: PAnsiChar;
  StateStr: string;
begin
  if ssl = nil then
    Exit;

  if not Assigned(SSL_get_SSL_CTX) then
    Exit;

  CtxHandle := SSL_get_SSL_CTX(ssl);
  Context := LookupContext(CtxHandle);
  if (Context = nil) or not Assigned(Context.FInfoCallback) then
    Exit;

  StateStr := '';
  if Assigned(SSL_state_string_long) then
  begin
    StatePtr := SSL_state_string_long(ssl);
    if StatePtr <> nil then
      StateStr := string(StatePtr);
  end;

  Context.FInfoCallback(where, ret, StateStr);
end;

function CompareX509Names(Name1, Name2: PX509_NAME): Boolean;
begin
  Result := False;
  
  if (Name1 = nil) or (Name2 = nil) then Exit;
  
  // 使用 OpenSSL API 进行精确比较
  if Assigned(X509_NAME_cmp) then
    Result := X509_NAME_cmp(Name1, Name2) = 0
  else
    // 如果 API 不可用，两个指针相同则认为相等
    Result := Name1 = Name2;
end;

// No longer need TOpenSSLBackend class - factory will create TOpenSSLLibrary directly

{ TOpenSSLLibrary }

constructor TOpenSSLLibrary.Create;
begin
  inherited Create;
  FInitialized := False;
end;

destructor TOpenSSLLibrary.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited Destroy;
end;

function TOpenSSLLibrary.Initialize: Boolean;
begin
  if FInitialized then
  begin
    Exit(True);
  end;
    
  // Load OpenSSL core libraries if not already loaded
  if not IsOpenSSLCoreLoaded then
    LoadOpenSSLCore;

  // Load all required API modules
  LoadOpenSSLERR;      // Load error handling functions FIRST
  LoadOpenSSLX509;
  LoadOpenSSLBIO;
  LoadOpenSSLCrypto;
  LoadEVP(GetCryptoLibHandle);
  LoadOpenSSLASN1(GetCryptoLibHandle);
  LoadOpenSSLPEM(GetCryptoLibHandle);
  LoadOpenSSLBN;
  LoadX509V3Functions(GetCryptoLibHandle);
  LoadPKCS12Module(GetCryptoLibHandle);
  LoadOpenSSLOCSP(GetCryptoLibHandle);
  LoadOpenSSLSSL;  // Load SSL module functions (SSL_connect, SSL_new, etc.)

  FInitialized := True;
  Result := True;
end;

procedure TOpenSSLLibrary.Finalize;
begin
  FInitialized := False;
end;

function TOpenSSLLibrary.IsInitialized: Boolean;
begin
  Result := FInitialized;
end;

function TOpenSSLLibrary.GetLibraryType: TSSLLibraryType;
begin
  Result := sslOpenSSL;
end;

function TOpenSSLLibrary.GetVersionString: string;
begin
  // Delegate to runtime OpenSSL version if available
  Result := GetOpenSSLVersion;
end;

function TOpenSSLLibrary.GetVersion: string;
begin
  Result := GetOpenSSLVersion;
end;


function TOpenSSLLibrary.GetVersionNumber: Cardinal;
begin
  // Get actual OpenSSL version number
  if Assigned(OpenSSL_version_num) then
    Result := OpenSSL_version_num()
  else
    Result := $30000000; // Fallback to 3.0.0 if not available
end;

function TOpenSSLLibrary.GetCompileFlags: string;
var
  Flags, Platform: string;
begin
  Result := '';

  if not IsOpenSSLCoreLoaded then
    LoadOpenSSLCore;

  if Assigned(OpenSSL_version) then
  begin
    Flags := string(OpenSSL_version(OPENSSL_CFLAGS));
    Platform := string(OpenSSL_version(OPENSSL_PLATFORM));

    if Flags <> '' then
      Result := 'cflags=' + Flags;

    if Platform <> '' then
    begin
      if Result <> '' then
        Result := Result + '; ';
      Result := Result + 'platform=' + Platform;
    end;
  end;
end;

function TOpenSSLLibrary.IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := aProtocol in [sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12, sslProtocolTLS13];
end;

function TOpenSSLLibrary.IsCipherSupported(const aCipherName: string): Boolean;
var
  Method: PSSL_METHOD;
  Ctx: PSSL_CTX;
  Cipher: AnsiString;
begin
  Result := False;

  if Trim(aCipherName) = '' then
    Exit;

  if not FInitialized then
    Initialize;

  if not Assigned(TLS_method) or not Assigned(SSL_CTX_new) or not Assigned(SSL_CTX_free) then
    Exit;

  Method := TLS_method();
  if Method = nil then
    Exit;

  Ctx := SSL_CTX_new(Method);
  if Ctx = nil then
    Exit;
  try
    Cipher := AnsiString(Trim(aCipherName));
    if Cipher = '' then
      Exit(False);

    if Assigned(SSL_CTX_set_ciphersuites) then
      if SSL_CTX_set_ciphersuites(Ctx, PAnsiChar(Cipher)) = 1 then
        Exit(True);

    if Assigned(SSL_CTX_set_cipher_list) then
      if SSL_CTX_set_cipher_list(Ctx, PAnsiChar(Cipher)) = 1 then
        Exit(True);
  finally
    SSL_CTX_free(Ctx);
  end;
end;

function TOpenSSLLibrary.IsFeatureSupported(const aFeatureName: string): Boolean;
var
  Name: string;
begin
  Name := LowerCase(Trim(aFeatureName));
  if Name = '' then
    Exit(False);

  if not FInitialized then
    Initialize;

  if Name = 'alpn' then
    Exit(Assigned(SSL_CTX_set_alpn_protos) and Assigned(SSL_CTX_set_alpn_select_cb));

  if Name = 'session_cache' then
    Exit(Assigned(SSL_CTX_set_session_cache_mode));

  if Name = 'session_ticket' then
    Exit(Assigned(SSL_CTX_set_tlsext_ticket_key_cb));

  if Name = 'ocsp' then
    Exit(Assigned(SSL_CTX_set_tlsext_status_cb));

  if Name = 'tls13' then
    Exit(Assigned(SSL_CTX_set_ciphersuites));

  if Name = 'verify_callback' then
    Exit(Assigned(SSL_CTX_set_cert_verify_callback));

  if Name = 'password_callback' then
    Exit(Assigned(SSL_CTX_set_default_passwd_cb));

  if Name = 'info_callback' then
    Exit(Assigned(SSL_CTX_set_info_callback));

  if Name = 'psk' then
    Exit(Assigned(SSL_CTX_set_psk_client_callback) or Assigned(SSL_CTX_set_psk_server_callback));

  if Name = 'keylog' then
    Exit(Assigned(SSL_CTX_set_keylog_callback));

  if Name = 'quic' then
    Exit(Assigned(SSL_get_stream_id));

  Result := False;
end;

procedure TOpenSSLLibrary.SetDefaultConfig(const aConfig: TSSLConfig);
begin
  FConfig := aConfig;
end;

function TOpenSSLLibrary.GetDefaultConfig: TSSLConfig;
begin
  Result := FConfig;
end;

function TOpenSSLLibrary.GetLastError: Integer;
begin
  // Get the last OpenSSL error code from the error queue
  Result := Integer(GetOpenSSLError);
end;

function TOpenSSLLibrary.GetLastErrorString: string;
var
  LError: Cardinal;
  LBuf: array[0..511] of AnsiChar;
  LErrorStr: string;
  LAllErrors: string;
begin
  // Get all errors from the OpenSSL error queue
  LAllErrors := '';

  if Assigned(ERR_get_error) and Assigned(ERR_error_string_n) then
  begin
    repeat
      LError := ERR_get_error();
      if LError <> 0 then
      begin
        // Get error string
        ERR_error_string_n(LError, @LBuf[0], SizeOf(LBuf));
        LErrorStr := string(LBuf);

        // Append to accumulated errors
        if LAllErrors <> '' then
          LAllErrors := LAllErrors + ' | ';
        LAllErrors := LAllErrors + LErrorStr;
      end;
    until LError = 0;
  end;

  // Return accumulated errors or default message
  if LAllErrors <> '' then
    Result := LAllErrors
  else
    Result := 'No SSL errors';
end;

procedure TOpenSSLLibrary.ClearError;
begin
  // Clear the entire OpenSSL error queue
  ClearOpenSSLErrors;
end;

function TOpenSSLLibrary.GetStatistics: TSSLStatistics;
begin
  Result := FStatistics;
end;

procedure TOpenSSLLibrary.ResetStatistics;
begin
  FillChar(FStatistics, SizeOf(FStatistics), 0);
end;

procedure TOpenSSLLibrary.SetLogCallback(aCallback: TSSLLogCallback);
begin
  FLogCallback := aCallback;
end;

procedure TOpenSSLLibrary.Log(aLevel: TSSLLogLevel; const aMessage: string);
begin
  if Assigned(FLogCallback) then
    FLogCallback(aLevel, aMessage);
end;

function TOpenSSLLibrary.CreateContext(aType: TSSLContextType): ISSLContext;
begin
  Result := TOpenSSLContext.Create(Self, aType);
end;

function TOpenSSLLibrary.CreateCertificate: ISSLCertificate;
var
  LCert: PX509;
begin
  // Create a new empty X509 certificate
  LCert := X509_new();
  if LCert <> nil then
    Result := TOpenSSLCertificate.Create(LCert, True)
  else
    Result := TOpenSSLCertificate.Create(nil, False);
end;

function TOpenSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := TOpenSSLCertificateStore.Create;
end;

{ TOpenSSLContext }

constructor TOpenSSLContext.Create(ALibrary: TOpenSSLLibrary; AType: TSSLContextType);
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FSessionCacheEnabled := False;
  FSessionTimeout := 0;
  FSessionCacheSize := 0;
  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;
  SetLength(FALPNWireData, 0);
  SetupContext;
end;

destructor TOpenSSLContext.Destroy;
begin
  if FSSLCtx <> nil then
  begin
    UnregisterContextInstance(Self);
    SSL_CTX_free(FSSLCtx);
  end;
  inherited Destroy;
end;

procedure TOpenSSLContext.SetupSSLCore;
var
  LMethod: PSSL_METHOD;
begin
  // Create SSL context using TLS_method (supports TLS 1.0-1.3)
  if not Assigned(TLS_method) or not Assigned(SSL_CTX_new) then
    Exit;
  
  LMethod := TLS_method();
  if LMethod = nil then
    Exit;
    
  FSSLCtx := SSL_CTX_new(LMethod);
  if FSSLCtx = nil then
    Exit;
    
  RegisterContextInstance(Self);
end;

procedure TOpenSSLContext.SetupSecurityDefaults;
begin
  if FSSLCtx = nil then
    Exit;
    
  // Disable insecure SSL 2.0 and SSL 3.0
  if Assigned(SSL_CTX_set_options) then
    SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_SSLv2 or SSL_OP_NO_SSLv3);

  // Enforce TLS 1.2+ minimum for modern security (NIST SP 800-52)
  // This prevents downgrade attacks and ensures strong cryptography
  if Assigned(SSL_CTX_set_min_proto_version) then
  begin
    SSL_CTX_set_min_proto_version(FSSLCtx, TLS1_2_VERSION);
    // Note: Users can override via SetProtocolVersions if compatibility needed
  end;
end;

procedure TOpenSSLContext.EnsureSystemCAsLoaded;
begin
  if FSystemCAsLoaded then Exit;
  if FSSLCtx = nil then Exit;

  // Only auto-load for client context
  if (FContextType = sslCtxClient) and Assigned(SSL_CTX_set_default_verify_paths) then
  begin
    // Ignore return value - user can manually load CAs if this fails
    SSL_CTX_set_default_verify_paths(FSSLCtx);
  end;
  
  FSystemCAsLoaded := True;
end;

procedure TOpenSSLContext.SetupDefaultVerification;
begin
  if FSSLCtx = nil then
    Exit;
    
  // System CA loading is now deferred to EnsureSystemCAsLoaded called in CreateConnection
  // This improves context creation performance (avoids file I/O unless needed)
  FSystemCAsLoaded := False;

  // Set appropriate verify mode based on context type
  if FContextType = sslCtxClient then
    SetVerifyMode([sslVerifyPeer])  // Clients verify server certificates
  else
    SetVerifyMode([]);               // Servers don't require client certs by default
end;

procedure TOpenSSLContext.SetupContext;
begin
  // Step 1: Create core SSL context
  SetupSSLCore;
  if FSSLCtx = nil then
    Exit;
    
  // Step 2: Configure security defaults (TLS 1.2+, disable weak protocols)
  SetupSecurityDefaults;
  
  // Step 3: Setup verification and CA loading
  SetupDefaultVerification;

  // Step 4: Cache initial session parameters
  FSessionCacheEnabled := GetSessionCacheMode;
  FSessionTimeout := GetSessionTimeout;
  FSessionCacheSize := GetSessionCacheSize;
end;

function TOpenSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TOpenSSLContext.SetProtocolVersions(aVersions: TSSLProtocolVersions);
var
  LMinProto, LMaxProto: TSSLProtocolVersion;
  LMinVersion, LMaxVersion: Integer;
begin
  FProtocolVersions := aVersions;

  // Apply protocol versions to SSL_CTX immediately
  if FSSLCtx = nil then
    Exit;

  if aVersions = [] then
    Exit;  // Empty set, use defaults

  // Find minimum and maximum protocol versions from the set
  LMinProto := Low(TSSLProtocolVersion);
  LMaxProto := High(TSSLProtocolVersion);

  // Find actual min
  for LMinProto := Low(TSSLProtocolVersion) to High(TSSLProtocolVersion) do
    if LMinProto in aVersions then
      Break;

  // Find actual max
  for LMaxProto := High(TSSLProtocolVersion) downto Low(TSSLProtocolVersion) do
    if LMaxProto in aVersions then
      Break;

  // Convert to OpenSSL version constants
  LMinVersion := ProtocolToOpenSSL(LMinProto);
  LMaxVersion := ProtocolToOpenSSL(LMaxProto);

  // Apply to SSL_CTX (OpenSSL 1.1.0+ API)
  if Assigned(SSL_CTX_set_min_proto_version) then
    SSL_CTX_set_min_proto_version(FSSLCtx, LMinVersion);

  if Assigned(SSL_CTX_set_max_proto_version) then
    SSL_CTX_set_max_proto_version(FSSLCtx, LMaxVersion);
end;

function TOpenSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TOpenSSLContext.LoadCertificate(const aFileName: string);
var
  LFileNameAnsi: AnsiString;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  LFileNameAnsi := AnsiString(aFileName);
  
  // Try loading as a certificate chain file first
  if Assigned(SSL_CTX_use_certificate_chain_file) then
  begin
    if SSL_CTX_use_certificate_chain_file(FSSLCtx, PAnsiChar(LFileNameAnsi)) = 1 then
      Exit;  // Success
  end;
  
  // Fall back to single certificate file (PEM format)
  if Assigned(SSL_CTX_use_certificate_file) then
  begin
    if SSL_CTX_use_certificate_file(FSSLCtx, PAnsiChar(LFileNameAnsi), SSL_FILETYPE_PEM) = 1 then
      Exit;  // Success
  end;
  
  raise ESSLCertificateException.Create(
    Format('Failed to load certificate from file: %s', [aFileName]),
    sslErrCertificate
  );
end;

procedure TOpenSSLContext.LoadCertificate(aStream: TStream);
var
  LBio: PBIO;
  LCert: PX509;
  LData: TBytes;
  LSize: Integer;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  if aStream = nil then
    RaiseInvalidParameter('Stream');
  
  // Read stream into memory
  LSize := aStream.Size - aStream.Position;
  SetLength(LData, LSize);
  aStream.Read(LData[0], LSize);
  
  // Create BIO from memory
  LBio := BIO_new_mem_buf(@LData[0], LSize);
  if LBio = nil then
    RaiseMemoryError('BIO creation');
  
  try
    // Try to read PEM certificate
    LCert := PEM_read_bio_X509(LBio, nil, nil, nil);
    if LCert = nil then
      raise ESSLCertificateException.Create(
        'Failed to parse certificate from stream',
        sslErrCertificate
      );
    
    try
      // Use the certificate in context
      if Assigned(SSL_CTX_use_certificate) then
      begin
        if SSL_CTX_use_certificate(FSSLCtx, LCert) <> 1 then
          raise ESSLCertificateException.Create(
            'Failed to use certificate in SSL context',
            sslErrCertificate
          );
      end;
    finally
      X509_free(LCert);
    end;
  finally
    BIO_free(LBio);
  end;
end;

procedure TOpenSSLContext.LoadCertificate(aCert: ISSLCertificate);
var
  LCert: PX509;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  if aCert = nil then
    RaiseInvalidParameter('Certificate');
  
  LCert := PX509(aCert.GetNativeHandle);
  if LCert = nil then
    raise ESSLCertificateException.Create(
      'Certificate has no native handle',
      sslErrCertificate
    );
  
  // Use the certificate in context
  if Assigned(SSL_CTX_use_certificate) then
  begin
    if SSL_CTX_use_certificate(FSSLCtx, LCert) <> 1 then
      raise ESSLCertificateException.Create(
        'Failed to use certificate in SSL context',
        sslErrCertificate
      );
  end;
end;

procedure TOpenSSLContext.LoadPrivateKey(const aFileName: string; const aPassword: string);
var
  LFileNameAnsi: AnsiString;
  LPasswordAnsi: AnsiString;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  LFileNameAnsi := AnsiString(aFileName);
  
  // Set password callback if password provided
  if aPassword <> '' then
  begin
    LPasswordAnsi := AnsiString(aPassword);
    if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
      SSL_CTX_set_default_passwd_cb_userdata(FSSLCtx, PAnsiChar(LPasswordAnsi));
  end;
  
  // Load private key from file (PEM format)
  if Assigned(SSL_CTX_use_PrivateKey_file) then
  begin
    if SSL_CTX_use_PrivateKey_file(FSSLCtx, PAnsiChar(LFileNameAnsi), SSL_FILETYPE_PEM) = 1 then
    begin
      // Verify that private key matches the certificate
      if Assigned(SSL_CTX_check_private_key) then
      begin
        if SSL_CTX_check_private_key(FSSLCtx) <> 1 then
          raise ESSLCertificateException.Create(
            'Private key does not match certificate',
            sslErrCertificate
          );
      end;
      Exit;  // Success
    end;
  end;
  
  raise ESSLException.Create(
    Format('Failed to load private key from file: %s', [aFileName]),
    sslErrCertificate
  );
end;

procedure TOpenSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string);
var
  LBio: PBIO;
  LKey: PEVP_PKEY;
  LData: TBytes;
  LSize: Integer;
  LPasswordAnsi: AnsiString;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  if aStream = nil then
    RaiseInvalidParameter('Stream');
  
  // Read stream into memory
  LSize := aStream.Size - aStream.Position;
  SetLength(LData, LSize);
  aStream.Read(LData[0], LSize);
  
  // Create BIO from memory
  LBio := BIO_new_mem_buf(@LData[0], LSize);
  if LBio = nil then
    RaiseMemoryError('BIO creation');
  
  try
    // Set password callback if needed
    if aPassword <> '' then
    begin
      LPasswordAnsi := AnsiString(aPassword);
      if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
        SSL_CTX_set_default_passwd_cb_userdata(FSSLCtx, PAnsiChar(LPasswordAnsi));
    end;
    
    // Try to read PEM private key
    LKey := PEM_read_bio_PrivateKey(LBio, nil, nil, nil);
    if LKey = nil then
      raise ESSLException.Create(
        'Failed to parse private key from stream',
        sslErrCertificate
      );
    
    try
      // Use the private key in context
      if Assigned(SSL_CTX_use_PrivateKey) then
      begin
        if SSL_CTX_use_PrivateKey(FSSLCtx, LKey) <> 1 then
          raise ESSLException.Create(
            'Failed to use private key in SSL context',
            sslErrCertificate
          );
        
        // Verify that private key matches the certificate
        if Assigned(SSL_CTX_check_private_key) then
        begin
          if SSL_CTX_check_private_key(FSSLCtx) <> 1 then
            raise ESSLCertificateException.Create(
              'Private key does not match certificate',
              sslErrCertificate
            );
        end;
      end;
    finally
      EVP_PKEY_free(LKey);
    end;
  finally
    BIO_free(LBio);
  end;
end;

procedure TOpenSSLContext.LoadCertificatePEM(const aPEM: string);
var
  Data: TBytes;
  Cert: PX509;
begin
  Data := TEncoding.ASCII.GetBytes(aPEM);
  if Length(Data) = 0 then Exit;
  
  // Implementation:
  // We can use LoadCertificateFromMemory helper and then SSL_CTX_use_certificate
  Cert := LoadCertificateFromMemory(@Data[0], Length(Data));
  if Cert <> nil then
  begin
    if SSL_CTX_use_certificate(FSSLCtx, Cert) <= 0 then
    begin
       X509_free(Cert);
       RaiseSSLError('LoadCertificatePEM failed: ' + GetOpenSSLErrorString, sslErrLoadFailed);
    end;
    X509_free(Cert); // CTX increases ref count
  end
  else
    RaiseSSLError('Failed to parse certificate PEM', sslErrLoadFailed);
end;

procedure TOpenSSLContext.LoadPrivateKeyPEM(const aPEM: string; const aPassword: string);
var
  Data: TBytes;
  PKey: PEVP_PKEY;
begin
  Data := TEncoding.ASCII.GetBytes(aPEM);
  if Length(Data) = 0 then Exit;
  
  PKey := LoadPrivateKeyFromMemory(@Data[0], Length(Data), aPassword);
  if PKey <> nil then
  begin
    if SSL_CTX_use_PrivateKey(FSSLCtx, PKey) <= 0 then
    begin
      EVP_PKEY_free(PKey);
      RaiseSSLError('LoadPrivateKeyPEM failed: ' + GetOpenSSLErrorString, sslErrLoadFailed);
    end;
    EVP_PKEY_free(PKey);
  end
  else
    RaiseSSLError('Failed to parse private key PEM', sslErrLoadFailed);
end;

procedure TOpenSSLContext.LoadCAFile(const aFileName: string);
var
  LFileNameAnsi: AnsiString;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  LFileNameAnsi := AnsiString(aFileName);
  
  // Load CA certificates from file
  if Assigned(SSL_CTX_load_verify_locations) then
  begin
    if SSL_CTX_load_verify_locations(FSSLCtx, PAnsiChar(LFileNameAnsi), nil) <> 1 then
      raise ESSLCertificateException.Create(
        Format('Failed to load CA file: %s', [aFileName]),
        sslErrCertificate
      );
  end
  else
    RaiseFunctionNotAvailable('SSL_CTX_load_verify_locations');
end;

procedure TOpenSSLContext.LoadCAPath(const aPath: string);
var
  LPathAnsi: AnsiString;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  LPathAnsi := AnsiString(aPath);
  
  // Load CA certificates from directory path
  if Assigned(SSL_CTX_load_verify_locations) then
  begin
    if SSL_CTX_load_verify_locations(FSSLCtx, nil, PAnsiChar(LPathAnsi)) <> 1 then
      raise ESSLCertificateException.Create(
        Format('Failed to load CA path: %s', [aPath]),
        sslErrCertificate
      );
  end
  else
    RaiseFunctionNotAvailable('SSL_CTX_load_verify_locations');
end;

procedure TOpenSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
var
  LStore: PX509_STORE;
begin
  if FSSLCtx = nil then
    RaiseNotInitialized('SSL context');
  
  if aStore = nil then
    RaiseInvalidParameter('Certificate store');
  
  LStore := PX509_STORE(aStore.GetNativeHandle);
  if LStore = nil then
    raise ESSLCertificateException.Create(
      'Certificate store has no native handle',
      sslErrCertificate
    );
  
  // Set the certificate store in the SSL context
  // Note: SSL_CTX_set1_cert_store increments reference count
  if Assigned(SSL_CTX_set1_cert_store) then
    SSL_CTX_set1_cert_store(FSSLCtx, LStore)
  else if Assigned(SSL_CTX_set_cert_store) then
    SSL_CTX_set_cert_store(FSSLCtx, LStore)
  else
    RaiseFunctionNotAvailable('SSL_CTX_set_cert_store');
end;

procedure TOpenSSLContext.SetVerifyMode(aMode: TSSLVerifyModes);
var
  LVerifyMode: Integer;
begin
  FVerifyMode := aMode;

  // Apply verify mode to SSL_CTX immediately
  if FSSLCtx = nil then
    Exit;

  // Convert TSSLVerifyModes set to OpenSSL verify flags
  LVerifyMode := SSL_VERIFY_NONE;

  if sslVerifyPeer in aMode then
    LVerifyMode := LVerifyMode or SSL_VERIFY_PEER;

  if sslVerifyFailIfNoPeerCert in aMode then
    LVerifyMode := LVerifyMode or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;

  if sslVerifyClientOnce in aMode then
    LVerifyMode := LVerifyMode or SSL_VERIFY_CLIENT_ONCE;

  // Apply the verify mode to the context
  if Assigned(SSL_CTX_set_verify) then
    SSL_CTX_set_verify(FSSLCtx, LVerifyMode, nil);
end;

function TOpenSSLContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TOpenSSLContext.SetVerifyDepth(aDepth: Integer);
begin
  FVerifyDepth := aDepth;
end;

function TOpenSSLContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TOpenSSLContext.SetVerifyCallback(aCallback: TSSLVerifyCallback);
begin
  FVerifyCallback := aCallback;

  if FSSLCtx = nil then
    Exit;

  if not Assigned(SSL_CTX_set_cert_verify_callback) then
    RaiseUnsupported('Verify callback');

  if Assigned(FVerifyCallback) then
    SSL_CTX_set_cert_verify_callback(FSSLCtx, @VerifyCertificateCallback, Self)
  else
    SSL_CTX_set_cert_verify_callback(FSSLCtx, nil, nil);
end;

procedure TOpenSSLContext.SetCipherList(const aCipherList: string);
var
  LCipherList: AnsiString;
begin
  FCipherList := aCipherList;

  if (FSSLCtx = nil) or (aCipherList = '') then
    Exit;

  if not Assigned(SSL_CTX_set_cipher_list) then
    RaiseFunctionNotAvailable('SSL_CTX_set_cipher_list');

  LCipherList := AnsiString(aCipherList);
  if SSL_CTX_set_cipher_list(FSSLCtx, PAnsiChar(LCipherList)) <> 1 then
  begin
    if FLibrary <> nil then
      FLibrary.Log(sslLogWarning, Format('Failed to apply cipher list: %s', [aCipherList]));
    RaiseSSLError('Failed to set cipher list', sslErrConfiguration);
  end;
end;

function TOpenSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TOpenSSLContext.SetCipherSuites(const aCipherSuites: string);
var
  LCipherSuites: AnsiString;
begin
  FCipherSuites := aCipherSuites;

  if (FSSLCtx = nil) or (aCipherSuites = '') then
    Exit;

  if not Assigned(SSL_CTX_set_ciphersuites) then
    RaiseUnsupported('TLS 1.3 cipher configuration');

  LCipherSuites := AnsiString(aCipherSuites);
  if SSL_CTX_set_ciphersuites(FSSLCtx, PAnsiChar(LCipherSuites)) <> 1 then
  begin
    if FLibrary <> nil then
      FLibrary.Log(sslLogWarning, Format('Failed to apply TLS 1.3 cipher suites: %s', [aCipherSuites]));
    RaiseSSLError('Failed to set TLS 1.3 cipher suites', sslErrConfiguration);
  end;
end;

function TOpenSSLContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

procedure TOpenSSLContext.SetSessionCacheMode(aEnabled: Boolean);
var
  Mode: Integer;
begin
  FSessionCacheEnabled := aEnabled;

  if FSSLCtx = nil then
    Exit;

  if not Assigned(SSL_CTX_set_session_cache_mode) then
    RaiseUnsupported('Session cache control');

  if aEnabled then
  begin
    case FContextType of
      sslCtxServer: Mode := SSL_SESS_CACHE_SERVER;
      sslCtxBoth: Mode := SSL_SESS_CACHE_BOTH;
    else
      Mode := SSL_SESS_CACHE_CLIENT;
    end;
  end
  else
    Mode := SSL_SESS_CACHE_OFF;

  SSL_CTX_set_session_cache_mode(FSSLCtx, Mode);
end;

function TOpenSSLContext.GetSessionCacheMode: Boolean;
var
  Mode: Integer;
begin
  if FSSLCtx = nil then
    Exit(FSessionCacheEnabled);

  if Assigned(SSL_CTX_get_session_cache_mode) then
  begin
    Mode := SSL_CTX_get_session_cache_mode(FSSLCtx);
    Result := (Mode and SSL_SESS_CACHE_OFF) = 0;
    FSessionCacheEnabled := Result;
  end
  else
    Result := FSessionCacheEnabled;
end;

procedure TOpenSSLContext.SetSessionTimeout(aTimeout: Integer);
begin
  FSessionTimeout := aTimeout;

  if (FSSLCtx = nil) or (aTimeout <= 0) then
    Exit;

  if not Assigned(SSL_CTX_set_timeout) then
    RaiseUnsupported('Session timeout configuration');

  SSL_CTX_set_timeout(FSSLCtx, aTimeout);
end;

function TOpenSSLContext.GetSessionTimeout: Integer;
begin
  if FSSLCtx = nil then
    Exit(FSessionTimeout);

  if Assigned(SSL_CTX_get_timeout) then
  begin
    Result := SSL_CTX_get_timeout(FSSLCtx);
    FSessionTimeout := Result;
  end
  else
    Result := FSessionTimeout;
end;

procedure TOpenSSLContext.SetSessionCacheSize(aSize: Integer);
begin
  FSessionCacheSize := aSize;

  if (FSSLCtx = nil) or (aSize <= 0) then
    Exit;

  if not Assigned(SSL_CTX_sess_set_cache_size) then
    RaiseUnsupported('Session cache sizing');

  SSL_CTX_sess_set_cache_size(FSSLCtx, aSize);
end;

function TOpenSSLContext.GetSessionCacheSize: Integer;
begin
  if FSSLCtx = nil then
    Exit(FSessionCacheSize);

  if Assigned(SSL_CTX_sess_get_cache_size) then
  begin
    Result := SSL_CTX_sess_get_cache_size(FSSLCtx);
    FSessionCacheSize := Result;
  end
  else
    Result := FSessionCacheSize;
end;

procedure TOpenSSLContext.SetOptions(const aOptions: TSSLOptions);
begin
  FOptions := aOptions;
end;

function TOpenSSLContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TOpenSSLContext.SetServerName(const aServerName: string);
begin
  FServerName := aServerName;
end;

function TOpenSSLContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TOpenSSLContext.SetALPNProtocols(const aProtocols: string);
begin
  FALPNProtocols := Trim(aProtocols);
  FALPNWireData := BuildALPNWireData(FALPNProtocols);

  if FSSLCtx = nil then
    Exit;

  if FALPNProtocols = '' then
  begin
    if Assigned(SSL_CTX_set_alpn_select_cb) then
      SSL_CTX_set_alpn_select_cb(FSSLCtx, nil, nil);
    Exit;
  end;

  if not Assigned(SSL_CTX_set_alpn_protos) then
    RaiseUnsupported('ALPN protocol negotiation');

  if (Length(FALPNWireData) = 0) or
    (SSL_CTX_set_alpn_protos(FSSLCtx, @FALPNWireData[0], Length(FALPNWireData)) <> 0) then
  begin
    if FLibrary <> nil then
      FLibrary.Log(sslLogWarning, Format('Failed to configure ALPN protocols: %s', [FALPNProtocols]));
    RaiseSSLError('Failed to configure ALPN protocols', sslErrConfiguration);
  end;

  if (FContextType <> sslCtxClient) and Assigned(SSL_CTX_set_alpn_select_cb) then
    SSL_CTX_set_alpn_select_cb(FSSLCtx, @ALPNSelectCallback, Self);
end;

function TOpenSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TOpenSSLContext.SetPasswordCallback(aCallback: TSSLPasswordCallback);
begin
  FPasswordCallback := aCallback;

  if FSSLCtx = nil then
    Exit;

  if not Assigned(SSL_CTX_set_default_passwd_cb) then
    RaiseUnsupported('Password callback');

  if Assigned(FPasswordCallback) then
  begin
    SSL_CTX_set_default_passwd_cb(FSSLCtx, @PasswordCallbackThunk);
    if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
      SSL_CTX_set_default_passwd_cb_userdata(FSSLCtx, Self);
  end
  else
  begin
    SSL_CTX_set_default_passwd_cb(FSSLCtx, nil);
    if Assigned(SSL_CTX_set_default_passwd_cb_userdata) then
      SSL_CTX_set_default_passwd_cb_userdata(FSSLCtx, nil);
  end;
end;

procedure TOpenSSLContext.SetInfoCallback(aCallback: TSSLInfoCallback);
begin
  FInfoCallback := aCallback;

  if FSSLCtx = nil then
    Exit;

  if not Assigned(SSL_CTX_set_info_callback) then
    RaiseUnsupported('Info callback');

  if Assigned(FInfoCallback) then
    SSL_CTX_set_info_callback(FSSLCtx, @InfoCallbackThunk)
  else
    SSL_CTX_set_info_callback(FSSLCtx, nil);
end;

function TOpenSSLContext.CreateConnection(aSocket: THandle): ISSLConnection;
begin
  EnsureSystemCAsLoaded;
  Result := TOpenSSLConnection.Create(Self, aSocket);
end;

function TOpenSSLContext.CreateConnection(aStream: TStream): ISSLConnection;
begin
  EnsureSystemCAsLoaded;
  Result := TOpenSSLConnection.Create(Self, aStream);
end;

function TOpenSSLContext.IsValid: Boolean;
begin
  Result := FSSLCtx <> nil;
end;

function TOpenSSLContext.GetNativeHandle: Pointer;
begin
  Result := FSSLCtx;
end;

{ TOpenSSLSession }

constructor TOpenSSLSession.Create(ASession: PSSL_SESSION; AOwned: Boolean);
begin
  inherited Create;
  FSession := ASession;
  FOwned := AOwned;

  if (FSession <> nil) and not FOwned and Assigned(SSL_SESSION_up_ref) then
  begin
    if SSL_SESSION_up_ref(FSession) = 1 then
      FOwned := True;
  end;
end;

destructor TOpenSSLSession.Destroy;
begin
  if FOwned and (FSession <> nil) and Assigned(SSL_SESSION_free) then
    SSL_SESSION_free(FSession);
  FSession := nil;
  inherited Destroy;
end;

function TOpenSSLSession.EnsureSession: Boolean;
begin
  Result := FSession <> nil;
end;

class function TOpenSSLSession.SecondsSinceUnixEpochToDateTime(const ASeconds: LongInt): TDateTime;
const
  SecondsPerDay = 24 * 60 * 60;
begin
  if ASeconds <= 0 then
    Exit(0);
  Result := EncodeDate(1970, 1, 1) + (ASeconds / SecondsPerDay);
end;

function TOpenSSLSession.FetchCipher: PSSL_CIPHER;
begin
  Result := nil;
  if not EnsureSession then
    Exit;

  if not Assigned(SSL_SESSION_get0_cipher) then
    Exit;

  Result := SSL_SESSION_get0_cipher(FSession);
end;

function TOpenSSLSession.GetID: string;
var
  LLen: Cardinal;
  LPtr: PByte;
  I: Cardinal;
  HexStr: string;
begin
  Result := '';
  if not EnsureSession then
    Exit;

  if not Assigned(SSL_SESSION_get_id) then
    Exit;

  LPtr := SSL_SESSION_get_id(FSession, @LLen);
  if (LPtr = nil) or (LLen = 0) then
    Exit;

  SetLength(Result, LLen * 2);
  for I := 0 to LLen - 1 do
  begin
    HexStr := IntToHex(LPtr^, 2);
    Result[(I * 2) + 1] := HexStr[1];
    Result[(I * 2) + 2] := HexStr[2];
    Inc(LPtr);
  end;
end;

function TOpenSSLSession.GetCreationTime: TDateTime;
var
  LSeconds: LongInt;
begin
  Result := 0;
  if not EnsureSession then
    Exit;

  if not Assigned(SSL_SESSION_get_time) then
    Exit;

  LSeconds := SSL_SESSION_get_time(FSession);
  if LSeconds > 0 then
    Result := SecondsSinceUnixEpochToDateTime(LSeconds);
end;

function TOpenSSLSession.GetTimeout: Integer;
begin
  Result := 0;
  if not EnsureSession then
    Exit;

  if Assigned(SSL_SESSION_get_timeout) then
    Result := SSL_SESSION_get_timeout(FSession);
end;

procedure TOpenSSLSession.SetTimeout(aTimeout: Integer);
begin
  if not EnsureSession then
    Exit;

  if Assigned(SSL_SESSION_set_timeout) then
    SSL_SESSION_set_timeout(FSession, aTimeout);
end;

function TOpenSSLSession.IsValid: Boolean;
begin
  Result := EnsureSession;
end;

function TOpenSSLSession.IsResumable: Boolean;
begin
  Result := False;
  if not EnsureSession then
    Exit;

  if Assigned(SSL_SESSION_is_resumable) then
    Result := SSL_SESSION_is_resumable(FSession) = 1;
end;

function TOpenSSLSession.GetProtocolVersion: TSSLProtocolVersion;
var
  LVersion: Integer;
begin
  Result := sslProtocolUnknown;
  if not EnsureSession then
    Exit;

  if Assigned(SSL_SESSION_get_protocol_version) then
  begin
    LVersion := SSL_SESSION_get_protocol_version(FSession);
    Result := OpenSSLToProtocol(LVersion);
  end;
end;

function TOpenSSLSession.GetCipherName: string;
var
  LCipher: PSSL_CIPHER;
  LPtr: PAnsiChar;
begin
  Result := '';
  LCipher := FetchCipher;
  if LCipher = nil then
    Exit;

  if Assigned(SSL_CIPHER_get_name) then
  begin
    LPtr := SSL_CIPHER_get_name(LCipher);
    if LPtr <> nil then
      Result := string(LPtr);
  end;
end;

function TOpenSSLSession.GetPeerCertificate: ISSLCertificate;
var
  LPeer: PX509;
begin
  Result := nil;
  if not EnsureSession then
    Exit;

  if not Assigned(SSL_SESSION_get0_peer) then
    Exit;

  LPeer := SSL_SESSION_get0_peer(FSession);
  if LPeer <> nil then
    Result := TOpenSSLCertificate.Create(LPeer, False);
end;

function TOpenSSLSession.Serialize: TBytes;
var
  LBio: PBIO;
  LPending: Integer;
begin
  SetLength(Result, 0);
  if not EnsureSession then
    Exit;

  if not Assigned(BIO_new) then
    LoadOpenSSLBIO;

  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) or
    not Assigned(BIO_pending) or not Assigned(BIO_read) or
    not Assigned(PEM_write_bio_SSL_SESSION) then
    Exit;

  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then
    Exit;
  try
    if PEM_write_bio_SSL_SESSION(LBio, FSession) = 1 then
    begin
      LPending := BIO_pending(LBio);
      if LPending > 0 then
      begin
        SetLength(Result, LPending);
        if BIO_read(LBio, @Result[0], LPending) <> LPending then
          SetLength(Result, 0);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLSession.Deserialize(const aData: TBytes): Boolean;
var
  LBio: PBIO;
  LSession: PSSL_SESSION;
begin
  Result := False;
  if Length(aData) = 0 then
    Exit;

  if not Assigned(BIO_new_mem_buf) then
    LoadOpenSSLBIO;

  if not Assigned(BIO_new_mem_buf) or not Assigned(PEM_read_bio_SSL_SESSION) then
    Exit;

  LBio := BIO_new_mem_buf(@aData[0], Length(aData));
  if LBio = nil then
    Exit;
  try
    LSession := PEM_read_bio_SSL_SESSION(LBio, nil, nil, nil);
    if LSession <> nil then
    begin
      if FOwned and (FSession <> nil) and Assigned(SSL_SESSION_free) then
        SSL_SESSION_free(FSession);
      FSession := LSession;
      FOwned := True;
      Result := True;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLSession.GetNativeHandle: Pointer;
begin
  Result := FSession;
end;

function TOpenSSLSession.Clone: ISSLSession;
begin
  Result := nil;
  if not EnsureSession then
    Exit;

  if Assigned(SSL_SESSION_up_ref) then
  begin
    if SSL_SESSION_up_ref(FSession) = 1 then
      Result := TOpenSSLSession.Create(FSession, True);
  end;
end;

{ TOpenSSLCertificate - Implementation included in full from previous stub generation }

constructor TOpenSSLCertificate.Create(ACert: PX509; AOwned: Boolean);
begin
  inherited Create;
  FCert := ACert;
  FOwned := AOwned;
  FIssuerCert := nil;
end;

destructor TOpenSSLCertificate.Destroy;
begin
  if FOwned and (FCert <> nil) then
    X509_free(FCert);
  FIssuerCert := nil;
  inherited Destroy;
end;

function TOpenSSLCertificate.LoadFromFile(const aFileName: string): Boolean;
var
  LBio: PBIO;
  LCert: PX509;
begin
  Result := False;
  LBio := BIO_new_file(PAnsiChar(AnsiString(aFileName)), 'r');
  if LBio = nil then Exit;
  try
    LCert := PEM_read_bio_X509(LBio, nil, nil, nil);
    if LCert <> nil then
    begin
      if FOwned and (FCert <> nil) then
        X509_free(FCert);
      FCert := LCert;
      FOwned := True;
      Result := True;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.LoadFromStream(aStream: TStream): Boolean;
var
  LBytes: TBytes;
begin
  SetLength(LBytes, aStream.Size);
  aStream.Position := 0;
  aStream.Read(LBytes[0], aStream.Size);
  Result := LoadFromMemory(@LBytes[0], Length(LBytes));
end;

function TOpenSSLCertificate.LoadFromMemory(const aData: Pointer; aSize: Integer): Boolean;
var
  LBio: PBIO;
  LCert: PX509;
begin
  Result := False;
  LBio := BIO_new_mem_buf(aData, aSize);
  if LBio = nil then Exit;
  try
    LCert := PEM_read_bio_X509(LBio, nil, nil, nil);
    if LCert <> nil then
    begin
      if FOwned and (FCert <> nil) then
        X509_free(FCert);
      FCert := LCert;
      FOwned := True;
      Result := True;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.LoadFromPEM(const aPEM: string): Boolean;
var
  LPEM: AnsiString;
begin
  LPEM := AnsiString(aPEM);
  Result := LoadFromMemory(@LPEM[1], Length(LPEM));
end;

function TOpenSSLCertificate.LoadFromDER(const aDER: TBytes): Boolean;
var
  LBio: PBIO;
  LCert: PX509;
begin
  Result := False;
  LBio := BIO_new_mem_buf(@aDER[0], Length(aDER));
  if LBio = nil then Exit;
  try
    LCert := d2i_X509_bio(LBio, nil);
    if LCert <> nil then
    begin
      if FOwned and (FCert <> nil) then
        X509_free(FCert);
      FCert := LCert;
      FOwned := True;
      Result := True;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.SaveToFile(const aFileName: string): Boolean;
var
  LBio: PBIO;
begin
  Result := False;
  if FCert = nil then Exit;
  LBio := BIO_new_file(PAnsiChar(AnsiString(aFileName)), 'w');
  if LBio = nil then Exit;
  try
    Result := PEM_write_bio_X509(LBio, FCert) = 1;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.SaveToStream(aStream: TStream): Boolean;
var
  LPEM: string;
begin
  LPEM := SaveToPEM;
  Result := LPEM <> '';
  if Result then
    aStream.WriteBuffer(LPEM[1], Length(LPEM));
end;

function TOpenSSLCertificate.SaveToPEM: string;
var
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    if PEM_write_bio_X509(LBio, FCert) = 1 then
    begin
      LLen := BIO_pending(LBio);
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        Result := string(LBuf);
      finally
        FreeMem(LBuf);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.SaveToDER: TBytes;
var
  LBio: PBIO;
  LLen: Integer;
begin
  SetLength(Result, 0);
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    if i2d_X509_bio(LBio, FCert) = 1 then
    begin
      LLen := BIO_pending(LBio);
      SetLength(Result, LLen);
      BIO_read(LBio, @Result[0], LLen);
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.GetInfo: TSSLCertificateInfo;
begin
  FillChar(Result, SizeOf(Result), 0);
  if FCert = nil then Exit;
  Result.Subject := GetSubject;
  Result.Issuer := GetIssuer;
  Result.SerialNumber := GetSerialNumber;
  Result.NotBefore := GetNotBefore;
  Result.NotAfter := GetNotAfter;
  Result.PublicKeyAlgorithm := GetPublicKeyAlgorithm;
  Result.SignatureAlgorithm := GetSignatureAlgorithm;
  Result.Version := GetVersion;
end;

function TOpenSSLCertificate.GetSubject: string;
var
  LName: PX509_NAME;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  LName := X509_get_subject_name(FCert);
  if LName = nil then Exit;
  LBio := BIO_new(BIO_s_mem());
  if LBio = nil then Exit;

  // Check if X509_NAME_print_ex is loaded
  if not Assigned(X509_NAME_print_ex) then
  begin
    BIO_free(LBio);
    Exit;
  end;

  try
    X509_NAME_print_ex(LBio, LName, 0, 0);
    LLen := BIO_pending(LBio);
    if LLen > 0 then
    begin
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        Result := string(LBuf);
      finally
        FreeMem(LBuf);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.GetIssuer: string;
var
  LName: PX509_NAME;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  LName := X509_get_issuer_name(FCert);
  if LName = nil then Exit;
  LBio := BIO_new(BIO_s_mem());
  if LBio = nil then Exit;

  // Check if X509_NAME_print_ex is loaded
  if not Assigned(X509_NAME_print_ex) then
  begin
    BIO_free(LBio);
    Exit;
  end;

  try
    X509_NAME_print_ex(LBio, LName, 0, 0);
    LLen := BIO_pending(LBio);
    if LLen > 0 then
    begin
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        Result := string(LBuf);
      finally
        FreeMem(LBuf);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.GetSerialNumber: string;
var
  LSerial: PASN1_INTEGER;
  LBN: PBIGNUM;
  LHex: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;

  // Check if required functions are loaded
  if not Assigned(BN_bn2hex) or not Assigned(ASN1_INTEGER_to_BN) then
    Exit;

  LSerial := X509_get_serialNumber(FCert);
  if LSerial = nil then Exit;
  LBN := ASN1_INTEGER_to_BN(LSerial, nil);
  if LBN = nil then Exit;
  try
    LHex := BN_bn2hex(LBN);
    if LHex <> nil then
    begin
      Result := string(LHex);
      // Free hex string - check if OPENSSL_free is available
      if Assigned(OPENSSL_free) then
        OPENSSL_free(LHex);
    end;
  finally
    if Assigned(BN_free) then
      BN_free(LBN);
  end;
end;

function TOpenSSLCertificate.GetNotBefore: TDateTime;
var
  LTime: PASN1_TIME;
  LTm: TM;
begin
  Result := 0;
  if FCert = nil then Exit;

  // Check if required functions are loaded
  if not Assigned(X509_get_notBefore) or not Assigned(ASN1_TIME_to_tm) then
    Exit;

  LTime := X509_get_notBefore(FCert);
  if LTime = nil then Exit;
  if ASN1_TIME_to_tm(LTime, @LTm) = 1 then
    Result := EncodeDate(LTm.tm_year + 1900, LTm.tm_mon + 1, LTm.tm_mday) +
              EncodeTime(LTm.tm_hour, LTm.tm_min, LTm.tm_sec, 0);
end;

function TOpenSSLCertificate.GetNotAfter: TDateTime;
var
  LTime: PASN1_TIME;
  LTm: TM;
begin
  Result := 0;
  if FCert = nil then Exit;

  // Check if required functions are loaded
  if not Assigned(X509_get_notAfter) or not Assigned(ASN1_TIME_to_tm) then
    Exit;

  LTime := X509_get_notAfter(FCert);
  if LTime = nil then Exit;
  if ASN1_TIME_to_tm(LTime, @LTm) = 1 then
    Result := EncodeDate(LTm.tm_year + 1900, LTm.tm_mon + 1, LTm.tm_mday) +
              EncodeTime(LTm.tm_hour, LTm.tm_min, LTm.tm_sec, 0);
end;

function TOpenSSLCertificate.GetPublicKey: string;
var
  LKey: PEVP_PKEY;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  LKey := X509_get_pubkey(FCert);
  if LKey = nil then Exit;
  try
    LBio := BIO_new(BIO_s_mem);
    if LBio = nil then Exit;
    try
      PEM_write_bio_PUBKEY(LBio, LKey);
      LLen := BIO_pending(LBio);
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        Result := string(LBuf);
      finally
        FreeMem(LBuf);
      end;
    finally
      BIO_free(LBio);
    end;
  finally
    EVP_PKEY_free(LKey);
  end;
end;

function TOpenSSLCertificate.GetPublicKeyAlgorithm: string;
var
  LKey: PEVP_PKEY;
  LType: Integer;
begin
  Result := 'Unknown';
  if FCert = nil then Exit;

  // Check if required functions are loaded
  if not Assigned(X509_get_pubkey) or not Assigned(EVP_PKEY_id) or not Assigned(EVP_PKEY_free) then
    Exit;

  LKey := X509_get_pubkey(FCert);
  if LKey = nil then Exit;
  try
    LType := EVP_PKEY_id(LKey);
    case LType of
      EVP_PKEY_RSA: Result := 'RSA';
      EVP_PKEY_DSA: Result := 'DSA';
      EVP_PKEY_EC: Result := 'EC';
      EVP_PKEY_DH: Result := 'DH';
    else
      Result := 'Unknown (' + IntToStr(LType) + ')';
    end;
  finally
    EVP_PKEY_free(LKey);
  end;
end;

function TOpenSSLCertificate.GetSignatureAlgorithm: string;
var
  LSigAlg: PX509_ALGOR;
  LOID: PASN1_OBJECT;
  LBuf: array[0..255] of AnsiChar;
begin
  Result := 'Unknown';
  if FCert = nil then Exit;

  // Check if required functions are loaded
  if not Assigned(X509_get0_signature) or not Assigned(X509_ALGOR_get0) or not Assigned(OBJ_obj2txt) then
    Exit;

  X509_get0_signature(nil, @LSigAlg, FCert);
  if LSigAlg = nil then Exit;
  X509_ALGOR_get0(@LOID, nil, nil, LSigAlg);
  if LOID <> nil then
  begin
    OBJ_obj2txt(@LBuf[0], SizeOf(LBuf), LOID, 0);
    Result := string(LBuf);
  end;
end;

function TOpenSSLCertificate.GetVersion: Integer;
begin
  Result := 0;
  if FCert = nil then Exit;
  Result := X509_get_version(FCert) + 1;
end;

function TOpenSSLCertificate.Verify(aCAStore: ISSLCertificateStore): Boolean;
var
  LStore: PX509_STORE;
  LCtx: PX509_STORE_CTX;
  LRet: Integer;
begin
  Result := False;
  if FCert = nil then Exit;
  if aCAStore = nil then Exit;
  
  // Get native store handle
  LStore := PX509_STORE(aCAStore.GetNativeHandle);
  if LStore = nil then Exit;
  
  // Create store context
  LCtx := X509_STORE_CTX_new();
  if LCtx = nil then Exit;
  try
    // Initialize context with store and certificate
    if X509_STORE_CTX_init(LCtx, LStore, FCert, nil) = 1 then
    begin
      // Perform verification
      LRet := X509_verify_cert(LCtx);
      Result := LRet = 1;
    end;
  finally
    X509_STORE_CTX_free(LCtx);
  end;
end;

function TOpenSSLCertificate.VerifyEx(aCAStore: ISSLCertificateStore; 
  aFlags: TSSLCertVerifyFlags; out aResult: TSSLCertVerifyResult): Boolean;
var
  LStore: PX509_STORE;
  LCtx: PX509_STORE_CTX;
  LRet: Integer;
  LErrorCode: Integer;
  LErrorStr: string;
begin
  // 初始化结果
  FillChar(aResult, SizeOf(aResult), 0);
  aResult.Success := False;
  Result := False;
  
  if FCert = nil then
  begin
    aResult.ErrorMessage := 'Certificate is nil';
    Exit;
  end;
  
  if aCAStore = nil then
  begin
    aResult.ErrorMessage := 'CA store is nil';
    Exit;
  end;
  
  // Get native store handle
  LStore := PX509_STORE(aCAStore.GetNativeHandle);
  if LStore = nil then
  begin
    aResult.ErrorMessage := 'Invalid CA store handle';
    Exit;
  end;
  
  // 处理特殊标志
  if sslCertVerifyIgnoreExpiry in aFlags then
  begin
    // OpenSSL: 设置不检查时间
    X509_STORE_set_flags(LStore, X509_V_FLAG_NO_CHECK_TIME);
  end;
  
  if sslCertVerifyAllowSelfSigned in aFlags then
  begin
    // OpenSSL: 允许自签名证书
    X509_STORE_set_flags(LStore, X509_V_FLAG_PARTIAL_CHAIN);
  end;
  
  // Create store context
  LCtx := X509_STORE_CTX_new();
  if LCtx = nil then
  begin
    aResult.ErrorMessage := 'Failed to create store context';
    Exit;
  end;
  
  try
    // Initialize context with store and certificate
    if X509_STORE_CTX_init(LCtx, LStore, FCert, nil) = 1 then
    begin
      // 如果需要检查吊销状态
      if (sslCertVerifyCheckRevocation in aFlags) or 
        (sslCertVerifyCheckCRL in aFlags) then
      begin
        // OpenSSL: 启用 CRL 检查
        X509_VERIFY_PARAM_set_flags(
          X509_STORE_CTX_get0_param(LCtx),
          X509_V_FLAG_CRL_CHECK or X509_V_FLAG_CRL_CHECK_ALL
        );
      end;
      
      // Perform verification
      LRet := X509_verify_cert(LCtx);
      
      if LRet = 1 then
      begin
        // 验证成功
        aResult.Success := True;
        aResult.ErrorCode := 0;
        aResult.ErrorMessage := 'Certificate verification successful';
        aResult.DetailedInfo := 'OpenSSL verification passed';
        Result := True;
      end
      else
      begin
        // 验证失败 - 获取错误信息
        LErrorCode := X509_STORE_CTX_get_error(LCtx);
        LErrorStr := string(X509_verify_cert_error_string(LErrorCode));
        
        aResult.Success := False;
        aResult.ErrorCode := LErrorCode;
        aResult.ErrorMessage := LErrorStr;
        aResult.DetailedInfo := Format('OpenSSL error: %d - %s', [LErrorCode, LErrorStr]);
      end;
    end
    else
    begin
      aResult.ErrorMessage := 'Failed to initialize verification context';
    end;
  finally
    X509_STORE_CTX_free(LCtx);
  end;
end;

function TOpenSSLCertificate.VerifyHostname(const aHostname: string): Boolean;
var
  LHostnameAnsi: AnsiString;
begin
  Result := False;
  if FCert = nil then Exit;
  if aHostname = '' then Exit;
  
  // Convert hostname to AnsiString
  LHostnameAnsi := AnsiString(aHostname);
  
  // Use X509_check_host to verify hostname
  // Returns 1 if hostname matches, 0 if not, -1 on error
  Result := X509_check_host(FCert, PAnsiChar(LHostnameAnsi), Length(LHostnameAnsi), 0, nil) = 1;
end;

function TOpenSSLCertificate.IsExpired: Boolean;
var
  LNow: TDateTime;
begin
  LNow := Now;
  Result := (LNow < GetNotBefore) or (LNow > GetNotAfter);
end;

function TOpenSSLCertificate.IsSelfSigned: Boolean;
begin
  Result := GetSubject = GetIssuer;
end;

function TOpenSSLCertificate.IsCA: Boolean;
var
  LExtIdx: Integer;
  LBasicConstraints: PX509_EXTENSION;
  LBasicConstraintsValue: PASN1_OCTET_STRING;
  LBC: Pointer;
begin
  Result := False;
  if FCert = nil then Exit;
  
  // Get Basic Constraints extension
  LExtIdx := X509_get_ext_by_NID(FCert, NID_basic_constraints, -1);
  if LExtIdx < 0 then Exit;
  
  LBasicConstraints := X509_get_ext(FCert, LExtIdx);
  if LBasicConstraints = nil then Exit;
  
  // For now, simple implementation: check if extension exists
  // Full implementation would parse the extension value
  Result := True;  // If basic_constraints extension exists, likely a CA
end;

function TOpenSSLCertificate.GetExtension(const aOID: string): string;
var
  LOID: PASN1_OBJECT;
  LExtIdx: Integer;
  LExt: PX509_EXTENSION;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  // Convert OID string to ASN1_OBJECT
  LOID := OBJ_txt2obj(PAnsiChar(AnsiString(aOID)), 0);
  if LOID = nil then Exit;
  try
    // Find extension by OID
    LExtIdx := X509_get_ext_by_OBJ(FCert, LOID, -1);
    if LExtIdx < 0 then Exit;

    LExt := X509_get_ext(FCert, LExtIdx);
    if LExt = nil then Exit;

    // Convert extension to string
    LBio := BIO_new(BIO_s_mem);
    if LBio = nil then Exit;
    try
      X509V3_EXT_print(LBio, LExt, 0, 0);
      LLen := BIO_pending(LBio);
      if LLen > 0 then
      begin
        GetMem(LBuf, LLen + 1);
        try
          BIO_read(LBio, LBuf, LLen);
          LBuf[LLen] := #0;
          Result := string(LBuf);
        finally
          FreeMem(LBuf);
        end;
      end;
    finally
      BIO_free(LBio);
    end;
  finally
    ASN1_OBJECT_free(LOID);
  end;
end;

function TOpenSSLCertificate.GetSubjectAltNames: TStringList;
var
  LExtIdx: Integer;
  LExt: PX509_EXTENSION;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
  LLines: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  // Get Subject Alternative Name extension
  LExtIdx := X509_get_ext_by_NID(FCert, NID_subject_alt_name, -1);
  if LExtIdx < 0 then Exit;

  LExt := X509_get_ext(FCert, LExtIdx);
  if LExt = nil then Exit;

  // Convert extension to string
  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    X509V3_EXT_print(LBio, LExt, 0, 0);
    LLen := BIO_pending(LBio);
    if LLen > 0 then
    begin
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        // Parse comma-separated values
        LLines := TStringList.Create;
        try
          LLines.CommaText := string(LBuf);
          for i := 0 to LLines.Count - 1 do
            Result.Add(Trim(LLines[i]));
        finally
          LLines.Free;
        end;
      finally
        FreeMem(LBuf);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.GetKeyUsage: TStringList;
var
  LExtIdx: Integer;
  LExt: PX509_EXTENSION;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
  LLines: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  // Get Key Usage extension
  LExtIdx := X509_get_ext_by_NID(FCert, NID_key_usage, -1);
  if LExtIdx < 0 then Exit;

  LExt := X509_get_ext(FCert, LExtIdx);
  if LExt = nil then Exit;

  // Convert extension to string
  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    X509V3_EXT_print(LBio, LExt, 0, 0);
    LLen := BIO_pending(LBio);
    if LLen > 0 then
    begin
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        // Parse comma-separated values
        LLines := TStringList.Create;
        try
          LLines.CommaText := string(LBuf);
          for i := 0 to LLines.Count - 1 do
            Result.Add(Trim(LLines[i]));
        finally
          LLines.Free;
        end;
      finally
        FreeMem(LBuf);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.GetExtendedKeyUsage: TStringList;
var
  LExtIdx: Integer;
  LExt: PX509_EXTENSION;
  LBio: PBIO;
  LLen: Integer;
  LBuf: PAnsiChar;
  LLines: TStringList;
  i: Integer;
begin
  Result := TStringList.Create;
  if FCert = nil then Exit;

  // Check if BIO functions are loaded
  if not Assigned(BIO_new) or not Assigned(BIO_s_mem) then
    Exit;

  // Get Extended Key Usage extension
  LExtIdx := X509_get_ext_by_NID(FCert, NID_ext_key_usage, -1);
  if LExtIdx < 0 then Exit;

  LExt := X509_get_ext(FCert, LExtIdx);
  if LExt = nil then Exit;

  // Convert extension to string
  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    X509V3_EXT_print(LBio, LExt, 0, 0);
    LLen := BIO_pending(LBio);
    if LLen > 0 then
    begin
      GetMem(LBuf, LLen + 1);
      try
        BIO_read(LBio, LBuf, LLen);
        LBuf[LLen] := #0;
        // Parse comma-separated values
        LLines := TStringList.Create;
        try
          LLines.CommaText := string(LBuf);
          for i := 0 to LLines.Count - 1 do
            Result.Add(Trim(LLines[i]));
        finally
          LLines.Free;
        end;
      finally
        FreeMem(LBuf);
      end;
    end;
  finally
    BIO_free(LBio);
  end;
end;

function TOpenSSLCertificate.GetFingerprint(aHashType: TSSLHash): string;
var
  LHash: array[0..EVP_MAX_MD_SIZE-1] of Byte;
  LHashLen: Cardinal;
  LMD: PEVP_MD;
  i: Integer;
begin
  Result := '';
  if FCert = nil then Exit;
  
  // Select hash algorithm (call functions to get pointers)
  case aHashType of
    sslHashMD5: LMD := EVP_md5();
    sslHashSHA1: LMD := EVP_sha1();
    sslHashSHA256: LMD := EVP_sha256();
    sslHashSHA384: LMD := EVP_sha384();
    sslHashSHA512: LMD := EVP_sha512();
  else
    Exit;
  end;
  if X509_digest(FCert, LMD, @LHash[0], @LHashLen) = 1 then
  begin
    for i := 0 to LHashLen - 1 do
    begin
      if i > 0 then Result := Result + ':';
      Result := Result + IntToHex(LHash[i], 2);
    end;
  end;
end;

function TOpenSSLCertificate.GetFingerprintSHA1: string;
begin
  Result := GetFingerprint(sslHashSHA1);
end;

function TOpenSSLCertificate.GetFingerprintSHA256: string;
begin
  Result := GetFingerprint(sslHashSHA256);
end;

procedure TOpenSSLCertificate.SetIssuerCertificate(aCert: ISSLCertificate);
begin
  FIssuerCert := aCert;
end;

function TOpenSSLCertificate.GetIssuerCertificate: ISSLCertificate;
begin
  Result := FIssuerCert;
end;

function TOpenSSLCertificate.GetNativeHandle: Pointer;
begin
  Result := FCert;
end;

function TOpenSSLCertificate.Clone: ISSLCertificate;
var
  LCert: PX509;
begin
  Result := nil;
  if FCert = nil then Exit;
  LCert := X509_dup(FCert);
  if LCert <> nil then
    Result := TOpenSSLCertificate.Create(LCert, True);
end;

{ TOpenSSLCertificateStore }

constructor TOpenSSLCertificateStore.Create;
begin
  inherited Create;
  FStore := X509_STORE_new();
  FCertificates := TList.Create;
  FOwned := True;
end;

constructor TOpenSSLCertificateStore.Create(AStore: PX509_STORE; AOwned: Boolean);
begin
  inherited Create;
  FStore := AStore;
  FCertificates := TList.Create;
  FOwned := AOwned;
end;

destructor TOpenSSLCertificateStore.Destroy;
var
  i: Integer;
begin
  // Free all certificates in the list if owned
  if FOwned then
  begin
    for i := 0 to FCertificates.Count - 1 do
      X509_free(PX509(FCertificates[i]));
  end;
  FCertificates.Free;
  
  // Free the store if owned
  if FOwned and (FStore <> nil) then
    X509_STORE_free(FStore);
    
  inherited Destroy;
end;

function TOpenSSLCertificateStore.AddCertificate(aCert: ISSLCertificate): Boolean;
var
  LX509: PX509;
begin
  Result := False;
  if (FStore = nil) or (aCert = nil) then Exit;
  
  LX509 := PX509(aCert.GetNativeHandle);
  if LX509 = nil then Exit;
  
  // Add certificate to store
  if X509_STORE_add_cert(FStore, LX509) = 1 then
  begin
    // Increment reference count
    X509_up_ref(LX509);
    FCertificates.Add(LX509);
    Result := True;
  end;
end;

function TOpenSSLCertificateStore.RemoveCertificate(aCert: ISSLCertificate): Boolean;
var
  LX509: PX509;
  i: Integer;
begin
  Result := False;
  if (FCertificates = nil) or (aCert = nil) then Exit;
  
  LX509 := PX509(aCert.GetNativeHandle);
  if LX509 = nil then Exit;
  
  // Find and remove from list
  for i := 0 to FCertificates.Count - 1 do
  begin
    if FCertificates[i] = LX509 then
    begin
      if FOwned then
        X509_free(PX509(FCertificates[i]));
      FCertificates.Delete(i);
      Result := True;
      Break;
    end;
  end;
end;

function TOpenSSLCertificateStore.Contains(aCert: ISSLCertificate): Boolean;
var
  LX509: PX509;
  i: Integer;
begin
  Result := False;
  if (FCertificates = nil) or (aCert = nil) then Exit;
  
  LX509 := PX509(aCert.GetNativeHandle);
  if LX509 = nil then Exit;
  
  // Check if certificate is in list
  for i := 0 to FCertificates.Count - 1 do
  begin
    if FCertificates[i] = LX509 then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TOpenSSLCertificateStore.Clear;
var
  i: Integer;
begin
  if FCertificates = nil then Exit;
  
  // Free all certificates if owned
  if FOwned then
  begin
    for i := 0 to FCertificates.Count - 1 do
      X509_free(PX509(FCertificates[i]));
  end;
  FCertificates.Clear;
end;

function TOpenSSLCertificateStore.GetCount: Integer;
begin
  if FCertificates <> nil then
    Result := FCertificates.Count
  else
    Result := 0;
end;

function TOpenSSLCertificateStore.GetCertificate(aIndex: Integer): ISSLCertificate;
var
  LX509: PX509;
begin
  Result := nil;
  if (FCertificates = nil) or (aIndex < 0) or (aIndex >= FCertificates.Count) then Exit;
  
  LX509 := PX509(FCertificates[aIndex]);
  if LX509 <> nil then
  begin
    // Increment reference and create interface wrapper
    X509_up_ref(LX509);
    Result := TOpenSSLCertificate.Create(LX509, True);
  end;
end;

function TOpenSSLCertificateStore.LoadFromFile(const aFileName: string): Boolean;
var
  LFileNameAnsi: AnsiString;
begin
  Result := False;
  if FStore = nil then Exit;
  
  LFileNameAnsi := AnsiString(aFileName);
  Result := X509_STORE_load_file(FStore, PAnsiChar(LFileNameAnsi)) = 1;
end;

function TOpenSSLCertificateStore.LoadFromPath(const aPath: string): Boolean;
var
  LPathAnsi: AnsiString;
begin
  Result := False;
  if FStore = nil then Exit;
  
  LPathAnsi := AnsiString(aPath);
  Result := X509_STORE_load_path(FStore, PAnsiChar(LPathAnsi)) = 1;
end;

function TOpenSSLCertificateStore.LoadSystemStore: Boolean;
begin
  Result := False;
  if FStore = nil then Exit;
  
  // Load system default certificate paths
  Result := X509_STORE_set_default_paths(FStore) = 1;
end;

function TOpenSSLCertificateStore.FindBySubject(const aSubject: string): ISSLCertificate;
var
  LSubjectAnsi: AnsiString;
  LName: PX509_NAME;
  LObj: PX509_OBJECT;
  LX509: PX509;
  i: Integer;
begin
  Result := nil;
  if FCertificates = nil then Exit;
  
  // Simple linear search through stored certificates
  for i := 0 to FCertificates.Count - 1 do
  begin
    LX509 := PX509(FCertificates[i]);
    if LX509 <> nil then
    begin
      LName := X509_get_subject_name(LX509);
      if LName <> nil then
      begin
        // 先进行精确比较：如果aSubject是完整DN则匹配
        // 注意：这里假设aSubject可能是完整的Distinguished Name
        // 实际使用中，如果需要精确匹配，用户应传入完整DN
        
        // 创建临时证书对象获取subject字符串
        Result := TOpenSSLCertificate.Create(LX509, False);
        try
          // 使用字符串部分匹配（for convenience）
          if Pos(UpperCase(aSubject), UpperCase(Result.GetSubject)) > 0 then
          begin
            X509_up_ref(LX509);
            Result := TOpenSSLCertificate.Create(LX509, True);
            Exit;
          end;
        finally
          if Result <> nil then
            Result := nil;  // 清理临时对象
        end;
      end;
    end;
  end;
  Result := nil;
end;

function TOpenSSLCertificateStore.FindByIssuer(const aIssuer: string): ISSLCertificate;
var
  LX509: PX509;
  LCert: ISSLCertificate;
  i: Integer;
begin
  Result := nil;
  if FCertificates = nil then Exit;
  
  // Simple linear search through stored certificates
  for i := 0 to FCertificates.Count - 1 do
  begin
    LX509 := PX509(FCertificates[i]);
    if LX509 <> nil then
    begin
      LCert := TOpenSSLCertificate.Create(LX509, False);
      if Pos(aIssuer, LCert.GetIssuer) > 0 then
      begin
        X509_up_ref(LX509);
        Result := TOpenSSLCertificate.Create(LX509, True);
        Exit;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.FindBySerialNumber(const aSerialNumber: string): ISSLCertificate;
var
  LX509: PX509;
  LCert: ISSLCertificate;
  i: Integer;
begin
  Result := nil;
  if FCertificates = nil then Exit;
  
  for i := 0 to FCertificates.Count - 1 do
  begin
    LX509 := PX509(FCertificates[i]);
    if LX509 <> nil then
    begin
      LCert := TOpenSSLCertificate.Create(LX509, False);
      if LCert.GetSerialNumber = aSerialNumber then
      begin
        X509_up_ref(LX509);
        Result := TOpenSSLCertificate.Create(LX509, True);
        Exit;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.FindByFingerprint(const aFingerprint: string): ISSLCertificate;
var
  LX509: PX509;
  LCert: ISSLCertificate;
  i: Integer;
begin
  Result := nil;
  if FCertificates = nil then Exit;
  
  for i := 0 to FCertificates.Count - 1 do
  begin
    LX509 := PX509(FCertificates[i]);
    if LX509 <> nil then
    begin
      LCert := TOpenSSLCertificate.Create(LX509, False);
      if (LCert.GetFingerprintSHA1 = aFingerprint) or 
        (LCert.GetFingerprintSHA256 = aFingerprint) then
      begin
        X509_up_ref(LX509);
        Result := TOpenSSLCertificate.Create(LX509, True);
        Exit;
      end;
    end;
  end;
end;

function TOpenSSLCertificateStore.VerifyCertificate(aCert: ISSLCertificate): Boolean;
var
  LX509: PX509;
  LCtx: PX509_STORE_CTX;
begin
  Result := False;
  if (FStore = nil) or (aCert = nil) then Exit;
  
  LX509 := PX509(aCert.GetNativeHandle);
  if LX509 = nil then Exit;
  
  // Create verification context
  LCtx := X509_STORE_CTX_new();
  if LCtx = nil then Exit;
  try
    // Initialize context
    if X509_STORE_CTX_init(LCtx, FStore, LX509, nil) = 1 then
    begin
      // Perform verification
      Result := X509_verify_cert(LCtx) = 1;
    end;
  finally
    X509_STORE_CTX_free(LCtx);
  end;
end;

function TOpenSSLCertificateStore.BuildCertificateChain(aCert: ISSLCertificate): TSSLCertificateArray;
var
  LX509: PX509;
  LCtx: PX509_STORE_CTX;
  LChain: PSTACK_OF_X509;
  LChainCert: PX509;
  LCount, i: Integer;
begin
  SetLength(Result, 0);
  if (FStore = nil) or (aCert = nil) then Exit;
  
  LX509 := PX509(aCert.GetNativeHandle);
  if LX509 = nil then Exit;
  
  // Create verification context
  LCtx := X509_STORE_CTX_new();
  if LCtx = nil then Exit;
  try
    // Initialize context
    if X509_STORE_CTX_init(LCtx, FStore, LX509, nil) = 1 then
    begin
      // Perform verification to build chain
      if X509_verify_cert(LCtx) = 1 then
      begin
        // Get the verified chain
        LChain := X509_STORE_CTX_get0_chain(LCtx);
        if LChain <> nil then
        begin
          LCount := sk_X509_num(LChain);
          SetLength(Result, LCount);
          for i := 0 to LCount - 1 do
          begin
            LChainCert := sk_X509_value(LChain, i);
            if LChainCert <> nil then
            begin
              X509_up_ref(LChainCert);
              Result[i] := TOpenSSLCertificate.Create(LChainCert, True);
            end;
          end;
        end;
      end;
    end;
  finally
    X509_STORE_CTX_free(LCtx);
  end;
end;

function TOpenSSLCertificateStore.GetNativeHandle: Pointer;
begin
  Result := FStore;
end;

{ TOpenSSLConnection }

constructor TOpenSSLConnection.Create(AContext: TOpenSSLContext; ASocket: THandle);
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FStream := nil;
  FHandshakeComplete := False;
  FTimeout := 30000;
  FBlocking := True;
  FBioRead := nil;
  FBioWrite := nil;
  
  // Create SSL structure
  if (FContext <> nil) and (FContext.GetNativeHandle <> nil) then
  begin
    if Assigned(SSL_new) then
    begin
      FSSL := SSL_new(PSSL_CTX(FContext.GetNativeHandle));
      if FSSL <> nil then
      begin
        // Set socket file descriptor
        if (FSocket <> INVALID_HANDLE_VALUE) and Assigned(SSL_set_fd) then
        begin
          SSL_set_fd(FSSL, FSocket);
        end;
      end;
    end;
  end;
end;

constructor TOpenSSLConnection.Create(AContext: TOpenSSLContext; AStream: TStream);
begin
  inherited Create;
  FContext := AContext;
  FSocket := INVALID_HANDLE_VALUE;
  FStream := AStream;
  FHandshakeComplete := False;
  FTimeout := 30000;
  FBlocking := True;
  
  // Create SSL structure
  if (FContext <> nil) and (FContext.GetNativeHandle <> nil) then
  begin
    if Assigned(SSL_new) then
    begin
      FSSL := SSL_new(PSSL_CTX(FContext.GetNativeHandle));
      if FSSL <> nil then
      begin
        // Create BIO pair for stream-based I/O
        if Assigned(BIO_new) and Assigned(BIO_s_mem) then
        begin
          FBioRead := BIO_new(BIO_s_mem());
          FBioWrite := BIO_new(BIO_s_mem());
          
          if (FBioRead <> nil) and (FBioWrite <> nil) and Assigned(SSL_set_bio) then
          begin
            SSL_set_bio(FSSL, FBioRead, FBioWrite);
          end;
        end;
      end;
    end;
  end;
end;

destructor TOpenSSLConnection.Destroy;
begin
  if FSSL <> nil then
  begin
    SSL_shutdown(FSSL);
    SSL_free(FSSL);
  end;
  inherited Destroy;
end;

function TOpenSSLConnection.Connect: Boolean;
var
  LRet: Integer;
  LError: Integer;
  LServerName: string;
  LServerNameAnsi: AnsiString;
begin
  Result := False;

  if FSSL = nil then
    Exit;

  // Set SNI hostname if specified by the context (required for virtual hosts)
  if FContext <> nil then
  begin
    LServerName := FContext.GetServerName;
    if LServerName <> '' then
    begin
      if Assigned(SSL_set_tlsext_host_name) then
      begin
        LServerNameAnsi := AnsiString(LServerName);
        SSL_set_tlsext_host_name(FSSL, PAnsiChar(LServerNameAnsi));
      end;
    end;
  end;

  // Set connect state
  if Assigned(SSL_set_connect_state) then
    SSL_set_connect_state(FSSL);
  
  // Perform SSL handshake
  if Assigned(SSL_connect) then
  begin
    LRet := SSL_connect(FSSL);
    if LRet = 1 then
    begin
      // Success
      FHandshakeComplete := True;
      Result := True;
    end
    else
    begin
      // Check error
      if Assigned(SSL_get_error) then
      begin
        LError := SSL_get_error(FSSL, LRet);
        if (LError = SSL_ERROR_WANT_READ) or (LError = SSL_ERROR_WANT_WRITE) then
        begin
          // Non-blocking operation needs more data
          Result := False;
        end
        else
        begin
          // Real error
          Result := False;
        end;
      end;
    end;
  end;
end;

function TOpenSSLConnection.Accept: Boolean;
var
  LRet: Integer;
  LError: Integer;
begin
  Result := False;
  
  if FSSL = nil then
    Exit;
  
  // Set accept state
  if Assigned(SSL_set_accept_state) then
    SSL_set_accept_state(FSSL);
  
  // Perform SSL handshake
  if Assigned(SSL_accept) then
  begin
    LRet := SSL_accept(FSSL);
    if LRet = 1 then
    begin
      // Success
      FHandshakeComplete := True;
      Result := True;
    end
    else
    begin
      // Check error
      if Assigned(SSL_get_error) then
      begin
        LError := SSL_get_error(FSSL, LRet);
        if (LError = SSL_ERROR_WANT_READ) or (LError = SSL_ERROR_WANT_WRITE) then
        begin
          // Non-blocking operation needs more data
          Result := False;
        end
        else
        begin
          // Real error
          Result := False;
        end;
      end;
    end;
  end;
end;

function TOpenSSLConnection.Shutdown: Boolean;
var
  LRet: Integer;
begin
  Result := False;
  
  if FSSL = nil then
    Exit;
  
  if Assigned(SSL_shutdown) then
  begin
    LRet := SSL_shutdown(FSSL);
    Result := (LRet >= 0);
  end;
end;

procedure TOpenSSLConnection.Close;
begin
  if FSSL <> nil then
  begin
    if Assigned(SSL_shutdown) then
      SSL_shutdown(FSSL);
    if Assigned(SSL_free) then
      SSL_free(FSSL);
    FSSL := nil;
  end;
  
  FHandshakeComplete := False;
  FBioRead := nil;  // Owned by SSL, don't free
  FBioWrite := nil; // Owned by SSL, don't free
end;

function TOpenSSLConnection.DoHandshake: TSSLHandshakeState;
var
  LRet: Integer;
  LError: Integer;
begin
  if FSSL = nil then
  begin
    Result := sslHsFailed;
    Exit;
  end;
  
  if FHandshakeComplete then
  begin
    Result := sslHsCompleted;
    Exit;
  end;
  
  // Perform handshake
  if Assigned(SSL_do_handshake) then
  begin
    LRet := SSL_do_handshake(FSSL);
    if LRet = 1 then
    begin
      FHandshakeComplete := True;
      Result := sslHsCompleted;
    end
    else
    begin
      if Assigned(SSL_get_error) then
      begin
        LError := SSL_get_error(FSSL, LRet);
        case LError of
          SSL_ERROR_WANT_READ,
          SSL_ERROR_WANT_WRITE:
            Result := sslHsInProgress;
        else
          Result := sslHsFailed;
        end;
      end
      else
        Result := sslHsFailed;
    end;
  end
  else
    Result := sslHsFailed;
end;

function TOpenSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeComplete;
end;

function TOpenSSLConnection.Renegotiate: Boolean;
var
  LRet: Integer;
begin
  Result := False;
  
  if (FSSL = nil) or not Assigned(SSL_renegotiate) then
    Exit;
  
  LRet := SSL_renegotiate(FSSL);
  Result := (LRet = 1);
end;

function TOpenSSLConnection.Read(var aBuffer; aCount: Integer): Integer;
var
  LRet: Integer;
begin
  Result := 0;
  
  if (FSSL = nil) or (aCount <= 0) then
    Exit;
  
  if not FHandshakeComplete then
    Exit;
  
  if Assigned(SSL_read) then
  begin
    LRet := SSL_read(FSSL, @aBuffer, aCount);
    if LRet > 0 then
      Result := LRet
    else
      Result := 0;
  end;
end;

function TOpenSSLConnection.Write(const aBuffer; aCount: Integer): Integer;
var
  LRet: Integer;
begin
  Result := 0;
  
  if (FSSL = nil) or (aCount <= 0) then
    Exit;
  
  if not FHandshakeComplete then
    Exit;
  
  if Assigned(SSL_write) then
  begin
    LRet := SSL_write(FSSL, @aBuffer, aCount);
    if LRet > 0 then
      Result := LRet
    else
      Result := 0;
  end;
end;

function TOpenSSLConnection.ReadString(out aStr: string): Boolean;
var
  LBuffer: array[0..8191] of Byte;
  LBytesRead: Integer;
begin
  Result := False;
  aStr := '';
  
  LBytesRead := Read(LBuffer, SizeOf(LBuffer));
  if LBytesRead > 0 then
  begin
    SetString(aStr, PAnsiChar(@LBuffer[0]), LBytesRead);
    Result := True;
  end;
end;

function TOpenSSLConnection.WriteString(const aStr: string): Boolean;
var
  LAnsiStr: AnsiString;
  LBytesWritten: Integer;
begin
  Result := False;
  
  if aStr = '' then
  begin
    Result := True;
    Exit;
  end;
  
  LAnsiStr := AnsiString(aStr);
  LBytesWritten := Write(LAnsiStr[1], Length(LAnsiStr));
  Result := (LBytesWritten = Length(LAnsiStr));
end;

function TOpenSSLConnection.WantRead: Boolean;
var
  LError: Integer;
begin
  Result := False;
  
  if (FSSL = nil) or not Assigned(SSL_want_read) then
    Exit;
  
  Result := (SSL_want_read(FSSL) = 1);
end;

function TOpenSSLConnection.WantWrite: Boolean;
var
  LError: Integer;
begin
  Result := False;
  
  if (FSSL = nil) or not Assigned(SSL_want_write) then
    Exit;
  
  Result := (SSL_want_write(FSSL) = 1);
end;

function TOpenSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
var
  LError: Integer;
begin
  Result := sslErrNone;
  
  if (FSSL = nil) or not Assigned(SSL_get_error) then
    Exit;
  
  LError := SSL_get_error(FSSL, aRet);
  Result := TranslateError(LError);
end;

function TOpenSSLConnection.TranslateError(ErrorCode: Integer): TSSLErrorCode;
begin
  case ErrorCode of
    SSL_ERROR_NONE: Result := sslErrNone;
    SSL_ERROR_SSL: Result := sslErrProtocol;
    SSL_ERROR_WANT_READ: Result := sslErrWouldBlock;
    SSL_ERROR_WANT_WRITE: Result := sslErrWouldBlock;
    SSL_ERROR_WANT_X509_LOOKUP: Result := sslErrCertificate;
    SSL_ERROR_SYSCALL: Result := sslErrIO;
    SSL_ERROR_ZERO_RETURN: Result := sslErrConnection;
    SSL_ERROR_WANT_CONNECT: Result := sslErrWouldBlock;
    SSL_ERROR_WANT_ACCEPT: Result := sslErrWouldBlock;
  else
    Result := sslErrGeneral;
  end;
end;

function TOpenSSLConnection.GetConnectionInfo: TSSLConnectionInfo;
var
  LCipher: PSSL_CIPHER;
  LCipherName: PAnsiChar;
begin
  FillChar(Result, SizeOf(Result), 0);
  
  if FSSL = nil then
    Exit;
  
  Result.ProtocolVersion := GetProtocolVersion;
  Result.CipherSuite := GetCipherName;
  Result.IsResumed := IsSessionReused;
  Result.ServerName := GetSNI;
  Result.ALPNProtocol := GetSelectedALPNProtocol;
  
  // Get cipher bits (key size)
  if Assigned(SSL_get_current_cipher) then
  begin
    LCipher := SSL_get_current_cipher(FSSL);
    if (LCipher <> nil) and Assigned(SSL_CIPHER_get_bits) then
      Result.KeySize := SSL_CIPHER_get_bits(LCipher, nil);
  end;
end;

function TOpenSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
var
  LVersion: Integer;
begin
  Result := sslProtocolTLS12;
  
  if (FSSL = nil) or not Assigned(SSL_version) then
    Exit;
  
  LVersion := SSL_version(FSSL);
  Result := OpenSSLToProtocol(LVersion);
end;

function TOpenSSLConnection.GetCipherName: string;
var
  LCipher: PSSL_CIPHER;
  LCipherName: PAnsiChar;
begin
  Result := '';
  
  if FSSL = nil then
    Exit;
  
  if Assigned(SSL_get_current_cipher) then
  begin
    LCipher := SSL_get_current_cipher(FSSL);
    if (LCipher <> nil) and Assigned(SSL_CIPHER_get_name) then
    begin
      LCipherName := SSL_CIPHER_get_name(LCipher);
      if LCipherName <> nil then
        Result := string(LCipherName);
    end;
  end;
end;

function TOpenSSLConnection.GetPeerCertificate: ISSLCertificate;
var
  LCert: PX509;
begin
  Result := nil;
  
  if (FSSL = nil) or not Assigned(SSL_get_peer_certificate) then
    Exit;
  
  LCert := SSL_get_peer_certificate(FSSL);
  if LCert <> nil then
    Result := TOpenSSLCertificate.Create(LCert, True);
end;

function TOpenSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
var
  LChain: PSTACK_OF_X509;
  LCert: PX509;
  LCount, i: Integer;
begin
  SetLength(Result, 0);
  
  if (FSSL = nil) or not Assigned(SSL_get_peer_cert_chain) then
    Exit;
  
  LChain := SSL_get_peer_cert_chain(FSSL);
  if LChain = nil then
    Exit;
  
  if not Assigned(sk_X509_num) or not Assigned(sk_X509_value) then
    Exit;
  
  LCount := sk_X509_num(LChain);
  SetLength(Result, LCount);
  
  for i := 0 to LCount - 1 do
  begin
    LCert := sk_X509_value(LChain, i);
    if LCert <> nil then
    begin
      // Increase reference count
      if Assigned(X509_up_ref) then
        X509_up_ref(LCert);
      Result[i] := TOpenSSLCertificate.Create(LCert, True);
    end;
  end;
end;

function TOpenSSLConnection.GetVerifyResult: Integer;
begin
  Result := 0;
  
  if (FSSL = nil) or not Assigned(SSL_get_verify_result) then
    Exit;
  
  Result := Integer(SSL_get_verify_result(FSSL));
end;

function TOpenSSLConnection.GetVerifyResultString: string;
var
  LResult: Int64;
  LStr: PAnsiChar;
begin
  Result := '';
  
  if FSSL = nil then
    Exit;
  
  if Assigned(SSL_get_verify_result) then
  begin
    LResult := SSL_get_verify_result(FSSL);
    if Assigned(X509_verify_cert_error_string) then
    begin
      LStr := X509_verify_cert_error_string(LResult);
      if LStr <> nil then
        Result := string(LStr);
    end;
  end;
end;

function TOpenSSLConnection.GetSession: ISSLSession;
var
  LSession: PSSL_SESSION;
begin
  Result := nil;
  if FSSL = nil then
    Exit;

  if Assigned(SSL_get1_session) then
    LSession := SSL_get1_session(FSSL)
  else if Assigned(SSL_get_session) then
  begin
    LSession := SSL_get_session(FSSL);
    if (LSession <> nil) and Assigned(SSL_SESSION_up_ref) then
      SSL_SESSION_up_ref(LSession);
  end
  else
    LSession := nil;

  if LSession <> nil then
    Result := TOpenSSLSession.Create(LSession, True);
end;

procedure TOpenSSLConnection.SetSession(aSession: ISSLSession);
var
  LHandle: PSSL_SESSION;
begin
  if FSSL = nil then
    Exit;

  if not Assigned(SSL_set_session) then
    RaiseUnsupported('Session assignment');

  if aSession <> nil then
    LHandle := PSSL_SESSION(aSession.GetNativeHandle)
  else
    LHandle := nil;

  SSL_set_session(FSSL, LHandle);
end;

function TOpenSSLConnection.IsSessionReused: Boolean;
begin
  Result := False;
  
  if (FSSL = nil) or not Assigned(SSL_session_reused) then
    Exit;
  
  Result := (SSL_session_reused(FSSL) = 1);
end;

function TOpenSSLConnection.GetSelectedALPNProtocol: string;
var
  LData: PByte;
  LLen: Cardinal;
begin
  Result := '';
  
  if (FSSL = nil) or not Assigned(SSL_get0_alpn_selected) then
    Exit;
  
  SSL_get0_alpn_selected(FSSL, @LData, @LLen);
  if (LData <> nil) and (LLen > 0) then
    SetString(Result, PAnsiChar(LData), LLen);
end;

function TOpenSSLConnection.GetALPNProtocol: string;
begin
  Result := GetSelectedALPNProtocol;
end;

function TOpenSSLConnection.GetSNI: string;
var
  LName: PAnsiChar;
begin
  Result := '';
  
  if (FSSL = nil) or not Assigned(SSL_get_servername) then
    Exit;
  
  LName := SSL_get_servername(FSSL, TLSEXT_NAMETYPE_host_name);
  if LName <> nil then
    Result := string(LName);
end;

function TOpenSSLConnection.IsConnected: Boolean;
begin
  Result := FHandshakeComplete and (FSSL <> nil);
end;

function TOpenSSLConnection.GetState: string;
var
  LState: PAnsiChar;
begin
  Result := '';
  
  if (FSSL = nil) or not Assigned(SSL_state_string) then
    Exit;
  
  LState := SSL_state_string(FSSL);
  if LState <> nil then
    Result := string(LState);
end;

function TOpenSSLConnection.GetStateString: string;
var
  LState: PAnsiChar;
begin
  Result := '';
  
  if (FSSL = nil) or not Assigned(SSL_state_string_long) then
    Exit;
  
  LState := SSL_state_string_long(FSSL);
  if LState <> nil then
    Result := string(LState);
end;

procedure TOpenSSLConnection.SetTimeout(aTimeout: Integer);
begin
  FTimeout := aTimeout;
end;

function TOpenSSLConnection.GetTimeout: Integer;
begin
  Result := FTimeout;
end;

procedure TOpenSSLConnection.SetBlocking(aBlocking: Boolean);
begin
  FBlocking := aBlocking;
end;

function TOpenSSLConnection.GetBlocking: Boolean;
begin
  Result := FBlocking;
end;

function TOpenSSLConnection.GetSocket: THandle;
begin
  Result := FSocket;
end;

function TOpenSSLConnection.GetStream: TStream;
begin
  Result := FStream;
end;

function TOpenSSLConnection.GetNativeHandle: Pointer;
begin
  Result := FSSL;
end;

function TOpenSSLConnection.GetContext: ISSLContext;
begin
  Result := FContext as ISSLContext;
end;

{ Helper functions }

function OpenSSLAvailable: Boolean;
begin
  Result := IsOpenSSLCoreLoaded;
end;

function LoadOpenSSL(const aLibraryPath: string): Boolean;
begin
  try
    // Note: For custom library paths, use SetCustomLibraryPaths() before calling this function
    // See fafafa.ssl.openssl.lib.pas for implementation
    if aLibraryPath <> '' then
      fafafa.ssl.openssl.lib.SetCustomLibraryPaths(aLibraryPath, aLibraryPath);
    LoadOpenSSLCore;
    Result := IsOpenSSLCoreLoaded;
  except
    Result := False;
  end;
end;

procedure UnloadOpenSSL;
begin
  UnloadOpenSSLCore;
end;

function GetOpenSSLVersion: string;
var
  LVersion: PAnsiChar;
begin
  if Assigned(OpenSSL_version) then
  begin
    LVersion := OpenSSL_version(0);  // 0 = OPENSSL_VERSION constant
    if LVersion <> nil then
      Result := string(LVersion)
    else
      Result := 'Unknown';
  end
  else
    Result := 'Unknown';
end;

function GetOpenSSLVersionNumber: Cardinal;
begin
  if Assigned(OpenSSL_version_num) then
    Result := OpenSSL_version_num()
  else
    Result := 0;
end;

function GetOpenSSLError: Cardinal;
begin
  if Assigned(ERR_get_error) then
    Result := ERR_get_error()
  else
    Result := 0;
end;

function GetOpenSSLErrorString(aError: Cardinal): string;
var
  LError: Cardinal;
  LBuf: array[0..255] of AnsiChar;
begin
  if aError = 0 then
  begin
    if Assigned(ERR_get_error) then
      LError := ERR_get_error()
    else
      LError := 0;
  end
  else
    LError := aError;
    
  if LError = 0 then
    Result := 'No error'
  else
  begin
    if Assigned(ERR_error_string_n) then
    begin
      ERR_error_string_n(LError, @LBuf[0], SizeOf(LBuf));
      Result := string(LBuf);
    end
    else
      Result := 'Error: ' + IntToStr(LError);
  end;
end;

procedure ClearOpenSSLErrors;
begin
  if Assigned(ERR_clear_error) then
    ERR_clear_error();
end;

function ClassifyOpenSSLError(aError: Cardinal): TSSLErrorCode;
var
  LLib, LReason: Integer;
begin
  // Default to general error
  Result := sslErrGeneral;

  if aError = 0 then
  begin
    Result := sslErrNone;
    Exit;
  end;

  // Extract library and reason codes from error
  LLib := ERR_GET_LIB_INLINE(aError);
  LReason := ERR_GET_REASON_INLINE(aError);

  // First check common error reasons (applicable across all libraries)
  case LReason of
    ERR_R_MALLOC_FAILURE:
      begin
        Result := sslErrMemory;
        Exit;
      end;
    ERR_R_PASSED_NULL_PARAMETER,
    ERR_R_PASSED_INVALID_ARGUMENT:
      begin
        Result := sslErrInvalidParam;
        Exit;
      end;
    ERR_R_INIT_FAIL:
      begin
        Result := sslErrNotInitialized;
        Exit;
      end;
    ERR_R_DISABLED:
      begin
        Result := sslErrUnsupported;
        Exit;
      end;
    ERR_R_OPERATION_FAIL:
      begin
        Result := sslErrGeneral;
        Exit;
      end;
  end;

  // Then classify by OpenSSL library code
  case LLib of
    ERR_LIB_NONE:
      Result := sslErrGeneral;

    ERR_LIB_SYS:
      Result := sslErrIO;  // System errors are usually I/O related

    ERR_LIB_BUF:
      Result := sslErrMemory;  // Buffer errors are memory issues

    ERR_LIB_PEM:
      Result := sslErrCertificate;  // PEM parsing errors are cert issues

    ERR_LIB_X509,
    ERR_LIB_X509V3,
    ERR_LIB_PKCS7,
    ERR_LIB_PKCS12,
    ERR_LIB_OCSP:
      Result := sslErrCertificate;  // All certificate-related libraries

    ERR_LIB_SSL:
      begin
        // SSL library errors - check specific reasons
        // Common SSL errors that indicate protocol issues
        Result := sslErrProtocol;
      end;

    ERR_LIB_BIO:
      Result := sslErrIO;  // BIO errors are I/O issues

    ERR_LIB_RSA,
    ERR_LIB_DSA,
    ERR_LIB_DH,
    ERR_LIB_EC,
    ERR_LIB_ECDSA,
    ERR_LIB_ECDH:
      Result := sslErrProtocol;  // Crypto algorithm errors

    ERR_LIB_EVP,
    ERR_LIB_CRYPTO:
      Result := sslErrProtocol;  // General crypto errors

    ERR_LIB_ASN1:
      Result := sslErrCertificate;  // ASN.1 parsing is usually cert-related

    ERR_LIB_ENGINE,
    ERR_LIB_DSO:
      Result := sslErrLibraryNotFound;  // Engine/dynamic library loading issues

    ERR_LIB_UI:
      Result := sslErrUnsupported;  // User interface not supported

  else
    Result := sslErrGeneral;
  end;
end;

function GetOpenSSLErrorCategory(aError: Cardinal): string;
var
  LLib: Integer;
  LLibName: string;
begin
  if aError = 0 then
  begin
    Result := 'No Error';
    Exit;
  end;

  // Get library code
  LLib := ERR_GET_LIB_INLINE(aError);

  // Translate library code to friendly name
  case LLib of
    ERR_LIB_NONE: LLibName := 'General';
    ERR_LIB_SYS: LLibName := 'System';
    ERR_LIB_BN: LLibName := 'BigNum';
    ERR_LIB_RSA: LLibName := 'RSA';
    ERR_LIB_DH: LLibName := 'Diffie-Hellman';
    ERR_LIB_EVP: LLibName := 'Envelope';
    ERR_LIB_BUF: LLibName := 'Buffer';
    ERR_LIB_OBJ: LLibName := 'Object';
    ERR_LIB_PEM: LLibName := 'PEM';
    ERR_LIB_DSA: LLibName := 'DSA';
    ERR_LIB_X509: LLibName := 'X.509';
    ERR_LIB_ASN1: LLibName := 'ASN.1';
    ERR_LIB_CONF: LLibName := 'Config';
    ERR_LIB_CRYPTO: LLibName := 'Crypto';
    ERR_LIB_EC: LLibName := 'Elliptic Curve';
    ERR_LIB_SSL: LLibName := 'SSL/TLS';
    ERR_LIB_BIO: LLibName := 'I/O';
    ERR_LIB_PKCS7: LLibName := 'PKCS#7';
    ERR_LIB_X509V3: LLibName := 'X.509v3';
    ERR_LIB_PKCS12: LLibName := 'PKCS#12';
    ERR_LIB_RAND: LLibName := 'Random';
    ERR_LIB_DSO: LLibName := 'Dynamic Library';
    ERR_LIB_ENGINE: LLibName := 'Engine';
    ERR_LIB_OCSP: LLibName := 'OCSP';
    ERR_LIB_UI: LLibName := 'User Interface';
    ERR_LIB_COMP: LLibName := 'Compression';
    ERR_LIB_ECDSA: LLibName := 'ECDSA';
    ERR_LIB_ECDH: LLibName := 'ECDH';
    ERR_LIB_HMAC: LLibName := 'HMAC';
    ERR_LIB_CMS: LLibName := 'CMS';
    ERR_LIB_TS: LLibName := 'Timestamp';
    ERR_LIB_CT: LLibName := 'Certificate Transparency';
    ERR_LIB_ASYNC: LLibName := 'Async';
    ERR_LIB_KDF: LLibName := 'Key Derivation';
    ERR_LIB_SM2: LLibName := 'SM2';
  else
    LLibName := Format('Library %d', [LLib]);
  end;

  Result := LLibName;
end;

function GetFriendlyErrorMessage(aError: Cardinal): string;
var
  LClassified: TSSLErrorCode;
  LCategory: string;
  LRawError: string;
  LSuggestion: string;
begin
  if aError = 0 then
  begin
    Result := 'No error';
    Exit;
  end;

  // Classify the error
  LClassified := ClassifyOpenSSLError(aError);

  // Get category name
  LCategory := GetOpenSSLErrorCategory(aError);

  // Get raw OpenSSL error string
  LRawError := GetOpenSSLErrorString(aError);

  // Build context-specific suggestion based on error type
  case LClassified of
    sslErrCertificate:
      LSuggestion := 'Check certificate validity, CA trust, and certificate chain';
    sslErrCertificateExpired:
      LSuggestion := 'Certificate has expired - obtain a new certificate';
    sslErrCertificateRevoked:
      LSuggestion := 'Certificate has been revoked - contact certificate authority';
    sslErrMemory:
      LSuggestion := 'Check system memory and free up resources';
    sslErrInvalidParam:
      LSuggestion := 'Verify function parameters are correct and non-null';
    sslErrNotInitialized:
      LSuggestion := 'Ensure SSL library is properly initialized before use';
    sslErrProtocol:
      LSuggestion := 'Check TLS protocol version compatibility and cipher suite support';
    sslErrHandshake:
      LSuggestion := 'Verify server is accessible and supports required TLS protocols';
    sslErrIO:
      LSuggestion := 'Check network connectivity and firewall settings';
    sslErrConnection:
      LSuggestion := 'Verify server address and port are correct';
    sslErrTimeout:
      LSuggestion := 'Increase timeout value or check network latency';
    sslErrUnsupported:
      LSuggestion := 'Feature not available - check OpenSSL version or build options';
    sslErrLibraryNotFound:
      LSuggestion := 'Ensure OpenSSL libraries are installed and accessible';
    sslErrVersionMismatch:
      LSuggestion := 'Check OpenSSL library version compatibility';
  else
    LSuggestion := 'Review error details and consult OpenSSL documentation';
  end;

  // Build formatted message
  Result := Format('[%s] %s:'#13#10 +
                  '  Problem: %s'#13#10 +
                  '  Details: %s'#13#10 +
                  '  Suggestion: %s',
                  [LCategory,
                    SSL_ERROR_MESSAGES[LClassified],
                    SSL_ERROR_MESSAGES[LClassified],
                    LRawError,
                    LSuggestion]);
end;

function LoadCertificateFromFile(const aFileName: string): PX509;
var
  LBio: PBIO;
begin
  Result := nil;

  // Check if required functions are loaded
  if not Assigned(BIO_new_file) or not Assigned(PEM_read_bio_X509) then
    Exit;

  // Open file and create BIO
  LBio := BIO_new_file(PAnsiChar(AnsiString(aFileName)), 'r');
  if LBio = nil then
    Exit;

  try
    // Read certificate from BIO
    Result := PEM_read_bio_X509(LBio, nil, nil, nil);
  finally
    BIO_free(LBio);
  end;
end;

function LoadCertificateFromMemory(const aData: Pointer; aSize: Integer): PX509;
var
  LBio: PBIO;
begin
  Result := nil;

  // Check parameters
  if (aData = nil) or (aSize <= 0) then
    Exit;

  // Check if required functions are loaded
  if not Assigned(BIO_new_mem_buf) or not Assigned(PEM_read_bio_X509) then
    Exit;

  // Create BIO from memory buffer
  LBio := BIO_new_mem_buf(aData, aSize);
  if LBio = nil then
    Exit;

  try
    // Read certificate from BIO
    Result := PEM_read_bio_X509(LBio, nil, nil, nil);
  finally
    BIO_free(LBio);
  end;
end;

function LoadPrivateKeyFromFile(const aFileName: string; const aPassword: string): PEVP_PKEY;
var
  LBio: PBIO;
  LPasswordAnsi: AnsiString;
  LPasswordPtr: Pointer;
begin
  Result := nil;

  // Check if required functions are loaded
  if not Assigned(BIO_new_file) or not Assigned(PEM_read_bio_PrivateKey) then
    Exit;

  // Open file and create BIO
  LBio := BIO_new_file(PAnsiChar(AnsiString(aFileName)), 'r');
  if LBio = nil then
    Exit;

  try
    // Prepare password pointer
    if aPassword <> '' then
    begin
      LPasswordAnsi := AnsiString(aPassword);
      LPasswordPtr := PAnsiChar(LPasswordAnsi);
    end
    else
      LPasswordPtr := nil;

    // Read private key from BIO
    Result := PEM_read_bio_PrivateKey(LBio, nil, nil, LPasswordPtr);
  finally
    BIO_free(LBio);
  end;
end;

function LoadPrivateKeyFromMemory(const aData: Pointer; aSize: Integer; const aPassword: string): PEVP_PKEY;
var
  LBio: PBIO;
  LPasswordAnsi: AnsiString;
  LPasswordPtr: Pointer;
begin
  Result := nil;

  // Check parameters
  if (aData = nil) or (aSize <= 0) then
    Exit;

  // Check if required functions are loaded
  if not Assigned(BIO_new_mem_buf) or not Assigned(PEM_read_bio_PrivateKey) then
    Exit;

  // Create BIO from memory buffer
  LBio := BIO_new_mem_buf(aData, aSize);
  if LBio = nil then
    Exit;

  try
    // Prepare password pointer
    if aPassword <> '' then
    begin
      LPasswordAnsi := AnsiString(aPassword);
      LPasswordPtr := PAnsiChar(LPasswordAnsi);
    end
    else
      LPasswordPtr := nil;

    // Read private key from BIO
    Result := PEM_read_bio_PrivateKey(LBio, nil, nil, LPasswordPtr);
  finally
    BIO_free(LBio);
  end;
end;

function VerifyCertificate(aCert: PX509; aCAStore: PX509_STORE): Boolean;
var
  LCtx: PX509_STORE_CTX;
  LRet: Integer;
begin
  Result := False;

  // Check parameters
  if (aCert = nil) or (aCAStore = nil) then
    Exit;

  // Check if required functions are loaded
  if not Assigned(X509_STORE_CTX_new) or not Assigned(X509_STORE_CTX_init) or
    not Assigned(X509_verify_cert) or not Assigned(X509_STORE_CTX_free) then
    Exit;

  // Create store context
  LCtx := X509_STORE_CTX_new();
  if LCtx = nil then
    Exit;

  try
    // Initialize context with store and certificate
    if X509_STORE_CTX_init(LCtx, aCAStore, aCert, nil) = 1 then
    begin
      // Perform verification
      LRet := X509_verify_cert(LCtx);
      Result := LRet = 1;
    end;
  finally
    X509_STORE_CTX_free(LCtx);
  end;
end;

function GetCertificateInfo(aCert: PX509): TSSLCertificateInfo;
var
  LCertObj: TOpenSSLCertificate;
begin
  FillChar(Result, SizeOf(Result), 0);

  // Check parameter
  if aCert = nil then
    Exit;

  // Create temporary certificate object (not owned, so it won't free aCert)
  LCertObj := TOpenSSLCertificate.Create(aCert, False);
  try
    // Extract certificate information using the class method
    Result := LCertObj.GetInfo;
  finally
    LCertObj.Free;
  end;
end;

function ProtocolToOpenSSL(aProtocol: TSSLProtocolVersion): Integer;
begin
  case aProtocol of
    sslProtocolSSL3: Result := SSL3_VERSION;
    sslProtocolTLS10: Result := TLS1_VERSION;
    sslProtocolTLS11: Result := TLS1_1_VERSION;
    sslProtocolTLS12: Result := TLS1_2_VERSION;
    sslProtocolTLS13: Result := TLS1_3_VERSION;
  else
    Result := 0;
  end;
end;

function OpenSSLToProtocol(aVersion: Integer): TSSLProtocolVersion;
begin
  case aVersion of
    SSL3_VERSION: Result := sslProtocolSSL3;
    TLS1_VERSION: Result := sslProtocolTLS10;
    TLS1_1_VERSION: Result := sslProtocolTLS11;
    TLS1_2_VERSION: Result := sslProtocolTLS12;
    TLS1_3_VERSION: Result := sslProtocolTLS13;
  else
    Result := sslProtocolTLS12;
  end;
end;

function GetProtocolName(aProtocol: TSSLProtocolVersion): string;
begin
  Result := SSL_PROTOCOL_NAMES[aProtocol];
end;

procedure RegisterOpenSSLBackend;
begin
  // Register OpenSSL library with the factory
  TSSLFactory.RegisterLibrary(sslOpenSSL, TOpenSSLLibrary, 'OpenSSL 3.x Support', 100);
end;

procedure UnregisterOpenSSLBackend;
begin
  // Unregister OpenSSL library from the factory
  TSSLFactory.UnregisterLibrary(sslOpenSSL);
end;

initialization
  RegisterOpenSSLBackend;
  
finalization
  if GContextRegistry <> nil then
    FreeAndNil(GContextRegistry);
  UnregisterOpenSSLBackend;

end.
