unit fafafa.ssl.openssl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets,
  fafafa.ssl.types,
  fafafa.ssl.intf,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.bio;

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
    FOptions: Cardinal;
    FServerName: string;
    FALPNProtocols: string;
    FCipherList: string;
    FCipherSuites: string;
    
    procedure SetupContext;
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
    procedure SetOptions(aOptions: Cardinal);
    function GetOptions: Cardinal;
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

  { TOpenSSLCertificate }
  TOpenSSLCertificate = class(TInterfacedObject, ISSLCertificate)
  private
    FCert: PX509;
    FOwned: Boolean;
  protected
    { ISSLCertificate implementation }
    function GetSubject: string;
    function GetIssuer: string;
    function GetSerialNumber: string;
    function GetNotBefore: TDateTime;
    function GetNotAfter: TDateTime;
    function GetPublicKeyAlgorithm: string;
    function GetSignatureAlgorithm: string;
    function GetVersion: Integer;
    function GetExtensions: TStringArray;
    function GetSANs: TStringArray;
    function GetFingerprint(Algorithm: TSSLHashAlgorithm): string;
    function ExportToPEM: string;
    function ExportToDER: TBytes;
    function Verify(ACACert: ISSLCertificate): Boolean;
  public
    constructor Create(ACert: PX509; AOwned: Boolean = True);
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
    FIsServer: Boolean;
    FHandshakeComplete: Boolean;
    
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
    function GetALPNProtocol: string;
    function GetSNI: string;
    function IsConnected: Boolean;
    function GetSocket: THandle;
    function GetStream: TStream;
    function GetNativeHandle: Pointer;
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

{ Certificate utilities }
function LoadCertificateFromFile(const aFileName: string): PX509;
function LoadCertificateFromMemory(const aData: Pointer; aSize: Integer): PX509;
function LoadPrivateKeyFromFile(const aFileName: string; const aPassword: string = ''): PEVP_PKEY;
function LoadPrivateKeyFromMemory(const aData: Pointer; aSize: Integer; const aPassword: string = ''): PEVP_PKEY;
function VerifyCertificate(aCert: PX509; aCAStore: PX509_STORE): Boolean;
function GetCertificateInfo(aCert: PX509): TSSLCertificate;

{ Cipher suite utilities }
function GetCipherName(aCipher: PSSL_CIPHER): string;
function GetCipherDescription(aCipher: PSSL_CIPHER): string;
function GetCipherBits(aCipher: PSSL_CIPHER): Integer;
function GetCipherVersion(aCipher: PSSL_CIPHER): string;

{ Protocol utilities }
function ProtocolToOpenSSL(aProtocol: TSSLProtocol): Integer;
function OpenSSLToProtocol(aVersion: Integer): TSSLProtocol;
function GetProtocolName(aProtocol: TSSLProtocol): string;

{ Initialization }
procedure RegisterOpenSSLBackend;
procedure UnregisterOpenSSLBackend;

implementation

uses
  SysUtils, DateUtils,
  {$IFDEF MSWINDOWS}WinSock2, Windows,{$ENDIF}
  {$IFDEF UNIX}Sockets,{$ENDIF}
  fafafa.ssl.factory,
  fafafa.ssl.openssl.bio,
  fafafa.ssl.openssl.err,
  fafafa.ssl.openssl.evp,
  fafafa.ssl.openssl.x509,
  fafafa.ssl.openssl.ssl,
  fafafa.ssl.openssl.core;

const
  {$IFNDEF MSWINDOWS}
  INVALID_HANDLE_VALUE = THandle(-1);
  {$ENDIF}

var
  GOpenSSLBackend: TOpenSSLBackend = nil;

{ TOpenSSLConnection }

constructor TOpenSSLConnection.Create(AContext: TOpenSSLContext; ASocket: THandle; AIsServer: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FSocket := ASocket;
  FIsServer := AIsServer;
  FConnected := False;
  
  // Create SSL structure
  FSSL := SSL_new(FContext.FSSLCtx);
  if FSSL = nil then
    raise ESSLError.Create('Failed to create SSL structure');
  
  // Create BIO pair for socket I/O
  if FSocket <> INVALID_HANDLE_VALUE then
  begin
    FBioRead := BIO_new_socket(FSocket, 0);
    FBioWrite := FBioRead;
    SSL_set_bio(FSSL, FBioRead, FBioWrite);
  end;
  
  // Set connection mode
  if FIsServer then
    SSL_set_accept_state(FSSL)
  else
    SSL_set_connect_state(FSSL);
end;

constructor TOpenSSLConnection.Create(AContext: TOpenSSLContext; ASendProc: TSSLDataProc; ARecvFunc: TSSLDataFunc; AIsServer: Boolean);
begin
  inherited Create;
  FContext := AContext;
  FSendProc := ASendProc;
  FRecvFunc := ARecvFunc;
  FSocket := INVALID_HANDLE_VALUE;
  FIsServer := AIsServer;
  FConnected := False;
  
  // Create SSL structure
  FSSL := SSL_new(FContext.FSSLCtx);
  if FSSL = nil then
    raise ESSLError.Create('Failed to create SSL structure');
  
  // Create memory BIO pair for custom I/O
  BIO_new_bio_pair(@FBioRead, 0, @FBioWrite, 0);
  SSL_set_bio(FSSL, FBioRead, FBioWrite);
  
  // Set connection mode
  if FIsServer then
    SSL_set_accept_state(FSSL)
  else
    SSL_set_connect_state(FSSL);
end;

destructor TOpenSSLConnection.Destroy;
begin
  if FSSL <> nil then
  begin
    SSL_shutdown(FSSL);
    SSL_free(FSSL);
  end;
  // BIOs are freed by SSL_free
  inherited Destroy;
end;

function TOpenSSLConnection.PerformHandshake: TSSLError;
var
  ret: Integer;
begin
  ret := SSL_do_handshake(FSSL);
  if ret = 1 then
  begin
    FConnected := True;
    Result := seNone;
  end
  else
    Result := TranslateError(SSL_get_error(FSSL, ret));
end;

function TOpenSSLConnection.SendData(const Data; Size: Integer): Integer;
begin
  if Assigned(FSendProc) then
    Result := FSendProc(Data, Size)
  else if FSocket <> INVALID_HANDLE_VALUE then
    Result := send(FSocket, Data, Size, 0)
  else
    Result := -1;
end;

function TOpenSSLConnection.ReceiveData(var Data; Size: Integer): Integer;
begin
  if Assigned(FRecvFunc) then
    Result := FRecvFunc(Data, Size)
  else if FSocket <> INVALID_HANDLE_VALUE then
    Result := recv(FSocket, Data, Size, 0)
  else
    Result := -1;
end;

function TOpenSSLConnection.Read(var Buffer; Count: Integer): Integer;
begin
  Result := SSL_read(FSSL, @Buffer, Count);
  if Result <= 0 then
  begin
    if SSL_get_error(FSSL, Result) in [SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE] then
      Result := 0;  // Non-blocking, no data available
  end;
end;

function TOpenSSLConnection.Write(const Buffer; Count: Integer): Integer;
begin
  Result := SSL_write(FSSL, @Buffer, Count);
  if Result <= 0 then
  begin
    if SSL_get_error(FSSL, Result) in [SSL_ERROR_WANT_READ, SSL_ERROR_WANT_WRITE] then
      Result := 0;  // Non-blocking, buffer full
  end;
end;

function TOpenSSLConnection.GetPeerCertificate: ISSLCertificate;
var
  cert: PX509;
begin
  cert := SSL_get_peer_certificate(FSSL);
  if cert <> nil then
  begin
    Result := TOpenSSLCertificate.Create(cert);
    X509_free(cert);
  end
  else
    Result := nil;
end;

function TOpenSSLConnection.GetCipherName: string;
var
  cipher: PSSL_CIPHER;
begin
  cipher := SSL_get_current_cipher(FSSL);
  if cipher <> nil then
    Result := string(SSL_CIPHER_get_name(cipher))
  else
    Result := '';
end;

function TOpenSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  case SSL_version(FSSL) of
    SSL3_VERSION: Result := spvSSL3;
    TLS1_VERSION: Result := spvTLS10;
    TLS1_1_VERSION: Result := spvTLS11;
    TLS1_2_VERSION: Result := spvTLS12;
    TLS1_3_VERSION: Result := spvTLS13;
  else
    Result := spvUnknown;
  end;
end;

function TOpenSSLConnection.GetALPNProtocol: string;
var
  data: PAnsiChar;
  len: Cardinal;
begin
  SSL_get0_alpn_selected(FSSL, @data, @len);
  if (data <> nil) and (len > 0) then
    SetString(Result, data, len)
  else
    Result := '';
end;

function TOpenSSLConnection.GetSNIHostname: string;
var
  servername: PAnsiChar;
begin
  servername := SSL_get_servername(FSSL, TLSEXT_NAMETYPE_host_name);
  if servername <> nil then
    Result := string(servername)
  else
    Result := '';
end;

function TOpenSSLConnection.IsConnected: Boolean;
begin
  Result := FConnected and (SSL_is_init_finished(FSSL) = 1);
end;

function TOpenSSLConnection.Shutdown: TSSLError;
var
  ret: Integer;
begin
  ret := SSL_shutdown(FSSL);
  if ret >= 0 then
  begin
    FConnected := False;
    Result := seNone;
  end
  else
    Result := TranslateError(SSL_get_error(FSSL, ret));
end;

function TOpenSSLConnection.TranslateError(ErrorCode: Integer): TSSLError;
begin
  case ErrorCode of
    SSL_ERROR_NONE: Result := seNone;
    SSL_ERROR_SSL: Result := seProtocol;
    SSL_ERROR_WANT_READ: Result := seWantRead;
    SSL_ERROR_WANT_WRITE: Result := seWantWrite;
    SSL_ERROR_WANT_X509_LOOKUP: Result := seWantRead;
    SSL_ERROR_SYSCALL: Result := seSystem;
    SSL_ERROR_ZERO_RETURN: Result := seClosed;
    SSL_ERROR_WANT_CONNECT: Result := seWantRead;
    SSL_ERROR_WANT_ACCEPT: Result := seWantRead;
  else
    Result := seUnknown;
  end;
end;

{ TOpenSSLBackend }

constructor TOpenSSLBackend.Create;
begin
  inherited Create;
  FLibraryPath := '';
  FInitialized := False;
  FLastError := 0;
  FLastErrorMessage := '';
end;

constructor TOpenSSLBackend.Create(const aLibraryPath: string);
begin
  inherited Create;
  FLibraryPath := aLibraryPath;
  FInitialized := False;
  FLastError := 0;
  FLastErrorMessage := '';
end;

destructor TOpenSSLBackend.Destroy;
begin
  if FInitialized then
    Finalize;
  inherited Destroy;
end;

function TOpenSSLBackend.GetName: string;
begin
  Result := 'OpenSSL';
end;

function TOpenSSLBackend.GetVersion: string;
begin
  if FInitialized and Assigned(OpenSSL_version) then
    Result := string(OpenSSL_version(OPENSSL_VERSION))
  else
    Result := 'Unknown';
end;

function TOpenSSLBackend.IsAvailable: Boolean;
begin
  Result := FInitialized or Initialize;
end;

function TOpenSSLBackend.Initialize: Boolean;
begin
  if FInitialized then
    Exit(True);
    
  try
    // Load OpenSSL libraries
    LoadOpenSSLCore(FLibraryPath);
    LoadOpenSSLEVP;
    LoadOpenSSLSSL;
    LoadOpenSSLX509;
    LoadOpenSSLBIO;
    
    // Initialize OpenSSL
    if Assigned(OPENSSL_init_ssl) then
    begin
      OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS or OPENSSL_INIT_LOAD_CRYPTO_STRINGS, nil);
      FInitialized := True;
    end
    else if Assigned(SSL_library_init) then
    begin
      // Fallback for older OpenSSL versions
      SSL_library_init;
      SSL_load_error_strings;
      OpenSSL_add_all_algorithms;
      FInitialized := True;
    end;
    
    Result := FInitialized;
  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Result := False;
    end;
  end;
end;

procedure TOpenSSLBackend.Finalize;
begin
  if not FInitialized then
    Exit;
    
  try
    // Cleanup OpenSSL
    if Assigned(OPENSSL_cleanup) then
      OPENSSL_cleanup
    else if Assigned(EVP_cleanup) then
    begin
      // Fallback for older OpenSSL versions
      EVP_cleanup;
      ERR_free_strings;
    end;
    
    // Unload libraries
    UnloadOpenSSLBIO;
    UnloadOpenSSLX509;
    UnloadOpenSSLSSL;
    UnloadOpenSSLEVP;
    UnloadOpenSSLCore;
    
    FInitialized := False;
  except
    // Ignore cleanup errors
  end;
end;

function TOpenSSLBackend.GetLastError: Integer;
begin
  Result := FLastError;
end;

function TOpenSSLBackend.GetLastErrorMessage: string;
begin
  Result := FLastErrorMessage;
end;

function TOpenSSLBackend.CreateContext(aContextType: TSSLContextType): ISSLContext;
begin
  if not FInitialized then
  begin
    if not Initialize then
    begin
      UpdateLastError;
      Exit(nil);
    end;
  end;
  
  try
    Result := TOpenSSLContext.Create(Self, aContextType);
  except
    on E: Exception do
    begin
      FLastErrorMessage := E.Message;
      Result := nil;
    end;
  end;
end;

function TOpenSSLBackend.GetSupportedProtocols: TSSLProtocols;
begin
  Result := [spSSLv3, spTLSv1, spTLSv1_1, spTLSv1_2, spTLSv1_3];
end;

function TOpenSSLBackend.GetSupportedCipherSuites: TStringArray;
var
  LCtx: PSSL_CTX;
  LSSL: PSSL;
  LCiphers: PSTACK_OF_SSL_CIPHER;
  LCount, i: Integer;
  LCipher: PSSL_CIPHER;
begin
  SetLength(Result, 0);
  
  if not FInitialized then
    Exit;
    
  LCtx := SSL_CTX_new(TLS_method);
  if LCtx = nil then
    Exit;
    
  try
    LSSL := SSL_new(LCtx);
    if LSSL = nil then
      Exit;
      
    try
      LCiphers := SSL_get_ciphers(LSSL);
      if LCiphers = nil then
        Exit;
        
      LCount := sk_SSL_CIPHER_num(LCiphers);
      SetLength(Result, LCount);
      
      for i := 0 to LCount - 1 do
      begin
        LCipher := sk_SSL_CIPHER_value(LCiphers, i);
        if LCipher <> nil then
          Result[i] := string(SSL_CIPHER_get_name(LCipher));
      end;
    finally
      SSL_free(LSSL);
    end;
  finally
    SSL_CTX_free(LCtx);
  end;
end;

procedure TOpenSSLBackend.UpdateLastError;
begin
  FLastError := ERR_get_error;
  FLastErrorMessage := GetOpenSSLErrorString;
end;

function TOpenSSLBackend.GetOpenSSLErrorString: string;
var
  LError: Cardinal;
  LBuf: array[0..255] of AnsiChar;
begin
  LError := ERR_get_error;
  if LError = 0 then
    Result := 'No error'
  else
  begin
    ERR_error_string_n(LError, @LBuf[0], SizeOf(LBuf));
    Result := string(LBuf);
  end;
end;

procedure TOpenSSLBackend.ClearErrors;
begin
  ERR_clear_error;
  FLastError := 0;
  FLastErrorMessage := '';
end;

{ TOpenSSLContext }

constructor TOpenSSLContext.Create(aBackend: TOpenSSLBackend; aContextType: TSSLContextType);
begin
  inherited Create;
  FBackend := aBackend;
  FContextType := aContextType;
  FConnected := False;
  FProtocol := spTLSv1_2;  // Default to TLS 1.2
  FVerifyMode := svmNone;
  FOptions := [soSingleDHUse, soSingleECDHUse];
  
  SetupContext;
end;

destructor TOpenSSLContext.Destroy;
begin
  if FSSLCtx <> nil then
    SSL_CTX_free(FSSLCtx);
  inherited Destroy;
end;

procedure TOpenSSLContext.SetupContext;
var
  LMethod: PSSL_METHOD;
begin
  // Select SSL method based on context type
  case FContextType of
    sctClient:
      LMethod := TLS_client_method;
    sctServer:
      LMethod := TLS_server_method;
  else
    LMethod := TLS_method;
  end;
  
  if LMethod = nil then
  begin
    // Fallback for older OpenSSL versions
    case FContextType of
      sctClient:
        LMethod := SSLv23_client_method;
      sctServer:
        LMethod := SSLv23_server_method;
    else
      LMethod := SSLv23_method;
    end;
  end;
  
  FSSLCtx := SSL_CTX_new(LMethod);
  if FSSLCtx = nil then
    raise Exception.Create('Failed to create SSL context');
    
  // Set default options
  SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_SSLv2);  // Disable SSLv2 by default
  
  // Set default cipher list
  SSL_CTX_set_cipher_list(FSSLCtx, 'HIGH:!aNULL:!eNULL:!EXPORT:!DES:!MD5:!PSK:!RC4');
end;

procedure TOpenSSLContext.SetProtocolVersion;
var
  LMinVersion, LMaxVersion: Integer;
begin
  case FProtocol of
    spSSLv3:
      begin
        LMinVersion := SSL3_VERSION;
        LMaxVersion := SSL3_VERSION;
      end;
    spTLSv1:
      begin
        LMinVersion := TLS1_VERSION;
        LMaxVersion := TLS1_VERSION;
      end;
    spTLSv1_1:
      begin
        LMinVersion := TLS1_1_VERSION;
        LMaxVersion := TLS1_1_VERSION;
      end;
    spTLSv1_2:
      begin
        LMinVersion := TLS1_2_VERSION;
        LMaxVersion := TLS1_2_VERSION;
      end;
    spTLSv1_3:
      begin
        LMinVersion := TLS1_3_VERSION;
        LMaxVersion := TLS1_3_VERSION;
      end;
  else
    begin
      LMinVersion := TLS1_VERSION;
      LMaxVersion := 0;  // No maximum
    end;
  end;
  
  if Assigned(SSL_CTX_set_min_proto_version) then
  begin
    SSL_CTX_set_min_proto_version(FSSLCtx, LMinVersion);
    if LMaxVersion > 0 then
      SSL_CTX_set_max_proto_version(FSSLCtx, LMaxVersion);
  end
  else
  begin
    // Fallback for older OpenSSL versions
    case FProtocol of
      spSSLv3:
        SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_TLSv1 or SSL_OP_NO_TLSv1_1 or SSL_OP_NO_TLSv1_2);
      spTLSv1:
        SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_SSLv3 or SSL_OP_NO_TLSv1_1 or SSL_OP_NO_TLSv1_2);
      spTLSv1_1:
        SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_SSLv3 or SSL_OP_NO_TLSv1 or SSL_OP_NO_TLSv1_2);
      spTLSv1_2:
        SSL_CTX_set_options(FSSLCtx, SSL_OP_NO_SSLv3 or SSL_OP_NO_TLSv1 or SSL_OP_NO_TLSv1_1);
    end;
  end;
end;

function TOpenSSLContext.LoadCertificates: Boolean;
begin
  Result := True;
  
  // Load certificate
  if FCertificateFile <> '' then
  begin
    if SSL_CTX_use_certificate_file(FSSLCtx, PAnsiChar(AnsiString(FCertificateFile)), SSL_FILETYPE_PEM) <= 0 then
      Exit(False);
  end;
  
  // Load private key
  if FPrivateKeyFile <> '' then
  begin
    if SSL_CTX_use_PrivateKey_file(FSSLCtx, PAnsiChar(AnsiString(FPrivateKeyFile)), SSL_FILETYPE_PEM) <= 0 then
      Exit(False);
      
    // Verify private key
    if SSL_CTX_check_private_key(FSSLCtx) <= 0 then
      Exit(False);
  end;
  
  // Load CA certificates
  if (FCAFile <> '') or (FCAPath <> '') then
  begin
    if SSL_CTX_load_verify_locations(FSSLCtx, 
      PAnsiChar(AnsiString(FCAFile)), 
      PAnsiChar(AnsiString(FCAPath))) <= 0 then
      Exit(False);
  end;
end;

function TOpenSSLContext.VerifyCallback(preverify: Integer; x509_ctx: PX509_STORE_CTX): Integer;
begin
  // This is a simplified verify callback
  // In production, you would implement more sophisticated verification
  Result := preverify;
end;

function TOpenSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

function TOpenSSLContext.SetProtocol(aProtocol: TSSLProtocol): Boolean;
begin
  FProtocol := aProtocol;
  SetProtocolVersion;
  Result := True;
end;

function TOpenSSLContext.SetCipherSuites(const aCipherSuites: string): Boolean;
begin
  FCipherSuites := aCipherSuites;
  Result := SSL_CTX_set_cipher_list(FSSLCtx, PAnsiChar(AnsiString(aCipherSuites))) = 1;
end;

function TOpenSSLContext.LoadCertificate(const aCertFile: string): Boolean;
begin
  FCertificateFile := aCertFile;
  Result := LoadCertificates;
end;

function TOpenSSLContext.LoadPrivateKey(const aKeyFile: string; const aPassword: string = ''): Boolean;
begin
  FPrivateKeyFile := aKeyFile;
  // TODO: Handle password-protected keys
  Result := LoadCertificates;
end;

function TOpenSSLContext.LoadCA(const aCAFile: string = ''; const aCAPath: string = ''): Boolean;
begin
  FCAFile := aCAFile;
  FCAPath := aCAPath;
  Result := LoadCertificates;
end;

function TOpenSSLContext.SetVerifyMode(aMode: TSSLVerifyMode): Boolean;
var
  LMode: Integer;
begin
  FVerifyMode := aMode;
  
  case aMode of
    svmNone:
      LMode := SSL_VERIFY_NONE;
    svmPeer:
      LMode := SSL_VERIFY_PEER;
    svmFailIfNoPeerCert:
      LMode := SSL_VERIFY_PEER or SSL_VERIFY_FAIL_IF_NO_PEER_CERT;
    svmClientOnce:
      LMode := SSL_VERIFY_PEER or SSL_VERIFY_CLIENT_ONCE;
  else
    LMode := SSL_VERIFY_NONE;
  end;
  
  SSL_CTX_set_verify(FSSLCtx, LMode, nil);
  Result := True;
end;

function TOpenSSLContext.SetHostName(const aHostName: string): Boolean;
begin
  FHostName := aHostName;
  Result := True;
end;

function TOpenSSLContext.SetOptions(aOptions: TSSLOptions): Boolean;
var
  LOpts: Cardinal;
begin
  FOptions := aOptions;
  LOpts := 0;
  
  if soSingleDHUse in aOptions then
    LOpts := LOpts or SSL_OP_SINGLE_DH_USE;
  if soSingleECDHUse in aOptions then
    LOpts := LOpts or SSL_OP_SINGLE_ECDH_USE;
  if soNoSessionResumptionOnRenegotiation in aOptions then
    LOpts := LOpts or SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION;
  if soNoTicket in aOptions then
    LOpts := LOpts or SSL_OP_NO_TICKET;
    
  SSL_CTX_set_options(FSSLCtx, LOpts);
  Result := True;
end;

function TOpenSSLContext.CreateSession: ISSLSession;
begin
  Result := TOpenSSLSession.Create(Self);
end;

function TOpenSSLContext.GetLastError: Integer;
begin
  Result := FBackend.GetLastError;
end;

function TOpenSSLContext.GetLastErrorMessage: string;
begin
  Result := FBackend.GetLastErrorMessage;
end;

{ TOpenSSLSession }

constructor TOpenSSLSession.Create(aContext: TOpenSSLContext);
begin
  inherited Create;
  FContext := aContext;
  FConnected := False;
  FHandshakeComplete := False;
  
  // Create SSL structure
  FSSL := SSL_new(FContext.FSSLCtx);
  if FSSL = nil then
    raise Exception.Create('Failed to create SSL session');
    
  // Create BIO pair for I/O
  if BIO_new_bio_pair(@FBioRead, 0, @FBioWrite, 0) <> 1 then
  begin
    SSL_free(FSSL);
    raise Exception.Create('Failed to create BIO pair');
  end;
  
  SSL_set_bio(FSSL, FBioRead, FBioWrite);
end;

destructor TOpenSSLSession.Destroy;
begin
  if FConnected then
    Shutdown;
    
  if FSSL <> nil then
    SSL_free(FSSL);
    
  inherited Destroy;
end;

{ ... Rest of TOpenSSLSession implementation ... }

{ Helper functions }

function OpenSSLAvailable: Boolean;
begin
  Result := IsOpenSSLCoreLoaded;
end;

function LoadOpenSSL(const aLibraryPath: string = ''): Boolean;
begin
  try
    LoadOpenSSLCore(aLibraryPath);
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
begin
  if Assigned(OpenSSL_version) then
    Result := string(OpenSSL_version(OPENSSL_VERSION))
  else
    Result := 'Unknown';
end;

function GetOpenSSLVersionNumber: Cardinal;
begin
  if Assigned(OpenSSL_version_num) then
    Result := OpenSSL_version_num
  else
    Result := 0;
end;

{ Error handling }

function GetOpenSSLError: Cardinal;
begin
  Result := ERR_get_error;
end;

function GetOpenSSLErrorString(aError: Cardinal = 0): string;
var
  LError: Cardinal;
  LBuf: array[0..255] of AnsiChar;
begin
  if aError = 0 then
    LError := ERR_get_error
  else
    LError := aError;
    
  if LError = 0 then
    Result := 'No error'
  else
  begin
    ERR_error_string_n(LError, @LBuf[0], SizeOf(LBuf));
    Result := string(LBuf);
  end;
end;

procedure ClearOpenSSLErrors;
begin
  ERR_clear_error;
end;

{ Registration }

procedure RegisterOpenSSLBackend;
begin
  if GOpenSSLBackend = nil then
    GOpenSSLBackend := TOpenSSLBackend.Create;
    
  TSSLFactory.RegisterBackend('OpenSSL', GOpenSSLBackend);
end;

procedure UnregisterOpenSSLBackend;
begin
  TSSLFactory.UnregisterBackend('OpenSSL');
  
  if GOpenSSLBackend <> nil then
  begin
    GOpenSSLBackend.Free;
    GOpenSSLBackend := nil;
  end;
end;

initialization
  RegisterOpenSSLBackend;
  
finalization
  UnregisterOpenSSLBackend;

end.