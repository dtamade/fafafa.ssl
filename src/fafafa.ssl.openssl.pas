unit fafafa.ssl.openssl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  fafafa.ssl.abstract.types,
  fafafa.ssl.abstract.intf,
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
  fafafa.ssl.openssl.api.crypto;

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

{ Initialization }
procedure RegisterOpenSSLBackend;
procedure UnregisterOpenSSLBackend;

implementation

uses
  fafafa.ssl.factory;

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
    Exit(True);
  // TODO: Load OpenSSL libraries
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
  Result := 'OpenSSL 3.0';
end;

function TOpenSSLLibrary.GetVersionNumber: Cardinal;
begin
  Result := $30000000;
end;

function TOpenSSLLibrary.GetCompileFlags: string;
begin
  Result := '';
end;

function TOpenSSLLibrary.IsProtocolSupported(aProtocol: TSSLProtocolVersion): Boolean;
begin
  Result := aProtocol in [sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12, sslProtocolTLS13];
end;

function TOpenSSLLibrary.IsCipherSupported(const aCipherName: string): Boolean;
begin
  Result := True;
end;

function TOpenSSLLibrary.IsFeatureSupported(const aFeatureName: string): Boolean;
begin
  Result := True;
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
  Result := 0;
end;

function TOpenSSLLibrary.GetLastErrorString: string;
begin
  Result := '';
end;

procedure TOpenSSLLibrary.ClearError;
begin
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
begin
  Result := TOpenSSLCertificate.Create(nil, False);
end;

function TOpenSSLLibrary.CreateCertificateStore: ISSLCertificateStore;
begin
  Result := nil; // TODO
end;

{ TOpenSSLContext }

constructor TOpenSSLContext.Create(ALibrary: TOpenSSLLibrary; AType: TSSLContextType);
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  SetupContext;
end;

destructor TOpenSSLContext.Destroy;
begin
  if FSSLCtx <> nil then
    SSL_CTX_free(FSSLCtx);
  inherited Destroy;
end;

procedure TOpenSSLContext.SetupContext;
begin
  // TODO: Setup SSL context
  FSSLCtx := nil;
end;

function TOpenSSLContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TOpenSSLContext.SetProtocolVersions(aVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := aVersions;
end;

function TOpenSSLContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TOpenSSLContext.LoadCertificate(const aFileName: string);
begin
  // TODO
end;

procedure TOpenSSLContext.LoadCertificate(aStream: TStream);
begin
  // TODO
end;

procedure TOpenSSLContext.LoadCertificate(aCert: ISSLCertificate);
begin
  // TODO
end;

procedure TOpenSSLContext.LoadPrivateKey(const aFileName: string; const aPassword: string);
begin
  // TODO
end;

procedure TOpenSSLContext.LoadPrivateKey(aStream: TStream; const aPassword: string);
begin
  // TODO
end;

procedure TOpenSSLContext.LoadCAFile(const aFileName: string);
begin
  // TODO
end;

procedure TOpenSSLContext.LoadCAPath(const aPath: string);
begin
  // TODO
end;

procedure TOpenSSLContext.SetCertificateStore(aStore: ISSLCertificateStore);
begin
  // TODO
end;

procedure TOpenSSLContext.SetVerifyMode(aMode: TSSLVerifyModes);
begin
  FVerifyMode := aMode;
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
  // TODO
end;

procedure TOpenSSLContext.SetCipherList(const aCipherList: string);
begin
  FCipherList := aCipherList;
end;

function TOpenSSLContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TOpenSSLContext.SetCipherSuites(const aCipherSuites: string);
begin
  FCipherSuites := aCipherSuites;
end;

function TOpenSSLContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

procedure TOpenSSLContext.SetSessionCacheMode(aEnabled: Boolean);
begin
  // TODO
end;

function TOpenSSLContext.GetSessionCacheMode: Boolean;
begin
  Result := False;
end;

procedure TOpenSSLContext.SetSessionTimeout(aTimeout: Integer);
begin
  // TODO
end;

function TOpenSSLContext.GetSessionTimeout: Integer;
begin
  Result := 0;
end;

procedure TOpenSSLContext.SetSessionCacheSize(aSize: Integer);
begin
  // TODO
end;

function TOpenSSLContext.GetSessionCacheSize: Integer;
begin
  Result := 0;
end;

procedure TOpenSSLContext.SetOptions(aOptions: Cardinal);
begin
  FOptions := aOptions;
end;

function TOpenSSLContext.GetOptions: Cardinal;
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
  FALPNProtocols := aProtocols;
end;

function TOpenSSLContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TOpenSSLContext.SetPasswordCallback(aCallback: TSSLPasswordCallback);
begin
  // TODO
end;

procedure TOpenSSLContext.SetInfoCallback(aCallback: TSSLInfoCallback);
begin
  // TODO
end;

function TOpenSSLContext.CreateConnection(aSocket: THandle): ISSLConnection;
begin
  Result := TOpenSSLConnection.Create(Self, aSocket);
end;

function TOpenSSLContext.CreateConnection(aStream: TStream): ISSLConnection;
begin
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
  LName := X509_get_subject_name(FCert);
  if LName = nil then Exit;
  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    X509_NAME_print_ex(LBio, LName, 0, 0);
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
  LName := X509_get_issuer_name(FCert);
  if LName = nil then Exit;
  LBio := BIO_new(BIO_s_mem);
  if LBio = nil then Exit;
  try
    X509_NAME_print_ex(LBio, LName, 0, 0);
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
end;

function TOpenSSLCertificate.GetSerialNumber: string;
var
  LSerial: PASN1_INTEGER;
  LBN: PBIGNUM;
  LHex: PAnsiChar;
begin
  Result := '';
  if FCert = nil then Exit;
  LSerial := X509_get_serialNumber(FCert);
  if LSerial = nil then Exit;
  LBN := ASN1_INTEGER_to_BN(LSerial, nil);
  if LBN = nil then Exit;
  try
      LHex := BN_bn2hex(LBN);
    if LHex <> nil then
    begin
      Result := string(LHex);
      OPENSSL_free(LHex);  // Use OPENSSL_free instead of CRYPTO_free
    end;
  finally
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
begin
  Result := False; // TODO
end;

function TOpenSSLCertificate.VerifyHostname(const aHostname: string): Boolean;
begin
  Result := False; // TODO
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
begin
  Result := False; // TODO
end;

function TOpenSSLCertificate.GetExtension(const aOID: string): string;
begin
  Result := ''; // TODO
end;

function TOpenSSLCertificate.GetSubjectAltNames: TStringList;
begin
  Result := TStringList.Create; // TODO
end;

function TOpenSSLCertificate.GetKeyUsage: TStringList;
begin
  Result := TStringList.Create; // TODO
end;

function TOpenSSLCertificate.GetExtendedKeyUsage: TStringList;
begin
  Result := TStringList.Create; // TODO
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
  if X509_digest(FCert, LMD, @LDigest[0], @LLen) = 1 then
  begin
    for I := 0 to LLen - 1 do
    begin
      if I > 0 then Result := Result + ':';
      Result := Result + IntToHex(LDigest[I], 2);
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
  // TODO: Create SSL structure
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
  // TODO: Create SSL structure
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
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.Accept: Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.Shutdown: Boolean;
begin
  Result := False; // TODO
end;

procedure TOpenSSLConnection.Close;
begin
  // TODO
end;

function TOpenSSLConnection.DoHandshake: TSSLHandshakeState;
begin
  Result := sslHsNotStarted; // TODO
end;

function TOpenSSLConnection.IsHandshakeComplete: Boolean;
begin
  Result := FHandshakeComplete;
end;

function TOpenSSLConnection.Renegotiate: Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.Read(var aBuffer; aCount: Integer): Integer;
begin
  Result := 0; // TODO
end;

function TOpenSSLConnection.Write(const aBuffer; aCount: Integer): Integer;
begin
  Result := 0; // TODO
end;

function TOpenSSLConnection.ReadString(out aStr: string): Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.WriteString(const aStr: string): Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.WantRead: Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.WantWrite: Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.GetError(aRet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone; // TODO
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
begin
  FillChar(Result, SizeOf(Result), 0);
  // TODO
end;

function TOpenSSLConnection.GetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS12; // TODO
end;

function TOpenSSLConnection.GetCipherName: string;
begin
  Result := ''; // TODO
end;

function TOpenSSLConnection.GetPeerCertificate: ISSLCertificate;
begin
  Result := nil; // TODO
end;

function TOpenSSLConnection.GetPeerCertificateChain: TSSLCertificateArray;
begin
  SetLength(Result, 0); // TODO
end;

function TOpenSSLConnection.GetVerifyResult: Integer;
begin
  Result := 0; // TODO
end;

function TOpenSSLConnection.GetVerifyResultString: string;
begin
  Result := ''; // TODO
end;

function TOpenSSLConnection.GetSession: ISSLSession;
begin
  Result := nil; // TODO
end;

procedure TOpenSSLConnection.SetSession(aSession: ISSLSession);
begin
  // TODO
end;

function TOpenSSLConnection.IsSessionReused: Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.GetSelectedALPNProtocol: string;
begin
  Result := ''; // TODO
end;

function TOpenSSLConnection.GetALPNProtocol: string;
begin
  Result := GetSelectedALPNProtocol;
end;

function TOpenSSLConnection.GetSNI: string;
begin
  Result := ''; // TODO
end;

function TOpenSSLConnection.IsConnected: Boolean;
begin
  Result := False; // TODO
end;

function TOpenSSLConnection.GetState: string;
begin
  Result := ''; // TODO
end;

function TOpenSSLConnection.GetStateString: string;
begin
  Result := ''; // TODO
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
    if aLibraryPath <> '' then
      Result := LoadOpenSSLLibrary(aLibraryPath)
    else
      Result := LoadOpenSSLLibrary;
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

function LoadCertificateFromFile(const aFileName: string): PX509;
begin
  Result := nil; // TODO
end;

function LoadCertificateFromMemory(const aData: Pointer; aSize: Integer): PX509;
begin
  Result := nil; // TODO
end;

function LoadPrivateKeyFromFile(const aFileName: string; const aPassword: string): PEVP_PKEY;
begin
  Result := nil; // TODO
end;

function LoadPrivateKeyFromMemory(const aData: Pointer; aSize: Integer; const aPassword: string): PEVP_PKEY;
begin
  Result := nil; // TODO
end;

function VerifyCertificate(aCert: PX509; aCAStore: PX509_STORE): Boolean;
begin
  Result := False; // TODO
end;

function GetCertificateInfo(aCert: PX509): TSSLCertificateInfo;
begin
  FillChar(Result, SizeOf(Result), 0); // TODO
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
  UnregisterOpenSSLBackend;

end.
