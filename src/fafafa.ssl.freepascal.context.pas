{**
 * Unit: fafafa.ssl.freepascal.context
 * Purpose: 纯 FreePascal 后端上下文骨架实现
 *}

unit fafafa.ssl.freepascal.context;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, Base64,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.logging,
  fafafa.ssl.freepascal.context.material;

type
  TFreePascalPin = record
    Hash: TBytes;
    PinType: Integer;
    Description: string;
    IsBackup: Boolean;
  end;

  TFreePascalContext = class(TInterfacedObject, ISSLContext,
    IFreePascalContextMaterial, IFreePascalContextTrustStore)
  private
    FLibrary: ISSLLibrary;
    FContextType: TSSLContextType;
    FProtocolVersions: TSSLProtocolVersions;
    FPreferredVersion: TSSLProtocolVersion;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FCipherList: string;
    FCipherSuites: string;
    FSessionCacheEnabled: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FOptions: TSSLOptions;
    FServerName: string;
    FALPNProtocols: string;
    FCertVerifyFlags: TSSLCertVerifyFlags;
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;
    FCertificateStore: ISSLCertificateStore;
    FPinningEnabled: Boolean;
    FPins: array of TFreePascalPin;

    FCertificateFile: string;
    FCertificateData: TBytes;
    FPrivateKeyFile: string;
    FPrivateKeyData: TBytes;
    FCAFile: string;
    FCAPath: string;

    function ReadStreamToBytes(AStream: TStream): TBytes;
  public
    constructor Create(ALibrary: ISSLLibrary; AType: TSSLContextType);

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

    function HasCertificateMaterial: Boolean;
    function HasPrivateKeyMaterial: Boolean;
    function GetCertificateMaterial: TBytes;
    function GetPrivateKeyMaterial: TBytes;
    function GetCertificateStore: ISSLCertificateStore;
    function GetCAFile: string;
    function GetCAPath: string;
    function GetVerifyCallback: TSSLVerifyCallback;
    function GetInfoCallback: TSSLInfoCallback;
    function GetPins: TFreePascalPinInfoArray;
  end;

implementation

uses
  fafafa.ssl.exceptions,
  fafafa.ssl.pem,
  fafafa.ssl.freepascal.keydecrypt,
  fafafa.ssl.freepascal.connection;

procedure RaisePasswordProtectedPrivateKeyLoadFailure(const AContext, AMessage: string;
  AErrorCode: TSSLErrorCode);
begin
  raise ESSLKeyException.CreateWithContext(
    AMessage,
    AErrorCode,
    AContext,
    0,
    sslFreePascal
  );
end;

function ResolveEncryptedPrivateKeyPassword(
  ACallback: TSSLPasswordCallback;
  const AProvidedPassword: string;
  AIsRetry: Boolean;
  out APassword: string
): Boolean;
begin
  APassword := '';
  if (not AIsRetry) and (AProvidedPassword <> '') then
  begin
    APassword := AProvidedPassword;
    Exit(True);
  end;

  if Assigned(ACallback) then
    Result := ACallback(APassword, AIsRetry) and (APassword <> '')
  else
    Result := False;
end;

function NormalizePrivateKeyMaterial(
  const AData: TBytes;
  const AProvidedPassword: string;
  ACallback: TSSLPasswordCallback;
  const AContext: string
): TBytes;
var
  LResolvedPassword: string;
  LDecrypted: TBytes;
  LWasEncrypted: Boolean;
  LError: string;
begin
  Result := Copy(AData, 0, Length(AData));

  if TryDecryptPrivateKeyMaterial(Result, '', LDecrypted, LWasEncrypted, LError) then
    Exit;

  if not LWasEncrypted then
    Exit;

  if not ResolveEncryptedPrivateKeyPassword(ACallback, AProvidedPassword, False, LResolvedPassword) then
    RaisePasswordProtectedPrivateKeyLoadFailure(
      AContext,
      'Encrypted private key requires password or password callback',
      sslErrConfiguration
    );

  if TryDecryptPrivateKeyMaterial(Result, LResolvedPassword, LDecrypted, LWasEncrypted, LError) then
  begin
    Result := LDecrypted;
    Exit;
  end;

  if ResolveEncryptedPrivateKeyPassword(ACallback, '', True, LResolvedPassword) and
    TryDecryptPrivateKeyMaterial(Result, LResolvedPassword, LDecrypted, LWasEncrypted, LError) then
  begin
    Result := LDecrypted;
    Exit;
  end;

  RaisePasswordProtectedPrivateKeyLoadFailure(
    AContext,
    'Failed to decrypt encrypted private key: ' + LError,
    sslErrLoadFailed
  );
end;

constructor TFreePascalContext.Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FProtocolVersions := [sslProtocolTLS13];
  FPreferredVersion := sslProtocolTLS13;
  FVerifyMode := [sslVerifyPeer];
  FVerifyDepth := SSL_DEFAULT_VERIFY_DEPTH;
  FCipherList := SSL_DEFAULT_CIPHER_LIST;
  FCipherSuites := SSL_DEFAULT_TLS13_CIPHERSUITES;
  FSessionCacheEnabled := True;
  FSessionTimeout := SSL_DEFAULT_SESSION_TIMEOUT;
  FSessionCacheSize := SSL_DEFAULT_SESSION_CACHE_SIZE;
  FOptions := [ssoEnableSessionCache, ssoEnableSessionTickets, ssoEnableSNI, ssoEnableALPN];
  FServerName := '';
  FALPNProtocols := '';
  FCertVerifyFlags := [sslCertVerifyDefault];
  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;
  FCertificateStore := nil;
  FPinningEnabled := False;
  SetLength(FPins, 0);
  SetLength(FCertificateData, 0);
  SetLength(FPrivateKeyData, 0);
end;

function TFreePascalContext.ReadStreamToBytes(AStream: TStream): TBytes;
var
  LSize: Int64;
begin
  Result := nil;
  if AStream = nil then
    RaiseInvalidParameter('AStream');

  LSize := AStream.Size - AStream.Position;
  if LSize < 0 then
    LSize := 0;

  if LSize > 0 then
  begin
    SetLength(Result, LSize);
    AStream.ReadBuffer(Result[0], LSize);
  end;
end;

function TFreePascalContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TFreePascalContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;

  if (FPreferredVersion <> sslProtocolUnknown) and
    not (FPreferredVersion in FProtocolVersions) then
    FPreferredVersion := sslProtocolUnknown;

  LogDeprecatedProtocolWarnings('FreePascal', AVersions);
end;

function TFreePascalContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TFreePascalContext.SetPreferredVersion(AVersion: TSSLProtocolVersion);
begin
  if (AVersion <> sslProtocolUnknown) and
    not (AVersion in FProtocolVersions) then
    RaiseInvalidParameter('PreferredVersion');

  FPreferredVersion := AVersion;
end;

function TFreePascalContext.GetPreferredVersion: TSSLProtocolVersion;
begin
  Result := FPreferredVersion;
end;

procedure TFreePascalContext.LoadCertificate(const AFileName: string);
var
  LStream: TFileStream;
begin
  if not FileExists(AFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('Certificate file not found: %s', [AFileName]),
      sslErrLoadFailed,
      'TFreePascalContext.LoadCertificate',
      0,
      sslFreePascal
    );

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    FCertificateData := ReadStreamToBytes(LStream);
    FCertificateFile := AFileName;
  finally
    LStream.Free;
  end;
end;

procedure TFreePascalContext.LoadCertificate(AStream: TStream);
begin
  FCertificateData := ReadStreamToBytes(AStream);
end;

procedure TFreePascalContext.LoadCertificate(ACert: ISSLCertificate);
begin
  if ACert = nil then
    RaiseInvalidParameter('ACert');
  FCertificateData := ACert.SaveToDER;
end;

procedure TFreePascalContext.LoadPrivateKey(const AFileName: string; const APassword: string);
var
  LStream: TFileStream;
begin
  if not FileExists(AFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('Private key file not found: %s', [AFileName]),
      sslErrLoadFailed,
      'TFreePascalContext.LoadPrivateKey',
      0,
      sslFreePascal
    );

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    FPrivateKeyData := ReadStreamToBytes(LStream);
    FPrivateKeyFile := AFileName;
  finally
    LStream.Free;
  end;
  FPrivateKeyData := NormalizePrivateKeyMaterial(
    FPrivateKeyData,
    APassword,
    FPasswordCallback,
    'TFreePascalContext.LoadPrivateKey'
  );
end;

procedure TFreePascalContext.LoadPrivateKey(AStream: TStream; const APassword: string);
begin
  FPrivateKeyData := ReadStreamToBytes(AStream);
  FPrivateKeyData := NormalizePrivateKeyMaterial(
    FPrivateKeyData,
    APassword,
    FPasswordCallback,
    'TFreePascalContext.LoadPrivateKey'
  );
end;

procedure TFreePascalContext.LoadCertificatePEM(const APEM: string);
var
  LAnsi: AnsiString;
begin
  LAnsi := AnsiString(APEM);
  SetLength(FCertificateData, Length(LAnsi));
  if Length(LAnsi) > 0 then
    Move(LAnsi[1], FCertificateData[0], Length(LAnsi));
end;

procedure TFreePascalContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string);
var
  LAnsi: AnsiString;
begin
  LAnsi := AnsiString(APEM);
  SetLength(FPrivateKeyData, Length(LAnsi));
  if Length(LAnsi) > 0 then
    Move(LAnsi[1], FPrivateKeyData[0], Length(LAnsi));
  FPrivateKeyData := NormalizePrivateKeyMaterial(
    FPrivateKeyData,
    APassword,
    FPasswordCallback,
    'TFreePascalContext.LoadPrivateKeyPEM'
  );
end;

procedure TFreePascalContext.LoadCAFile(const AFileName: string);
begin
  if not FileExists(AFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('CA file not found: %s', [AFileName]),
      sslErrLoadFailed,
      'TFreePascalContext.LoadCAFile',
      0,
      sslFreePascal
    );

  FCAFile := AFileName;
end;

procedure TFreePascalContext.LoadCAPath(const APath: string);
begin
  if not DirectoryExists(APath) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('CA path not found: %s', [APath]),
      sslErrLoadFailed,
      'TFreePascalContext.LoadCAPath',
      0,
      sslFreePascal
    );

  FCAPath := APath;
end;

procedure TFreePascalContext.SetCertificateStore(AStore: ISSLCertificateStore);
begin
  FCertificateStore := AStore;
end;

procedure TFreePascalContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
end;

function TFreePascalContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TFreePascalContext.SetVerifyDepth(ADepth: Integer);
begin
  if ADepth < 0 then
    RaiseInvalidParameter('VerifyDepth');
  FVerifyDepth := ADepth;
end;

function TFreePascalContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TFreePascalContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
  FVerifyCallback := ACallback;
end;

procedure TFreePascalContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
end;

function TFreePascalContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TFreePascalContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TFreePascalContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

procedure TFreePascalContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheEnabled := AEnabled;
end;

function TFreePascalContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheEnabled;
end;

procedure TFreePascalContext.SetSessionTimeout(ATimeout: Integer);
begin
  if ATimeout < 0 then
    RaiseInvalidParameter('SessionTimeout');
  FSessionTimeout := ATimeout;
end;

function TFreePascalContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TFreePascalContext.SetSessionCacheSize(ASize: Integer);
begin
  if ASize < 0 then
    RaiseInvalidParameter('SessionCacheSize');
  FSessionCacheSize := ASize;
end;

function TFreePascalContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

procedure TFreePascalContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TFreePascalContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TFreePascalContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TFreePascalContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TFreePascalContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TFreePascalContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TFreePascalContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TFreePascalContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TFreePascalContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
  FPasswordCallback := ACallback;
end;

procedure TFreePascalContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
  FInfoCallback := ACallback;
end;

procedure TFreePascalContext.AddCertificatePin(const AHash: TBytes; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean);
var
  LIndex: Integer;
begin
  if Length(AHash) <> 32 then
    RaiseInvalidParameter('PinHash');

  LIndex := Length(FPins);
  SetLength(FPins, LIndex + 1);
  FPins[LIndex].Hash := Copy(AHash, 0, Length(AHash));
  FPins[LIndex].PinType := APinType;
  FPins[LIndex].Description := ADescription;
  FPins[LIndex].IsBackup := AIsBackup;
end;

procedure TFreePascalContext.AddCertificatePinBase64(const ABase64Hash: string; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean);
var
  LDecoded: string;
  LAnsi: AnsiString;
  LHash: TBytes;
begin
  LDecoded := DecodeStringBase64(ABase64Hash);
  LAnsi := AnsiString(LDecoded);
  SetLength(LHash, Length(LAnsi));
  if Length(LAnsi) > 0 then
    Move(LAnsi[1], LHash[0], Length(LAnsi));

  AddCertificatePin(LHash, APinType, ADescription, AIsBackup);
end;

procedure TFreePascalContext.SetCertificatePinningEnabled(AEnabled: Boolean);
begin
  FPinningEnabled := AEnabled;
end;

function TFreePascalContext.GetCertificatePinningEnabled: Boolean;
begin
  Result := FPinningEnabled;
end;

procedure TFreePascalContext.ClearCertificatePins;
begin
  SetLength(FPins, 0);
end;

function TFreePascalContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  Result := TFreePascalConnection.Create(Self as ISSLContext, ASocket);
end;

function TFreePascalContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  Result := TFreePascalConnection.Create(Self as ISSLContext, AStream);
end;

function TFreePascalContext.IsValid: Boolean;
begin
  Result := True;
end;

function TFreePascalContext.HasCertificateMaterial: Boolean;
begin
  Result := Length(FCertificateData) > 0;
end;

function TFreePascalContext.HasPrivateKeyMaterial: Boolean;
begin
  Result := Length(FPrivateKeyData) > 0;
end;

function TFreePascalContext.GetCertificateMaterial: TBytes;
begin
  Result := Copy(FCertificateData, 0, Length(FCertificateData));
end;

function TFreePascalContext.GetPrivateKeyMaterial: TBytes;
begin
  Result := Copy(FPrivateKeyData, 0, Length(FPrivateKeyData));
end;

function TFreePascalContext.GetCertificateStore: ISSLCertificateStore;
begin
  Result := FCertificateStore;
end;

function TFreePascalContext.GetCAFile: string;
begin
  Result := FCAFile;
end;

function TFreePascalContext.GetCAPath: string;
begin
  Result := FCAPath;
end;

function TFreePascalContext.GetVerifyCallback: TSSLVerifyCallback;
begin
  Result := FVerifyCallback;
end;

function TFreePascalContext.GetInfoCallback: TSSLInfoCallback;
begin
  Result := FInfoCallback;
end;

function TFreePascalContext.GetPins: TFreePascalPinInfoArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, Length(FPins));
  for I := 0 to High(FPins) do
  begin
    Result[I].Hash := Copy(FPins[I].Hash, 0, Length(FPins[I].Hash));
    Result[I].PinType := FPins[I].PinType;
    Result[I].Description := FPins[I].Description;
    Result[I].IsBackup := FPins[I].IsBackup;
  end;
end;

end.
