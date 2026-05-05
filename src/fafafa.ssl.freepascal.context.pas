{**
 * Unit: fafafa.ssl.freepascal.context
 * Purpose: 纯 FreePascal 后端上下文骨架实现
 *}

unit fafafa.ssl.freepascal.context;

{$mode ObjFPC}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

interface

uses
  SysUtils, Classes, Base64, DateUtils,
  fafafa.ssl.base,
  fafafa.ssl.errors,
  fafafa.ssl.logging,
  fafafa.ssl.freepascal.context.material,
  fafafa.ssl.freepascal.earlydatareplay,
  fafafa.ssl.freepascal.session;

type
  TFreePascalPin = record
    Hash: TBytes;
    PinType: Integer;
    Description: string;
    IsBackup: Boolean;
  end;

  TFreePascalResumptionCacheEntry = record
    Key: string;
    Session: ISSLSession;
  end;

  TFreePascalContext = class(TInterfacedObject,
    ISSLContext,
    ISSLHttpHooksAccess,
    ISSLEarlyDataContext,
    ISSLServerOCSPStaplingContext,
    IFreePascalContextMaterial,
    IFreePascalContextTrustStore,
    IFreePascalContextRevocationMaterial,
    IFreePascalContextServerStaplingMaterial,
    IFreePascalContextEarlyDataReplayProviderInstaller,
    IFreePascalContextEarlyDataReplayInstaller,
    IFreePascalContextEarlyDataReplayDirectoryInstaller,
    IFreePascalResumptionCache,
    IFreePascalEarlyDataReplayLedger,
    IFreePascalEarlyDataReplayLedgerAccess)
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
    FClientEarlyDataEnabled: Boolean;
    FServerEarlyDataPolicy: TSSLEarlyDataServerPolicy;
    FServerMaxEarlyDataSize: Cardinal;
    FCertVerifyFlags: TSSLCertVerifyFlags;
    FVerifyCallback: TSSLVerifyCallback;
    FPasswordCallback: TSSLPasswordCallback;
    FInfoCallback: TSSLInfoCallback;
    FHTTPGetCallback: TSSLHTTPGetCallback;
    FHTTPPostCallback: TSSLHTTPPostCallback;
    FCertificateStore: ISSLCertificateStore;
    FPinningEnabled: Boolean;
    FPins: array of TFreePascalPin;
    FResumptionCache: array of TFreePascalResumptionCacheEntry;
    FDefaultEarlyDataReplayLedger: IFreePascalManagedEarlyDataReplayLedger;
    FActiveEarlyDataReplayLedger: IFreePascalEarlyDataReplayLedger;

    FCertificateFile: string;
    FCertificateData: TBytes;
    FPrivateKeyFile: string;
    FPrivateKeyData: TBytes;
    FCAFile: string;
    FCAPath: string;
    FCRLMaterial: TStringList;
    FServerStapledOCSPResponse: TBytes;

    function ReadStreamToBytes(AStream: TStream): TBytes;
    function TicketKey(const ATicket: TBytes): string;
    procedure PruneResumptionCache;
    procedure EnforceResumptionCacheLimit;
  public
    constructor Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
    destructor Destroy; override;

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

    procedure SetClientEarlyDataEnabled(AEnabled: Boolean);
    function GetClientEarlyDataEnabled: Boolean;
    procedure SetServerEarlyDataPolicy(APolicy: TSSLEarlyDataServerPolicy);
    function GetServerEarlyDataPolicy: TSSLEarlyDataServerPolicy;
    procedure SetServerMaxEarlyDataSize(ASize: Cardinal);
    function GetServerMaxEarlyDataSize: Cardinal;

    procedure SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
    function GetCertVerifyFlags: TSSLCertVerifyFlags;

    procedure SetPasswordCallback(ACallback: TSSLPasswordCallback);
    procedure SetInfoCallback(ACallback: TSSLInfoCallback);
    procedure SetHTTPGetCallback(ACallback: TSSLHTTPGetCallback);
    function GetHTTPGetCallback: TSSLHTTPGetCallback;
    procedure SetHTTPPostCallback(ACallback: TSSLHTTPPostCallback);
    function GetHTTPPostCallback: TSSLHTTPPostCallback;

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
    function BuildVerificationStore: ISSLCertificateStore;
    procedure ClearCRLMaterial;
    procedure AddCRLPEM(const APEM: string);
    procedure AddCRLFile(const AFileName: string);
    function BuildCRLStore: TStringList;
    procedure ClearServerStapledOCSPResponse;
    procedure SetServerStapledOCSPResponse(const AResponseDER: TBytes);
    procedure LoadServerStapledOCSPResponseFile(const AFileName: string);
    function HasServerStapledOCSPResponse: Boolean;
    function GetServerStapledOCSPResponse: TBytes;

    function CanIssueSessionTickets: Boolean;
    function TryGetResumptionSession(const ATicket: TBytes; out ASession: ISSLSession): Boolean;
    procedure StoreResumptionSession(ASession: ISSLSession);
    function InstallReplayProviderBackedLedger(
      AProvider: IFreePascalEarlyDataReplayProvider
    ): Boolean;
    function InstallFileBackedReplayLedger(const AFileName: string): Boolean;
    function InstallDirectoryBackedReplayLedger(
      const ADirectoryName: string
    ): Boolean;
    function GetEarlyDataReplayLedger: IFreePascalEarlyDataReplayLedger;
    procedure SetEarlyDataReplayLedger(ALedger: IFreePascalEarlyDataReplayLedger);
    procedure ResetEarlyDataReplayLedger;
    function TryAcquireEarlyDataSession(ASession: ISSLSession): Boolean;
  end;

implementation

uses
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.earlydatareplay.dirstore,
  fafafa.ssl.freepascal.earlydatareplay.fileprovider,
  fafafa.ssl.freepascal.connection;

constructor TFreePascalContext.Create(ALibrary: ISSLLibrary; AType: TSSLContextType);
begin
  inherited Create;
  FLibrary := ALibrary;
  FContextType := AType;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
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
  FClientEarlyDataEnabled := False;
  FServerEarlyDataPolicy := sslEarlyDataServerReject;
  FServerMaxEarlyDataSize := 0;
  FCertVerifyFlags := [sslCertVerifyDefault];
  FVerifyCallback := nil;
  FPasswordCallback := nil;
  FInfoCallback := nil;
  FHTTPGetCallback := nil;
  FHTTPPostCallback := nil;
  FCertificateStore := nil;
  FPinningEnabled := False;
  SetLength(FPins, 0);
  SetLength(FResumptionCache, 0);
  if AType in [sslCtxServer, sslCtxBoth] then
    FDefaultEarlyDataReplayLedger :=
      TFreePascalDefaultPersistentEarlyDataReplayLedger.Create(
        FSessionCacheEnabled,
        FSessionCacheSize
      )
  else
    FDefaultEarlyDataReplayLedger := TFreePascalInMemoryEarlyDataReplayLedger.Create(
      FSessionCacheEnabled,
      FSessionCacheSize
    );
  FActiveEarlyDataReplayLedger := FDefaultEarlyDataReplayLedger;
  SetLength(FCertificateData, 0);
  SetLength(FPrivateKeyData, 0);
  SetLength(FServerStapledOCSPResponse, 0);
  FCRLMaterial := TStringList.Create;
end;

destructor TFreePascalContext.Destroy;
begin
  FCRLMaterial.Free;
  inherited Destroy;
end;

function TFreePascalContext.ReadStreamToBytes(AStream: TStream): TBytes;
var
  LSize: Int64;
begin
  if AStream = nil then
    RaiseInvalidParameter('AStream');

  LSize := AStream.Size - AStream.Position;
  if LSize < 0 then
    LSize := 0;

  SetLength(Result, LSize);
  if LSize > 0 then
    AStream.ReadBuffer(Result[0], LSize);
end;

function TFreePascalContext.TicketKey(const ATicket: TBytes): string;
const
  HEX_DIGITS: array[0..15] of Char = '0123456789abcdef';
var
  I: Integer;
begin
  SetLength(Result, Length(ATicket) * 2);
  for I := 0 to High(ATicket) do
  begin
    Result[I * 2 + 1] := HEX_DIGITS[(ATicket[I] shr 4) and $0F];
    Result[I * 2 + 2] := HEX_DIGITS[ATicket[I] and $0F];
  end;
end;

procedure TFreePascalContext.PruneResumptionCache;
var
  I: Integer;
  LWriteIndex: Integer;
begin
  LWriteIndex := 0;
  for I := 0 to High(FResumptionCache) do
    if (FResumptionCache[I].Key <> '') and
       (FResumptionCache[I].Session <> nil) and
       FResumptionCache[I].Session.IsValid then
    begin
      if LWriteIndex <> I then
        FResumptionCache[LWriteIndex] := FResumptionCache[I];
      Inc(LWriteIndex);
    end;
  SetLength(FResumptionCache, LWriteIndex);
end;

procedure TFreePascalContext.EnforceResumptionCacheLimit;
var
  I: Integer;
  LOverflow: Integer;
begin
  if FSessionCacheSize <= 0 then
  begin
    SetLength(FResumptionCache, 0);
    Exit;
  end;

  if Length(FResumptionCache) <= FSessionCacheSize then
    Exit;

  LOverflow := Length(FResumptionCache) - FSessionCacheSize;
  for I := 0 to FSessionCacheSize - 1 do
    FResumptionCache[I] := FResumptionCache[I + LOverflow];
  SetLength(FResumptionCache, FSessionCacheSize);
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

  if APassword <> '' then;
end;

procedure TFreePascalContext.LoadPrivateKey(AStream: TStream; const APassword: string);
begin
  FPrivateKeyData := ReadStreamToBytes(AStream);
  if APassword <> '' then;
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

  if APassword <> '' then;
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
var
  LManagedLedger: IFreePascalManagedEarlyDataReplayLedger;
begin
  FSessionCacheEnabled := AEnabled;
  if not AEnabled then
    SetLength(FResumptionCache, 0);

  if FDefaultEarlyDataReplayLedger <> nil then
    FDefaultEarlyDataReplayLedger.SetEnabled(AEnabled);

  if Supports(FActiveEarlyDataReplayLedger, IFreePascalManagedEarlyDataReplayLedger, LManagedLedger) then
    LManagedLedger.SetEnabled(AEnabled);
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
var
  LManagedLedger: IFreePascalManagedEarlyDataReplayLedger;
begin
  if ASize < 0 then
    RaiseInvalidParameter('SessionCacheSize');
  FSessionCacheSize := ASize;
  EnforceResumptionCacheLimit;

  if FDefaultEarlyDataReplayLedger <> nil then
    FDefaultEarlyDataReplayLedger.SetCapacity(ASize);

  if Supports(FActiveEarlyDataReplayLedger, IFreePascalManagedEarlyDataReplayLedger, LManagedLedger) then
    LManagedLedger.SetCapacity(ASize);
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

procedure TFreePascalContext.SetClientEarlyDataEnabled(AEnabled: Boolean);
begin
  FClientEarlyDataEnabled := AEnabled;
end;

function TFreePascalContext.GetClientEarlyDataEnabled: Boolean;
begin
  Result := FClientEarlyDataEnabled;
end;

procedure TFreePascalContext.SetServerEarlyDataPolicy(APolicy: TSSLEarlyDataServerPolicy);
begin
  FServerEarlyDataPolicy := APolicy;
end;

function TFreePascalContext.GetServerEarlyDataPolicy: TSSLEarlyDataServerPolicy;
begin
  Result := FServerEarlyDataPolicy;
end;

procedure TFreePascalContext.SetServerMaxEarlyDataSize(ASize: Cardinal);
begin
  FServerMaxEarlyDataSize := ASize;
end;

function TFreePascalContext.GetServerMaxEarlyDataSize: Cardinal;
begin
  Result := FServerMaxEarlyDataSize;
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

procedure TFreePascalContext.SetHTTPGetCallback(ACallback: TSSLHTTPGetCallback);
begin
  FHTTPGetCallback := ACallback;
end;

function TFreePascalContext.GetHTTPGetCallback: TSSLHTTPGetCallback;
begin
  Result := FHTTPGetCallback;
end;

procedure TFreePascalContext.SetHTTPPostCallback(ACallback: TSSLHTTPPostCallback);
begin
  FHTTPPostCallback := ACallback;
end;

function TFreePascalContext.GetHTTPPostCallback: TSSLHTTPPostCallback;
begin
  Result := FHTTPPostCallback;
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

function TFreePascalContext.BuildVerificationStore: ISSLCertificateStore;
var
  I: Integer;
  LCertificate: ISSLCertificate;
  LClonedCertificate: ISSLCertificate;
begin
  Result := nil;

  if FLibrary = nil then
    Exit;

  try
    Result := FLibrary.CreateCertificateStore;
  except
    Exit(nil);
  end;

  if Result = nil then
    Exit;

  if FCertificateStore <> nil then
    for I := 0 to FCertificateStore.GetCount - 1 do
    begin
      LCertificate := FCertificateStore.GetCertificate(I);
      if LCertificate = nil then
        Continue;

      LClonedCertificate := LCertificate.Clone;
      if LClonedCertificate <> nil then
        Result.AddCertificate(LClonedCertificate);
    end;

  if FCAFile <> '' then
    Result.LoadFromFile(FCAFile);

  if FCAPath <> '' then
    Result.LoadFromPath(FCAPath);

  if Result.GetCount = 0 then
    Result := nil;
end;

procedure TFreePascalContext.ClearCRLMaterial;
begin
  FCRLMaterial.Clear;
end;

procedure TFreePascalContext.AddCRLPEM(const APEM: string);
begin
  if Trim(APEM) = '' then
    RaiseInvalidParameter('APEM');
  FCRLMaterial.Add(APEM);
end;

procedure TFreePascalContext.AddCRLFile(const AFileName: string);
var
  LCRLText: TStringList;
begin
  if not FileExists(AFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('CRL file not found: %s', [AFileName]),
      sslErrLoadFailed,
      'TFreePascalContext.AddCRLFile',
      0,
      sslFreePascal
    );

  LCRLText := TStringList.Create;
  try
    LCRLText.LoadFromFile(AFileName);
    AddCRLPEM(LCRLText.Text);
  finally
    LCRLText.Free;
  end;
end;

function TFreePascalContext.BuildCRLStore: TStringList;
begin
  if (FCRLMaterial = nil) or (FCRLMaterial.Count = 0) then
    Exit(nil);

  Result := TStringList.Create;
  Result.Assign(FCRLMaterial);
end;

procedure TFreePascalContext.ClearServerStapledOCSPResponse;
begin
  SetLength(FServerStapledOCSPResponse, 0);
end;

procedure TFreePascalContext.SetServerStapledOCSPResponse(const AResponseDER: TBytes);
begin
  if Length(AResponseDER) = 0 then
  begin
    ClearServerStapledOCSPResponse;
    Exit;
  end;

  FServerStapledOCSPResponse := Copy(AResponseDER, 0, Length(AResponseDER));
end;

procedure TFreePascalContext.LoadServerStapledOCSPResponseFile(const AFileName: string);
const
  MAX_OCSP_RESPONSE_SIZE = 1024 * 1024; // 1MB
var
  LStream: TFileStream;
  LSize: Int64;
begin
  if not FileExists(AFileName) then
    raise ESSLFileNotFoundException.CreateWithContext(
      Format('Server stapled OCSP response file not found: %s', [AFileName]),
      sslErrLoadFailed,
      'TFreePascalContext.LoadServerStapledOCSPResponseFile',
      0,
      sslFreePascal
    );

  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
  try
    LSize := LStream.Size;
    if LSize = 0 then
      raise ESSLInvalidArgument.Create(
        'OCSP response file is empty',
        sslErrInvalidParam
      );
    if LSize > MAX_OCSP_RESPONSE_SIZE then
      raise ESSLInvalidArgument.Create(
        Format('OCSP response file too large (%d bytes, max %d)',
          [LSize, MAX_OCSP_RESPONSE_SIZE]),
        sslErrInvalidParam
      );
    SetServerStapledOCSPResponse(ReadStreamToBytes(LStream));
  finally
    LStream.Free;
  end;
end;

function TFreePascalContext.HasServerStapledOCSPResponse: Boolean;
begin
  Result := Length(FServerStapledOCSPResponse) > 0;
end;

function TFreePascalContext.GetServerStapledOCSPResponse: TBytes;
begin
  Result := Copy(FServerStapledOCSPResponse, 0, Length(FServerStapledOCSPResponse));
end;

function TFreePascalContext.CanIssueSessionTickets: Boolean;
begin
  Result :=
    (FContextType = sslCtxServer) and
    FSessionCacheEnabled and
    (FSessionCacheSize <> 0);
end;

function TFreePascalContext.TryGetResumptionSession(const ATicket: TBytes; out ASession: ISSLSession): Boolean;
var
  I: Integer;
  LKey: string;
begin
  ASession := nil;
  Result := False;

  if not CanIssueSessionTickets or (Length(ATicket) = 0) then
    Exit;

  PruneResumptionCache;
  LKey := TicketKey(ATicket);
  for I := 0 to High(FResumptionCache) do
    if FResumptionCache[I].Key = LKey then
    begin
      if FResumptionCache[I].Session <> nil then
        ASession := FResumptionCache[I].Session.Clone;
      Exit(ASession <> nil);
    end;
end;

procedure TFreePascalContext.StoreResumptionSession(ASession: ISSLSession);
var
  I: Integer;
  LEntryIndex: Integer;
  LKey: string;
  LStoredSession: ISSLSession;
  LResumptionSession: IFreePascalResumptionSession;
begin
  if not CanIssueSessionTickets then
    Exit;
  if (ASession = nil) or (not ASession.IsValid) or (not ASession.IsResumable) then
    Exit;
  if not Supports(ASession, IFreePascalResumptionSession, LResumptionSession) then
    Exit;

  LStoredSession := ASession.Clone;
  if LStoredSession = nil then
    Exit;
  LStoredSession.SetTimeout(FSessionTimeout);

  LKey := TicketKey(LResumptionSession.GetTicket);
  if LKey = '' then
    Exit;

  PruneResumptionCache;
  LEntryIndex := -1;
  for I := 0 to High(FResumptionCache) do
    if FResumptionCache[I].Key = LKey then
    begin
      LEntryIndex := I;
      Break;
    end;

  if LEntryIndex >= 0 then
  begin
    FResumptionCache[LEntryIndex].Session := LStoredSession;
    Exit;
  end;

  LEntryIndex := Length(FResumptionCache);
  SetLength(FResumptionCache, LEntryIndex + 1);
  FResumptionCache[LEntryIndex].Key := LKey;
  FResumptionCache[LEntryIndex].Session := LStoredSession;
  EnforceResumptionCacheLimit;
end;

function TFreePascalContext.InstallFileBackedReplayLedger(
  const AFileName: string
): Boolean;
var
  LProvider: IFreePascalEarlyDataReplayProvider;
begin
  Result := False;

  if Trim(AFileName) = '' then
    Exit;

  try
    LProvider := TFreePascalFileEarlyDataReplayProvider.Create(AFileName);
    Result := InstallReplayProviderBackedLedger(LProvider);
  except
    Result := False;
  end;
end;

function TFreePascalContext.InstallDirectoryBackedReplayLedger(
  const ADirectoryName: string
): Boolean;
var
  LStore: IFreePascalEarlyDataReplayStore;
begin
  Result := False;

  if Trim(ADirectoryName) = '' then
    Exit;

  try
    LStore := TFreePascalDirectoryEarlyDataReplayStore.Create(ADirectoryName);
    Result := InstallStoreBackedReplayLedger(Self, LStore);
  except
    Result := False;
  end;
end;

function TFreePascalContext.InstallReplayProviderBackedLedger(
  AProvider: IFreePascalEarlyDataReplayProvider
): Boolean;
var
  LLedger: IFreePascalManagedEarlyDataReplayLedger;
begin
  Result := False;

  if AProvider = nil then
    Exit;

  try
    LLedger := TFreePascalProviderBackedEarlyDataReplayLedger.Create(
      AProvider,
      FSessionCacheEnabled,
      FSessionCacheSize
    );
    SetEarlyDataReplayLedger(LLedger);
    Result := True;
  except
    Result := False;
  end;
end;

function TFreePascalContext.GetEarlyDataReplayLedger: IFreePascalEarlyDataReplayLedger;
begin
  Result := FActiveEarlyDataReplayLedger;
  if Result = nil then
    Result := FDefaultEarlyDataReplayLedger;
end;

procedure TFreePascalContext.SetEarlyDataReplayLedger(
  ALedger: IFreePascalEarlyDataReplayLedger
);
var
  LManagedLedger: IFreePascalManagedEarlyDataReplayLedger;
begin
  if ALedger = nil then
    FActiveEarlyDataReplayLedger := FDefaultEarlyDataReplayLedger
  else
    FActiveEarlyDataReplayLedger := ALedger;

  if Supports(FActiveEarlyDataReplayLedger, IFreePascalManagedEarlyDataReplayLedger, LManagedLedger) then
  begin
    LManagedLedger.SetEnabled(FSessionCacheEnabled);
    LManagedLedger.SetCapacity(FSessionCacheSize);
  end;
end;

procedure TFreePascalContext.ResetEarlyDataReplayLedger;
begin
  FActiveEarlyDataReplayLedger := FDefaultEarlyDataReplayLedger;
end;

function TFreePascalContext.TryAcquireEarlyDataSession(ASession: ISSLSession): Boolean;
var
  LLedger: IFreePascalEarlyDataReplayLedger;
begin
  Result := False;
  if (not FSessionCacheEnabled) or (FSessionCacheSize <= 0) then
    Exit;

  LLedger := GetEarlyDataReplayLedger;
  if LLedger = nil then
    Exit;

  Result := LLedger.TryAcquireEarlyDataSession(ASession);
end;

end.
