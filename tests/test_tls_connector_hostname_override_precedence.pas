program test_tls_connector_hostname_override_precedence;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.connection.base,
  fafafa.ssl.tls;

type
  TTestConnectorConnection = class(TBaseSSLConnection, ISSLClientConnection)
  private
    FServerName: string;
    FSession: ISSLSession;
  public
    constructor Create(AContext: ISSLContext); override;
    procedure SetServerName(const AServerName: string);
    function GetServerName: string;
  protected
    function DoRead(var ABuffer; ACount: Integer): Integer; override;
    function DoWrite(const ABuffer; ACount: Integer): Integer; override;
    function DoConnect: Boolean; override;
    function DoAccept: Boolean; override;
    function DoHandshakeInternal: TSSLHandshakeState; override;
    function DoShutdown: Boolean; override;
    procedure DoClose; override;
    function DoRenegotiate: Boolean; override;
    function DoGetError(ARet: Integer): TSSLErrorCode; override;
    function DoWantRead: Boolean; override;
    function DoWantWrite: Boolean; override;
    function DoGetProtocolVersion: TSSLProtocolVersion; override;
    function DoGetCipherName: string; override;
    function DoGetPeerCertificate: ISSLCertificate; override;
    function DoGetPeerCertificateChain: TSSLCertificateArray; override;
    function DoGetVerifyResult: Integer; override;
    function DoGetVerifyResultString: string; override;
    function DoGetSession: ISSLSession; override;
    procedure DoSetSession(ASession: ISSLSession); override;
    function DoIsSessionReused: Boolean; override;
    function DoGetSelectedALPNProtocol: string; override;
    function DoGetState: string; override;
    function DoGetNativeHandle: Pointer; override;
  end;

  TTestConnectorContext = class(TInterfacedObject, ISSLContext)
  private
    FContextType: TSSLContextType;
    FProtocolVersions: TSSLProtocolVersions;
    FPreferredVersion: TSSLProtocolVersion;
    FVerifyMode: TSSLVerifyModes;
    FVerifyDepth: Integer;
    FCipherList: string;
    FCipherSuites: string;
    FSessionCacheMode: Boolean;
    FSessionTimeout: Integer;
    FSessionCacheSize: Integer;
    FOptions: TSSLOptions;
    FServerName: string;
    FALPNProtocols: string;
    FCertVerifyFlags: TSSLCertVerifyFlags;
    FCertificateStore: ISSLCertificateStore;
    FPinningEnabled: Boolean;
  public
    constructor Create(AContextType: TSSLContextType);
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
  end;

procedure Require(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
  begin
    WriteLn('[FAIL] ', AMessage);
    Halt(1);
  end;
end;

procedure RequireEquals(const AName, AExpected, AActual: string);
begin
  if AExpected <> AActual then
  begin
    WriteLn('[FAIL] ', AName, ' expected="', AExpected, '" actual="', AActual, '"');
    Halt(1);
  end;
end;

constructor TTestConnectorConnection.Create(AContext: ISSLContext);
begin
  inherited Create(AContext);
  FServerName := '';
  if AContext.GetContextType = sslCtxClient then
  begin
    {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
    FServerName := AContext.GetServerName;
    {$POP}
  end;
  FSession := nil;
end;

procedure TTestConnectorConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TTestConnectorConnection.GetServerName: string;
begin
  Result := FServerName;
end;

function TTestConnectorConnection.DoRead(var ABuffer; ACount: Integer): Integer;
begin
  Result := 0;
end;

function TTestConnectorConnection.DoWrite(const ABuffer; ACount: Integer): Integer;
begin
  Result := ACount;
end;

function TTestConnectorConnection.DoConnect: Boolean;
begin
  Result := True;
end;

function TTestConnectorConnection.DoAccept: Boolean;
begin
  Result := True;
end;

function TTestConnectorConnection.DoHandshakeInternal: TSSLHandshakeState;
begin
  Result := sslHsCompleted;
end;

function TTestConnectorConnection.DoShutdown: Boolean;
begin
  Result := True;
end;

procedure TTestConnectorConnection.DoClose;
begin
end;

function TTestConnectorConnection.DoRenegotiate: Boolean;
begin
  Result := False;
end;

function TTestConnectorConnection.DoGetError(ARet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone;
end;

function TTestConnectorConnection.DoWantRead: Boolean;
begin
  Result := False;
end;

function TTestConnectorConnection.DoWantWrite: Boolean;
begin
  Result := False;
end;

function TTestConnectorConnection.DoGetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS13;
end;

function TTestConnectorConnection.DoGetCipherName: string;
begin
  Result := 'TEST';
end;

function TTestConnectorConnection.DoGetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TTestConnectorConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := nil;
end;

function TTestConnectorConnection.DoGetVerifyResult: Integer;
begin
  Result := 0;
end;

function TTestConnectorConnection.DoGetVerifyResultString: string;
begin
  Result := 'ok';
end;

function TTestConnectorConnection.DoGetSession: ISSLSession;
begin
  Result := FSession;
end;

procedure TTestConnectorConnection.DoSetSession(ASession: ISSLSession);
begin
  FSession := ASession;
end;

function TTestConnectorConnection.DoIsSessionReused: Boolean;
begin
  Result := False;
end;

function TTestConnectorConnection.DoGetSelectedALPNProtocol: string;
begin
  Result := '';
end;

function TTestConnectorConnection.DoGetState: string;
begin
  Result := 'CONNECTED';
end;

function TTestConnectorConnection.DoGetNativeHandle: Pointer;
begin
  Result := nil;
end;

constructor TTestConnectorContext.Create(AContextType: TSSLContextType);
begin
  inherited Create;
  FContextType := AContextType;
  FProtocolVersions := [sslProtocolTLS12, sslProtocolTLS13];
  FPreferredVersion := sslProtocolTLS13;
  FVerifyMode := [];
  FVerifyDepth := 0;
  FCipherList := '';
  FCipherSuites := '';
  FSessionCacheMode := True;
  FSessionTimeout := 0;
  FSessionCacheSize := 0;
  FOptions := [];
  FServerName := '';
  FALPNProtocols := '';
  FCertVerifyFlags := [];
  FCertificateStore := nil;
  FPinningEnabled := False;
end;

function TTestConnectorContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TTestConnectorContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;
end;

function TTestConnectorContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TTestConnectorContext.SetPreferredVersion(AVersion: TSSLProtocolVersion);
begin
  FPreferredVersion := AVersion;
end;

function TTestConnectorContext.GetPreferredVersion: TSSLProtocolVersion;
begin
  Result := FPreferredVersion;
end;

procedure TTestConnectorContext.LoadCertificate(const AFileName: string);
begin
end;

procedure TTestConnectorContext.LoadCertificate(AStream: TStream);
begin
end;

procedure TTestConnectorContext.LoadCertificate(ACert: ISSLCertificate);
begin
end;

procedure TTestConnectorContext.LoadPrivateKey(const AFileName: string; const APassword: string = '');
begin
end;

procedure TTestConnectorContext.LoadPrivateKey(AStream: TStream; const APassword: string = '');
begin
end;

procedure TTestConnectorContext.LoadCertificatePEM(const APEM: string);
begin
end;

procedure TTestConnectorContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');
begin
end;

procedure TTestConnectorContext.LoadCAFile(const AFileName: string);
begin
end;

procedure TTestConnectorContext.LoadCAPath(const APath: string);
begin
end;

procedure TTestConnectorContext.SetCertificateStore(AStore: ISSLCertificateStore);
begin
  FCertificateStore := AStore;
end;

procedure TTestConnectorContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
end;

function TTestConnectorContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TTestConnectorContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
end;

function TTestConnectorContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TTestConnectorContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
end;

procedure TTestConnectorContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
end;

function TTestConnectorContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TTestConnectorContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TTestConnectorContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

procedure TTestConnectorContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheMode := AEnabled;
end;

function TTestConnectorContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheMode;
end;

procedure TTestConnectorContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
end;

function TTestConnectorContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TTestConnectorContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
end;

function TTestConnectorContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

procedure TTestConnectorContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TTestConnectorContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TTestConnectorContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TTestConnectorContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TTestConnectorContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TTestConnectorContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TTestConnectorContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TTestConnectorContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TTestConnectorContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
end;

procedure TTestConnectorContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
end;

procedure TTestConnectorContext.AddCertificatePin(const AHash: TBytes; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean = False);
begin
end;

procedure TTestConnectorContext.AddCertificatePinBase64(const ABase64Hash: string; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean = False);
begin
end;

procedure TTestConnectorContext.SetCertificatePinningEnabled(AEnabled: Boolean);
begin
  FPinningEnabled := AEnabled;
end;

function TTestConnectorContext.GetCertificatePinningEnabled: Boolean;
begin
  Result := FPinningEnabled;
end;

procedure TTestConnectorContext.ClearCertificatePins;
begin
end;

function TTestConnectorContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  Result := TTestConnectorConnection.Create(Self as ISSLContext);
end;

function TTestConnectorContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  Result := TTestConnectorConnection.Create(Self as ISSLContext);
end;

function TTestConnectorContext.IsValid: Boolean;
begin
  Result := True;
end;

procedure TestNonEmptyOverride;
var
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LTransport: TMemoryStream;
  LStream: TSSLStream;
  LClientConn: ISSLClientConnection;
begin
  LContext := TTestConnectorContext.Create(sslCtxClient);
  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LConnector := TSSLConnector.FromContext(LContext);
  LTransport := TMemoryStream.Create;
  try
    LStream := LConnector.ConnectStream(LTransport, 'override.example');
    try
      Require(Supports(LStream.Connection, ISSLClientConnection, LClientConn),
        'Non-empty override connection should support ISSLClientConnection');
      RequireEquals('Non-empty override should win', 'override.example',
        LClientConn.GetServerName);
    finally
      LStream.Free;
    end;
  finally
    LTransport.Free;
  end;
end;

procedure TestExplicitEmptyOverrideClearsFallback;
var
  LContext: ISSLContext;
  LConnector: TSSLConnector;
  LTransport: TMemoryStream;
  LStream: TSSLStream;
  LClientConn: ISSLClientConnection;
begin
  LContext := TTestConnectorContext.Create(sslCtxClient);
  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LConnector := TSSLConnector.FromContext(LContext);
  LTransport := TMemoryStream.Create;
  try
    LStream := LConnector.ConnectStream(LTransport, '');
    try
      Require(Supports(LStream.Connection, ISSLClientConnection, LClientConn),
        'Explicit empty override connection should support ISSLClientConnection');
      RequireEquals('Explicit empty hostname override should clear inherited context fallback', '',
        LClientConn.GetServerName);
    finally
      LStream.Free;
    end;
  finally
    LTransport.Free;
  end;
end;

begin
  WriteLn('fafafa.ssl - TLS connector hostname override precedence');
  TestNonEmptyOverride;
  TestExplicitEmptyOverrideClearsFallback;
  WriteLn('[PASS] TLS connector hostname override precedence');
end.
