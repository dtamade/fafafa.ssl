program test_connection_builder_hostname_override_precedence;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.base,
  fafafa.ssl.connection.base,
  fafafa.ssl.connection.builder;

type
  TTestBuilderConnection = class(TBaseSSLConnection, ISSLClientConnection)
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

  TTestBuilderContext = class(TInterfacedObject, ISSLContext)
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

constructor TTestBuilderConnection.Create(AContext: ISSLContext);
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

procedure TTestBuilderConnection.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TTestBuilderConnection.GetServerName: string;
begin
  Result := FServerName;
end;

function TTestBuilderConnection.DoRead(var ABuffer; ACount: Integer): Integer;
begin
  Result := 0;
end;

function TTestBuilderConnection.DoWrite(const ABuffer; ACount: Integer): Integer;
begin
  Result := ACount;
end;

function TTestBuilderConnection.DoConnect: Boolean;
begin
  Result := True;
end;

function TTestBuilderConnection.DoAccept: Boolean;
begin
  Result := True;
end;

function TTestBuilderConnection.DoHandshakeInternal: TSSLHandshakeState;
begin
  Result := sslHsCompleted;
end;

function TTestBuilderConnection.DoShutdown: Boolean;
begin
  Result := True;
end;

procedure TTestBuilderConnection.DoClose;
begin
end;

function TTestBuilderConnection.DoRenegotiate: Boolean;
begin
  Result := False;
end;

function TTestBuilderConnection.DoGetError(ARet: Integer): TSSLErrorCode;
begin
  Result := sslErrNone;
end;

function TTestBuilderConnection.DoWantRead: Boolean;
begin
  Result := False;
end;

function TTestBuilderConnection.DoWantWrite: Boolean;
begin
  Result := False;
end;

function TTestBuilderConnection.DoGetProtocolVersion: TSSLProtocolVersion;
begin
  Result := sslProtocolTLS13;
end;

function TTestBuilderConnection.DoGetCipherName: string;
begin
  Result := 'TEST';
end;

function TTestBuilderConnection.DoGetPeerCertificate: ISSLCertificate;
begin
  Result := nil;
end;

function TTestBuilderConnection.DoGetPeerCertificateChain: TSSLCertificateArray;
begin
  Result := nil;
end;

function TTestBuilderConnection.DoGetVerifyResult: Integer;
begin
  Result := 0;
end;

function TTestBuilderConnection.DoGetVerifyResultString: string;
begin
  Result := 'ok';
end;

function TTestBuilderConnection.DoGetSession: ISSLSession;
begin
  Result := FSession;
end;

procedure TTestBuilderConnection.DoSetSession(ASession: ISSLSession);
begin
  FSession := ASession;
end;

function TTestBuilderConnection.DoIsSessionReused: Boolean;
begin
  Result := False;
end;

function TTestBuilderConnection.DoGetSelectedALPNProtocol: string;
begin
  Result := '';
end;

function TTestBuilderConnection.DoGetState: string;
begin
  Result := 'CONNECTED';
end;

function TTestBuilderConnection.DoGetNativeHandle: Pointer;
begin
  Result := nil;
end;

constructor TTestBuilderContext.Create(AContextType: TSSLContextType);
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

function TTestBuilderContext.GetContextType: TSSLContextType;
begin
  Result := FContextType;
end;

procedure TTestBuilderContext.SetProtocolVersions(AVersions: TSSLProtocolVersions);
begin
  FProtocolVersions := AVersions;
end;

function TTestBuilderContext.GetProtocolVersions: TSSLProtocolVersions;
begin
  Result := FProtocolVersions;
end;

procedure TTestBuilderContext.SetPreferredVersion(AVersion: TSSLProtocolVersion);
begin
  FPreferredVersion := AVersion;
end;

function TTestBuilderContext.GetPreferredVersion: TSSLProtocolVersion;
begin
  Result := FPreferredVersion;
end;

procedure TTestBuilderContext.LoadCertificate(const AFileName: string);
begin
end;

procedure TTestBuilderContext.LoadCertificate(AStream: TStream);
begin
end;

procedure TTestBuilderContext.LoadCertificate(ACert: ISSLCertificate);
begin
end;

procedure TTestBuilderContext.LoadPrivateKey(const AFileName: string; const APassword: string = '');
begin
end;

procedure TTestBuilderContext.LoadPrivateKey(AStream: TStream; const APassword: string = '');
begin
end;

procedure TTestBuilderContext.LoadCertificatePEM(const APEM: string);
begin
end;

procedure TTestBuilderContext.LoadPrivateKeyPEM(const APEM: string; const APassword: string = '');
begin
end;

procedure TTestBuilderContext.LoadCAFile(const AFileName: string);
begin
end;

procedure TTestBuilderContext.LoadCAPath(const APath: string);
begin
end;

procedure TTestBuilderContext.SetCertificateStore(AStore: ISSLCertificateStore);
begin
  FCertificateStore := AStore;
end;

procedure TTestBuilderContext.SetVerifyMode(AMode: TSSLVerifyModes);
begin
  FVerifyMode := AMode;
end;

function TTestBuilderContext.GetVerifyMode: TSSLVerifyModes;
begin
  Result := FVerifyMode;
end;

procedure TTestBuilderContext.SetVerifyDepth(ADepth: Integer);
begin
  FVerifyDepth := ADepth;
end;

function TTestBuilderContext.GetVerifyDepth: Integer;
begin
  Result := FVerifyDepth;
end;

procedure TTestBuilderContext.SetVerifyCallback(ACallback: TSSLVerifyCallback);
begin
end;

procedure TTestBuilderContext.SetCipherList(const ACipherList: string);
begin
  FCipherList := ACipherList;
end;

function TTestBuilderContext.GetCipherList: string;
begin
  Result := FCipherList;
end;

procedure TTestBuilderContext.SetCipherSuites(const ACipherSuites: string);
begin
  FCipherSuites := ACipherSuites;
end;

function TTestBuilderContext.GetCipherSuites: string;
begin
  Result := FCipherSuites;
end;

procedure TTestBuilderContext.SetSessionCacheMode(AEnabled: Boolean);
begin
  FSessionCacheMode := AEnabled;
end;

function TTestBuilderContext.GetSessionCacheMode: Boolean;
begin
  Result := FSessionCacheMode;
end;

procedure TTestBuilderContext.SetSessionTimeout(ATimeout: Integer);
begin
  FSessionTimeout := ATimeout;
end;

function TTestBuilderContext.GetSessionTimeout: Integer;
begin
  Result := FSessionTimeout;
end;

procedure TTestBuilderContext.SetSessionCacheSize(ASize: Integer);
begin
  FSessionCacheSize := ASize;
end;

function TTestBuilderContext.GetSessionCacheSize: Integer;
begin
  Result := FSessionCacheSize;
end;

procedure TTestBuilderContext.SetOptions(const AOptions: TSSLOptions);
begin
  FOptions := AOptions;
end;

function TTestBuilderContext.GetOptions: TSSLOptions;
begin
  Result := FOptions;
end;

procedure TTestBuilderContext.SetServerName(const AServerName: string);
begin
  FServerName := AServerName;
end;

function TTestBuilderContext.GetServerName: string;
begin
  Result := FServerName;
end;

procedure TTestBuilderContext.SetALPNProtocols(const AProtocols: string);
begin
  FALPNProtocols := AProtocols;
end;

function TTestBuilderContext.GetALPNProtocols: string;
begin
  Result := FALPNProtocols;
end;

procedure TTestBuilderContext.SetCertVerifyFlags(AFlags: TSSLCertVerifyFlags);
begin
  FCertVerifyFlags := AFlags;
end;

function TTestBuilderContext.GetCertVerifyFlags: TSSLCertVerifyFlags;
begin
  Result := FCertVerifyFlags;
end;

procedure TTestBuilderContext.SetPasswordCallback(ACallback: TSSLPasswordCallback);
begin
end;

procedure TTestBuilderContext.SetInfoCallback(ACallback: TSSLInfoCallback);
begin
end;

procedure TTestBuilderContext.AddCertificatePin(const AHash: TBytes; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean = False);
begin
end;

procedure TTestBuilderContext.AddCertificatePinBase64(const ABase64Hash: string; APinType: Integer;
  const ADescription: string; AIsBackup: Boolean = False);
begin
end;

procedure TTestBuilderContext.SetCertificatePinningEnabled(AEnabled: Boolean);
begin
  FPinningEnabled := AEnabled;
end;

function TTestBuilderContext.GetCertificatePinningEnabled: Boolean;
begin
  Result := FPinningEnabled;
end;

procedure TTestBuilderContext.ClearCertificatePins;
begin
end;

function TTestBuilderContext.CreateConnection(ASocket: THandle): ISSLConnection;
begin
  Result := TTestBuilderConnection.Create(Self as ISSLContext);
end;

function TTestBuilderContext.CreateConnection(AStream: TStream): ISSLConnection;
begin
  Result := TTestBuilderConnection.Create(Self as ISSLContext);
end;

function TTestBuilderContext.IsValid: Boolean;
begin
  Result := True;
end;

procedure TestDefaultFallback;
var
  LContext: ISSLContext;
  LBuilder: ISSLConnectionBuilder;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LResult: TSSLOperationResult;
  LStream: TMemoryStream;
begin
  LContext := TTestBuilderContext.Create(sslCtxClient);
  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LBuilder := TSSLConnectionBuilder.Create
    .WithContext(LContext);

  LStream := TMemoryStream.Create;
  try
    LResult := LBuilder.WithStream(LStream).TryBuildClient(LConnection);
    Require(LResult.Success, 'Default fallback build should succeed');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      'Default fallback connection should support ISSLClientConnection');
    RequireEquals('Default fallback ServerName', 'ctx.default.example',
      LClientConn.GetServerName);
  finally
    LStream.Free;
  end;
end;

procedure TestExplicitOverride;
var
  LContext: ISSLContext;
  LBuilder: ISSLConnectionBuilder;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LResult: TSSLOperationResult;
  LStream: TMemoryStream;
begin
  LContext := TTestBuilderContext.Create(sslCtxClient);
  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LBuilder := TSSLConnectionBuilder.Create
    .WithContext(LContext)
    .WithHostname('override.example');

  LStream := TMemoryStream.Create;
  try
    LResult := LBuilder.WithStream(LStream).TryBuildClient(LConnection);
    Require(LResult.Success, 'Explicit override build should succeed');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      'Explicit override connection should support ISSLClientConnection');
    RequireEquals('Explicit override ServerName', 'override.example',
      LClientConn.GetServerName);
  finally
    LStream.Free;
  end;
end;

procedure TestExplicitEmptyOverrideClearsFallback;
var
  LContext: ISSLContext;
  LBuilder: ISSLConnectionBuilder;
  LConnection: ISSLConnection;
  LClientConn: ISSLClientConnection;
  LResult: TSSLOperationResult;
  LStream: TMemoryStream;
begin
  LContext := TTestBuilderContext.Create(sslCtxClient);
  {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
  LContext.SetServerName('ctx.default.example');
  {$POP}

  LBuilder := TSSLConnectionBuilder.Create
    .WithContext(LContext)
    .WithHostname('');

  LStream := TMemoryStream.Create;
  try
    LResult := LBuilder.WithStream(LStream).TryBuildClient(LConnection);
    Require(LResult.Success, 'Explicit empty override build should succeed');
    Require(Supports(LConnection, ISSLClientConnection, LClientConn),
      'Explicit empty override connection should support ISSLClientConnection');
    RequireEquals('Explicit empty override clears fallback', '',
      LClientConn.GetServerName);
  finally
    LStream.Free;
  end;
end;

begin
  WriteLn('fafafa.ssl - connection builder hostname override precedence');
  TestDefaultFallback;
  TestExplicitOverride;
  TestExplicitEmptyOverrideClearsFallback;
  WriteLn('[PASS] connection builder hostname override precedence');
end.
