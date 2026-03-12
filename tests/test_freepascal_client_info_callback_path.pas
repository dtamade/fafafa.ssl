program test_freepascal_client_info_callback_path;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.cert,
  fafafa.ssl.cert.builder;

type
  TScriptedDuplexStream = class(TStream)
  private
    FReadBuffer: TBytes;
    FReadPos: Integer;
    FWriteBuffer: TBytes;
  public
    constructor Create(const AReadBuffer: TBytes);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function CapturedWriteBuffer: TBytes;
  end;

  TServerResponderStream = class(TScriptedDuplexStream)
  private
    FServerCertPEM: string;
    FServerKeyPEM: string;
    FGenerated: Boolean;
  public
    constructor Create(const AServerCertPEM, AServerKeyPEM: string);
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TInfoCallbackProbe = class
  private
    FStates: array of string;
  public
    procedure HandleInfo(const AWhere: Integer; const ARet: Integer; const AState: string);
    function SawState(const AState: string): Boolean;
    function EventCount: Integer;
  end;

constructor TScriptedDuplexStream.Create(const AReadBuffer: TBytes);
begin
  inherited Create;
  FReadBuffer := Copy(AReadBuffer, 0, Length(AReadBuffer));
  FReadPos := 0;
  SetLength(FWriteBuffer, 0);
end;

function TScriptedDuplexStream.Read(var Buffer; Count: Longint): Longint;
var
  LRemaining: Integer;
begin
  LRemaining := Length(FReadBuffer) - FReadPos;
  if LRemaining <= 0 then
    Exit(0);

  Result := Count;
  if Result > LRemaining then
    Result := LRemaining;
  if Result > 0 then
    Move(FReadBuffer[FReadPos], Buffer, Result);
  Inc(FReadPos, Result);
end;

function TScriptedDuplexStream.Write(const Buffer; Count: Longint): Longint;
var
  LOldLen: Integer;
begin
  LOldLen := Length(FWriteBuffer);
  SetLength(FWriteBuffer, LOldLen + Count);
  if Count > 0 then
    Move(Buffer, FWriteBuffer[LOldLen], Count);
  Result := Count;
end;

function TScriptedDuplexStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  case Origin of
    soBeginning: FReadPos := Offset;
    soCurrent: Inc(FReadPos, Offset);
    soEnd: FReadPos := Length(FReadBuffer) + Offset;
  end;

  if FReadPos < 0 then
    FReadPos := 0;
  if FReadPos > Length(FReadBuffer) then
    FReadPos := Length(FReadBuffer);
  Result := FReadPos;
end;

function TScriptedDuplexStream.CapturedWriteBuffer: TBytes;
begin
  Result := Copy(FWriteBuffer, 0, Length(FWriteBuffer));
end;

constructor TServerResponderStream.Create(const AServerCertPEM, AServerKeyPEM: string);
begin
  inherited Create(nil);
  FServerCertPEM := AServerCertPEM;
  FServerKeyPEM := AServerKeyPEM;
  FGenerated := False;
end;

function BuildServerFlightFromClientHello(
  const AClientHello: TBytes;
  const AServerCertPEM, AServerKeyPEM: string
): TBytes;
var
  LServerCtx: ISSLContext;
  LServerConn: ISSLConnection;
  LServerTransport: TScriptedDuplexStream;
begin
  LServerCtx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  if LServerCtx = nil then
    raise Exception.Create('FreePascal server context should be created');
  LServerCtx.SetPreferredVersion(sslProtocolTLS13);
  LServerCtx.LoadCertificatePEM(AServerCertPEM);
  LServerCtx.LoadPrivateKeyPEM(AServerKeyPEM);

  LServerTransport := TScriptedDuplexStream.Create(AClientHello);
  try
    LServerConn := LServerCtx.CreateConnection(LServerTransport);
    if LServerConn = nil then
      raise Exception.Create('Server connection should be created');
    if LServerConn.Accept then
      raise Exception.Create('Server accept should not fully complete in scripted one-way harness');
    Result := LServerTransport.CapturedWriteBuffer;
  finally
    LServerTransport.Free;
  end;
end;

function TServerResponderStream.Write(const Buffer; Count: Longint): Longint;
var
  LData: TBytes;
begin
  Result := inherited Write(Buffer, Count);
  if FGenerated or (Count <= 0) then
    Exit;

  SetLength(LData, Count);
  Move(Buffer, LData[0], Count);
  FReadBuffer := BuildServerFlightFromClientHello(LData, FServerCertPEM, FServerKeyPEM);
  FReadPos := 0;
  FGenerated := True;
end;

procedure TInfoCallbackProbe.HandleInfo(const AWhere: Integer; const ARet: Integer; const AState: string);
var
  LIndex: Integer;
begin
  LIndex := Length(FStates);
  SetLength(FStates, LIndex + 1);
  FStates[LIndex] := AState;
end;

function TInfoCallbackProbe.SawState(const AState: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to High(FStates) do
    if SameText(FStates[I], AState) then
      Exit(True);
end;

function TInfoCallbackProbe.EventCount: Integer;
begin
  Result := Length(FStates);
end;

procedure Fail(const AMessage: string);
begin
  WriteLn('❌ ', AMessage);
  Halt(1);
end;

procedure AssertTrue(ACondition: Boolean; const AMessage: string);
begin
  if not ACondition then
    Fail(AMessage);
end;

function CreateTrustedStore(const AServerCertPEM: string): ISSLCertificateStore;
var
  LTrustedCert: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  AssertTrue(Result <> nil, 'Trusted store should be created');

  LTrustedCert := TSSLFactory.CreateCertificate(sslFreePascal);
  AssertTrue(LTrustedCert <> nil, 'Trusted certificate instance should be created');
  AssertTrue(LTrustedCert.LoadFromPEM(AServerCertPEM),
    'Trusted certificate should load generated PEM');
  AssertTrue(Result.AddCertificate(LTrustedCert),
    'Trusted store should accept generated server certificate');
end;

procedure RequireServerName(const AConnection: ISSLConnection; const AHost: string);
var
  LClientConn: ISSLClientConnection;
begin
  AssertTrue(Supports(AConnection, ISSLClientConnection, LClientConn),
    'Connection should expose ISSLClientConnection');
  LClientConn.SetServerName(AHost);
end;

procedure TestInfoCallbackOnClientHandshakePath;
var
  LKeyPair: IKeyPairWithCertificate;
  LServerCertPEM: string;
  LServerKeyPEM: string;
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
  LProbe: TInfoCallbackProbe;
begin
  LKeyPair := TCertificate.CreateServerCert('alt.example.com', ['alt.example.com']);
  AssertTrue(LKeyPair <> nil, 'Server key pair should be created');
  LKeyPair.SaveToPEM(LServerCertPEM, LServerKeyPEM);

  LProbe := TInfoCallbackProbe.Create;
  try
    LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
    LClientCtx.SetPreferredVersion(sslProtocolTLS13);
    LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));
    LClientCtx.SetInfoCallback(@LProbe.HandleInfo);

    LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
    try
      LClientConn := LClientCtx.CreateConnection(LClientTransport);
      AssertTrue(LClientConn <> nil, 'Client connection should be created');
      RequireServerName(LClientConn, 'alt.example.com');
      AssertTrue(LClientConn.Connect, 'Trusted handshake should succeed');
      AssertTrue(LProbe.SawState('handshake_start'),
        'Info callback should observe handshake_start');
      AssertTrue(LProbe.SawState('handshake_done'),
        'Info callback should observe handshake_done');
      AssertTrue(LProbe.EventCount >= 2,
        'Successful handshake should emit at least two info callback events');
    finally
      LClientTransport.Free;
    end;
  finally
    LProbe.Free;
  end;

  LProbe := TInfoCallbackProbe.Create;
  try
    LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
    AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created for verify failure');
    LClientCtx.SetPreferredVersion(sslProtocolTLS13);
    LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));
    LClientCtx.SetInfoCallback(@LProbe.HandleInfo);

    LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
    try
      LClientConn := LClientCtx.CreateConnection(LClientTransport);
      AssertTrue(LClientConn <> nil, 'Client connection should be created for verify failure');
      RequireServerName(LClientConn, 'wrong.example.com');
      AssertTrue(not LClientConn.Connect, 'Hostname mismatch should fail');
      AssertTrue(LProbe.SawState('handshake_start'),
        'Verify-failure path should still emit handshake_start');
      AssertTrue(LProbe.SawState('verify_failed'),
        'Verify-failure path should emit verify_failed');
    finally
      LClientTransport.Free;
    end;
  finally
    LProbe.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal client info callback path...');
  TestInfoCallbackOnClientHandshakePath;
  WriteLn('✅ FreePascal client info callback path checks passed');
end.
