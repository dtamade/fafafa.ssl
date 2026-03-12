program test_freepascal_session_surface_foundation;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.session,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.clienthello,
  fafafa.ssl.tls13.x25519;

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
    FServerALPN: string;
    FGenerated: Boolean;
  public
    constructor Create(const AServerALPN: string);
    function Write(const Buffer; Count: Longint): Longint; override;
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

constructor TServerResponderStream.Create(const AServerALPN: string);
begin
  inherited Create(nil);
  FServerALPN := AServerALPN;
  FGenerated := False;
end;

function BuildServerFlightFromClientHello(const AClientHello: TBytes; const AServerALPN: string): TBytes;
var
  LServerCtx: ISSLContext;
  LServerConn: ISSLConnection;
  LServerTransport: TScriptedDuplexStream;
begin
  LServerCtx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  if LServerCtx = nil then
    raise Exception.Create('FreePascal server context should be created');
  LServerCtx.SetPreferredVersion(sslProtocolTLS13);
  LServerCtx.LoadCertificate('tests/certificate/test_certs/signer_cert.pem');
  LServerCtx.LoadPrivateKey('tests/certificate/test_certs/signer_key.pem');
  LServerCtx.SetALPNProtocols(AServerALPN);

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
  FReadBuffer := BuildServerFlightFromClientHello(LData, FServerALPN);
  FReadPos := 0;
  FGenerated := True;
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

function CreateTrustedStoreFromFile(const AFileName: string): ISSLCertificateStore;
var
  LTrustedCert: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  AssertTrue(Result <> nil, 'Trusted store should be created');

  LTrustedCert := TSSLFactory.CreateCertificate(sslFreePascal);
  AssertTrue(LTrustedCert <> nil, 'Trusted certificate instance should be created');
  AssertTrue(LTrustedCert.LoadFromFile(AFileName),
    'Trusted certificate should load from file');
  AssertTrue(Result.AddCertificate(LTrustedCert),
    'Trusted store should accept trusted certificate');
end;

procedure TestSessionSurfaceAfterHandshake;
var
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
  LSession: ISSLSession;
  LClone: ISSLSession;
  LSerialized: TBytes;
  LRoundTrip: TFreePascalSession;
begin
  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetALPNProtocols('h2,http/1.1');
  LClientCtx.SetCertificateStore(CreateTrustedStoreFromFile('tests/certificate/test_certs/ca_cert.pem'));
  LClientCtx.SetCertVerifyFlags([sslCertVerifyDefault, sslCertVerifyIgnoreHostname]);

  LClientTransport := TServerResponderStream.Create('h2,http/1.1');
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created');
    AssertTrue(LClientConn.Connect, 'Client connect should succeed for session surface contract');

    LSession := LClientConn.GetSession;
    AssertTrue(LSession <> nil, 'Client should expose non-nil session snapshot after handshake');
    AssertTrue(LSession.GetID <> '', 'Session snapshot should expose non-empty ID');
    AssertTrue(LSession.GetProtocolVersion = sslProtocolTLS13,
      'Session snapshot should expose negotiated protocol version');
    AssertTrue(LSession.GetCipherName <> '',
      'Session snapshot should expose negotiated cipher');
    AssertTrue(LSession.GetPeerCertificate <> nil,
      'Session snapshot should expose peer certificate');

    LSerialized := LSession.Serialize;
    AssertTrue(Length(LSerialized) > 0,
      'Session snapshot should serialize to non-empty payload');

    LClone := LSession.Clone;
    AssertTrue(LClone <> nil, 'Session snapshot should support cloning');
    AssertTrue(LClone.GetID = LSession.GetID,
      'Session clone should preserve session ID');

    LRoundTrip := TFreePascalSession.Create;
    try
      AssertTrue(LRoundTrip.Deserialize(LSerialized),
        'Session snapshot should deserialize into a fresh FreePascal session');
      AssertTrue(LRoundTrip.GetID = LSession.GetID,
        'Deserialized session should preserve session ID');
    finally
      LRoundTrip.Free;
    end;
  finally
    LClientTransport.Free;
  end;
end;

procedure TestSetSessionSurfaceRemainsUsable;
var
  LClientCtx: ISSLContext;
  LFirstConn: ISSLConnection;
  LSecondConn: ISSLConnection;
  LFirstTransport: TServerResponderStream;
  LSecondTransport: TServerResponderStream;
  LSession: ISSLSession;
begin
  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created for SetSession');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetALPNProtocols('h2,http/1.1');
  LClientCtx.SetCertificateStore(CreateTrustedStoreFromFile('tests/certificate/test_certs/ca_cert.pem'));
  LClientCtx.SetCertVerifyFlags([sslCertVerifyDefault, sslCertVerifyIgnoreHostname]);

  LFirstTransport := TServerResponderStream.Create('h2,http/1.1');
  try
    LFirstConn := LClientCtx.CreateConnection(LFirstTransport);
    AssertTrue(LFirstConn <> nil, 'First client connection should be created');
    AssertTrue(LFirstConn.Connect, 'First session-producing handshake should succeed');
    LSession := LFirstConn.GetSession;
    AssertTrue(LSession <> nil, 'First connection should expose session snapshot');
  finally
    LFirstTransport.Free;
  end;

  LSecondTransport := TServerResponderStream.Create('h2,http/1.1');
  try
    LSecondConn := LClientCtx.CreateConnection(LSecondTransport);
    AssertTrue(LSecondConn <> nil, 'Second client connection should be created');
    LSecondConn.SetSession(LSession);
    AssertTrue(LSecondConn.Connect,
      'Connection should remain usable after SetSession on pure Pascal backend');
    AssertTrue(not LSecondConn.IsSessionReused,
      'PurePascal session surface foundation should keep IsSessionReused explicit False until resumption is implemented');
  finally
    LSecondTransport.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal session surface foundation...');
  TestSessionSurfaceAfterHandshake;
  TestSetSessionSurfaceRemainsUsable;
  WriteLn('✅ FreePascal session surface foundation checks passed');
end.
