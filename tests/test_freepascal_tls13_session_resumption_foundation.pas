program test_freepascal_tls13_session_resumption_foundation;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.freepascal.session,
  fafafa.ssl.tls13.posthandshake,
  fafafa.ssl.tls13.wire;

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
    FSession: ISSLSession;
    FGenerated: Boolean;
  public
    constructor Create(const AServerCertPEM, AServerKeyPEM: string; ASession: ISSLSession);
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

constructor TServerResponderStream.Create(const AServerCertPEM, AServerKeyPEM: string; ASession: ISSLSession);
begin
  inherited Create(nil);
  FServerCertPEM := AServerCertPEM;
  FServerKeyPEM := AServerKeyPEM;
  FSession := ASession;
  FGenerated := False;
end;

function BuildServerFlightFromClientHello(
  const AClientHello: TBytes;
  const AServerCertPEM, AServerKeyPEM: string;
  ASession: ISSLSession
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
  LServerCtx.LoadCertificate(AServerCertPEM);
  LServerCtx.LoadPrivateKey(AServerKeyPEM);

  LServerTransport := TScriptedDuplexStream.Create(AClientHello);
  try
    LServerConn := LServerCtx.CreateConnection(LServerTransport);
    if LServerConn = nil then
      raise Exception.Create('Server connection should be created');
    if ASession <> nil then
      LServerConn.SetSession(ASession);
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
  FReadBuffer := BuildServerFlightFromClientHello(LData, FServerCertPEM, FServerKeyPEM, FSession);
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

function BuildResumableSession: ISSLSession;
var
  LTicket: TTLS13NewSessionTicket;
  LPSK: TBytes;
begin
  InitTLS13NewSessionTicket(LTicket);
  LTicket.Valid := True;
  LTicket.TicketLifetime := 86400;
  LTicket.TicketAgeAdd := $10203040;
  LTicket.TicketNonce := TBytes.Create($01, $02, $03, $04);
  LTicket.Ticket := TBytes.Create($AA, $BB, $CC, $DD, $EE, $FF);
  LPSK := TBytes.Create(
    $00,$01,$02,$03,$04,$05,$06,$07,
    $08,$09,$0A,$0B,$0C,$0D,$0E,$0F,
    $10,$11,$12,$13,$14,$15,$16,$17,
    $18,$19,$1A,$1B,$1C,$1D,$1E,$1F
  );
  Result := TFreePascalSession.CreateResumptionSnapshot(
    sslProtocolTLS13,
    TLS13_CIPHER_CHACHA20_POLY1305_SHA256,
    'TLS_CHACHA20_POLY1305_SHA256',
    nil,
    nil,
    LTicket,
    LPSK,
    600
  );
end;

procedure TestTLS13SessionResumptionFoundation;
var
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
  LSession: ISSLSession;
begin
  LSession := BuildResumableSession;
  AssertTrue(LSession <> nil, 'Resumable session should be created');

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetVerifyMode([]);

  LClientTransport := TServerResponderStream.Create(
    'tests/certificate/test_certs/signer_cert.pem',
    'tests/certificate/test_certs/signer_key.pem',
    LSession
  );
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created');
    LClientConn.SetSession(LSession);
    AssertTrue(LClientConn.Connect,
      'Resumption client handshake should complete with configured session');
    AssertTrue(LClientConn.IsSessionReused,
      'Resumption handshake should set IsSessionReused to True');
  finally
    LClientTransport.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal TLS 1.3 session resumption foundation...');
  TestTLS13SessionResumptionFoundation;
  WriteLn('✅ FreePascal TLS 1.3 session resumption foundation checks passed');
end.
