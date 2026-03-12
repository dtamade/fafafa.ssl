program test_freepascal_client_chain_verification_path;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.cert.builder,
  fafafa.ssl.freepascal.session;

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
    FServerCertChainPEM: string;
    FServerKeyPEM: string;
    FGenerated: Boolean;
  public
    constructor Create(const AServerCertChainPEM, AServerKeyPEM: string);
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

constructor TServerResponderStream.Create(const AServerCertChainPEM, AServerKeyPEM: string);
begin
  inherited Create(nil);
  FServerCertChainPEM := AServerCertChainPEM;
  FServerKeyPEM := AServerKeyPEM;
  FGenerated := False;
end;

function BuildServerFlightFromClientHello(
  const AClientHello: TBytes;
  const AServerCertChainPEM, AServerKeyPEM: string
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
  LServerCtx.LoadCertificatePEM(AServerCertChainPEM);
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
  FReadBuffer := BuildServerFlightFromClientHello(LData, FServerCertChainPEM, FServerKeyPEM);
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

function CreateTrustedStore(const ARootCertPEM: string): ISSLCertificateStore;
var
  LTrustedCert: ISSLCertificate;
begin
  Result := TSSLFactory.CreateCertificateStore(sslFreePascal);
  AssertTrue(Result <> nil, 'Trusted store should be created');

  LTrustedCert := TSSLFactory.CreateCertificate(sslFreePascal);
  AssertTrue(LTrustedCert <> nil, 'Trusted root certificate instance should be created');
  AssertTrue(LTrustedCert.LoadFromPEM(ARootCertPEM),
    'Trusted root certificate should load PEM');
  AssertTrue(Result.AddCertificate(LTrustedCert),
    'Trusted root certificate should be accepted');
end;

procedure RequireServerName(const AConnection: ISSLConnection; const AHost: string);
var
  LClientConn: ISSLClientConnection;
begin
  AssertTrue(Supports(AConnection, ISSLClientConnection, LClientConn),
    'Connection should expose ISSLClientConnection');
  LClientConn.SetServerName(AHost);
end;

procedure TestClientVerifiesLeafIntermediateChainWithTrustedRoot;
var
  LRootPair: IKeyPairWithCertificate;
  LIntermediatePair: IKeyPairWithCertificate;
  LLeafPair: IKeyPairWithCertificate;
  LRootCertPEM: string;
  LRootKeyPEM: string;
  LIntermediateCertPEM: string;
  LIntermediateKeyPEM: string;
  LLeafCertPEM: string;
  LLeafKeyPEM: string;
  LServerChainPEM: string;
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
  LSession: ISSLSession;
  LResumptionSession: IFreePascalResumptionSession;
  LSessionChain: TSSLCertificateArray;
begin
  LRootPair := TCertificateBuilder.Create
    .WithCommonName('fp-root.example')
    .ValidFor(365)
    .WithRSAKey(2048)
    .AsCA
    .SelfSigned;
  AssertTrue(LRootPair <> nil, 'Root CA should be created');
  LRootPair.SaveToPEM(LRootCertPEM, LRootKeyPEM);

  LIntermediatePair := TCertificateBuilder.Create
    .WithCommonName('fp-intermediate.example')
    .ValidFor(365)
    .WithRSAKey(2048)
    .AsCA
    .SignedBy(LRootPair.Certificate, LRootPair.PrivateKey);
  AssertTrue(LIntermediatePair <> nil, 'Intermediate CA should be created');
  LIntermediatePair.SaveToPEM(LIntermediateCertPEM, LIntermediateKeyPEM);

  LLeafPair := TCertificateBuilder.Create
    .WithCommonName('alt.example.com')
    .ValidFor(90)
    .WithRSAKey(2048)
    .AsServerCert
    .AddSubjectAltName('DNS:alt.example.com')
    .SignedBy(LIntermediatePair.Certificate, LIntermediatePair.PrivateKey);
  AssertTrue(LLeafPair <> nil, 'Leaf server certificate should be created');
  LLeafPair.SaveToPEM(LLeafCertPEM, LLeafKeyPEM);

  LServerChainPEM := LLeafCertPEM + LineEnding + LIntermediateCertPEM;

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LRootCertPEM));

  LClientTransport := TServerResponderStream.Create(LServerChainPEM, LLeafKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(LClientConn.Connect,
      'Client should verify leaf+intermediate chain using trusted root: ' +
      LClientConn.GetVerifyResultString);
    AssertTrue(LClientConn.GetVerifyResult = 0,
      'Trusted chain should keep verify result at success');
    LSession := LClientConn.GetSession;
    AssertTrue(LSession <> nil, 'Trusted chain handshake should expose session snapshot');
    AssertTrue(Supports(LSession, IFreePascalResumptionSession, LResumptionSession),
      'Session snapshot should expose internal resumption session view');
    LSessionChain := LResumptionSession.GetPeerCertificateChain;
    AssertTrue(Length(LSessionChain) >= 2,
      'Session snapshot should preserve peer leaf+intermediate chain');
    AssertTrue(LSessionChain[0] <> nil, 'Session chain should keep leaf certificate');
    AssertTrue(LSessionChain[1] <> nil, 'Session chain should keep intermediate certificate');
  finally
    LClientTransport.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal client chain verification path...');
  TestClientVerifiesLeafIntermediateChainWithTrustedRoot;
  WriteLn('✅ FreePascal client chain verification path checks passed');
end.
