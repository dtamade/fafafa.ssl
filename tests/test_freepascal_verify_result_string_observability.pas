program test_freepascal_verify_result_string_observability;

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

procedure AssertEquals(const AExpected, AActual, AMessage: string);
begin
  if AExpected <> AActual then
    Fail(AMessage + ' expected="' + AExpected + '" actual="' + AActual + '"');
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

procedure TestVerifyResultStringObservability;
var
  LKeyPair: IKeyPairWithCertificate;
  LServerCertPEM: string;
  LServerKeyPEM: string;
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
begin
  LKeyPair := TCertificate.CreateServerCert('alt.example.com', ['alt.example.com']);
  AssertTrue(LKeyPair <> nil, 'Server key pair should be created');
  LKeyPair.SaveToPEM(LServerCertPEM, LServerKeyPEM);

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Verified client connection should be created');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(LClientConn.Connect, 'Verified handshake should succeed');
    AssertEquals('Verification passed', LClientConn.GetVerifyResultString,
      'Verified handshake should expose successful verification string');
  finally
    LClientTransport.Free;
  end;

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'Second client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetVerifyMode([]);

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Non-verified client connection should be created');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(LClientConn.Connect, 'Non-verified handshake should still succeed');
    AssertEquals('Verification disabled', LClientConn.GetVerifyResultString,
      'Non-verified handshake should expose disabled verification string');
  finally
    LClientTransport.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal verify result string observability...');
  TestVerifyResultStringObservability;
  WriteLn('✅ FreePascal verify result string observability checks passed');
end.
