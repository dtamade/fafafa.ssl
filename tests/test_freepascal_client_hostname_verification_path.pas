program test_freepascal_client_hostname_verification_path;

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

procedure TestHostnameVerificationOnClientHandshakePath;
var
  LKeyPair: IKeyPairWithCertificate;
  LServerCertPEM: string;
  LServerKeyPEM: string;
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
begin
  // CN is intentionally different from SAN, so the positive case proves
  // that SAN-based hostname verification is actually wired into the client path.
  LKeyPair := TCertificate.CreateServerCert('cn.example.com', ['alt.example.com']);
  AssertTrue(LKeyPair <> nil, 'Server key pair should be created');
  LKeyPair.SaveToPEM(LServerCertPEM, LServerKeyPEM);
  AssertTrue((LServerCertPEM <> '') and (LServerKeyPEM <> ''),
    'Server key pair should export certificate and key PEM');

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for matching hostname');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(LClientConn.Connect,
      'Trusted matching hostname should complete client handshake: ' +
      LClientConn.GetVerifyResultString);
    AssertTrue(LClientConn.GetVerifyResult = 0,
      'Trusted matching hostname should keep verify result at success');
    LClientConn := nil;
  finally
    LClientTransport.Free;
  end;

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for mismatch hostname');
    RequireServerName(LClientConn, 'wrong.example.com');
    AssertTrue(not LClientConn.Connect,
      'Hostname mismatch should fail client handshake verification');
    AssertTrue(LClientConn.GetError(-1) = sslErrVerificationFailed,
      'Hostname mismatch should surface verification error classification');
    AssertTrue(Pos('hostname', LowerCase(LClientConn.GetVerifyResultString)) > 0,
      'Hostname mismatch should mention hostname in verify result string');
    LClientConn := nil;
  finally
    LClientTransport.Free;
  end;

  // SAN present -> CN must not be used as fallback.
  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for SAN precedence check');
    RequireServerName(LClientConn, 'cn.example.com');
    AssertTrue(not LClientConn.Connect,
      'SAN should override CN: CN match must not bypass SAN mismatch');
    AssertTrue(LClientConn.GetError(-1) = sslErrVerificationFailed,
      'SAN mismatch should surface verification error classification');
    LClientConn := nil;
  finally
    LClientTransport.Free;
  end;

  // Ignore hostname flag should allow handshake to succeed even with mismatch.
  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created for ignore-hostname');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));
  LClientCtx.SetCertVerifyFlags(LClientCtx.GetCertVerifyFlags + [sslCertVerifyIgnoreHostname]);

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for ignore-hostname mismatch');
    RequireServerName(LClientConn, 'wrong.example.com');
    AssertTrue(LClientConn.Connect,
      'Ignore-hostname should allow mismatch handshake to complete: ' +
      LClientConn.GetVerifyResultString);
    LClientConn := nil;
  finally
    LClientTransport.Free;
  end;

  // Missing server name should fail hostname verification (unless ignore-hostname is set).
  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'FreePascal client context should be created for missing-server-name');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for missing-server-name case');
    AssertTrue(not LClientConn.Connect,
      'Missing server name should fail client handshake verification');
    AssertTrue(LClientConn.GetError(-1) = sslErrVerificationFailed,
      'Missing server name should surface verification error classification');
    AssertTrue(Pos('server name', LowerCase(LClientConn.GetVerifyResultString)) > 0,
      'Missing server name should mention server name in verify result string');
    LClientConn := nil;
  finally
    LClientTransport.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal client hostname verification path...');
  TestHostnameVerificationOnClientHandshakePath;
  WriteLn('✅ FreePascal client hostname verification path checks passed');
end.
