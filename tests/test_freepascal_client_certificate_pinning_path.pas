program test_freepascal_client_certificate_pinning_path;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.cert.builder,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.asn1;

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

procedure AppendByte(var ADest: TBytes; AValue: Byte);
var
  LLen: Integer;
begin
  LLen := Length(ADest);
  SetLength(ADest, LLen + 1);
  ADest[LLen] := AValue;
end;

procedure AppendBytes(var ADest: TBytes; const ASource: TBytes);
var
  LOldLen, LAddLen: Integer;
begin
  LAddLen := Length(ASource);
  if LAddLen <= 0 then
    Exit;
  LOldLen := Length(ADest);
  SetLength(ADest, LOldLen + LAddLen);
  Move(ASource[0], ADest[LOldLen], LAddLen);
end;

procedure AppendASN1Length(var ADest: TBytes; ALength: Integer);
var
  LTemp: TBytes;
  LValue: Integer;
begin
  if ALength < $80 then
  begin
    AppendByte(ADest, Byte(ALength));
    Exit;
  end;

  SetLength(LTemp, 0);
  LValue := ALength;
  while LValue > 0 do
  begin
    SetLength(LTemp, Length(LTemp) + 1);
    Move(LTemp[0], LTemp[1], Length(LTemp) - 1);
    LTemp[0] := Byte(LValue and $FF);
    LValue := LValue shr 8;
  end;

  AppendByte(ADest, Byte($80 or Length(LTemp)));
  AppendBytes(ADest, LTemp);
end;

procedure AppendASN1Tag(var ADest: TBytes; const ATag: TASN1Tag);
var
  LFirst: Byte;
  LParts: array of Byte;
  LTagNumber: Cardinal;
  I: Integer;
begin
  LFirst := Byte(Integer(ATag.TagClass) shl 6);
  if ATag.Constructed then
    LFirst := LFirst or ASN1_CONSTRUCTED;

  if ATag.TagNumber < $1F then
  begin
    AppendByte(ADest, LFirst or Byte(ATag.TagNumber));
    Exit;
  end;

  AppendByte(ADest, LFirst or $1F);
  SetLength(LParts, 0);
  LTagNumber := ATag.TagNumber;
  repeat
    SetLength(LParts, Length(LParts) + 1);
    Move(LParts[0], LParts[1], Length(LParts) - 1);
    LParts[0] := Byte(LTagNumber and $7F);
    LTagNumber := LTagNumber shr 7;
  until LTagNumber = 0;

  for I := 0 to High(LParts) - 1 do
    LParts[I] := LParts[I] or $80;
  AppendBytes(ADest, LParts);
end;

function EncodeASN1Node(ANode: TASN1Node): TBytes;
var
  I: Integer;
  LContent: TBytes;
begin
  Result := nil;
  SetLength(Result, 0);
  SetLength(LContent, 0);

  if ANode.Tag.Constructed then
  begin
    for I := 0 to ANode.ChildCount - 1 do
      AppendBytes(LContent, EncodeASN1Node(ANode.GetChild(I)));
  end
  else
    LContent := Copy(ANode.RawData, 0, Length(ANode.RawData));

  AppendASN1Tag(Result, ANode.Tag);
  AppendASN1Length(Result, Length(LContent));
  AppendBytes(Result, LContent);
end;

function TryExtractSPKIDER(const ACertificate: ISSLCertificate; out ASPKIDER: TBytes): Boolean;
var
  LDER: TBytes;
  LReader: TASN1Reader;
  LRoot: TASN1Node;
  LTBS: TASN1Node;
  LIndex: Integer;
begin
  Result := False;
  SetLength(ASPKIDER, 0);
  if ACertificate = nil then
    Exit;

  LDER := ACertificate.SaveToDER;
  if Length(LDER) = 0 then
    Exit;

  LReader := TASN1Reader.Create(LDER);
  try
    LRoot := LReader.Parse;
    if (LRoot = nil) or (not LRoot.IsSequence) or (LRoot.ChildCount < 1) then
      Exit;

    LTBS := LRoot.GetChild(0);
    if (LTBS = nil) or (not LTBS.IsSequence) then
      Exit;

    LIndex := 0;
    if (LTBS.ChildCount > 0) and LTBS.GetChild(0).IsContextTag(0) then
      Inc(LIndex);
    Inc(LIndex, 5);
    if LTBS.ChildCount <= LIndex then
      Exit;

    ASPKIDER := EncodeASN1Node(LTBS.GetChild(LIndex));
    Result := Length(ASPKIDER) > 0;
  finally
    LReader.Free;
  end;
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
    'Trusted certificate should be accepted');
end;

procedure RequireServerName(const AConnection: ISSLConnection; const AHost: string);
var
  LClientConn: ISSLClientConnection;
begin
  AssertTrue(Supports(AConnection, ISSLClientConnection, LClientConn),
    'Connection should expose ISSLClientConnection');
  LClientConn.SetServerName(AHost);
end;

procedure TestCertificateAndPublicKeyPinningPath;
var
  LKeyPair: IKeyPairWithCertificate;
  LServerCertPEM: string;
  LServerKeyPEM: string;
  LServerCert: ISSLCertificate;
  LCertHash: TBytes;
  LPublicKeyHash: TBytes;
  LSPKIDER: TBytes;
  LClientCtx: ISSLContext;
  LClientConn: ISSLConnection;
  LClientTransport: TServerResponderStream;
  LWrongHash: TBytes;
begin
  LKeyPair := TCertificateBuilder.Create
    .WithCommonName('alt.example.com')
    .ValidFor(90)
    .WithRSAKey(2048)
    .AsServerCert
    .AddSubjectAltName('DNS:alt.example.com')
    .SelfSigned;
  AssertTrue(LKeyPair <> nil, 'Server key pair should be created');
  LKeyPair.SaveToPEM(LServerCertPEM, LServerKeyPEM);

  LServerCert := TSSLFactory.CreateCertificate(sslFreePascal);
  AssertTrue(LServerCert <> nil, 'Server certificate object should be created');
  AssertTrue(LServerCert.LoadFromPEM(LServerCertPEM),
    'Server certificate object should load PEM');
  LCertHash := SHA256(LServerCert.SaveToDER);
  AssertTrue(TryExtractSPKIDER(LServerCert, LSPKIDER),
    'Test should extract SPKI DER from server certificate');
  LPublicKeyHash := SHA256(LSPKIDER);

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'Client context should be created for certificate pin');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));
  LClientCtx.AddCertificatePin(LCertHash, 0, 'leaf cert pin', False);
  LClientCtx.SetCertificatePinningEnabled(True);

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for certificate pin');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(LClientConn.Connect,
      'Matching certificate pin should allow handshake');
  finally
    LClientTransport.Free;
  end;

  SetLength(LWrongHash, Length(LCertHash));
  if Length(LWrongHash) > 0 then
    LWrongHash[0] := $FF;

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'Client context should be created for wrong pin');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));
  LClientCtx.AddCertificatePin(LWrongHash, 0, 'wrong cert pin', False);
  LClientCtx.SetCertificatePinningEnabled(True);

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for wrong pin');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(not LClientConn.Connect,
      'Mismatching certificate pin should fail handshake');
    AssertTrue(Pos('pin', LowerCase(LClientConn.GetVerifyResultString)) > 0,
      'Pinning failure should mention pin semantics');
  finally
    LClientTransport.Free;
  end;

  LClientCtx := TSSLFactory.CreateContext(sslCtxClient, sslFreePascal);
  AssertTrue(LClientCtx <> nil, 'Client context should be created for public-key pin');
  LClientCtx.SetPreferredVersion(sslProtocolTLS13);
  LClientCtx.SetCertificateStore(CreateTrustedStore(LServerCertPEM));
  LClientCtx.AddCertificatePin(LPublicKeyHash, 1, 'leaf public key pin', False);
  LClientCtx.SetCertificatePinningEnabled(True);

  LClientTransport := TServerResponderStream.Create(LServerCertPEM, LServerKeyPEM);
  try
    LClientConn := LClientCtx.CreateConnection(LClientTransport);
    AssertTrue(LClientConn <> nil, 'Client connection should be created for public-key pin');
    RequireServerName(LClientConn, 'alt.example.com');
    AssertTrue(LClientConn.Connect,
      'Matching public-key pin should allow handshake');
  finally
    LClientTransport.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal client certificate pinning path...');
  TestCertificateAndPublicKeyPinningPath;
  WriteLn('✅ FreePascal client certificate pinning path checks passed');
end.
