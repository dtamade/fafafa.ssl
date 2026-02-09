program test_freepascal_server_accept_skeleton;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.parser,
  fafafa.ssl.tls13.clienthello,
  fafafa.ssl.tls13.x25519;

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

procedure AssertEqualsWord(AExpected, AActual: Word; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=0x%.4x actual=0x%.4x)', [AMessage, AExpected, AActual]));
end;

procedure TestServerAcceptSkeleton;
var
  LCtx: ISSLContext;
  LConn: ISSLConnection;
  LIOStream: TMemoryStream;
  LClientPrivate: TBytes;
  LClientPublic: TBytes;
  LClientHelloRecord: TBytes;
  LServerResponse: TBytes;
  LServerResponseLen: Integer;
  LHeader: TTLSRecordHeader;
  LHandshake: TBytes;
  LServerHello: TTLS13ServerHelloInfo;
  LAcceptResult: Boolean;
  LVerifyStr: string;
begin
  LCtx := TSSLFactory.CreateContext(sslCtxServer, sslFreePascal);
  AssertTrue(LCtx <> nil, 'FreePascal server context should be created');
  LCtx.SetPreferredVersion(sslProtocolTLS13);
  LCtx.LoadCertificate('tests/certificate/test_certs/signer_cert.pem');
  LCtx.LoadPrivateKey('tests/certificate/test_certs/signer_key.pem');

  GenerateX25519KeyPair(LClientPrivate, LClientPublic);
  LClientHelloRecord := BuildTLS13ClientHelloRecord('localhost', '', LClientPublic);

  LIOStream := TMemoryStream.Create;
  try
    if Length(LClientHelloRecord) > 0 then
      LIOStream.WriteBuffer(LClientHelloRecord[0], Length(LClientHelloRecord));
    LIOStream.Position := 0;

    LConn := LCtx.CreateConnection(LIOStream);
    AssertTrue(LConn <> nil, 'Server connection should be created');

    LAcceptResult := LConn.Accept;
    AssertTrue(not LAcceptResult, 'Server accept should fail in one-way stream test');

    LVerifyStr := LowerCase(LConn.GetVerifyResultString);
    AssertTrue(
      (LConn.GetError(-1) = sslErrIO) or (LConn.GetError(-1) = sslErrProtocol) or (LConn.GetError(-1) = sslErrUnsupported),
      'Accept failure should be IO/protocol/unsupported'
    );
    AssertTrue(
      (Pos('client finished', LVerifyStr) > 0) or
      (Pos('certificateverify signer', LVerifyStr) > 0) or
      (Pos('placeholder_certverify', LVerifyStr) > 0),
      'Failure reason should indicate missing client Finished or pending CertificateVerify signer'
    );

    AssertTrue(LConn.GetProtocolVersion = sslProtocolTLS13,
      'Server skeleton should at least negotiate TLS 1.3 before stopping');
    AssertTrue(LConn.GetCipherName = 'TLS_CHACHA20_POLY1305_SHA256',
      'Server skeleton should select CHACHA20-POLY1305');

    LServerResponseLen := LIOStream.Size - Length(LClientHelloRecord);
    AssertTrue(LServerResponseLen > 0, 'Server should write a ServerHello record to transport');

    SetLength(LServerResponse, LServerResponseLen);
    LIOStream.Position := Length(LClientHelloRecord);
    if LServerResponseLen > 0 then
      LIOStream.ReadBuffer(LServerResponse[0], LServerResponseLen);

    AssertTrue(ParseTLSRecordHeader(LServerResponse, LHeader), 'Server response record header should parse');
    AssertTrue(LHeader.ContentType = TLS_CONTENT_TYPE_HANDSHAKE, 'First server response should be handshake record');
    AssertTrue(TryExtractHandshakePayloadFromRecord(LServerResponse, LHandshake),
      'Handshake payload extraction should succeed');
    AssertTrue(TryParseServerHelloFromHandshake(LHandshake, LServerHello),
      'ServerHello parsing should succeed');

    AssertTrue(LServerHello.Valid, 'Parsed ServerHello should be valid');
    AssertEqualsWord(TLS13_VERSION, LServerHello.SelectedVersion, 'Selected version should be TLS 1.3');
    AssertEqualsWord(TLS13_CIPHER_CHACHA20_POLY1305_SHA256, LServerHello.SelectedCipherSuite,
      'Selected cipher should be CHACHA20-POLY1305');
    AssertTrue(LServerHello.HasKeyShare, 'ServerHello should contain key_share');
    AssertEqualsWord(TLS13_GROUP_X25519, LServerHello.KeyShareGroup, 'ServerHello key_share group should be X25519');
    AssertEqualsWord(32, LServerHello.KeyShareLength, 'ServerHello key_share length should be 32');
  finally
    LIOStream.Free;
  end;
end;

begin
  WriteLn('Testing FreePascal TLS1.3 server accept skeleton...');

  TestServerAcceptSkeleton;

  WriteLn('✅ FreePascal server accept skeleton checks passed');
end.
