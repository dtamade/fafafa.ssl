program test_tls12_clienthello;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls12.wire,
  fafafa.ssl.tls12.clienthello;

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

procedure AssertEqualsByte(AExpected, AActual: Byte; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=0x%.2x actual=0x%.2x)', [AMessage, AExpected, AActual]));
end;

function BytesEqual(const ALeft, ARight: TBytes): Boolean;
var
  I: Integer;
begin
  if Length(ALeft) <> Length(ARight) then
    Exit(False);

  Result := True;
  for I := 0 to High(ALeft) do
    if ALeft[I] <> ARight[I] then
      Exit(False);
end;

procedure AssertBytesEqual(const AExpected, AActual: TBytes; const AMessage: string);
begin
  if not BytesEqual(AExpected, AActual) then
    Fail(AMessage);
end;

function FindExtensionData(const AExtensions: TBytes; AExtensionType: Word; out AData: TBytes): Boolean;
var
  LOffset: Integer;
  LExtType: Word;
  LExtLen: Word;
begin
  Result := False;
  SetLength(AData, 0);
  LOffset := 0;
  while LOffset + 4 <= Length(AExtensions) do
  begin
    LExtType := ReadUInt16(AExtensions, LOffset);
    LExtLen := ReadUInt16(AExtensions, LOffset + 2);
    Inc(LOffset, 4);
    if LOffset + LExtLen > Length(AExtensions) then
      Fail('Extension block overflow');
    if LExtType = AExtensionType then
    begin
      SetLength(AData, LExtLen);
      if LExtLen > 0 then
        Move(AExtensions[LOffset], AData[0], LExtLen);
      Exit(True);
    end;
    Inc(LOffset, LExtLen);
  end;
end;

procedure TestBuildDeterministicClientHello;
var
  LClientRandom: TBytes;
  LSessionID: TBytes;
  LHandshake: TBytes;
  LBodyLen: Cardinal;
  LOffset: Integer;
  LCipherSuitesLen: Word;
  LCompressionLen: Byte;
  LExtensionsLen: Word;
  LExtensions: TBytes;
  LExtData: TBytes;
begin
  SetLength(LClientRandom, 32);
  FillChar(LClientRandom[0], Length(LClientRandom), $11);
  SetLength(LSessionID, 32);
  FillChar(LSessionID[0], Length(LSessionID), $22);

  LHandshake := BuildTLS12ClientHelloHandshakeWithParams(
    'example.com',
    'h2,http/1.1',
    LClientRandom,
    LSessionID,
    [TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256,
     TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256]
  );

  AssertEqualsByte(TLS_HANDSHAKE_TYPE_CLIENT_HELLO, LHandshake[0], 'Handshake type mismatch');
  LBodyLen := ReadUInt24(LHandshake, 1);
  AssertTrue(Length(LHandshake) = 4 + Integer(LBodyLen), 'Handshake length mismatch');

  LOffset := 4;
  AssertEqualsWord(TLS12_VERSION, ReadUInt16(LHandshake, LOffset), 'ClientHello version mismatch');
  Inc(LOffset, 2);

  AssertBytesEqual(LClientRandom, Copy(LHandshake, LOffset, 32), 'ClientHello random mismatch');
  Inc(LOffset, 32);

  AssertEqualsByte(32, LHandshake[LOffset], 'Session ID length mismatch');
  Inc(LOffset);
  AssertBytesEqual(LSessionID, Copy(LHandshake, LOffset, 32), 'Session ID bytes mismatch');
  Inc(LOffset, 32);

  LCipherSuitesLen := ReadUInt16(LHandshake, LOffset);
  AssertEqualsWord(4, LCipherSuitesLen, 'Cipher suites length mismatch');
  Inc(LOffset, 2);
  AssertEqualsWord(TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256, ReadUInt16(LHandshake, LOffset),
    'First cipher suite mismatch');
  Inc(LOffset, 2);
  AssertEqualsWord(TLS12_CIPHER_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256, ReadUInt16(LHandshake, LOffset),
    'Second cipher suite mismatch');
  Inc(LOffset, 2);

  LCompressionLen := LHandshake[LOffset];
  AssertEqualsByte(1, LCompressionLen, 'Compression methods length mismatch');
  Inc(LOffset);
  AssertEqualsByte(TLS_COMPRESSION_NULL, LHandshake[LOffset], 'Compression method mismatch');
  Inc(LOffset);

  LExtensionsLen := ReadUInt16(LHandshake, LOffset);
  Inc(LOffset, 2);
  AssertTrue(LOffset + LExtensionsLen = Length(LHandshake), 'Extensions length mismatch');
  LExtensions := Copy(LHandshake, LOffset, LExtensionsLen);

  AssertTrue(FindExtensionData(LExtensions, TLS_EXTENSION_SERVER_NAME, LExtData),
    'SNI extension should exist');
  AssertTrue(FindExtensionData(LExtensions, TLS_EXTENSION_ALPN, LExtData),
    'ALPN extension should exist');
  AssertTrue(FindExtensionData(LExtensions, TLS_EXTENSION_SUPPORTED_GROUPS, LExtData),
    'supported_groups extension should exist');
  AssertTrue(FindExtensionData(LExtensions, TLS_EXTENSION_EC_POINT_FORMATS, LExtData),
    'ec_point_formats extension should exist');
  AssertTrue(FindExtensionData(LExtensions, TLS_EXTENSION_SIGNATURE_ALGORITHMS, LExtData),
    'signature_algorithms extension should exist');
  AssertTrue(not FindExtensionData(LExtensions, TLS_EXTENSION_SUPPORTED_VERSIONS, LExtData),
    'TLS1.2 ClientHello should not send supported_versions extension');
end;

procedure TestBuildClientHelloRecord;
var
  LClientRandom: TBytes;
  LSessionID: TBytes;
  LRecord: TBytes;
  LHeader: TTLS12RecordHeader;
begin
  SetLength(LClientRandom, 32);
  FillChar(LClientRandom[0], Length(LClientRandom), $44);
  SetLength(LSessionID, 16);
  FillChar(LSessionID[0], Length(LSessionID), $55);

  LRecord := BuildTLS12ClientHelloRecordWithParams(
    'localhost',
    '',
    LClientRandom,
    LSessionID,
    [TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256]
  );

  AssertTrue(ParseTLS12RecordHeader(LRecord, LHeader), 'TLS1.2 record header should parse');
  AssertEqualsByte(TLS_CONTENT_TYPE_HANDSHAKE, LHeader.ContentType, 'Record content type mismatch');
  AssertEqualsWord(TLS12_VERSION, LHeader.ProtocolVersion, 'Record version mismatch');
  AssertTrue(LHeader.Length = Length(LRecord) - 5, 'Record length field mismatch');
end;

begin
  WriteLn('Testing TLS 1.2 ClientHello builder...');

  TestBuildDeterministicClientHello;
  TestBuildClientHelloRecord;

  WriteLn('✅ TLS 1.2 ClientHello checks passed');
end.
