program test_tls12_foundation;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls12.wire,
  fafafa.ssl.tls12.clienthello,
  fafafa.ssl.tls12.serverhello.parser;

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

function BuildExtensionHeader(AType: Word; const AData: TBytes): TBytes;
begin
  Result := nil;
  AppendUInt16(Result, AType);
  AppendUInt16(Result, Word(Length(AData)));
  AppendBytes(Result, AData);
end;

function BuildSyntheticServerHelloHandshake: TBytes;
var
  LBody: TBytes;
  LRandom: TBytes;
  LSessionID: TBytes;
  LExtensions: TBytes;
  LExtData: TBytes;
begin
  SetLength(LRandom, 32);
  FillChar(LRandom[0], Length(LRandom), $44);
  SetLength(LSessionID, 32);
  FillChar(LSessionID[0], Length(LSessionID), $55);

  LBody := nil;
  AppendUInt16(LBody, TLS12_VERSION);
  AppendBytes(LBody, LRandom);
  AppendByte(LBody, Byte(Length(LSessionID)));
  AppendBytes(LBody, LSessionID);
  AppendUInt16(LBody, TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256);
  AppendByte(LBody, TLS_COMPRESSION_NULL);

  LExtensions := nil;
  LExtData := nil;
  AppendUInt16(LExtData, 3);
  AppendByte(LExtData, 2);
  AppendByte(LExtData, Ord('h'));
  AppendByte(LExtData, Ord('2'));
  AppendBytes(LExtensions, BuildExtensionHeader(TLS_EXTENSION_ALPN, LExtData));

  AppendUInt16(LBody, Word(Length(LExtensions)));
  AppendBytes(LBody, LExtensions);

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_SERVER_HELLO);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildSyntheticCertificateHandshake: TBytes;
var
  LBody: TBytes;
  LCertList: TBytes;
  LCertA: TBytes;
  LCertB: TBytes;
begin
  LCertA := TBytes.Create($30, $82, $01, $01, $AA, $BB);
  LCertB := TBytes.Create($30, $82, $02, $02, $CC, $DD, $EE);

  LCertList := nil;
  AppendUInt24(LCertList, Length(LCertA));
  AppendBytes(LCertList, LCertA);
  AppendUInt24(LCertList, Length(LCertB));
  AppendBytes(LCertList, LCertB);

  LBody := nil;
  AppendUInt24(LBody, Length(LCertList));
  AppendBytes(LBody, LCertList);

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_CERTIFICATE);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildSyntheticServerKeyExchangeHandshake: TBytes;
var
  LBody: TBytes;
  LPublicKey: TBytes;
  LSignature: TBytes;
begin
  SetLength(LPublicKey, 32);
  FillChar(LPublicKey[0], Length(LPublicKey), $66);
  SetLength(LSignature, 16);
  FillChar(LSignature[0], Length(LSignature), $77);

  LBody := nil;
  AppendByte(LBody, 3);
  AppendUInt16(LBody, TLS_GROUP_X25519);
  AppendByte(LBody, Byte(Length(LPublicKey)));
  AppendBytes(LBody, LPublicKey);
  AppendUInt16(LBody, TLS_SIG_RSA_PKCS1_SHA256);
  AppendUInt16(LBody, Word(Length(LSignature)));
  AppendBytes(LBody, LSignature);

  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_SERVER_KEY_EXCHANGE);
  AppendUInt24(Result, Length(LBody));
  AppendBytes(Result, LBody);
end;

function BuildSyntheticServerHelloDoneHandshake: TBytes;
begin
  Result := nil;
  AppendByte(Result, TLS_HANDSHAKE_TYPE_SERVER_HELLO_DONE);
  AppendUInt24(Result, 0);
end;

procedure TestBuildClientHelloRecord;
var
  LRecord: TBytes;
  LHeader: TTLS12RecordHeader;
  LHandshake: TBytes;
begin
  LRecord := BuildTLS12ClientHelloRecord(
    'example.com',
    'h2',
    [TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256]
  );

  AssertTrue(ParseTLS12RecordHeader(LRecord, LHeader), 'TLS1.2 ClientHello record header should parse');
  AssertEqualsByte(TLS_CONTENT_TYPE_HANDSHAKE, LHeader.ContentType, 'TLS1.2 ClientHello record content type mismatch');
  AssertEqualsWord(TLS12_VERSION, LHeader.ProtocolVersion, 'TLS1.2 ClientHello record version mismatch');

  AssertTrue(TryExtractTLS12HandshakePayloadFromRecord(LRecord, LHandshake),
    'TLS1.2 ClientHello handshake should be extractable from record');
  AssertEqualsByte(TLS_HANDSHAKE_TYPE_CLIENT_HELLO, LHandshake[0], 'TLS1.2 ClientHello handshake type mismatch');
end;

procedure TestParseSyntheticServerHello;
var
  LHandshake: TBytes;
  LRecord: TBytes;
  LPayload: TBytes;
  LInfo: TTLS12ServerHelloInfo;
begin
  LHandshake := BuildSyntheticServerHelloHandshake;
  LRecord := BuildTLS12Plaintext(TLS_CONTENT_TYPE_HANDSHAKE, LHandshake);

  AssertTrue(TryExtractTLS12HandshakePayloadFromRecord(LRecord, LPayload),
    'TLS1.2 ServerHello payload extraction failed');
  AssertTrue(TryParseTLS12ServerHelloFromHandshake(LPayload, LInfo),
    'TLS1.2 ServerHello parse failed');

  AssertTrue(LInfo.Valid, 'TLS1.2 ServerHello should be marked valid');
  AssertEqualsWord(TLS12_VERSION, LInfo.ServerVersion, 'TLS1.2 ServerHello version mismatch');
  AssertEqualsWord(TLS12_CIPHER_ECDHE_RSA_WITH_AES_128_GCM_SHA256, LInfo.SelectedCipherSuite,
    'TLS1.2 ServerHello cipher mismatch');
  AssertEqualsByte(TLS_COMPRESSION_NULL, LInfo.CompressionMethod, 'TLS1.2 ServerHello compression mismatch');
  AssertTrue(LInfo.SelectedALPNProtocol = 'h2', 'TLS1.2 ServerHello ALPN mismatch');
end;

procedure TestParseSyntheticCertificate;
var
  LHandshake: TBytes;
  LCerts: TTLS12CertificateList;
  LError: string;
begin
  LHandshake := BuildSyntheticCertificateHandshake;
  AssertTrue(TryParseTLS12CertificateFromHandshake(LHandshake, LCerts, LError),
    'TLS1.2 Certificate parse failed: ' + LError);
  AssertTrue(Length(LCerts) = 2, 'TLS1.2 Certificate message should expose two certificates');
  AssertTrue((Length(LCerts[0]) = 6) and (LCerts[0][4] = $AA), 'TLS1.2 first certificate mismatch');
  AssertTrue((Length(LCerts[1]) = 7) and (LCerts[1][4] = $CC), 'TLS1.2 second certificate mismatch');
end;

procedure TestParseSyntheticServerKeyExchange;
var
  LHandshake: TBytes;
  LInfo: TTLS12ServerKeyExchangeInfo;
  LError: string;
begin
  LHandshake := BuildSyntheticServerKeyExchangeHandshake;
  AssertTrue(TryParseTLS12ServerKeyExchangeECDHERSAFromHandshake(LHandshake, LInfo, LError),
    'TLS1.2 ServerKeyExchange parse failed: ' + LError);
  AssertTrue(LInfo.Valid, 'TLS1.2 ServerKeyExchange should be valid');
  AssertEqualsByte(3, LInfo.CurveType, 'TLS1.2 ServerKeyExchange curve_type mismatch');
  AssertEqualsWord(TLS_GROUP_X25519, LInfo.NamedCurve, 'TLS1.2 ServerKeyExchange named_curve mismatch');
  AssertTrue((Length(LInfo.PublicKey) = 32) and (LInfo.PublicKey[0] = $66),
    'TLS1.2 ServerKeyExchange public key mismatch');
  AssertEqualsWord(TLS_SIG_RSA_PKCS1_SHA256, LInfo.SignatureAlgorithm,
    'TLS1.2 ServerKeyExchange signature algorithm mismatch');
  AssertTrue((Length(LInfo.Signature) = 16) and (LInfo.Signature[0] = $77),
    'TLS1.2 ServerKeyExchange signature mismatch');
end;

procedure TestParseSyntheticServerHelloDone;
var
  LHandshake: TBytes;
begin
  LHandshake := BuildSyntheticServerHelloDoneHandshake;
  AssertTrue(TryParseTLS12ServerHelloDoneFromHandshake(LHandshake),
    'TLS1.2 ServerHelloDone parse failed');
end;

begin
  WriteLn('Testing TLS 1.2 foundation units...');

  TestBuildClientHelloRecord;
  TestParseSyntheticServerHello;
  TestParseSyntheticCertificate;
  TestParseSyntheticServerKeyExchange;
  TestParseSyntheticServerHelloDone;

  WriteLn('✅ TLS 1.2 foundation checks passed');
end.
