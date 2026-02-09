program test_tls13_servercertverify;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.clienthello,
  fafafa.ssl.tls13.clienthello.parser,
  fafafa.ssl.pem,
  fafafa.ssl.tls13.servercertverify;

function LoadFileBytes(const AFileName: string): TBytes;
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, LStream.Size);
    if LStream.Size > 0 then
      LStream.ReadBuffer(Result[0], LStream.Size);
  finally
    LStream.Free;
  end;
end;

function LoadFileText(const AFileName: string): string;
var
  LLines: TStringList;
begin
  LLines := TStringList.Create;
  try
    LLines.LoadFromFile(AFileName);
    Result := LLines.Text;
  finally
    LLines.Free;
  end;
end;

function TryReadDERLength(const AData: TBytes; var AOffset: Integer; out ALength: Integer): Boolean;
var
  LFirst: Byte;
  LCount: Integer;
  I: Integer;
begin
  ALength := 0;
  Result := False;

  if (AOffset < 0) or (AOffset >= Length(AData)) then
    Exit;

  LFirst := AData[AOffset];
  Inc(AOffset);

  if (LFirst and $80) = 0 then
  begin
    ALength := LFirst;
    Exit(True);
  end;

  LCount := LFirst and $7F;
  if (LCount <= 0) or (LCount > 4) or (AOffset + LCount > Length(AData)) then
    Exit;

  ALength := 0;
  for I := 1 to LCount do
  begin
    ALength := (ALength shl 8) or AData[AOffset];
    Inc(AOffset);
  end;

  Result := True;
end;

function TryLocatePKCS1PrivateExponentValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
var
  LOffset: Integer;
  LSeqLength: Integer;
  LSeqEnd: Integer;
  LFieldIndex: Integer;
begin
  AValueOffset := -1;
  AValueLength := 0;
  Result := False;

  if Length(ADER) < 4 then
    Exit;

  LOffset := 0;
  if ADER[LOffset] <> $30 then
    Exit;
  Inc(LOffset);

  if not TryReadDERLength(ADER, LOffset, LSeqLength) then
    Exit;

  LSeqEnd := LOffset + LSeqLength;
  if LSeqEnd > Length(ADER) then
    Exit;

  LFieldIndex := 0;
  while LOffset < LSeqEnd do
  begin
    if ADER[LOffset] <> $02 then
      Exit;
    Inc(LOffset);

    if not TryReadDERLength(ADER, LOffset, AValueLength) then
      Exit;

    if (AValueLength < 0) or (LOffset + AValueLength > LSeqEnd) then
      Exit;

    if LFieldIndex = 3 then
    begin
      AValueOffset := LOffset;
      Exit(True);
    end;

    Inc(LOffset, AValueLength);
    Inc(LFieldIndex);
  end;
end;

function TryMutatePKCS1PrivateExponent(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
var
  LValueOffset: Integer;
  LValueLength: Integer;
begin
  SetLength(ADestDER, 0);
  Result := False;

  if not TryLocatePKCS1PrivateExponentValue(ASourceDER, LValueOffset, LValueLength) then
    Exit;
  if LValueLength <= 0 then
    Exit;

  ADestDER := Copy(ASourceDER, 0, Length(ASourceDER));
  ADestDER[LValueOffset + LValueLength - 1] := ADestDER[LValueOffset + LValueLength - 1] xor $01;

  Result := True;
end;

function TryLocatePKCS1PrimePValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
var
  LOffset: Integer;
  LSeqLength: Integer;
  LSeqEnd: Integer;
  LFieldIndex: Integer;
begin
  AValueOffset := -1;
  AValueLength := 0;
  Result := False;

  if Length(ADER) < 4 then
    Exit;

  LOffset := 0;
  if ADER[LOffset] <> $30 then
    Exit;
  Inc(LOffset);

  if not TryReadDERLength(ADER, LOffset, LSeqLength) then
    Exit;

  LSeqEnd := LOffset + LSeqLength;
  if LSeqEnd > Length(ADER) then
    Exit;

  LFieldIndex := 0;
  while LOffset < LSeqEnd do
  begin
    if ADER[LOffset] <> $02 then
      Exit;
    Inc(LOffset);

    if not TryReadDERLength(ADER, LOffset, AValueLength) then
      Exit;

    if (AValueLength < 0) or (LOffset + AValueLength > LSeqEnd) then
      Exit;

    if LFieldIndex = 4 then
    begin
      AValueOffset := LOffset;
      Exit(True);
    end;

    Inc(LOffset, AValueLength);
    Inc(LFieldIndex);
  end;
end;

function TryMutatePKCS1PrimeP(const ASourceDER: TBytes; out ADestDER: TBytes): Boolean;
var
  LValueOffset: Integer;
  LValueLength: Integer;
begin
  SetLength(ADestDER, 0);
  Result := False;

  if not TryLocatePKCS1PrimePValue(ASourceDER, LValueOffset, LValueLength) then
    Exit;
  if LValueLength <= 1 then
    Exit;

  ADestDER := Copy(ASourceDER, 0, Length(ASourceDER));
  ADestDER[LValueOffset + LValueLength - 1] := ADestDER[LValueOffset + LValueLength - 1] xor $02;
  ADestDER[LValueOffset + LValueLength - 1] := ADestDER[LValueOffset + LValueLength - 1] or $01;

  Result := True;
end;

function TryLocatePKCS8PrivateKeyOctetStringValue(
  const ADER: TBytes;
  out AValueOffset: Integer;
  out AValueLength: Integer
): Boolean;
var
  LOffset: Integer;
  LSeqLength: Integer;
  LChildLength: Integer;
begin
  AValueOffset := -1;
  AValueLength := 0;
  Result := False;

  if Length(ADER) < 8 then
    Exit;

  LOffset := 0;
  if ADER[LOffset] <> $30 then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, LSeqLength) then
    Exit;
  if LOffset + LSeqLength > Length(ADER) then
    Exit;

  if (LOffset >= Length(ADER)) or (ADER[LOffset] <> $02) then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, LChildLength) then
    Exit;
  Inc(LOffset, LChildLength);

  if (LOffset >= Length(ADER)) or (ADER[LOffset] <> $30) then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, LChildLength) then
    Exit;
  Inc(LOffset, LChildLength);

  if (LOffset >= Length(ADER)) or (ADER[LOffset] <> $04) then
    Exit;
  Inc(LOffset);
  if not TryReadDERLength(ADER, LOffset, AValueLength) then
    Exit;
  if LOffset + AValueLength > Length(ADER) then
    Exit;

  AValueOffset := LOffset;
  Result := True;
end;

function TryExtractFirstPrivateKeyDER(
  const APEMBlob: TBytes;
  out ADER: TBytes;
  out AType: TPEMType
): Boolean;
var
  LReader: TPEMReader;
  LBlocks: TPEMBlockArray;
  LText: string;
  I: Integer;
begin
  SetLength(ADER, 0);
  AType := pemUnknown;
  Result := False;

  LReader := TPEMReader.Create;
  try
    LText := TEncoding.ANSI.GetString(APEMBlob);
    LReader.LoadFromString(LText);
    LBlocks := LReader.GetPrivateKeys;
    for I := 0 to High(LBlocks) do
    begin
      if LBlocks[I].IsEncrypted then
        Continue;
      if not (LBlocks[I].BlockType in [pemPrivateKey, pemRSAPrivateKey]) then
        Continue;

      ADER := Copy(LBlocks[I].Data, 0, Length(LBlocks[I].Data));
      AType := LBlocks[I].BlockType;
      Exit(Length(ADER) > 0);
    end;
  finally
    LReader.Free;
  end;
end;

function BuildMutatedPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1PrivateExponent(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrivateExponent(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedPrimePPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if TryMutatePKCS1PrimeP(LDER, Result) then
          Exit;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrimeP(LInnerPKCS1, LMutatedPKCS1) then
          Exit;

        if Length(LMutatedPKCS1) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1[0], Result[LOffset], LLength);
      end;
  end;
end;

function BuildMutatedPrimePAndPrivateExponentPrivateKeyBlob(const APEMBlob: TBytes): TBytes;
var
  LDER: TBytes;
  LMutatedPKCS1: TBytes;
  LMutatedPKCS1B: TBytes;
  LInnerPKCS1: TBytes;
  LType: TPEMType;
  LOffset: Integer;
  LLength: Integer;
begin
  SetLength(Result, 0);

  if not TryExtractFirstPrivateKeyDER(APEMBlob, LDER, LType) then
    Exit;

  case LType of
    pemRSAPrivateKey:
      begin
        if not TryMutatePKCS1PrimeP(LDER, LMutatedPKCS1) then
          Exit;
        if not TryMutatePKCS1PrivateExponent(LMutatedPKCS1, LMutatedPKCS1B) then
          Exit;
        Result := LMutatedPKCS1B;
      end;

    pemPrivateKey:
      begin
        if not TryLocatePKCS8PrivateKeyOctetStringValue(LDER, LOffset, LLength) then
          Exit;

        LInnerPKCS1 := Copy(LDER, LOffset, LLength);
        if not TryMutatePKCS1PrimeP(LInnerPKCS1, LMutatedPKCS1) then
          Exit;
        if not TryMutatePKCS1PrivateExponent(LMutatedPKCS1, LMutatedPKCS1B) then
          Exit;

        if Length(LMutatedPKCS1B) <> LLength then
          Exit;

        Result := Copy(LDER, 0, Length(LDER));
        Move(LMutatedPKCS1B[0], Result[LOffset], LLength);
      end;
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

procedure AssertEqualsWord(AExpected, AActual: Word; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=0x%.4x actual=0x%.4x)', [AMessage, AExpected, AActual]));
end;

procedure AssertEqualsInt(AExpected, AActual: Integer; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=%d actual=%d)', [AMessage, AExpected, AActual]));
end;

procedure TestSelectSchemeFromClientHello;
var
  LClientKeyShare: TBytes;
  LHandshake: TBytes;
  LInfo: TTLS13ClientHelloInfo;
  LErr: string;
  LScheme: Word;
begin
  SetLength(LClientKeyShare, 32);
  FillChar(LClientKeyShare[0], 32, $33);

  LHandshake := BuildTLS13ClientHelloHandshake('localhost', '', LClientKeyShare);
  AssertTrue(TryParseTLS13ClientHelloFromHandshake(LHandshake, LInfo, LErr),
    'Parse ClientHello failed: ' + LErr);

  AssertTrue(TrySelectTLS13ServerCertificateVerifyScheme(LInfo, LScheme, LErr),
    'Scheme selection failed: ' + LErr);
  AssertEqualsWord(TLS13_SIG_RSA_PSS_RSAE_SHA256, LScheme,
    'Scheme selector should prioritize rsa_pss_rsae_sha256');
end;

procedure TestBuildCertVerifyInput;
const
  CONTEXT = 'TLS 1.3, server CertificateVerify';
var
  LHash: TBytes;
  LInput: TBytes;
  I: Integer;
  LOffset: Integer;
begin
  SetLength(LHash, 32);
  for I := 0 to 31 do
    LHash[I] := Byte(I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LHash);
  AssertEqualsInt(64 + Length(CONTEXT) + 1 + 32, Length(LInput), 'CertificateVerify input length mismatch');

  for I := 0 to 63 do
    AssertTrue(LInput[I] = $20, 'CertificateVerify input must start with 64 spaces');

  LOffset := 64;
  for I := 1 to Length(CONTEXT) do
    AssertTrue(LInput[LOffset + I - 1] = Byte(Ord(CONTEXT[I])), 'Context string byte mismatch');

  AssertTrue(LInput[64 + Length(CONTEXT)] = 0, 'Context separator must be 0x00');
  for I := 0 to 31 do
    AssertTrue(LInput[64 + Length(CONTEXT) + 1 + I] = LHash[I], 'Transcript hash bytes mismatch');
end;

procedure TestBuildCertificateVerifyHandshake;
var
  LSignature: TBytes;
  LHandshake: TBytes;
  LLen: Cardinal;
begin
  SetLength(LSignature, 16);
  FillChar(LSignature[0], 16, $AB);

  LHandshake := BuildTLS13CertificateVerifyHandshake(TLS13_SIG_RSA_PSS_RSAE_SHA256, LSignature);

  AssertTrue(Length(LHandshake) = 4 + 2 + 2 + 16, 'CertificateVerify handshake total length mismatch');
  AssertTrue(LHandshake[0] = TLS_HANDSHAKE_TYPE_CERTIFICATE_VERIFY, 'Handshake type mismatch');

  LLen := ReadUInt24(LHandshake, 1);
  AssertEqualsInt(2 + 2 + 16, Integer(LLen), 'CertificateVerify body length mismatch');
  AssertEqualsWord(TLS13_SIG_RSA_PSS_RSAE_SHA256, ReadUInt16(LHandshake, 4), 'Signature scheme mismatch');
  AssertEqualsWord(16, ReadUInt16(LHandshake, 6), 'Signature length field mismatch');
end;

procedure TestPlaceholderSignature;
var
  LHash: TBytes;
  LSigA, LSigB: TBytes;
  I: Integer;
  LDiff: Integer;
begin
  SetLength(LHash, 32);
  for I := 0 to 31 do
    LHash[I] := Byte($90 + I);

  LSigA := BuildTLS13PlaceholderSignatureFromTranscriptHash(LHash, 64);
  LSigB := BuildTLS13PlaceholderSignatureFromTranscriptHash(LHash, 64);

  AssertEqualsInt(64, Length(LSigA), 'Placeholder signature length mismatch');
  AssertEqualsInt(64, Length(LSigB), 'Placeholder signature length mismatch');

  LDiff := 0;
  for I := 0 to 63 do
  begin
    if LSigA[I] <> LSigB[I] then
      Inc(LDiff);
  end;
  AssertEqualsInt(0, LDiff, 'Placeholder signature should be deterministic for same input');
end;

procedure TestRealRSASignature;
var
  LKeyBlob: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigPSSA, LSigPSSB: TBytes;
  LSigPKCS1A, LSigPKCS1B: TBytes;
  LErr: string;
  I, LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($21 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PSS_RSAE_SHA256,
      LKeyBlob,
      LInput,
      LSigPSSA,
      LErr
    ),
    'RSA-PSS signing failed: ' + LErr
  );
  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PSS_RSAE_SHA256,
      LKeyBlob,
      LInput,
      LSigPSSB,
      LErr
    ),
    'RSA-PSS signing failed on second call: ' + LErr
  );
  AssertEqualsInt(256, Length(LSigPSSA), 'RSA-PSS signature length should match 2048-bit key');
  AssertEqualsInt(256, Length(LSigPSSB), 'RSA-PSS signature length should match 2048-bit key');

  LDiff := 0;
  for I := 0 to Length(LSigPSSA) - 1 do
    if LSigPSSA[I] <> LSigPSSB[I] then
      Inc(LDiff);
  AssertTrue(LDiff > 0, 'RSA-PSS signatures should vary because of randomized salt');

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigPKCS1A,
      LErr
    ),
    'RSA-PKCS1 signing failed: ' + LErr
  );
  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigPKCS1B,
      LErr
    ),
    'RSA-PKCS1 signing failed on second call: ' + LErr
  );
  AssertEqualsInt(256, Length(LSigPKCS1A), 'RSA-PKCS1 signature length should match 2048-bit key');
  AssertEqualsInt(256, Length(LSigPKCS1B), 'RSA-PKCS1 signature length should match 2048-bit key');

  LDiff := 0;
  for I := 0 to Length(LSigPKCS1A) - 1 do
    if LSigPKCS1A[I] <> LSigPKCS1B[I] then
      Inc(LDiff);
  AssertEqualsInt(0, LDiff, 'RSA-PKCS1 signatures should be deterministic for same input/key');
end;

procedure TestSignerUnitHasNoExternalBigIntDependency;
var
  LSource: string;
begin
  LSource := LowerCase(LoadFileText('src/fafafa.ssl.tls13.servercertverify.pas'));

  AssertTrue(
    Pos('fafafa.ssl.openssl', LSource) = 0,
    'TLS13 server CertificateVerify signer must not depend on OpenSSL units'
  );
  AssertTrue(
    Pos('gmp', LSource) = 0,
    'TLS13 server CertificateVerify signer must not depend on GMP'
  );
end;

procedure TestRSASignatureUsesCRTWhenAvailable;
var
  LKeyBlob: TBytes;
  LMutatedKeyDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigOriginal: TBytes;
  LSigMutated: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  LMutatedKeyDER := BuildMutatedPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedKeyDER) > 0, 'Failed to produce DER key with corrupted privateExponent');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($A0 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigOriginal,
      LErr
    ),
    'RSA-PKCS1 signing with original key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedKeyDER,
      LInput,
      LSigMutated,
      LErr
    ),
    'RSA-PKCS1 signing with mutated key failed: ' + LErr
  );

  AssertEqualsInt(Length(LSigOriginal), Length(LSigMutated), 'Signature length mismatch');

  LDiff := 0;
  for I := 0 to Length(LSigOriginal) - 1 do
    if LSigOriginal[I] <> LSigMutated[I] then
      Inc(LDiff);

  AssertEqualsInt(0, LDiff,
    'Corrupted privateExponent should not affect RSA-PKCS1 signature when CRT components are used');
end;

procedure TestRSASignatureFallsBackWhenCRTInconsistent;
var
  LKeyBlob: TBytes;
  LMutatedPKeyDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigOriginal: TBytes;
  LSigMutated: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  LMutatedPKeyDER := BuildMutatedPrimePPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedPKeyDER) > 0, 'Failed to produce DER key with corrupted prime p');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($B0 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigOriginal,
      LErr
    ),
    'RSA-PKCS1 signing with original key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedPKeyDER,
      LInput,
      LSigMutated,
      LErr
    ),
    'RSA-PKCS1 signing with CRT-inconsistent key should fallback and still succeed: ' + LErr
  );

  AssertEqualsInt(Length(LSigOriginal), Length(LSigMutated), 'Signature length mismatch');

  LDiff := 0;
  for I := 0 to Length(LSigOriginal) - 1 do
    if LSigOriginal[I] <> LSigMutated[I] then
      Inc(LDiff);

  AssertEqualsInt(0, LDiff,
    'Corrupted prime p should not change signature when signer falls back to private exponent path');
end;

procedure TestRSASignatureUsesCorruptedExponentWhenCRTBroken;
var
  LKeyBlob: TBytes;
  LMutatedBothDER: TBytes;
  LTranscriptHash: TBytes;
  LInput: TBytes;
  LSigOriginal: TBytes;
  LSig: TBytes;
  LErr: string;
  I: Integer;
  LDiff: Integer;
begin
  LKeyBlob := LoadFileBytes('tests/certificate/test_certs/signer_key.pem');
  AssertTrue(Length(LKeyBlob) > 0, 'Signer key blob should not be empty');

  LMutatedBothDER := BuildMutatedPrimePAndPrivateExponentPrivateKeyBlob(LKeyBlob);
  AssertTrue(Length(LMutatedBothDER) > 0, 'Failed to produce DER key with corrupted prime p + private exponent');

  SetLength(LTranscriptHash, 32);
  for I := 0 to 31 do
    LTranscriptHash[I] := Byte($C0 + I);

  LInput := BuildTLS13ServerCertificateVerifyInputSHA256(LTranscriptHash);

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LKeyBlob,
      LInput,
      LSigOriginal,
      LErr
    ),
    'RSA-PKCS1 signing with original key failed: ' + LErr
  );

  AssertTrue(
    TryBuildTLS13CertificateVerifySignature(
      TLS13_SIG_RSA_PKCS1_SHA256,
      LMutatedBothDER,
      LInput,
      LSig,
      LErr
    ),
    'Signing should still succeed via fallback exponent path: ' + LErr
  );

  AssertEqualsInt(Length(LSigOriginal), Length(LSig), 'Signature length mismatch');

  LDiff := 0;
  for I := 0 to Length(LSigOriginal) - 1 do
    if LSigOriginal[I] <> LSig[I] then
      Inc(LDiff);

  AssertTrue(LDiff > 0,
    'Corrupted private exponent should produce a different signature when CRT is broken and fallback is used');
end;

begin
  WriteLn('Testing TLS 1.3 server CertificateVerify helpers...');

  TestSelectSchemeFromClientHello;
  TestBuildCertVerifyInput;
  TestBuildCertificateVerifyHandshake;
  TestPlaceholderSignature;
  TestSignerUnitHasNoExternalBigIntDependency;
  TestRSASignatureUsesCRTWhenAvailable;
  TestRSASignatureFallsBackWhenCRTInconsistent;
  TestRSASignatureUsesCorruptedExponentWhenCRTBroken;
  TestRealRSASignature;

  WriteLn('✅ TLS 1.3 server CertificateVerify helper checks passed');
end.
