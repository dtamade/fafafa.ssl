{**
 * Unit: fafafa.ssl.tls12.rsa.verify
 * Purpose: TLS 1.2 RSA PKCS#1 v1.5 SHA-256 verify helpers
 *}

unit fafafa.ssl.tls12.rsa.verify;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function TryVerifyTLS12RSAPKCS1v15SHA256Signature(
  const AMessage: TBytes;
  const AModulus: TBytes;
  const APublicExponent: TBytes;
  const ASignature: TBytes;
  out AError: string
): Boolean;

function TryVerifyTLS12RSAPKCS1v15SHA512Signature(
  const AMessage: TBytes;
  const AModulus: TBytes;
  const APublicExponent: TBytes;
  const ASignature: TBytes;
  out AError: string
): Boolean;

function TryVerifyTLS12RSAPSSSHA256Signature(
  const AMessage: TBytes;
  const AModulus: TBytes;
  const APublicExponent: TBytes;
  const ASignature: TBytes;
  out AError: string
): Boolean;

implementation

uses
  fafafa.ssl.errors,
  fafafa.ssl.crypto.hash,
  fafafa.ssl.tls13.bigint;

const
  SHA256_DIGESTINFO_PREFIX: array[0..18] of Byte = (
    $30, $31, $30, $0D,
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $01,
    $05, $00,
    $04, $20
  );
  SHA512_DIGESTINFO_PREFIX: array[0..18] of Byte = (
    $30, $51, $30, $0D,
    $06, $09, $60, $86, $48, $01, $65, $03, $04, $02, $03,
    $05, $00,
    $04, $40
  );

function StripLeadingZeroBytes(const AData: TBytes): TBytes;
var
  I: Integer;
begin
  Result := nil;
  I := 0;
  while (I < Length(AData)) and (AData[I] = 0) do
    Inc(I);

  if I >= Length(AData) then
  begin
    SetLength(Result, 1);
    Result[0] := 0;
    Exit;
  end;

  Result := Copy(AData, I, Length(AData) - I);
end;

function LeftPadBytes(const AData: TBytes; ATargetLength: Integer): TBytes;
var
  LOffset: Integer;
begin
  Result := nil;
  if ATargetLength < 0 then
    RaiseInvalidParameter('RSALeftPadLength');
  if Length(AData) > ATargetLength then
    RaiseInvalidParameter('RSALeftPadInputLength');

  SetLength(Result, ATargetLength);
  if ATargetLength = 0 then
    Exit;

  FillChar(Result[0], ATargetLength, 0);
  LOffset := ATargetLength - Length(AData);
  if Length(AData) > 0 then
    Move(AData[0], Result[LOffset], Length(AData));
end;

function TryBuildRSAPKCS1v15EncodedMessageSHA256(
  const AMessage: TBytes;
  AModulusLength: Integer;
  out AEncoded: TBytes;
  out AError: string
): Boolean;
var
  LHash: TBytes;
  LTLen: Integer;
  LPSLen: Integer;
  LOffset: Integer;
  I: Integer;
begin
  SetLength(AEncoded, 0);
  AError := '';
  Result := False;

  if AModulusLength <= 0 then
  begin
    AError := 'RSA modulus length is invalid';
    Exit;
  end;

  LHash := SHA256(AMessage);
  LTLen := Length(SHA256_DIGESTINFO_PREFIX) + Length(LHash);
  if AModulusLength < LTLen + 11 then
  begin
    AError := 'RSA modulus is too short for PKCS#1 v1.5 SHA-256 encoding';
    Exit;
  end;

  LPSLen := AModulusLength - LTLen - 3;
  SetLength(AEncoded, AModulusLength);

  AEncoded[0] := 0;
  AEncoded[1] := 1;
  for I := 0 to LPSLen - 1 do
    AEncoded[2 + I] := $FF;

  LOffset := 2 + LPSLen;
  AEncoded[LOffset] := 0;
  Inc(LOffset);

  Move(SHA256_DIGESTINFO_PREFIX[0], AEncoded[LOffset], Length(SHA256_DIGESTINFO_PREFIX));
  Inc(LOffset, Length(SHA256_DIGESTINFO_PREFIX));
  Move(LHash[0], AEncoded[LOffset], Length(LHash));

  Result := True;
end;

function TryBuildRSAPKCS1v15EncodedMessageSHA512(
  const AMessage: TBytes;
  AModulusLength: Integer;
  out AEncoded: TBytes;
  out AError: string
): Boolean;
var
  LHash: TBytes;
  LTLen: Integer;
  LPSLen: Integer;
  LOffset: Integer;
  I: Integer;
begin
  SetLength(AEncoded, 0);
  AError := '';
  Result := False;

  if AModulusLength <= 0 then
  begin
    AError := 'RSA modulus length is invalid';
    Exit;
  end;

  LHash := SHA512(AMessage);
  LTLen := Length(SHA512_DIGESTINFO_PREFIX) + Length(LHash);
  if AModulusLength < LTLen + 11 then
  begin
    AError := 'RSA modulus is too short for PKCS#1 v1.5 SHA-512 encoding';
    Exit;
  end;

  LPSLen := AModulusLength - LTLen - 3;
  SetLength(AEncoded, AModulusLength);
  AEncoded[0] := 0;
  AEncoded[1] := 1;
  for I := 0 to LPSLen - 1 do
    AEncoded[2 + I] := $FF;

  LOffset := 2 + LPSLen;
  AEncoded[LOffset] := 0;
  Inc(LOffset);
  Move(SHA512_DIGESTINFO_PREFIX[0], AEncoded[LOffset], Length(SHA512_DIGESTINFO_PREFIX));
  Inc(LOffset, Length(SHA512_DIGESTINFO_PREFIX));
  Move(LHash[0], AEncoded[LOffset], Length(LHash));
  Result := True;
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

function MGF1_SHA256(const ASeed: TBytes; AMaskLength: Integer): TBytes;
var
  LCounter: Cardinal;
  LOffset: Integer;
  LInput: TBytes;
  LHash: TBytes;
  LCopyLen: Integer;
begin
  Result := nil;
  if AMaskLength < 0 then
    RaiseInvalidParameter('MGF1MaskLength');

  SetLength(Result, AMaskLength);
  if AMaskLength = 0 then
    Exit;

  SetLength(LInput, Length(ASeed) + 4);
  if Length(ASeed) > 0 then
    Move(ASeed[0], LInput[0], Length(ASeed));

  LCounter := 0;
  LOffset := 0;
  while LOffset < AMaskLength do
  begin
    LInput[Length(ASeed)] := Byte((LCounter shr 24) and $FF);
    LInput[Length(ASeed) + 1] := Byte((LCounter shr 16) and $FF);
    LInput[Length(ASeed) + 2] := Byte((LCounter shr 8) and $FF);
    LInput[Length(ASeed) + 3] := Byte(LCounter and $FF);

    LHash := SHA256(LInput);
    LCopyLen := Length(LHash);
    if LCopyLen > AMaskLength - LOffset then
      LCopyLen := AMaskLength - LOffset;
    if LCopyLen > 0 then
      Move(LHash[0], Result[LOffset], LCopyLen);
    Inc(LOffset, LCopyLen);
    Inc(LCounter);
  end;
end;

function TryVerifyTLS12RSAPKCS1v15SHA256Signature(
  const AMessage: TBytes;
  const AModulus: TBytes;
  const APublicExponent: TBytes;
  const ASignature: TBytes;
  out AError: string
): Boolean;
var
  LExpectedEM: TBytes;
  LRecoveredEM: TBytes;
  LRecoveredRaw: TBytes;
  LModulus: TBytes;
  LExponent: TBytes;
  LSignature: TBytes;
begin
  AError := '';
  Result := False;

  if Length(AMessage) = 0 then
  begin
    AError := 'RSA verify input is empty';
    Exit;
  end;
  if (Length(AModulus) = 0) or (Length(APublicExponent) = 0) then
  begin
    AError := 'RSA public key is incomplete';
    Exit;
  end;
  if Length(ASignature) = 0 then
  begin
    AError := 'RSA signature is empty';
    Exit;
  end;

  LModulus := StripLeadingZeroBytes(AModulus);
  LExponent := StripLeadingZeroBytes(APublicExponent);
  LSignature := StripLeadingZeroBytes(ASignature);

  if not TryBuildRSAPKCS1v15EncodedMessageSHA256(AMessage, Length(LModulus), LExpectedEM, AError) then
    Exit;

  if not TryBigIntModExpFromUnsignedBytes(LSignature, LExponent, LModulus, LRecoveredRaw, AError) then
    Exit;

  try
    LRecoveredEM := LeftPadBytes(LRecoveredRaw, Length(LModulus));
  except
    on E: Exception do
    begin
      AError := 'Failed to normalize RSA recovered message: ' + E.Message;
      Exit;
    end;
  end;

  if not BytesEqual(LExpectedEM, LRecoveredEM) then
  begin
    AError := 'RSA PKCS#1 v1.5 SHA-256 signature mismatch';
    Exit;
  end;

  Result := True;
end;

function TryVerifyTLS12RSAPKCS1v15SHA512Signature(
  const AMessage: TBytes;
  const AModulus: TBytes;
  const APublicExponent: TBytes;
  const ASignature: TBytes;
  out AError: string
): Boolean;
var
  LExpectedEM: TBytes;
  LRecoveredEM: TBytes;
  LRecoveredRaw: TBytes;
  LModulus: TBytes;
  LExponent: TBytes;
  LSignature: TBytes;
begin
  AError := '';
  Result := False;

  if Length(AMessage) = 0 then
  begin
    AError := 'RSA verify input is empty';
    Exit;
  end;
  if (Length(AModulus) = 0) or (Length(APublicExponent) = 0) then
  begin
    AError := 'RSA public key is incomplete';
    Exit;
  end;
  if Length(ASignature) = 0 then
  begin
    AError := 'RSA signature is empty';
    Exit;
  end;

  LModulus := StripLeadingZeroBytes(AModulus);
  LExponent := StripLeadingZeroBytes(APublicExponent);
  LSignature := StripLeadingZeroBytes(ASignature);

  if not TryBuildRSAPKCS1v15EncodedMessageSHA512(AMessage, Length(LModulus), LExpectedEM, AError) then
    Exit;

  if not TryBigIntModExpFromUnsignedBytes(LSignature, LExponent, LModulus, LRecoveredRaw, AError) then
    Exit;

  try
    LRecoveredEM := LeftPadBytes(LRecoveredRaw, Length(LModulus));
  except
    on E: Exception do
    begin
      AError := 'Failed to normalize RSA recovered message: ' + E.Message;
      Exit;
    end;
  end;

  if not BytesEqual(LExpectedEM, LRecoveredEM) then
  begin
    AError := 'RSA PKCS#1 v1.5 SHA-512 signature mismatch';
    Exit;
  end;

  Result := True;
end;

function TryVerifyTLS12RSAPSSSHA256Signature(
  const AMessage: TBytes;
  const AModulus: TBytes;
  const APublicExponent: TBytes;
  const ASignature: TBytes;
  out AError: string
): Boolean;
const
  HASH_SIZE = 32;
  SALT_SIZE = 32;
var
  LRecoveredRaw: TBytes;
  LEM: TBytes;
  LModulus: TBytes;
  LExponent: TBytes;
  LSignature: TBytes;
  LEMBits: Integer;
  LEMLen: Integer;
  LUnusedBits: Integer;
  LMaskedDB: TBytes;
  LH: TBytes;
  LDBMask: TBytes;
  LDB: TBytes;
  LPSLen: Integer;
  LSalt: TBytes;
  LMHash: TBytes;
  LMPrime: TBytes;
  LHPrime: TBytes;
  I: Integer;
begin
  AError := '';
  Result := False;

  if Length(AMessage) = 0 then
  begin
    AError := 'RSA verify input is empty';
    Exit;
  end;
  if (Length(AModulus) = 0) or (Length(APublicExponent) = 0) then
  begin
    AError := 'RSA public key is incomplete';
    Exit;
  end;
  if Length(ASignature) = 0 then
  begin
    AError := 'RSA signature is empty';
    Exit;
  end;

  LModulus := StripLeadingZeroBytes(AModulus);
  LExponent := StripLeadingZeroBytes(APublicExponent);
  LSignature := StripLeadingZeroBytes(ASignature);

  if not TryBigIntModExpFromUnsignedBytes(LSignature, LExponent, LModulus, LRecoveredRaw, AError) then
    Exit;

  try
    LEM := LeftPadBytes(LRecoveredRaw, Length(LModulus));
  except
    on E: Exception do
    begin
      AError := 'Failed to normalize RSA recovered message: ' + E.Message;
      Exit;
    end;
  end;

  LEMBits := Length(LModulus) * 8 - 1;
  LEMLen := (LEMBits + 7) div 8;
  if Length(LEM) <> LEMLen then
  begin
    AError := 'RSA-PSS encoded message length mismatch';
    Exit;
  end;
  if (LEMLen < HASH_SIZE + SALT_SIZE + 2) or (LEM[LEMLen - 1] <> $BC) then
  begin
    AError := 'RSA-PSS encoded trailer is invalid';
    Exit;
  end;

  SetLength(LMaskedDB, LEMLen - HASH_SIZE - 1);
  Move(LEM[0], LMaskedDB[0], Length(LMaskedDB));
  SetLength(LH, HASH_SIZE);
  Move(LEM[Length(LMaskedDB)], LH[0], HASH_SIZE);

  LUnusedBits := 8 * LEMLen - LEMBits;
  if (LUnusedBits > 0) and ((LMaskedDB[0] and not ($FF shr LUnusedBits)) <> 0) then
  begin
    AError := 'RSA-PSS leftmost bits are not zero';
    Exit;
  end;

  LDBMask := MGF1_SHA256(LH, Length(LMaskedDB));
  SetLength(LDB, Length(LMaskedDB));
  for I := 0 to High(LDB) do
    LDB[I] := LMaskedDB[I] xor LDBMask[I];
  if LUnusedBits > 0 then
    LDB[0] := LDB[0] and ($FF shr LUnusedBits);

  LPSLen := Length(LDB) - SALT_SIZE - 1;
  if LPSLen < 0 then
  begin
    AError := 'RSA-PSS DB layout is invalid';
    Exit;
  end;
  for I := 0 to LPSLen - 1 do
    if LDB[I] <> 0 then
    begin
      AError := 'RSA-PSS PS padding is invalid';
      Exit;
    end;
  if LDB[LPSLen] <> 1 then
  begin
    AError := 'RSA-PSS salt separator is invalid';
    Exit;
  end;

  SetLength(LSalt, SALT_SIZE);
  Move(LDB[LPSLen + 1], LSalt[0], SALT_SIZE);

  LMHash := SHA256(AMessage);
  SetLength(LMPrime, 8 + HASH_SIZE + SALT_SIZE);
  FillChar(LMPrime[0], 8, 0);
  Move(LMHash[0], LMPrime[8], HASH_SIZE);
  Move(LSalt[0], LMPrime[8 + HASH_SIZE], SALT_SIZE);
  LHPrime := SHA256(LMPrime);

  if not BytesEqual(LH, LHPrime) then
  begin
    AError := 'RSA-PSS hash mismatch';
    Exit;
  end;

  Result := True;
end;

end.
