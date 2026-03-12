program test_tls12_recordcrypto;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls12.wire,
  fafafa.ssl.tls12.recordcrypto;

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

procedure AssertEqualsByte(AExpected, AActual: Byte; const AMessage: string);
begin
  if AExpected <> AActual then
    Fail(Format('%s (expected=0x%.2x actual=0x%.2x)', [AMessage, AExpected, AActual]));
end;

function HexNibble(AChar: Char): Byte;
begin
  case AChar of
    '0'..'9': Result := Ord(AChar) - Ord('0');
    'a'..'f': Result := 10 + Ord(AChar) - Ord('a');
    'A'..'F': Result := 10 + Ord(AChar) - Ord('A');
  else
    Fail('Invalid hex character: ' + AChar);
    Result := 0;
  end;
end;

function HexToBytes(const AHex: string): TBytes;
var
  I, LLen: Integer;
begin
  Result := nil;
  LLen := Length(AHex);
  if (LLen = 0) or ((LLen and 1) <> 0) then
    Fail('Invalid hex length');
  SetLength(Result, LLen div 2);
  for I := 0 to High(Result) do
    Result[I] := (HexNibble(AHex[2 * I + 1]) shl 4) or HexNibble(AHex[2 * I + 2]);
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

procedure TestNonceAndAAD;
var
  LWriteIV: TBytes;
  LNonce: TBytes;
  LAAD: TBytes;
begin
  LWriteIV := HexToBytes('a1a2a3a4a5a6a7a8a9aaabac');

  LNonce := BuildTLS12ChaCha20Poly1305Nonce(LWriteIV, $0102030405060708);
  AssertBytesEqual(
    HexToBytes('a1a2a3a4a4a4a4acacacaca4'),
    LNonce,
    'TLS1.2 ChaCha nonce mismatch'
  );

  LAAD := BuildTLS12AdditionalData($0102030405060708, TLS_CONTENT_TYPE_HANDSHAKE, TLS12_VERSION, 13);
  AssertBytesEqual(
    HexToBytes('0102030405060708160303000d'),
    LAAD,
    'TLS1.2 AEAD additional data mismatch'
  );
end;

procedure TestChaCha20Poly1305RoundTrip;
var
  LKey: TBytes;
  LWriteIV: TBytes;
  LPlaintext: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  LError: string;
begin
  LKey := HexToBytes(
    '11111111111111111111111111111111' +
    '11111111111111111111111111111111'
  );
  LWriteIV := HexToBytes('a1a2a3a4a5a6a7a8a9aaabac');
  LPlaintext := HexToBytes('1400000cfe897da2900f47a2691ff564');

  AssertTrue(
    TryEncryptTLS12ChaCha20Poly1305Record(
      LKey,
      LWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LPlaintext,
      LEncrypted,
      LError
    ),
    'TLS1.2 ChaCha encrypt should succeed: ' + LError
  );
  AssertTrue(Length(LEncrypted) = Length(LPlaintext) + 16,
    'TLS1.2 ChaCha encrypted payload length mismatch');
  AssertTrue(not BytesEqual(LEncrypted, LPlaintext),
    'TLS1.2 ChaCha encrypted payload should differ from plaintext');

  AssertTrue(
    TryDecryptTLS12ChaCha20Poly1305Record(
      LKey,
      LWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LEncrypted,
      LDecrypted,
      LError
    ),
    'TLS1.2 ChaCha decrypt should succeed: ' + LError
  );
  AssertBytesEqual(LPlaintext, LDecrypted, 'TLS1.2 ChaCha roundtrip plaintext mismatch');

  LEncrypted[High(LEncrypted)] := LEncrypted[High(LEncrypted)] xor $01;
  AssertTrue(
    not TryDecryptTLS12ChaCha20Poly1305Record(
      LKey,
      LWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LEncrypted,
      LDecrypted,
      LError
    ),
    'TLS1.2 ChaCha decrypt should reject modified ciphertext/tag'
  );
end;

begin
  WriteLn('Testing TLS 1.2 record crypto helpers...');
  TestNonceAndAAD;
  TestChaCha20Poly1305RoundTrip;
  WriteLn('✅ TLS 1.2 record crypto checks passed');
end.
