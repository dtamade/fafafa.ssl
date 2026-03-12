program test_tls12_recordcrypto_aes256_gcm;

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

procedure TestAES256GCMRoundTrip;
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
    '22222222222222222222222222222222'
  );
  LWriteIV := HexToBytes('a1a2a3a4');
  LPlaintext := HexToBytes('1400000c112233445566778899aabbcc');

  AssertTrue(
    TryEncryptTLS12AES256GCMRecord(
      LKey,
      LWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LPlaintext,
      LEncrypted,
      LError
    ),
    'TLS1.2 AES256-GCM encrypt should succeed: ' + LError
  );

  AssertTrue(
    TryDecryptTLS12AES256GCMRecord(
      LKey,
      LWriteIV,
      0,
      TLS_CONTENT_TYPE_HANDSHAKE,
      TLS12_VERSION,
      LEncrypted,
      LDecrypted,
      LError
    ),
    'TLS1.2 AES256-GCM decrypt should succeed: ' + LError
  );
  AssertBytesEqual(LPlaintext, LDecrypted, 'TLS1.2 AES256-GCM roundtrip plaintext mismatch');
end;

begin
  WriteLn('Testing TLS 1.2 AES256-GCM record crypto helpers...');
  TestAES256GCMRoundTrip;
  WriteLn('✅ TLS 1.2 AES256-GCM record crypto checks passed');
end.
