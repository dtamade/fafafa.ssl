program test_tls13_aead;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls13.wire,
  fafafa.ssl.tls13.aead;

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

procedure TestChaChaSuiteRoundtrip;
var
  LKey, LNonce, LAAD, LPlain: TBytes;
  LEncrypted: TBytes;
  LRecovered: TBytes;
  LError: string;
begin
  LKey := HexToBytes('1c9240a5eb55d38af333888604f6b5f0473917c1402b80099dca5cbc207075c0');
  LNonce := HexToBytes('000000000102030405060708');
  LAAD := HexToBytes('f33388860000000000004e91');
  LPlain := HexToBytes('000102030405060708090a0b0c0d0e0f10111213');

  AssertTrue(
    TryTLS13AEADEncrypt(
      TLS13_CIPHER_CHACHA20_POLY1305_SHA256,
      LKey,
      LNonce,
      LAAD,
      LPlain,
      LEncrypted,
      LError
    ),
    'TLS13 AEAD encrypt should succeed for CHACHA suite'
  );

  AssertTrue(
    TryTLS13AEADDecrypt(
      TLS13_CIPHER_CHACHA20_POLY1305_SHA256,
      LKey,
      LNonce,
      LAAD,
      LEncrypted,
      LRecovered,
      LError
    ),
    'TLS13 AEAD decrypt should succeed for CHACHA suite'
  );

  AssertBytesEqual(LPlain, LRecovered, 'Recovered plaintext mismatch');
end;

procedure TestUnsupportedSuite;
var
  LKey, LNonce, LAAD, LPlain, LEncrypted: TBytes;
  LError: string;
begin
  SetLength(LKey, 16);
  SetLength(LNonce, 12);
  SetLength(LAAD, 0);
  SetLength(LPlain, 1);
  LPlain[0] := 0;

  AssertTrue(
    not TryTLS13AEADEncrypt(
      TLS13_CIPHER_AES_128_GCM_SHA256,
      LKey,
      LNonce,
      LAAD,
      LPlain,
      LEncrypted,
      LError
    ),
    'AES suite should be unsupported in pure FreePascal AEAD for now'
  );
  AssertTrue(Pos('unsupported', LowerCase(LError)) > 0, 'Unsupported error message expected');
end;

begin
  WriteLn('Testing TLS 1.3 AEAD dispatch...');

  TestChaChaSuiteRoundtrip;
  TestUnsupportedSuite;

  WriteLn('✅ TLS 1.3 AEAD dispatch checks passed');
end.
