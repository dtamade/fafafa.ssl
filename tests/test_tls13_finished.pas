program test_tls13_finished;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls13.finished;

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

procedure TestFinishedVector;
var
  LServerTrafficSecret: TBytes;
  LTranscriptHash: TBytes;
  LFinishedKey: TBytes;
  LVerifyData: TBytes;
begin
  LServerTrafficSecret := HexToBytes('7adbfeda325088ba2201c0175d8ea186e4d5408e3b6bd2dcb3d61f471cbf3b61');
  LTranscriptHash := HexToBytes('fc4394acdb9d481a21b9614b831016b5b5e656e5e237bc8ed8eb0eb540c2c8aa');

  LFinishedKey := TLS13FinishedKeySHA256(LServerTrafficSecret);
  AssertBytesEqual(
    HexToBytes('94275d4e6ccb8fdeae0515373eb705ce28bab307e0780fba65d22c2c8991d527'),
    LFinishedKey,
    'Finished key mismatch'
  );

  LVerifyData := TLS13ComputeFinishedVerifyDataFromTrafficSecretSHA256(LServerTrafficSecret, LTranscriptHash);
  AssertBytesEqual(
    HexToBytes('8987cbe20f79f174b8aaf0ab6d399252c30b3285eb1c71ba1b5eb6ae19f67b74'),
    LVerifyData,
    'Finished verify_data mismatch'
  );

  AssertTrue(
    TLS13VerifyFinishedSHA256(LServerTrafficSecret, LTranscriptHash, LVerifyData),
    'Finished verify should succeed'
  );

  LVerifyData[0] := LVerifyData[0] xor $01;
  AssertTrue(
    not TLS13VerifyFinishedSHA256(LServerTrafficSecret, LTranscriptHash, LVerifyData),
    'Finished verify should fail when verify_data is modified'
  );
end;

procedure TestFinishedVectorSHA384;
var
  LServerTrafficSecret: TBytes;
  LTranscriptHash: TBytes;
  LFinishedKey: TBytes;
  LVerifyData: TBytes;
begin
  LServerTrafficSecret := HexToBytes(
    '000102030405060708090a0b0c0d0e0f' +
    '101112131415161718191a1b1c1d1e1f' +
    '202122232425262728292a2b2c2d2e2f'
  );
  LTranscriptHash := HexToBytes(
    '808182838485868788898a8b8c8d8e8f' +
    '909192939495969798999a9b9c9d9e9f' +
    'a0a1a2a3a4a5a6a7a8a9aaabacadaeaf'
  );

  LFinishedKey := TLS13FinishedKeySHA384(LServerTrafficSecret);
  AssertBytesEqual(
    HexToBytes(
      'fcbe325d88fe0a23ac276c591cdbfe90' +
      '895612d7c0cbcdb21e3d1ffc20d96ed8' +
      '148a1610d115f29b6771bccdf7a29fe2'
    ),
    LFinishedKey,
    'SHA384 finished key mismatch'
  );

  LVerifyData := TLS13ComputeFinishedVerifyDataFromTrafficSecretSHA384(
    LServerTrafficSecret,
    LTranscriptHash
  );
  AssertBytesEqual(
    HexToBytes(
      'd72bc9a44b4c8b6200ee19a1703f28f1' +
      '676f100ce3f0527d648e8c31a7c38dea' +
      '1ce41d81b56e3643876466630575f6bc'
    ),
    LVerifyData,
    'SHA384 finished verify_data mismatch'
  );

  AssertTrue(
    TLS13VerifyFinishedSHA384(LServerTrafficSecret, LTranscriptHash, LVerifyData),
    'SHA384 finished verify should succeed'
  );

  LVerifyData[0] := LVerifyData[0] xor $01;
  AssertTrue(
    not TLS13VerifyFinishedSHA384(LServerTrafficSecret, LTranscriptHash, LVerifyData),
    'SHA384 finished verify should fail when verify_data is modified'
  );
end;

begin
  WriteLn('Testing TLS 1.3 finished verification helpers...');

  TestFinishedVector;
  TestFinishedVectorSHA384;

  WriteLn('✅ TLS 1.3 finished checks passed');
end.
