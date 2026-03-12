program test_tls12_finished;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls12.finished;

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

procedure TestFinishedVerifyDataSHA256;
var
  LMasterSecret: TBytes;
  LTranscriptData: TBytes;
  LVerifyData: TBytes;
begin
  LMasterSecret := HexToBytes(
    '14fcc514aa33a42f06bbab62455ab6a1cd1929561563228f4c585ce599dc583c' +
    'f4cbb178f366ea8ff2c682de971f50a9'
  );
  LTranscriptData := HexToBytes(
    '4444444444444444444444444444444444444444444444444444444444444444'
  );

  LVerifyData := TLS12ComputeClientFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
  AssertBytesEqual(
    HexToBytes('fe897da2900f47a2691ff564'),
    LVerifyData,
    'TLS1.2 client Finished verify_data mismatch'
  );

  LVerifyData := TLS12ComputeServerFinishedVerifyData_SHA256(LMasterSecret, LTranscriptData);
  AssertBytesEqual(
    HexToBytes('633371bd2aa9feee76b5106b'),
    LVerifyData,
    'TLS1.2 server Finished verify_data mismatch'
  );
end;

begin
  WriteLn('Testing TLS 1.2 Finished helpers...');
  TestFinishedVerifyDataSHA256;
  WriteLn('✅ TLS 1.2 Finished checks passed');
end.
