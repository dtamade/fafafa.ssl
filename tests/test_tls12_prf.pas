program test_tls12_prf;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.tls12.prf;

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

procedure TestTLS12PRFSHA256;
var
  LSecret: TBytes;
  LSeed: TBytes;
  LOutput: TBytes;
begin
  LSecret := HexToBytes('9bbe436ba940f017b17652849a71db35');
  LSeed := HexToBytes('a0ba9f936cda311827a6f796ffd5198c');

  LOutput := TLS12PRF_SHA256(LSecret, 'test label', LSeed, 48);
  AssertBytesEqual(
    HexToBytes('e3f229ba727be17b8d122620557cd453c2aab21d07c3d495329b52d4e61edb5a6b301791e90d35c9c9a46b4e14baf9af'),
    LOutput,
    'TLS1.2 PRF-SHA256 output mismatch'
  );
end;

procedure TestTLS12PRFSHA384;
var
  LSecret: TBytes;
  LSeed: TBytes;
  LOutput: TBytes;
begin
  LSecret := HexToBytes('9bbe436ba940f017b17652849a71db35');
  LSeed := HexToBytes('a0ba9f936cda311827a6f796ffd5198c');

  LOutput := TLS12PRF_SHA384(LSecret, 'test label', LSeed, 48);
  AssertBytesEqual(
    HexToBytes('dd88775cd827187b67a3f7652b5c13f715791cc46e0274a6d3fb16651103defc544cd8afb68369a219bb918b8b21ddb1'),
    LOutput,
    'TLS1.2 PRF-SHA384 output mismatch'
  );
end;

procedure TestTLS12MasterSecretAndKeyBlockSHA256;
var
  LPreMasterSecret: TBytes;
  LClientRandom: TBytes;
  LServerRandom: TBytes;
  LMasterSecret: TBytes;
  LKeyBlock: TBytes;
begin
  LPreMasterSecret := HexToBytes(
    '0303' +
    '11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111'
  );
  LClientRandom := HexToBytes(
    '2222222222222222222222222222222222222222222222222222222222222222'
  );
  LServerRandom := HexToBytes(
    '3333333333333333333333333333333333333333333333333333333333333333'
  );

  LMasterSecret := TLS12MasterSecret_SHA256(LPreMasterSecret, LClientRandom, LServerRandom);
  AssertBytesEqual(
    HexToBytes('14fcc514aa33a42f06bbab62455ab6a1cd1929561563228f4c585ce599dc583cf4cbb178f366ea8ff2c682de971f50a9'),
    LMasterSecret,
    'TLS1.2 master secret mismatch'
  );

  LKeyBlock := TLS12KeyBlock_SHA256(LMasterSecret, LServerRandom, LClientRandom, 96);
  AssertBytesEqual(
    HexToBytes('6d62f76e3f7c6f0c789808772833a19af7c076878389689a8ba350238edd6fbbe0a4389a6a616940363ceb46c27e4982e90232d191ec422fa54bc82d367399e4d4f6bdb7915f60eb943a87bc3031bd80363221f9d2d4d25c8fdd6113c75ae195'),
    LKeyBlock,
    'TLS1.2 key block mismatch'
  );
end;

begin
  WriteLn('Testing TLS 1.2 PRF helpers...');

  TestTLS12PRFSHA256;
  TestTLS12PRFSHA384;
  TestTLS12MasterSecretAndKeyBlockSHA256;

  WriteLn('✅ TLS 1.2 PRF checks passed');
end.
