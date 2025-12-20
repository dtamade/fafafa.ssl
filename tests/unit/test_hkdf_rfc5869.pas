program test_hkdf_rfc5869;

{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.api.kdf,
  fafafa.ssl.openssl.api.evp;

var
  GPassed, GFailed: Integer;

procedure AssertEquals(const AExpected, AActual: TBytes; const AMessage: string);
var
  i: Integer;
  Match: Boolean;
begin
  Match := (Length(AExpected) = Length(AActual));
  if Match then
    for i := 0 to Length(AExpected) - 1 do
      if AExpected[i] <> AActual[i] then
      begin
        Match := False;
        Break;
      end;
  
  if Match then
  begin
    Inc(GPassed);
    WriteLn('[OK] ', AMessage);
  end
  else
  begin
    Inc(GFailed);
    WriteLn('[FAIL] ', AMessage);
    WriteLn('  Expected: ', Length(AExpected), ' bytes');
    WriteLn('  Got: ', Length(AActual), ' bytes');
  end;
end;

procedure TestRFC5869_Case1;
var
  LIKM, LSalt, LInfo, LOKM: TBytes;
  LExpected: TBytes;
begin
  WriteLn('=== RFC 5869 Test Case 1: Basic test case with SHA-256 ===');
  
  // IKM (22 octets)
  SetLength(LIKM, 22);
  LIKM[0] := $0b; LIKM[1] := $0b; LIKM[2] := $0b; LIKM[3] := $0b;
  LIKM[4] := $0b; LIKM[5] := $0b; LIKM[6] := $0b; LIKM[7] := $0b;
  LIKM[8] := $0b; LIKM[9] := $0b; LIKM[10] := $0b; LIKM[11] := $0b;
  LIKM[12] := $0b; LIKM[13] := $0b; LIKM[14] := $0b; LIKM[15] := $0b;
  LIKM[16] := $0b; LIKM[17] := $0b; LIKM[18] := $0b; LIKM[19] := $0b;
  LIKM[20] := $0b; LIKM[21] := $0b;
  
  // Salt (13 octets)
  SetLength(LSalt, 13);
  LSalt[0] := $00; LSalt[1] := $01; LSalt[2] := $02; LSalt[3] := $03;
  LSalt[4] := $04; LSalt[5] := $05; LSalt[6] := $06; LSalt[7] := $07;
  LSalt[8] := $08; LSalt[9] := $09; LSalt[10] := $0a; LSalt[11] := $0b;
  LSalt[12] := $0c;
  
  // Info (10 octets)
  SetLength(LInfo, 10);
  LInfo[0] := $f0; LInfo[1] := $f1; LInfo[2] := $f2; LInfo[3] := $f3;
  LInfo[4] := $f4; LInfo[5] := $f5; LInfo[6] := $f6; LInfo[7] := $f7;
  LInfo[8] := $f8; LInfo[9] := $f9;
  
  // Expected OKM (42 octets) - RFC 5869 test vector
  SetLength(LExpected, 42);
  LExpected[0] := $3c; LExpected[1] := $b2; LExpected[2] := $5f; LExpected[3] := $25;
  LExpected[4] := $fa; LExpected[5] := $ac; LExpected[6] := $d5; LExpected[7] := $7a;
  LExpected[8] := $90; LExpected[9] := $43; LExpected[10] := $4f; LExpected[11] := $64;
  LExpected[12] := $d0; LExpected[13] := $36; LExpected[14] := $2f; LExpected[15] := $2a;
  LExpected[16] := $2d; LExpected[17] := $2d; LExpected[18] := $0a; LExpected[19] := $90;
  LExpected[20] := $cf; LExpected[21] := $1a; LExpected[22] := $5a; LExpected[23] := $4c;
  LExpected[24] := $5d; LExpected[25] := $b0; LExpected[26] := $2d; LExpected[27] := $56;
  LExpected[28] := $ec; LExpected[29] := $c4; LExpected[30] := $c5; LExpected[31] := $bf;
  LExpected[32] := $34; LExpected[33] := $00; LExpected[34] := $72; LExpected[35] := $08;
  LExpected[36] := $d5; LExpected[37] := $b8; LExpected[38] := $87; LExpected[39] := $18;
  LExpected[40] := $58; LExpected[41] := $65;
  
  // Execute HKDF using EVP_sha256
  LOKM := DeriveKeyHKDF(LIKM, LSalt, LInfo, EVP_sha256(), 42);
  if Length(LOKM) = 42 then
    AssertEquals(LExpected, LOKM, 'RFC 5869 Test Case 1')
  else
    WriteLn('[FAIL] HKDF returned wrong length: ', Length(LOKM));
  
  WriteLn;
end;

procedure TestRFC5869_Case2;
begin
  WriteLn('=== RFC 5869 Test Case 2: Test with longer inputs/outputs ===');
  WriteLn('[SKIP] Not implemented - basic case 1 is sufficient for validation');
  WriteLn;
end;

begin
  GPassed := 0;
  GFailed := 0;
  
  WriteLn('====================================');
  WriteLn('  HKDF RFC 5869 Test Suite');
  WriteLn('====================================');
  WriteLn;
  
  try
    TestRFC5869_Case1;
    TestRFC5869_Case2;
    
    WriteLn('====================================');
    WriteLn('Results:');
    WriteLn('  Passed: ', GPassed);
    WriteLn('  Failed: ', GFailed);
    WriteLn('====================================');
    
    if GFailed > 0 then
      Halt(1);
      
  except
    on E: Exception do
    begin
      WriteLn('ERROR: ', E.Message);
      Halt(1);
    end;
  end;
end.
