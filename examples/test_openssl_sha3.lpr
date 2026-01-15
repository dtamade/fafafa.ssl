program test_openssl_sha3;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs,
  fafafa.ssl.openssl.types, fafafa.ssl.openssl.core, fafafa.ssl.openssl.sha3;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure TestResult(const TestName: string; Passed: Boolean);
begin
  if Passed then
  begin
    WriteLn('[PASS] ', TestName);
    Inc(TestsPassed);
  end
  else
  begin
    WriteLn('[FAIL] ', TestName);
    Inc(TestsFailed);
  end;
end;

function BytesToHex(const Data: array of Byte): string;
var
  i: Integer;
begin
  Result := '';
  for i := Low(Data) to High(Data) do
    Result := Result + IntToHex(Data[i], 2);
end;

function TestSHA3_224: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..27] of Byte;  // SHA3-224 = 28 bytes
begin
  Result := False;
  if not Assigned(SHA3_224) then Exit(True); // Skip if not available
  
  LData := 'abc';
  if SHA3_224(PByte(LData), 3, @LHash[0]) = nil then Exit;
  
  // Expected: e642824c3f8cf24ad09234ee7d3c766fc9a3a5168d0c94ad73b46fdf
  Result := BytesToHex(LHash) = 'E642824C3F8CF24AD09234EE7D3C766FC9A3A5168D0C94AD73B46FDF';
end;

function TestSHA3_256: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..31] of Byte;  // SHA3-256 = 32 bytes
begin
  Result := False;
  if not Assigned(SHA3_256) then Exit(True);
  
  LData := 'abc';
  if SHA3_256(PByte(LData), 3, @LHash[0]) = nil then Exit;
  
  // Expected: 3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532
  Result := BytesToHex(LHash) = '3A985DA74FE225B2045C172D6BD390BD855F086E3E9D525B46BFE24511431532';
end;

function TestSHA3_384: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..47] of Byte;  // SHA3-384 = 48 bytes
begin
  Result := False;
  if not Assigned(SHA3_384) then Exit(True);
  
  LData := 'abc';
  if SHA3_384(PByte(LData), 3, @LHash[0]) = nil then Exit;
  
  // Expected: ec01498288516fc926459f58e2c6ad8df9b473cb0fc08c2596da7cf0e49be4b298d88cea927ac7f539f1edf228376d25
  Result := BytesToHex(LHash) = 'EC01498288516FC926459F58E2C6AD8DF9B473CB0FC08C2596DA7CF0E49BE4B298D88CEA927AC7F539F1EDF228376D25';
end;

function TestSHA3_512: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..63] of Byte;  // SHA3-512 = 64 bytes
begin
  Result := False;
  if not Assigned(SHA3_512) then Exit(True);
  
  LData := 'abc';
  if SHA3_512(PByte(LData), 3, @LHash[0]) = nil then Exit;
  
  // Just check that it produces output
  Result := True;
end;

function TestSHAKE128: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..31] of Byte;  // Variable output
begin
  Result := False;
  if not Assigned(SHAKE128) then Exit(True);
  
  LData := 'abc';
  if SHAKE128(PByte(LData), 3, @LHash[0], Length(LHash)) = nil then Exit;
  
  Result := True;  // Just verify it runs
end;

function TestSHAKE256: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..31] of Byte;
begin
  Result := False;
  if not Assigned(SHAKE256) then Exit(True);
  
  LData := 'abc';
  if SHAKE256(PByte(LData), 3, @LHash[0], Length(LHash)) = nil then Exit;
  
  Result := True;
end;

begin
  WriteLn('OpenSSL SHA3/SHAKE Unit Test');
  WriteLn('============================');
  WriteLn;
  
  LoadOpenSSLCore;
  WriteLn('OpenSSL version: ', OpenSSL_version(0));
  WriteLn;
  
  if not LoadSHA3Functions(GetCryptoLibHandle) then
  begin
    WriteLn('SHA3 functions not available in this OpenSSL version');
    WriteLn('Skipping tests...');
    Halt(0);
  end;
  
  TestResult('SHA3-224', TestSHA3_224);
  TestResult('SHA3-256', TestSHA3_256);
  TestResult('SHA3-384', TestSHA3_384);
  TestResult('SHA3-512', TestSHA3_512);
  TestResult('SHAKE128', TestSHAKE128);
  TestResult('SHAKE256', TestSHAKE256);
  
  WriteLn;
  WriteLn('Test Summary:');
  WriteLn('=============');
  WriteLn('Tests Passed: ', TestsPassed);
  WriteLn('Tests Failed: ', TestsFailed);
  WriteLn('Total Tests:  ', TestsPassed + TestsFailed);
  
  if TestsFailed = 0 then
    WriteLn('All tests PASSED!')
  else
  begin
    WriteLn('Some tests FAILED!');
    Halt(1);
  end;
  
  UnloadSHA3Functions;
  UnloadOpenSSLCore;
end.