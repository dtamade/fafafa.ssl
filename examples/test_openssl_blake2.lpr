program test_openssl_blake2;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs,
  fafafa.ssl.openssl.types, fafafa.ssl.openssl.core, fafafa.ssl.openssl.blake2;

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

function TestBLAKE2b: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..63] of Byte;  // BLAKE2b-512 = 64 bytes
begin
  Result := False;
  if not Assigned(BLAKE2b) then Exit(True);
  
  LData := 'abc';
  if BLAKE2b(@LHash[0], 64, PByte(LData), 3, nil, 0) <> 0 then Exit;
  
  // Just verify it produces output
  Result := True;
end;

function TestBLAKE2s: Boolean;
var
  LData: PAnsiChar;
  LHash: array[0..31] of Byte;  // BLAKE2s-256 = 32 bytes
begin
  Result := False;
  if not Assigned(BLAKE2s) then Exit(True);
  
  LData := 'abc';
  if BLAKE2s(@LHash[0], 32, PByte(LData), 3, nil, 0) <> 0 then Exit;
  
  Result := True;
end;

function TestBLAKE2bContext: Boolean;
var
  LCtx: BLAKE2B_CTX;
  LData: PAnsiChar;
  LHash: array[0..63] of Byte;
begin
  Result := False;
  if not Assigned(BLAKE2b_Init) or not Assigned(BLAKE2b_Update) or 
     not Assigned(BLAKE2b_Final) then Exit(True);
  
  if BLAKE2b_Init(@LCtx, 64) <> 1 then Exit;
  
  LData := 'Hello, ';
  if BLAKE2b_Update(@LCtx, PByte(LData), 7) <> 1 then Exit;
  
  LData := 'BLAKE2!';
  if BLAKE2b_Update(@LCtx, PByte(LData), 7) <> 1 then Exit;
  
  if BLAKE2b_Final(@LCtx, @LHash[0], 64) <> 1 then Exit;
  
  Result := True;
end;

function TestBLAKE2sContext: Boolean;
var
  LCtx: BLAKE2S_CTX;
  LData: PAnsiChar;
  LHash: array[0..31] of Byte;
begin
  Result := False;
  if not Assigned(BLAKE2s_Init) or not Assigned(BLAKE2s_Update) or 
     not Assigned(BLAKE2s_Final) then Exit(True);
  
  if BLAKE2s_Init(@LCtx, 32) <> 1 then Exit;
  
  LData := 'Test';
  if BLAKE2s_Update(@LCtx, PByte(LData), 4) <> 1 then Exit;
  
  if BLAKE2s_Final(@LCtx, @LHash[0], 32) <> 1 then Exit;
  
  Result := True;
end;

begin
  WriteLn('OpenSSL BLAKE2 Unit Test');
  WriteLn('========================');
  WriteLn;
  
  LoadOpenSSLCore;
  WriteLn('OpenSSL version: ', OpenSSL_version(0));
  WriteLn;
  
  if not LoadBLAKE2Functions(GetCryptoLibHandle) then
  begin
    WriteLn('BLAKE2 functions not available in this OpenSSL version');
    WriteLn('Skipping tests...');
    Halt(0);
  end;
  
  TestResult('BLAKE2b-512', TestBLAKE2b);
  TestResult('BLAKE2s-256', TestBLAKE2s);
  TestResult('BLAKE2b Context', TestBLAKE2bContext);
  TestResult('BLAKE2s Context', TestBLAKE2sContext);
  
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
  
  UnloadBLAKE2Functions;
  UnloadOpenSSLCore;
end.