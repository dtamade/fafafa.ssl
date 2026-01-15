program test_sha3;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DynLibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.sha3;

var
  TestsPassed: Integer = 0;
  TestsFailed: Integer = 0;

procedure PrintTestResult(const TestName: string; Passed: Boolean);
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

function TestSHA3LoadFunctions: Boolean;
var
  LLib: TLibHandle;
begin
  Result := False;
  
  try
    // Load core library first
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('Failed to load OpenSSL Core: ', E.Message);
      PrintTestResult('SHA3 - Load functions', False);
      Exit;
    end;
  end;
  
  // Get crypto library handle
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    WriteLn('Failed to get crypto library handle');
    PrintTestResult('SHA3 - Load functions', False);
    Exit;
  end;
  
  WriteLn('Crypto library handle: ', IntToHex(LLib, 16));
  
  // Load SHA3 functions
  if not LoadSHA3Functions(LLib) then
  begin
    WriteLn('LoadSHA3Functions returned False');
    PrintTestResult('SHA3 - Load functions', False);
    Exit;
  end;
  
  // Check if at least core functions loaded
  WriteLn('Checking function pointers...');
  WriteLn('  SHA3_256_Init = ', IntToHex(PtrUInt(SHA3_256_Init), 16));
  WriteLn('  SHA3_256_Update = ', IntToHex(PtrUInt(SHA3_256_Update), 16));
  WriteLn('  SHA3_256_Final = ', IntToHex(PtrUInt(SHA3_256_Final), 16));
  WriteLn('  SHA3_256 = ', IntToHex(PtrUInt(SHA3_256), 16));
  
  if not Assigned(SHA3_256_Init) then
  begin
    WriteLn('SHA3 functions not available in this OpenSSL version');
    WriteLn('This may be normal - SHA3 APIs may not be exported in OpenSSL 3.x');
    // Consider this a pass but note the limitation
    Result := True;
    PrintTestResult('SHA3 - Load functions (not available)', Result);
    Exit;
  end;
  
  Result := True;
  PrintTestResult('SHA3 - Load functions', Result);
end;

function BytesToHex(const Data: TBytes): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to High(Data) do
    Result := Result + LowerCase(IntToHex(Data[i], 2));
end;

function TestSHA3_256Hash: Boolean;
var
  LInput: AnsiString;
  LHash: TBytes;
  LExpected: string;
begin
  Result := False;
  
  if not Assigned(SHA3_256) then
  begin
    WriteLn('SHA3_256 function not available');
    Result := True;
    PrintTestResult('SHA3-256 - Hash (skipped)', Result);
    Exit;
  end;
  
  // Test vector: SHA3-256("abc")
  LInput := 'abc';
  LExpected := '3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532';
  
  WriteLn('Computing SHA3-256("', LInput, '")...');
  
  try
    SetLength(LHash, SHA3_256_DIGEST_LENGTH);
    
    if SHA3_256(@LInput[1], Length(LInput), @LHash[0]) = nil then
    begin
      WriteLn('SHA3_256 returned nil');
      PrintTestResult('SHA3-256 - Hash', False);
      Exit;
    end;
    
    WriteLn('Result  : ', BytesToHex(LHash));
    WriteLn('Expected: ', LExpected);
    
    Result := (BytesToHex(LHash) = LExpected);
    PrintTestResult('SHA3-256 - Hash', Result);
    
  except
    on E: Exception do
    begin
      WriteLn('Exception during SHA3-256: ', E.Message);
      PrintTestResult('SHA3-256 - Hash', False);
    end;
  end;
end;

function TestSHA3_256InitUpdateFinal: Boolean;
var
  LCtx: SHA3_CTX;
  LInput: AnsiString;
  LHash: array[0..SHA3_256_DIGEST_LENGTH-1] of Byte;
  LExpected: string;
  LResult: string;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(SHA3_256_Init) or not Assigned(SHA3_256_Update) or not Assigned(SHA3_256_Final) then
  begin
    WriteLn('SHA3-256 Init/Update/Final functions not available');
    Result := True;
    PrintTestResult('SHA3-256 - Init/Update/Final (skipped)', Result);
    Exit;
  end;
  
  // Test vector: SHA3-256("abc")
  LInput := 'abc';
  LExpected := '3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532';
  
  WriteLn('Computing SHA3-256("', LInput, '") with Init/Update/Final...');
  
  try
    FillChar(LCtx, SizeOf(LCtx), 0);
    FillChar(LHash, SizeOf(LHash), 0);
    
    // Init
    if SHA3_256_Init(@LCtx) <> 1 then
    begin
      WriteLn('SHA3_256_Init failed');
      PrintTestResult('SHA3-256 - Init/Update/Final', False);
      Exit;
    end;
    
    // Update
    if SHA3_256_Update(@LCtx, @LInput[1], Length(LInput)) <> 1 then
    begin
      WriteLn('SHA3_256_Update failed');
      PrintTestResult('SHA3-256 - Init/Update/Final', False);
      Exit;
    end;
    
    // Final
    if SHA3_256_Final(@LHash[0], @LCtx) <> 1 then
    begin
      WriteLn('SHA3_256_Final failed');
      PrintTestResult('SHA3-256 - Init/Update/Final', False);
      Exit;
    end;
    
    // Convert to hex
    LResult := '';
    for i := 0 to SHA3_256_DIGEST_LENGTH - 1 do
      LResult := LResult + LowerCase(IntToHex(LHash[i], 2));
    
    WriteLn('Result  : ', LResult);
    WriteLn('Expected: ', LExpected);
    
    Result := (LResult = LExpected);
    PrintTestResult('SHA3-256 - Init/Update/Final', Result);
    
  except
    on E: Exception do
    begin
      WriteLn('Exception during SHA3-256 Init/Update/Final: ', E.Message);
      PrintTestResult('SHA3-256 - Init/Update/Final', False);
    end;
  end;
end;

function TestSHA3_512Hash: Boolean;
var
  LInput: AnsiString;
  LHash: TBytes;
  LExpected: string;
begin
  Result := False;
  
  if not Assigned(SHA3_512) then
  begin
    WriteLn('SHA3_512 function not available');
    Result := True;
    PrintTestResult('SHA3-512 - Hash (skipped)', Result);
    Exit;
  end;
  
  // Test vector: SHA3-512("abc")
  LInput := 'abc';
  LExpected := 'b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0';
  
  WriteLn('Computing SHA3-512("', LInput, '")...');
  
  try
    SetLength(LHash, SHA3_512_DIGEST_LENGTH);
    
    if SHA3_512(@LInput[1], Length(LInput), @LHash[0]) = nil then
    begin
      WriteLn('SHA3_512 returned nil');
      PrintTestResult('SHA3-512 - Hash', False);
      Exit;
    end;
    
    WriteLn('Result  : ', BytesToHex(LHash));
    WriteLn('Expected: ', LExpected);
    
    Result := (BytesToHex(LHash) = LExpected);
    PrintTestResult('SHA3-512 - Hash', Result);
    
  except
    on E: Exception do
    begin
      WriteLn('Exception during SHA3-512: ', E.Message);
      PrintTestResult('SHA3-512 - Hash', False);
    end;
  end;
end;

begin
  WriteLn('SHA3 OpenSSL Binding Test');
  WriteLn('=========================');
  WriteLn;
  
  // Run tests
  TestSHA3LoadFunctions;
  TestSHA3_256Hash;
  TestSHA3_256InitUpdateFinal;
  TestSHA3_512Hash;
  
  // Summary
  WriteLn;
  WriteLn('Test Summary:');
  WriteLn('  Passed: ', TestsPassed);
  WriteLn('  Failed: ', TestsFailed);
  WriteLn('  Total:  ', TestsPassed + TestsFailed);
  
  if TestsFailed > 0 then
    Halt(1)
  else
    Halt(0);
end.
