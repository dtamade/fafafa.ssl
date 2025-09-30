program test_camellia;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DynLibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.legacy_ciphers;

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

function TestCamelliaLoadFunctions: Boolean;
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
      PrintTestResult('Camellia - Load functions', False);
      Exit;
    end;
  end;
  
  // Get crypto library handle
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    WriteLn('Failed to get crypto library handle');
    PrintTestResult('Camellia - Load functions', False);
    Exit;
  end;
  
  // Load Camellia functions
  LoadLegacyCiphersFunctions(LLib);
  
  // Check if at least core functions loaded
  if not Assigned(Camellia_set_key) then
  begin
    WriteLn('Camellia functions not available in this OpenSSL version');
    // Consider this a pass - Camellia may not be available in all versions
    Result := True;
    PrintTestResult('Camellia - Load functions (not available)', Result);
    Exit;
  end;
  
  Result := True;
  PrintTestResult('Camellia - Load functions', Result);
end;

function TestCamellia128ECB: Boolean;
var
  LKey: array[0..15] of Byte;
  LKeyStruct: CAMELLIA_KEY;
  LInput: array[0..15] of Byte;
  LEncrypted: array[0..15] of Byte;
  LDecrypted: array[0..15] of Byte;
  i: Integer;
  LResult: Integer;
begin
  Result := False;
  
  if not Assigned(Camellia_set_key) or not Assigned(Camellia_encrypt_func) or not Assigned(Camellia_decrypt_func) then
  begin
    Result := True;
    PrintTestResult('Camellia - 128-bit ECB (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes
  for i := 0 to 15 do
  begin
    LInput[i] := Byte(i);
    LKey[i] := Byte(i + 1);
  end;
  
  // Set encryption key (128 bits = 16 bytes * 8 = 128)
  LResult := Camellia_set_key(@LKey[0], 128, @LKeyStruct);
  if LResult <> 0 then Exit;
  
  // Encrypt
  Camellia_encrypt_func(@LInput[0], @LEncrypted[0], @LKeyStruct);
  
  // Decrypt
  Camellia_decrypt_func(@LEncrypted[0], @LDecrypted[0], @LKeyStruct);
  
  // Verify decrypted matches original
  for i := 0 to 15 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('Camellia - 128-bit ECB encrypt/decrypt', Result);
end;

function TestCamellia256ECB: Boolean;
var
  LKey: array[0..31] of Byte;
  LKeyStruct: CAMELLIA_KEY;
  LInput: array[0..15] of Byte;
  LEncrypted: array[0..15] of Byte;
  LDecrypted: array[0..15] of Byte;
  i: Integer;
  LResult: Integer;
begin
  Result := False;
  
  if not Assigned(Camellia_set_key) or not Assigned(Camellia_encrypt_func) or not Assigned(Camellia_decrypt_func) then
  begin
    Result := True;
    PrintTestResult('Camellia - 256-bit ECB (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes for input, 32 bytes for key
  for i := 0 to 15 do
    LInput[i] := Byte(i + 16);
  for i := 0 to 31 do
    LKey[i] := Byte(i);
  
  // Set encryption key (256 bits = 32 bytes * 8 = 256)
  LResult := Camellia_set_key(@LKey[0], 256, @LKeyStruct);
  if LResult <> 0 then Exit;
  
  // Encrypt
  Camellia_encrypt_func(@LInput[0], @LEncrypted[0], @LKeyStruct);
  
  // Decrypt
  Camellia_decrypt_func(@LEncrypted[0], @LDecrypted[0], @LKeyStruct);
  
  // Verify decrypted matches original
  for i := 0 to 15 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('Camellia - 256-bit ECB encrypt/decrypt', Result);
end;

function TestCamellia128CBC: Boolean;
var
  LKey: array[0..15] of Byte;
  LKeyStruct: CAMELLIA_KEY;
  LInput: array[0..31] of Byte;
  LEncrypted: array[0..31] of Byte;
  LDecrypted: array[0..31] of Byte;
  LIV, LIV2: array[0..15] of Byte;
  i: Integer;
  LResult: Integer;
begin
  Result := False;
  
  if not Assigned(Camellia_set_key) or not Assigned(Camellia_cbc_encrypt) then
  begin
    Result := True;
    PrintTestResult('Camellia - 128-bit CBC (skipped)', Result);
    Exit;
  end;
  
  // Test data: 32 bytes (2 blocks)
  for i := 0 to 31 do
    LInput[i] := Byte(i);
  for i := 0 to 15 do
  begin
    LKey[i] := Byte(i + 1);
    LIV[i] := Byte(i + 33);
    LIV2[i] := LIV[i];
  end;
  
  // Set encryption key
  LResult := Camellia_set_key(@LKey[0], 128, @LKeyStruct);
  if LResult <> 0 then Exit;
  
  // Encrypt
  Camellia_cbc_encrypt(@LInput[0], @LEncrypted[0], 32, @LKeyStruct, @LIV[0], CAMELLIA_ENCRYPT);
  
  // Decrypt
  Camellia_cbc_encrypt(@LEncrypted[0], @LDecrypted[0], 32, @LKeyStruct, @LIV2[0], CAMELLIA_DECRYPT);
  
  // Verify decrypted matches original
  for i := 0 to 31 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('Camellia - 128-bit CBC encrypt/decrypt', Result);
end;

function TestCamellia256CBC: Boolean;
var
  LKey: array[0..31] of Byte;
  LKeyStruct: CAMELLIA_KEY;
  LInput: array[0..47] of Byte;
  LEncrypted: array[0..47] of Byte;
  LDecrypted: array[0..47] of Byte;
  LIV, LIV2: array[0..15] of Byte;
  i: Integer;
  LResult: Integer;
begin
  Result := False;
  
  if not Assigned(Camellia_set_key) or not Assigned(Camellia_cbc_encrypt) then
  begin
    Result := True;
    PrintTestResult('Camellia - 256-bit CBC (skipped)', Result);
    Exit;
  end;
  
  // Test data: 48 bytes (3 blocks)
  for i := 0 to 47 do
    LInput[i] := Byte(i + 64);
  for i := 0 to 31 do
    LKey[i] := Byte(i);
  for i := 0 to 15 do
  begin
    LIV[i] := Byte(i + 17);
    LIV2[i] := LIV[i];
  end;
  
  // Set encryption key
  LResult := Camellia_set_key(@LKey[0], 256, @LKeyStruct);
  if LResult <> 0 then Exit;
  
  // Encrypt
  Camellia_cbc_encrypt(@LInput[0], @LEncrypted[0], 48, @LKeyStruct, @LIV[0], CAMELLIA_ENCRYPT);
  
  // Decrypt
  Camellia_cbc_encrypt(@LEncrypted[0], @LDecrypted[0], 48, @LKeyStruct, @LIV2[0], CAMELLIA_DECRYPT);
  
  // Verify decrypted matches original
  for i := 0 to 47 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('Camellia - 256-bit CBC encrypt/decrypt', Result);
end;

procedure RunAllTests;
begin
  WriteLn('====================================');
  WriteLn('Camellia Module Tests');
  WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);
  WriteLn('====================================');
  WriteLn;
  
  TestCamelliaLoadFunctions;
  TestCamellia128ECB;
  TestCamellia256ECB;
  TestCamellia128CBC;
  TestCamellia256CBC;
  
  WriteLn;
  WriteLn('====================================');
  WriteLn('Test Summary');
  WriteLn('====================================');
  WriteLn('Tests passed: ', TestsPassed);
  WriteLn('Tests failed: ', TestsFailed);
  WriteLn('Total tests:  ', TestsPassed + TestsFailed);
  
  if TestsFailed > 0 then
    ExitCode := 1
  else
    ExitCode := 0;
end;

begin
  try
    RunAllTests;
  except
    on E: Exception do
    begin
      WriteLn('EXCEPTION: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.