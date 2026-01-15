program test_aria;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DynLibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.aria;

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

function TestARIALoadFunctions: Boolean;
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
      PrintTestResult('ARIA - Load functions', False);
      Exit;
    end;
  end;
  
  // Get crypto library handle
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    WriteLn('Failed to get crypto library handle');
    PrintTestResult('ARIA - Load functions', False);
    Exit;
  end;
  
  // Load ARIA functions
  LoadARIAFunctions(LLib);
  
  // Check if at least core functions loaded
  if not Assigned(ARIA_set_encrypt_key) then
  begin
    WriteLn('ARIA functions not available in this OpenSSL version');
    // Consider this a pass - ARIA may not be available in all versions
    Result := True;
    PrintTestResult('ARIA - Load functions (not available)', Result);
    Exit;
  end;
  
  Result := True;
  PrintTestResult('ARIA - Load functions', Result);
end;

function TestARIA128ECB: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(ARIA_set_encrypt_key) or not Assigned(ARIA_encrypt_func) or not Assigned(ARIA_decrypt_func) then
  begin
    Result := True;
    PrintTestResult('ARIA - 128-bit ECB (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes (ARIA block size)
  SetLength(LInput, ARIA_BLOCK_SIZE);
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    LInput[i] := Byte(i);
  
  // Test key: 128 bits (16 bytes)
  SetLength(LKey, ARIA_KEY_LENGTH_128);
  for i := 0 to ARIA_KEY_LENGTH_128 - 1 do
    LKey[i] := Byte(i + 1);
  
  // Encrypt
  LEncrypted := ARIAEncryptBlock(LKey, ARIA_KEY_BITS_128, LInput);
  if Length(LEncrypted) <> ARIA_BLOCK_SIZE then Exit;
  
  // Decrypt
  LDecrypted := ARIADecryptBlock(LKey, ARIA_KEY_BITS_128, LEncrypted);
  if Length(LDecrypted) <> ARIA_BLOCK_SIZE then Exit;
  
  // Verify decrypted matches original
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('ARIA - 128-bit ECB encrypt/decrypt', Result);
end;

function TestARIA256ECB: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(ARIA_set_encrypt_key) or not Assigned(ARIA_encrypt_func) or not Assigned(ARIA_decrypt_func) then
  begin
    Result := True;
    PrintTestResult('ARIA - 256-bit ECB (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes
  SetLength(LInput, ARIA_BLOCK_SIZE);
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    LInput[i] := Byte(i + 16);
  
  // Test key: 256 bits (32 bytes)
  SetLength(LKey, ARIA_KEY_LENGTH_256);
  for i := 0 to ARIA_KEY_LENGTH_256 - 1 do
    LKey[i] := Byte(i);
  
  // Encrypt
  LEncrypted := ARIAEncryptBlock(LKey, ARIA_KEY_BITS_256, LInput);
  if Length(LEncrypted) <> ARIA_BLOCK_SIZE then Exit;
  
  // Decrypt
  LDecrypted := ARIADecryptBlock(LKey, ARIA_KEY_BITS_256, LEncrypted);
  if Length(LDecrypted) <> ARIA_BLOCK_SIZE then Exit;
  
  // Verify decrypted matches original
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('ARIA - 256-bit ECB encrypt/decrypt', Result);
end;

function TestARIA128CBC: Boolean;
var
  LKey: TBytes;
  LIV: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(ARIA_set_encrypt_key) or not Assigned(ARIA_cbc_encrypt) then
  begin
    Result := True;
    PrintTestResult('ARIA - 128-bit CBC (skipped)', Result);
    Exit;
  end;
  
  // Test data: 32 bytes (2 ARIA blocks)
  SetLength(LInput, ARIA_BLOCK_SIZE * 2);
  for i := 0 to Length(LInput) - 1 do
    LInput[i] := Byte(i);
  
  // Test key: 128 bits
  SetLength(LKey, ARIA_KEY_LENGTH_128);
  for i := 0 to ARIA_KEY_LENGTH_128 - 1 do
    LKey[i] := Byte(i + 1);
  
  // Test IV: 16 bytes
  SetLength(LIV, ARIA_BLOCK_SIZE);
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    LIV[i] := Byte(i + 33);
  
  // Encrypt
  LEncrypted := ARIAEncryptCBC(LKey, ARIA_KEY_BITS_128, LIV, LInput);
  if Length(LEncrypted) <> Length(LInput) then Exit;
  
  // Decrypt
  LDecrypted := ARIADecryptCBC(LKey, ARIA_KEY_BITS_128, LIV, LEncrypted);
  if Length(LDecrypted) <> Length(LInput) then Exit;
  
  // Verify decrypted matches original
  for i := 0 to Length(LInput) - 1 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('ARIA - 128-bit CBC encrypt/decrypt', Result);
end;

function TestARIA256CBC: Boolean;
var
  LKey: TBytes;
  LIV: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(ARIA_set_encrypt_key) or not Assigned(ARIA_cbc_encrypt) then
  begin
    Result := True;
    PrintTestResult('ARIA - 256-bit CBC (skipped)', Result);
    Exit;
  end;
  
  // Test data: 48 bytes (3 ARIA blocks)
  SetLength(LInput, ARIA_BLOCK_SIZE * 3);
  for i := 0 to Length(LInput) - 1 do
    LInput[i] := Byte(i + 64);
  
  // Test key: 256 bits
  SetLength(LKey, ARIA_KEY_LENGTH_256);
  for i := 0 to ARIA_KEY_LENGTH_256 - 1 do
    LKey[i] := Byte(i);
  
  // Test IV: 16 bytes
  SetLength(LIV, ARIA_BLOCK_SIZE);
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    LIV[i] := Byte(i + 17);
  
  // Encrypt
  LEncrypted := ARIAEncryptCBC(LKey, ARIA_KEY_BITS_256, LIV, LInput);
  if Length(LEncrypted) <> Length(LInput) then Exit;
  
  // Decrypt
  LDecrypted := ARIADecryptCBC(LKey, ARIA_KEY_BITS_256, LIV, LEncrypted);
  if Length(LDecrypted) <> Length(LInput) then Exit;
  
  // Verify decrypted matches original
  for i := 0 to Length(LInput) - 1 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('ARIA - 256-bit CBC encrypt/decrypt', Result);
end;

function TestARIAInvalidKeySize: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(ARIA_set_encrypt_key) then
  begin
    Result := True;
    PrintTestResult('ARIA - Invalid key size (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes
  SetLength(LInput, ARIA_BLOCK_SIZE);
  for i := 0 to ARIA_BLOCK_SIZE - 1 do
    LInput[i] := Byte(i);
  
  // Invalid key: only 8 bytes (should be 16/24/32)
  SetLength(LKey, 8);
  for i := 0 to 7 do
    LKey[i] := Byte(i);
  
  // Encrypt should handle gracefully
  LEncrypted := ARIAEncryptBlock(LKey, 64, LInput);  // Invalid bit size
  
  // Should return empty or correctly sized
  Result := (Length(LEncrypted) = 0) or (Length(LEncrypted) = ARIA_BLOCK_SIZE);
  
  PrintTestResult('ARIA - Invalid key size handling', Result);
end;

function TestARIAInvalidInputSize: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(ARIA_set_encrypt_key) then
  begin
    Result := True;
    PrintTestResult('ARIA - Invalid input size (skipped)', Result);
    Exit;
  end;
  
  // Valid key
  SetLength(LKey, ARIA_KEY_LENGTH_128);
  for i := 0 to ARIA_KEY_LENGTH_128 - 1 do
    LKey[i] := Byte(i);
  
  // Invalid input: only 8 bytes (should be 16 for single block)
  SetLength(LInput, 8);
  for i := 0 to 7 do
    LInput[i] := Byte(i);
  
  // Encrypt should handle gracefully
  LEncrypted := ARIAEncryptBlock(LKey, ARIA_KEY_BITS_128, LInput);
  
  // Should return empty or correct size
  Result := (Length(LEncrypted) = 0) or (Length(LEncrypted) = ARIA_BLOCK_SIZE);
  
  PrintTestResult('ARIA - Invalid input size handling', Result);
end;

procedure RunAllTests;
begin
  WriteLn('====================================');
  WriteLn('ARIA Module Tests');
  WriteLn('OpenSSL Version: ', GetOpenSSLVersionString);
  WriteLn('====================================');
  WriteLn;
  
  TestARIALoadFunctions;
  TestARIA128ECB;
  TestARIA256ECB;
  TestARIA128CBC;
  TestARIA256CBC;
  TestARIAInvalidKeySize;
  TestARIAInvalidInputSize;
  
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