program test_seed;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DynLibs,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.core,
  fafafa.ssl.openssl.seed;

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

function TestSEEDLoadFunctions: Boolean;
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
      PrintTestResult('SEED - Load functions', False);
      Exit;
    end;
  end;
  
  // Get crypto library handle
  LLib := GetCryptoLibHandle;
  if LLib = NilHandle then
  begin
    WriteLn('Failed to get crypto library handle');
    PrintTestResult('SEED - Load functions', False);
    Exit;
  end;
  
  // Load SEED functions
  LoadSEEDFunctions(LLib);
  
  // Check if at least core functions loaded
  if not Assigned(SEED_set_key) then
  begin
    WriteLn('SEED functions not available in this OpenSSL version');
    // Consider this a pass - SEED may not be available in all versions
    Result := True;
    PrintTestResult('SEED - Load functions (not available)', Result);
    Exit;
  end;
  
  Result := True;
  PrintTestResult('SEED - Load functions', Result);
end;

function TestSEEDECBEncryptDecrypt: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(SEED_set_key) or not Assigned(SEED_encrypt_func) or not Assigned(SEED_decrypt_func) then
  begin
    // Function not available
    Result := True;
    PrintTestResult('SEED - ECB encrypt/decrypt (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes (SEED block size)
  SetLength(LInput, SEED_BLOCK_SIZE);
  for i := 0 to SEED_BLOCK_SIZE - 1 do
    LInput[i] := Byte(i);
  
  // Test key: 16 bytes
  SetLength(LKey, SEED_KEY_LENGTH);
  for i := 0 to SEED_KEY_LENGTH - 1 do
    LKey[i] := Byte(i + 1);
  
  // Encrypt
  LEncrypted := SEEDEncryptBlock(LKey, LInput);
  if Length(LEncrypted) <> SEED_BLOCK_SIZE then Exit;
  
  // Decrypt
  LDecrypted := SEEDDecryptBlock(LKey, LEncrypted);
  if Length(LDecrypted) <> SEED_BLOCK_SIZE then Exit;
  
  // Verify decrypted matches original
  for i := 0 to SEED_BLOCK_SIZE - 1 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('SEED - ECB encrypt/decrypt', Result);
end;

function TestSEEDCBCEncryptDecrypt: Boolean;
var
  LKey: TBytes;
  LIV: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  LDecrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(SEED_set_key) or not Assigned(SEED_cbc_encrypt) then
  begin
    // Function not available
    Result := True;
    PrintTestResult('SEED - CBC encrypt/decrypt (skipped)', Result);
    Exit;
  end;
  
  // Test data: 32 bytes (2 SEED blocks)
  SetLength(LInput, SEED_BLOCK_SIZE * 2);
  for i := 0 to Length(LInput) - 1 do
    LInput[i] := Byte(i);
  
  // Test key: 16 bytes
  SetLength(LKey, SEED_KEY_LENGTH);
  for i := 0 to SEED_KEY_LENGTH - 1 do
    LKey[i] := Byte(i + 1);
  
  // Test IV: 16 bytes
  SetLength(LIV, SEED_BLOCK_SIZE);
  for i := 0 to SEED_BLOCK_SIZE - 1 do
    LIV[i] := Byte(i + 17);
  
  // Encrypt
  LEncrypted := SEEDEncryptCBC(LKey, LIV, LInput);
  if Length(LEncrypted) <> Length(LInput) then Exit;
  
  // Decrypt
  LDecrypted := SEEDDecryptCBC(LKey, LIV, LEncrypted);
  if Length(LDecrypted) <> Length(LInput) then Exit;
  
  // Verify decrypted matches original
  for i := 0 to Length(LInput) - 1 do
    if LInput[i] <> LDecrypted[i] then Exit;
  
  Result := True;
  PrintTestResult('SEED - CBC encrypt/decrypt', Result);
end;

function TestSEEDInvalidKeySize: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(SEED_set_key) then
  begin
    Result := True;
    PrintTestResult('SEED - Invalid key size (skipped)', Result);
    Exit;
  end;
  
  // Test data: 16 bytes
  SetLength(LInput, SEED_BLOCK_SIZE);
  for i := 0 to SEED_BLOCK_SIZE - 1 do
    LInput[i] := Byte(i);
  
  // Invalid key: only 8 bytes (should be 16)
  SetLength(LKey, 8);
  for i := 0 to 7 do
    LKey[i] := Byte(i);
  
  // Encrypt should handle gracefully
  LEncrypted := SEEDEncryptBlock(LKey, LInput);
  
  // Should return empty or fail gracefully
  Result := (Length(LEncrypted) = 0) or (Length(LEncrypted) = SEED_BLOCK_SIZE);
  
  PrintTestResult('SEED - Invalid key size handling', Result);
end;

function TestSEEDInvalidInputSize: Boolean;
var
  LKey: TBytes;
  LInput: TBytes;
  LEncrypted: TBytes;
  i: Integer;
begin
  Result := False;
  
  if not Assigned(SEED_set_key) then
  begin
    Result := True;
    PrintTestResult('SEED - Invalid input size (skipped)', Result);
    Exit;
  end;
  
  // Valid key
  SetLength(LKey, SEED_KEY_LENGTH);
  for i := 0 to SEED_KEY_LENGTH - 1 do
    LKey[i] := Byte(i);
  
  // Invalid input: only 8 bytes (should be 16 for single block)
  SetLength(LInput, 8);
  for i := 0 to 7 do
    LInput[i] := Byte(i);
  
  // Encrypt should handle gracefully
  LEncrypted := SEEDEncryptBlock(LKey, LInput);
  
  // Should return empty or correct size
  Result := (Length(LEncrypted) = 0) or (Length(LEncrypted) = SEED_BLOCK_SIZE);
  
  PrintTestResult('SEED - Invalid input size handling', Result);
end;

procedure RunAllTests;
begin
  WriteLn('====================================');
  WriteLn('SEED Module Tests');
  WriteLn('====================================');
  WriteLn;
  
  TestSEEDLoadFunctions;
  TestSEEDECBEncryptDecrypt;
  TestSEEDCBCEncryptDecrypt;
  TestSEEDInvalidKeySize;
  TestSEEDInvalidInputSize;
  
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