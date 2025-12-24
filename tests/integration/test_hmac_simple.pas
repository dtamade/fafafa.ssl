program test_hmac_simple;

{$mode objfpc}{$H+}

uses
  SysUtils, ctypes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.hmac;

procedure PrintTestHeader(const TestName: string);
begin
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test: ', TestName);
  WriteLn('========================================');
end;

procedure PrintTestResult(const TestName: string; Success: Boolean);
begin
  if Success then
    WriteLn('[PASS] ', TestName)
  else
    WriteLn('[FAIL] ', TestName);
end;

function BytesToHex(const data: array of Byte; len: Integer): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to len - 1 do
    Result := Result + LowerCase(IntToHex(data[i], 2));
end;

// Test 1: HMAC-SHA1 One-Shot
function Test_HMAC_SHA1_OneShot: Boolean;
var
  key: AnsiString;
  data: AnsiString;
  digest: array[0..19] of Byte; // SHA1 = 20 bytes
  result_ptr: PByte;
  hex_digest: string;
  expected: string;
begin
  PrintTestHeader('HMAC-SHA1 One-Shot');
  Result := False;
  
  key := 'secret';
  data := 'Hello World!';
  expected := '5efed98b0787c83f9cb0135ba283c390ca49320e'; // Calculated by OpenSSL
  
  Write('Computing HMAC-SHA1... ');
  FillChar(digest, SizeOf(digest), 0);
  result_ptr := HMAC_SHA1(@key[1], Length(key), @data[1], Length(data), @digest[0]);
  if result_ptr = nil then
  begin
    WriteLn('FAILED (HMAC returned nil)');
    Exit;
  end;
  WriteLn('OK');
  
  hex_digest := BytesToHex(digest, 20);
  WriteLn('HMAC-SHA1 digest: ', hex_digest);
  WriteLn('Expected:         ', expected);
  
  Write('Verifying digest... ');
  if hex_digest = expected then
  begin
    WriteLn('OK (match)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (mismatch)');
  end;
  
  PrintTestResult('HMAC-SHA1 One-Shot', Result);
end;

// Test 2: HMAC-SHA256 One-Shot
function Test_HMAC_SHA256_OneShot: Boolean;
var
  key: AnsiString;
  data: AnsiString;
  digest: array[0..31] of Byte; // SHA256 = 32 bytes
  result_ptr: PByte;
  hex_digest: string;
  expected: string;
begin
  PrintTestHeader('HMAC-SHA256 One-Shot');
  Result := False;
  
  key := 'secret';
  data := 'Hello World!';
  expected := '6fa7b4dea28ee348df10f9bb595ad985ff150a4adfd6131cca677d9acee07dc6'; // Calculated by OpenSSL
  
  Write('Computing HMAC-SHA256... ');
  FillChar(digest, SizeOf(digest), 0);
  result_ptr := HMAC_SHA256(@key[1], Length(key), @data[1], Length(data), @digest[0]);
  if result_ptr = nil then
  begin
    WriteLn('FAILED (HMAC returned nil)');
    Exit;
  end;
  WriteLn('OK');
  
  hex_digest := BytesToHex(digest, 32);
  WriteLn('HMAC-SHA256 digest: ', hex_digest);
  WriteLn('Expected:           ', expected);
  
  Write('Verifying digest... ');
  if hex_digest = expected then
  begin
    WriteLn('OK (match)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (mismatch)');
  end;
  
  PrintTestResult('HMAC-SHA256 One-Shot', Result);
end;

// Test 3: HMAC-SHA512 One-Shot
function Test_HMAC_SHA512_OneShot: Boolean;
var
  key: AnsiString;
  data: AnsiString;
  digest: array[0..63] of Byte; // SHA512 = 64 bytes
  result_ptr: PByte;
  hex_digest: string;
begin
  PrintTestHeader('HMAC-SHA512 One-Shot');
  Result := False;
  
  key := 'secret';
  data := 'Hello World!';
  
  Write('Computing HMAC-SHA512... ');
  FillChar(digest, SizeOf(digest), 0);
  result_ptr := HMAC_SHA512(@key[1], Length(key), @data[1], Length(data), @digest[0]);
  if result_ptr = nil then
  begin
    WriteLn('FAILED (HMAC returned nil)');
    Exit;
  end;
  WriteLn('OK');
  
  hex_digest := BytesToHex(digest, 64);
  WriteLn('HMAC-SHA512 digest: ', Copy(hex_digest, 1, 64), '...');
  WriteLn('                    (', Length(hex_digest), ' hex chars = ', Length(hex_digest) div 2, ' bytes)');
  
  Write('Verifying digest length... ');
  if Length(hex_digest) = 128 then // 64 bytes * 2 hex chars
  begin
    WriteLn('OK (64 bytes)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (expected 64 bytes, got ', Length(hex_digest) div 2, ')');
  end;
  
  PrintTestResult('HMAC-SHA512 One-Shot', Result);
end;

// Test 4: HMAC Context-Based (Init, Update, Final)
function Test_HMAC_Context_Based: Boolean;
var
  ctx: PHMAC_CTX;
  key: AnsiString;
  data1, data2, data3: AnsiString;
  digest: array[0..31] of Byte; // SHA256
  digest_len: Cardinal;
  hex_digest: string;
begin
  PrintTestHeader('HMAC Context-Based (Init, Update, Final)');
  Result := False;
  
  key := 'secret';
  data1 := 'Hello ';
  data2 := 'World';
  data3 := '!';
  
  Write('Creating HMAC context... ');
  ctx := HMAC_CTX_new();
  if ctx = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Initializing HMAC-SHA256... ');
  if HMAC_Init_ex(ctx, @key[1], Length(key), EVP_sha256(), nil) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Updating HMAC (part 1)... ');
  if HMAC_Update(ctx, @data1[1], Length(data1)) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Updating HMAC (part 2)... ');
  if HMAC_Update(ctx, @data2[1], Length(data2)) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Updating HMAC (part 3)... ');
  if HMAC_Update(ctx, @data3[1], Length(data3)) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Finalizing HMAC... ');
  FillChar(digest, SizeOf(digest), 0);
  digest_len := 0;
  if HMAC_Final(ctx, @digest[0], @digest_len) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx);
    Exit;
  end;
  WriteLn('OK (digest length: ', digest_len, ' bytes)');
  
  hex_digest := BytesToHex(digest, digest_len);
  WriteLn('HMAC-SHA256 digest: ', hex_digest);
  
  // Should match the one-shot version with "Hello World!"
  Write('Comparing with one-shot version... ');
  if hex_digest = '6fa7b4dea28ee348df10f9bb595ad985ff150a4adfd6131cca677d9acee07dc6' then
  begin
    WriteLn('OK (match)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (mismatch)');
  end;
  
  HMAC_CTX_free(ctx);
  PrintTestResult('HMAC Context-Based', Result);
end;

// Test 5: HMAC Context Reset
function Test_HMAC_Context_Reset: Boolean;
var
  ctx: PHMAC_CTX;
  key: AnsiString;
  data: AnsiString;
  digest1, digest2: array[0..31] of Byte;
  digest_len: Cardinal;
  hex1, hex2: string;
begin
  PrintTestHeader('HMAC Context Reset');
  Result := False;
  
  key := 'secret';
  data := 'Test Data';
  
  Write('Creating HMAC context... ');
  ctx := HMAC_CTX_new();
  if ctx = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  // First computation
  Write('First HMAC computation... ');
  HMAC_Init_ex(ctx, @key[1], Length(key), EVP_sha256(), nil);
  HMAC_Update(ctx, @data[1], Length(data));
  FillChar(digest1, SizeOf(digest1), 0);
  digest_len := 0;
  HMAC_Final(ctx, @digest1[0], @digest_len);
  hex1 := BytesToHex(digest1, digest_len);
  WriteLn('OK');
  WriteLn('First digest:  ', hex1);
  
  // Reset context
  Write('Resetting HMAC context... ');
  if HMAC_CTX_reset(ctx) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx);
    Exit;
  end;
  WriteLn('OK');
  
  // Second computation (should produce the same result)
  Write('Second HMAC computation... ');
  HMAC_Init_ex(ctx, @key[1], Length(key), EVP_sha256(), nil);
  HMAC_Update(ctx, @data[1], Length(data));
  FillChar(digest2, SizeOf(digest2), 0);
  digest_len := 0;
  HMAC_Final(ctx, @digest2[0], @digest_len);
  hex2 := BytesToHex(digest2, digest_len);
  WriteLn('OK');
  WriteLn('Second digest: ', hex2);
  
  Write('Comparing digests... ');
  if hex1 = hex2 then
  begin
    WriteLn('OK (match)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (mismatch)');
  end;
  
  HMAC_CTX_free(ctx);
  PrintTestResult('HMAC Context Reset', Result);
end;

// Test 6: HMAC Context Copy
function Test_HMAC_Context_Copy: Boolean;
var
  ctx1, ctx2: PHMAC_CTX;
  key: AnsiString;
  data1, data2: AnsiString;
  digest1, digest2: array[0..31] of Byte;
  digest_len: Cardinal;
  hex1, hex2: string;
begin
  PrintTestHeader('HMAC Context Copy');
  Result := False;
  
  key := 'secret';
  data1 := 'Part 1 ';
  data2 := 'Part 2';
  
  Write('Creating HMAC contexts... ');
  ctx1 := HMAC_CTX_new();
  ctx2 := HMAC_CTX_new();
  if (ctx1 = nil) or (ctx2 = nil) then
  begin
    WriteLn('FAILED');
    if ctx1 <> nil then HMAC_CTX_free(ctx1);
    if ctx2 <> nil then HMAC_CTX_free(ctx2);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Initializing and updating first context... ');
  HMAC_Init_ex(ctx1, @key[1], Length(key), EVP_sha256(), nil);
  HMAC_Update(ctx1, @data1[1], Length(data1));
  WriteLn('OK');
  
  Write('Copying context... ');
  if HMAC_CTX_copy(ctx2, ctx1) <> 1 then
  begin
    WriteLn('FAILED');
    HMAC_CTX_free(ctx1);
    HMAC_CTX_free(ctx2);
    Exit;
  end;
  WriteLn('OK');
  
  // Complete both contexts with the same data
  Write('Completing both contexts... ');
  HMAC_Update(ctx1, @data2[1], Length(data2));
  HMAC_Update(ctx2, @data2[1], Length(data2));
  
  FillChar(digest1, SizeOf(digest1), 0);
  FillChar(digest2, SizeOf(digest2), 0);
  digest_len := 0;
  
  HMAC_Final(ctx1, @digest1[0], @digest_len);
  hex1 := BytesToHex(digest1, digest_len);
  
  digest_len := 0;
  HMAC_Final(ctx2, @digest2[0], @digest_len);
  hex2 := BytesToHex(digest2, digest_len);
  WriteLn('OK');
  
  WriteLn('First context digest:  ', hex1);
  WriteLn('Second context digest: ', hex2);
  
  Write('Comparing digests... ');
  if hex1 = hex2 then
  begin
    WriteLn('OK (match)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (mismatch)');
  end;
  
  HMAC_CTX_free(ctx1);
  HMAC_CTX_free(ctx2);
  PrintTestResult('HMAC Context Copy', Result);
end;

// Test 7: HMAC Size Query
function Test_HMAC_Size: Boolean;
var
  ctx: PHMAC_CTX;
  key: AnsiString;
  size: size_t;
begin
  PrintTestHeader('HMAC Size Query');
  Result := False;
  
  key := 'secret';
  
  Write('Creating HMAC context... ');
  ctx := HMAC_CTX_new();
  if ctx = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Initializing with SHA256... ');
  HMAC_Init_ex(ctx, @key[1], Length(key), EVP_sha256(), nil);
  WriteLn('OK');
  
  Write('Querying HMAC size... ');
  size := HMAC_size(ctx);
  WriteLn('OK (size: ', size, ' bytes)');
  
  Write('Verifying size... ');
  if size = 32 then // SHA256 = 32 bytes
  begin
    WriteLn('OK (32 bytes for SHA256)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (expected 32, got ', size, ')');
  end;
  
  HMAC_CTX_free(ctx);
  PrintTestResult('HMAC Size Query', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('HMAC Module Integration Test');
  WriteLn('=============================');
  WriteLn('');
  
  // Initialize OpenSSL Core
  Write('Initializing OpenSSL Core... ');
  try
    LoadOpenSSLCore;
    WriteLn('OK (', GetOpenSSLVersionString, ')');
  except
    on E: Exception do
    begin
      WriteLn('FAILED');
      WriteLn('Error: ', E.Message);
      ExitCode := 1;
      Exit;
    end;
  end;
  
  // Load EVP functions (required for hash algorithms)
  Write('Loading EVP functions... ');
  if not LoadEVP(GetCryptoLibHandle) then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load EVP functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  // Load HMAC functions
  Write('Loading HMAC functions... ');
  if not LoadOpenSSLHMAC then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load HMAC functions');
    UnloadEVP;
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 7;
  
  try
    // Run all tests
    if Test_HMAC_SHA1_OneShot then Inc(PassedTests);
    if Test_HMAC_SHA256_OneShot then Inc(PassedTests);
    if Test_HMAC_SHA512_OneShot then Inc(PassedTests);
    if Test_HMAC_Context_Based then Inc(PassedTests);
    if Test_HMAC_Context_Reset then Inc(PassedTests);
    if Test_HMAC_Context_Copy then Inc(PassedTests);
    if Test_HMAC_Size then Inc(PassedTests);
    
  except
    on E: Exception do
    begin
      WriteLn('');
      WriteLn('EXCEPTION: ', E.ClassName, ': ', E.Message);
    end;
  end;
  
  // Summary
  WriteLn('');
  WriteLn('========================================');
  WriteLn('Test Summary');
  WriteLn('========================================');
  WriteLn('Total tests: ', TotalTests);
  WriteLn('Passed: ', PassedTests);
  WriteLn('Failed: ', TotalTests - PassedTests);
  WriteLn('Success rate: ', (PassedTests * 100) div TotalTests, '%');
  
  if PassedTests = TotalTests then
  begin
    WriteLn('');
    WriteLn('ALL TESTS PASSED!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn('');
    WriteLn('SOME TESTS FAILED!');
    ExitCode := 1;
  end;
  
  UnloadOpenSSLHMAC;
  UnloadEVP;
  UnloadOpenSSLCore;
end.
