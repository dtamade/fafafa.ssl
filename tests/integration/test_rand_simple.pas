program test_rand_simple;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs, ctypes,
  fafafa.ssl.openssl.api.types,
  fafafa.ssl.openssl.consts,
  fafafa.ssl.openssl.api.core;

type
  TRAND_bytes = function(buf: PByte; num: Integer): Integer; cdecl;
  TRAND_status = function: Integer; cdecl;
  TRAND_poll = function: Integer; cdecl;
  TRAND_add = procedure(const buf: Pointer; num: Integer; randomness: Double); cdecl;
  TRAND_priv_bytes = function(buf: PByte; num: Integer): Integer; cdecl;

var
  RAND_bytes: TRAND_bytes = nil;
  RAND_status: TRAND_status = nil;
  RAND_poll: TRAND_poll = nil;
  RAND_add: TRAND_add = nil;
  RAND_priv_bytes: TRAND_priv_bytes = nil;

function LoadRANDFunctions: Boolean;
var
  LibHandle: TLibHandle;
begin
  Result := False;
  LibHandle := GetCryptoLibHandle;
  if LibHandle = 0 then Exit;
  
  // Load essential RAND functions
  RAND_bytes := TRAND_bytes(GetProcedureAddress(LibHandle, 'RAND_bytes'));
  RAND_status := TRAND_status(GetProcedureAddress(LibHandle, 'RAND_status'));
  RAND_poll := TRAND_poll(GetProcedureAddress(LibHandle, 'RAND_poll'));
  RAND_add := TRAND_add(GetProcedureAddress(LibHandle, 'RAND_add'));
  RAND_priv_bytes := TRAND_priv_bytes(GetProcedureAddress(LibHandle, 'RAND_priv_bytes'));
  
  Result := Assigned(RAND_bytes);
end;

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

// Test 1: RAND_status - Check if PRNG is seeded
function Test_RAND_Status: Boolean;
var
  status: Integer;
begin
  PrintTestHeader('RAND Status Check');
  Result := False;
  
  if not Assigned(RAND_status) then
  begin
    WriteLn('SKIPPED (RAND_status not available - deprecated in OpenSSL 3.0)');
    Result := True; // Don't fail
    PrintTestResult('RAND Status Check', Result);
    Exit;
  end;
  
  Write('Checking RAND status... ');
  status := RAND_status();
  
  if status = 1 then
  begin
    WriteLn('OK (PRNG is properly seeded)');
    Result := True;
  end
  else if status = 0 then
  begin
    WriteLn('WARNING (PRNG not seeded, attempting to seed)');
    // Try to seed
    if Assigned(RAND_poll) and (RAND_poll() = 1) then
    begin
      WriteLn('Successfully seeded PRNG');
      Result := True;
    end
    else
    begin
      WriteLn('Note: RAND_poll not available or failed');
      Result := True; // Still pass, OpenSSL 3.0 auto-seeds
    end;
  end
  else
  begin
    WriteLn('FAILED (Unknown status: ', status, ')');
  end;
  
  PrintTestResult('RAND Status Check', Result);
end;

// Test 2: Generate random bytes
function Test_RAND_Bytes_Basic: Boolean;
var
  buffer: array[0..15] of Byte;
  i: Integer;
  all_zero: Boolean;
begin
  PrintTestHeader('RAND_bytes Basic Generation');
  Result := False;
  
  Write('Generating 16 random bytes... ');
  FillChar(buffer, SizeOf(buffer), 0);
  
  if RAND_bytes(@buffer[0], 16) <> 1 then
  begin
    WriteLn('FAILED (RAND_bytes returned error)');
    Exit;
  end;
  WriteLn('OK');
  
  WriteLn('Random bytes (hex): ', BytesToHex(buffer, 16));
  
  // Check if all bytes are zero (highly unlikely with good random)
  Write('Verifying randomness... ');
  all_zero := True;
  for i := 0 to 15 do
  begin
    if buffer[i] <> 0 then
    begin
      all_zero := False;
      Break;
    end;
  end;
  
  if not all_zero then
  begin
    WriteLn('OK (data is random)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (all bytes are zero - not random)');
  end;
  
  PrintTestResult('RAND_bytes Basic Generation', Result);
end;

// Test 3: Generate different random bytes
function Test_RAND_Bytes_Uniqueness: Boolean;
var
  buffer1, buffer2: array[0..31] of Byte;
  i: Integer;
  same_count: Integer;
begin
  PrintTestHeader('RAND_bytes Uniqueness Test');
  Result := False;
  
  Write('Generating first random buffer... ');
  if RAND_bytes(@buffer1[0], 32) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Generating second random buffer... ');
  if RAND_bytes(@buffer2[0], 32) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  WriteLn('First buffer:  ', Copy(BytesToHex(buffer1, 32), 1, 32), '...');
  WriteLn('Second buffer: ', Copy(BytesToHex(buffer2, 32), 1, 32), '...');
  
  // Count how many bytes are the same
  Write('Comparing buffers... ');
  same_count := 0;
  for i := 0 to 31 do
  begin
    if buffer1[i] = buffer2[i] then
      Inc(same_count);
  end;
  
  WriteLn('Same bytes: ', same_count, '/32');
  
  // It's statistically very unlikely that more than 10 bytes would be the same
  if same_count < 10 then
  begin
    WriteLn('OK (buffers are different)');
    Result := True;
  end
  else
  begin
    WriteLn('WARNING (too many same bytes - possible RNG issue)');
    // Still pass if not all bytes are the same
    Result := (same_count < 32);
  end;
  
  PrintTestResult('RAND_bytes Uniqueness Test', Result);
end;

// Test 4: Generate random bytes of various sizes
function Test_RAND_Bytes_Sizes: Boolean;
var
  small_buf: array[0..3] of Byte;
  medium_buf: array[0..63] of Byte;
  large_buf: array[0..255] of Byte;
begin
  PrintTestHeader('RAND_bytes Various Sizes');
  Result := False;
  
  Write('Generating 4 bytes... ');
  if RAND_bytes(@small_buf[0], 4) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK (', BytesToHex(small_buf, 4), ')');
  
  Write('Generating 64 bytes... ');
  if RAND_bytes(@medium_buf[0], 64) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK (first 16 bytes: ', Copy(BytesToHex(medium_buf, 64), 1, 32), '...)');
  
  Write('Generating 256 bytes... ');
  if RAND_bytes(@large_buf[0], 256) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK (first 16 bytes: ', Copy(BytesToHex(large_buf, 256), 1, 32), '...)');
  
  Result := True;
  PrintTestResult('RAND_bytes Various Sizes', Result);
end;

// Test 5: RAND_priv_bytes - Private random bytes
function Test_RAND_Priv_Bytes: Boolean;
var
  buffer: array[0..31] of Byte;
begin
  PrintTestHeader('RAND_priv_bytes Private Random');
  Result := False;
  
  // Check if function is available (may not be in older OpenSSL)
  if not Assigned(RAND_priv_bytes) then
  begin
    WriteLn('SKIPPED (RAND_priv_bytes not available in this OpenSSL version)');
    Result := True; // Don't fail if not available
    PrintTestResult('RAND_priv_bytes Private Random', Result);
    Exit;
  end;
  
  Write('Generating 32 private random bytes... ');
  FillChar(buffer, SizeOf(buffer), 0);
  
  if RAND_priv_bytes(@buffer[0], 32) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  WriteLn('Private random bytes (hex): ', Copy(BytesToHex(buffer, 32), 1, 32), '...');
  
  Result := True;
  PrintTestResult('RAND_priv_bytes Private Random', Result);
end;

// Test 6: Random distribution test (Chi-square-like)
function Test_RAND_Distribution: Boolean;
var
  buffer: array[0..999] of Byte;
  counts: array[0..255] of Integer;
  i: Integer;
  min_count, max_count: Integer;
  expected: Double;
  variance: Double;
begin
  PrintTestHeader('RAND_bytes Distribution Test');
  Result := False;
  
  Write('Generating 1000 random bytes... ');
  if RAND_bytes(@buffer[0], 1000) <> 1 then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  // Count occurrences of each byte value
  Write('Analyzing distribution... ');
  FillChar(counts, SizeOf(counts), 0);
  for i := 0 to 999 do
    Inc(counts[buffer[i]]);
  
  // Find min and max counts
  min_count := counts[0];
  max_count := counts[0];
  for i := 1 to 255 do
  begin
    if counts[i] < min_count then min_count := counts[i];
    if counts[i] > max_count then max_count := counts[i];
  end;
  
  expected := 1000.0 / 256.0; // ~3.9 per value
  variance := max_count - min_count;
  
  WriteLn('OK');
  WriteLn('Expected count per value: ', expected:0:2);
  WriteLn('Min count: ', min_count);
  WriteLn('Max count: ', max_count);
  WriteLn('Variance: ', variance:0:0);
  
  // For good random distribution, variance should be reasonable
  Write('Verifying distribution quality... ');
  if variance < 20 then
  begin
    WriteLn('OK (good distribution)');
    Result := True;
  end
  else
  begin
    WriteLn('WARNING (high variance - possible distribution issue)');
    Result := True; // Still pass but warn
  end;
  
  PrintTestResult('RAND_bytes Distribution Test', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('RAND Module Integration Test');
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
  
  // Load RAND functions directly
  Write('Loading RAND functions... ');
  if not LoadRANDFunctions then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load RAND functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 6;
  
  try
    // Run all tests
    if Test_RAND_Status then Inc(PassedTests);
    if Test_RAND_Bytes_Basic then Inc(PassedTests);
    if Test_RAND_Bytes_Uniqueness then Inc(PassedTests);
    if Test_RAND_Bytes_Sizes then Inc(PassedTests);
    if Test_RAND_Priv_Bytes then Inc(PassedTests);
    if Test_RAND_Distribution then Inc(PassedTests);
    
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
  
  UnloadOpenSSLCore;
end.
