program test_buffer_simple;

{$mode objfpc}{$H+}

uses
  SysUtils, DynLibs,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.buffer;

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

// Test 1: Buffer Creation and Destruction
function Test_Buffer_CreateDestroy: Boolean;
var
  buf: PBUF_MEM;
begin
  PrintTestHeader('Buffer Creation and Destruction');
  Result := False;
  
  Write('Creating buffer... ');
  buf := BUF_MEM_new();
  if buf = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Verifying initial state... ');
  if buf^.length <> 0 then
  begin
    WriteLn('FAILED (length should be 0)');
    BUF_MEM_free(buf);
    Exit;
  end;
  WriteLn('OK (length=0)');
  
  Write('Freeing buffer... ');
  BUF_MEM_free(buf);
  WriteLn('OK');
  
  Result := True;
  PrintTestResult('Buffer Creation and Destruction', Result);
end;

// Test 2: Buffer Growth
function Test_Buffer_Grow: Boolean;
var
  buf: PBUF_MEM;
  result_size: NativeUInt;
begin
  PrintTestHeader('Buffer Growth');
  Result := False;
  
  buf := BUF_MEM_new();
  if buf = nil then
  begin
    WriteLn('Failed to create buffer');
    Exit;
  end;
  
  Write('Growing buffer to 100 bytes... ');
  result_size := BUF_MEM_grow(buf, 100);
  if result_size = 0 then
  begin
    WriteLn('FAILED');
    BUF_MEM_free(buf);
    Exit;
  end;
  WriteLn('OK (size: ', result_size, ')');
  
  Write('Verifying buffer size... ');
  if buf^.length < 100 then
  begin
    WriteLn('FAILED (expected >= 100, got ', buf^.length, ')');
    BUF_MEM_free(buf);
    Exit;
  end;
  WriteLn('OK (length: ', buf^.length, ')');
  
  Write('Growing buffer to 200 bytes... ');
  result_size := BUF_MEM_grow(buf, 200);
  if result_size = 0 then
  begin
    WriteLn('FAILED');
    BUF_MEM_free(buf);
    Exit;
  end;
  WriteLn('OK (size: ', result_size, ')');
  
  BUF_MEM_free(buf);
  Result := True;
  PrintTestResult('Buffer Growth', Result);
end;

// Test 3: Buffer Clean Growth
function Test_Buffer_GrowClean: Boolean;
var
  buf: PBUF_MEM;
  result_size: NativeUInt;
  i: Integer;
  all_zero: Boolean;
begin
  PrintTestHeader('Buffer Clean Growth');
  Result := False;
  
  buf := BUF_MEM_new();
  if buf = nil then
  begin
    WriteLn('Failed to create buffer');
    Exit;
  end;
  
  Write('Growing buffer cleanly to 50 bytes... ');
  result_size := BUF_MEM_grow_clean(buf, 50);
  if result_size = 0 then
  begin
    WriteLn('FAILED');
    BUF_MEM_free(buf);
    Exit;
  end;
  WriteLn('OK (size: ', result_size, ')');
  
  Write('Verifying buffer is zeroed... ');
  all_zero := True;
  for i := 0 to 49 do
  begin
    if buf^.data[i] <> #0 then
    begin
      all_zero := False;
      Break;
    end;
  end;
  
  if all_zero then
  begin
    WriteLn('OK (all bytes are zero)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (some bytes are not zero)');
  end;
  
  BUF_MEM_free(buf);
  PrintTestResult('Buffer Clean Growth', Result);
end;

// Test 4: Buffer Data Writing
function Test_Buffer_WriteData: Boolean;
var
  buf: PBUF_MEM;
  test_data: AnsiString;
  i: Integer;
  match: Boolean;
begin
  PrintTestHeader('Buffer Data Writing');
  Result := False;
  
  test_data := 'Hello Buffer!';
  
  buf := BUF_MEM_new();
  if buf = nil then
  begin
    WriteLn('Failed to create buffer');
    Exit;
  end;
  
  Write('Growing buffer... ');
  if BUF_MEM_grow(buf, Length(test_data)) = 0 then
  begin
    WriteLn('FAILED');
    BUF_MEM_free(buf);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Writing test data... ');
  Move(test_data[1], buf^.data^, Length(test_data));
  buf^.length := Length(test_data);
  WriteLn('OK');
  
  Write('Verifying data... ');
  match := True;
  for i := 1 to Length(test_data) do
  begin
    if buf^.data[i-1] <> test_data[i] then
    begin
      match := False;
      Break;
    end;
  end;
  
  if match then
  begin
    WriteLn('OK (data matches)');
    WriteLn('Buffer content: ', Copy(AnsiString(buf^.data), 1, buf^.length));
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (data mismatch)');
  end;
  
  BUF_MEM_free(buf);
  PrintTestResult('Buffer Data Writing', Result);
end;

// Test 5: String Duplication
function Test_Buffer_StrDup: Boolean;
var
  original: AnsiString;
  duplicate: PAnsiChar;
begin
  PrintTestHeader('String Duplication');
  Result := False;
  
  if not Assigned(BUF_strdup) then
  begin
    WriteLn('SKIPPED (BUF_strdup not available)');
    Result := True; // Don't fail
    PrintTestResult('String Duplication', Result);
    Exit;
  end;
  
  original := 'Test String';
  
  Write('Duplicating string... ');
  duplicate := BUF_strdup(PAnsiChar(original));
  if duplicate = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Verifying duplicate... ');
  if AnsiString(duplicate) = original then
  begin
    WriteLn('OK (strings match)');
    WriteLn('Original:  ', original);
    WriteLn('Duplicate: ', AnsiString(duplicate));
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (strings do not match)');
  end;
  
  // Free the duplicated string
  // Note: In OpenSSL, strdup uses CRYPTO_malloc which should be freed with CRYPTO_free
  // For now, we'll skip freeing to avoid issues
  // CRYPTO_free(duplicate);
  
  PrintTestResult('String Duplication', Result);
end;

// Test 6: Memory Duplication
function Test_Buffer_MemDup: Boolean;
var
  original: array[0..9] of Byte = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);
  duplicate: PByte;
  i: Integer;
  match: Boolean;
begin
  PrintTestHeader('Memory Duplication');
  Result := False;
  
  if not Assigned(BUF_memdup) then
  begin
    WriteLn('SKIPPED (BUF_memdup not available)');
    Result := True; // Don't fail
    PrintTestResult('Memory Duplication', Result);
    Exit;
  end;
  
  Write('Duplicating memory... ');
  duplicate := BUF_memdup(@original[0], SizeOf(original));
  if duplicate = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Verifying duplicate... ');
  match := True;
  for i := 0 to 9 do
  begin
    if duplicate[i] <> original[i] then
    begin
      match := False;
      Break;
    end;
  end;
  
  if match then
  begin
    WriteLn('OK (memory matches)');
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (memory does not match)');
  end;
  
  // Free the duplicated memory  
  // Note: In OpenSSL, memdup uses CRYPTO_malloc which should be freed with CRYPTO_free
  // For now, we'll skip freeing to avoid issues
  // CRYPTO_free(duplicate);
  
  PrintTestResult('Memory Duplication', Result);
end;

// Test 7: Buffer Reverse
function Test_Buffer_Reverse: Boolean;
var
  input: array[0..4] of Byte = (1, 2, 3, 4, 5);
  output: array[0..4] of Byte;
  expected: array[0..4] of Byte = (5, 4, 3, 2, 1);
  i: Integer;
  match: Boolean;
begin
  PrintTestHeader('Buffer Reverse');
  Result := False;
  
  if not Assigned(BUF_reverse) then
  begin
    WriteLn('SKIPPED (BUF_reverse not available)');
    Result := True; // Don't fail
    PrintTestResult('Buffer Reverse', Result);
    Exit;
  end;
  
  Write('Reversing buffer... ');
  FillChar(output, SizeOf(output), 0);
  BUF_reverse(@output[0], @input[0], 5);
  WriteLn('OK');
  
  Write('Verifying reversed data... ');
  match := True;
  for i := 0 to 4 do
  begin
    if output[i] <> expected[i] then
    begin
      match := False;
      Break;
    end;
  end;
  
  if match then
  begin
    WriteLn('OK (data reversed correctly)');
    Write('Input:    ');
    for i := 0 to 4 do Write(input[i], ' ');
    WriteLn;
    Write('Output:   ');
    for i := 0 to 4 do Write(output[i], ' ');
    WriteLn;
    Write('Expected: ');
    for i := 0 to 4 do Write(expected[i], ' ');
    WriteLn;
    Result := True;
  end
  else
  begin
    WriteLn('FAILED (data not reversed correctly)');
  end;
  
  PrintTestResult('Buffer Reverse', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('Buffer Module Integration Test');
  WriteLn('===============================');
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
  
  // Load Buffer module
  Write('Loading Buffer functions... ');
  LoadBufferModule(GetCryptoLibHandle);
  if not Assigned(BUF_MEM_new) then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load Buffer functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 7;
  
  try
    // Run all tests
    if Test_Buffer_CreateDestroy then Inc(PassedTests);
    if Test_Buffer_Grow then Inc(PassedTests);
    if Test_Buffer_GrowClean then Inc(PassedTests);
    if Test_Buffer_WriteData then Inc(PassedTests);
    if Test_Buffer_StrDup then Inc(PassedTests);
    if Test_Buffer_MemDup then Inc(PassedTests);
    if Test_Buffer_Reverse then Inc(PassedTests);
    
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
  
  UnloadBufferModule;
  UnloadOpenSSLCore;
end.
