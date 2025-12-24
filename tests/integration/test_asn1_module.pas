program test_asn1_module;

{$mode objfpc}{$H+}

uses
  SysUtils, DateUtils, ctypes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.consts,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.asn1;


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

// Test 1: ASN1_INTEGER Basic Operations
function Test_ASN1_INTEGER_Basic: Boolean;
var
  int1: PASN1_INTEGER;
  value: clong;
begin
  PrintTestHeader('ASN1_INTEGER Basic Operations');
  Result := False;
  
  Write('Creating ASN1_INTEGER... ');
  int1 := ASN1_INTEGER_new();
  if int1 = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting value to 42... ');
  if ASN1_INTEGER_set(int1, 42) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(int1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Getting value... ');
  value := ASN1_INTEGER_get(int1);
  if value <> 42 then
  begin
    WriteLn('FAILED (expected 42, got ', value, ')');
    ASN1_INTEGER_free(int1);
    Exit;
  end;
  WriteLn('OK (value=', value, ')');
  
  ASN1_INTEGER_free(int1);
  Result := True;
  PrintTestResult('ASN1_INTEGER Basic Operations', Result);
end;

// Test 2: ASN1_INTEGER 64-bit Operations
function Test_ASN1_INTEGER_Int64: Boolean;
var
  int1: PASN1_INTEGER;
  value: Int64;
  test_val: Int64;
begin
  PrintTestHeader('ASN1_INTEGER 64-bit Operations');
  Result := False;
  
  int1 := ASN1_INTEGER_new();
  if int1 = nil then
  begin
    WriteLn('FAILED to create integer');
    Exit;
  end;
  
  test_val := 9876543210;
  Write('Setting 64-bit value (', test_val, ')... ');
  if ASN1_INTEGER_set_int64(int1, test_val) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(int1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Getting 64-bit value... ');
  value := 0;
  if ASN1_INTEGER_get_int64(@value, int1) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(int1);
    Exit;
  end;
  
  if value <> test_val then
  begin
    WriteLn('FAILED (expected ', test_val, ', got ', value, ')');
    ASN1_INTEGER_free(int1);
    Exit;
  end;
  WriteLn('OK (value=', value, ')');
  
  ASN1_INTEGER_free(int1);
  Result := True;
  PrintTestResult('ASN1_INTEGER 64-bit Operations', Result);
end;

// Test 3: ASN1_INTEGER Duplication and Comparison
function Test_ASN1_INTEGER_DupCmp: Boolean;
var
  int1, int2: PASN1_INTEGER;
begin
  PrintTestHeader('ASN1_INTEGER Duplication and Comparison');
  Result := False;
  
  int1 := ASN1_INTEGER_new();
  if int1 = nil then
  begin
    WriteLn('FAILED to create integer');
    Exit;
  end;
  ASN1_INTEGER_set(int1, 12345);
  
  Write('Duplicating ASN1_INTEGER... ');
  int2 := ASN1_INTEGER_dup(int1);
  if int2 = nil then
  begin
    WriteLn('FAILED');
    ASN1_INTEGER_free(int1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Comparing integers... ');
  if ASN1_INTEGER_cmp(int1, int2) <> 0 then
  begin
    WriteLn('FAILED (integers differ)');
    ASN1_INTEGER_free(int1);
    ASN1_INTEGER_free(int2);
    Exit;
  end;
  WriteLn('OK (integers are equal)');
  
  ASN1_INTEGER_free(int1);
  ASN1_INTEGER_free(int2);
  Result := True;
  PrintTestResult('ASN1_INTEGER Duplication and Comparison', Result);
end;

// Test 4: ASN1_STRING Basic Operations
function Test_ASN1_STRING_Basic: Boolean;
var
  str1: PASN1_STRING;
  data: PByte;
  len: Integer;
  test_data: AnsiString;
begin
  PrintTestHeader('ASN1_STRING Basic Operations');
  Result := False;
  
  test_data := 'Hello ASN.1!';
  
  Write('Creating ASN1_STRING... ');
  str1 := ASN1_STRING_new();
  if str1 = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting string data... ');
  if ASN1_STRING_set(str1, PAnsiChar(test_data), Length(test_data)) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_STRING_free(str1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Getting string length... ');
  len := ASN1_STRING_length(str1);
  if len <> Length(test_data) then
  begin
    WriteLn('FAILED (expected ', Length(test_data), ', got ', len, ')');
    ASN1_STRING_free(str1);
    Exit;
  end;
  WriteLn('OK (length=', len, ')');
  
  Write('Getting string data... ');
  data := ASN1_STRING_get0_data(str1);
  if data = nil then
  begin
    WriteLn('FAILED');
    ASN1_STRING_free(str1);
    Exit;
  end;
  WriteLn('OK');
  WriteLn('String content: ', PAnsiChar(data));
  
  ASN1_STRING_free(str1);
  Result := True;
  PrintTestResult('ASN1_STRING Basic Operations', Result);
end;

// Test 5: ASN1_STRING Type-Specific
function Test_ASN1_STRING_Typed: Boolean;
var
  str1: PASN1_STRING;
  str_type: Integer;
begin
  PrintTestHeader('ASN1_STRING Type-Specific');
  Result := False;
  
  Write('Creating UTF8 string... ');
  str1 := ASN1_STRING_type_new(V_ASN1_UTF8STRING);
  if str1 = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Verifying string type... ');
  str_type := ASN1_STRING_type(str1);
  if str_type <> V_ASN1_UTF8STRING then
  begin
    WriteLn('FAILED (expected ', V_ASN1_UTF8STRING, ', got ', str_type, ')');
    ASN1_STRING_free(str1);
    Exit;
  end;
  WriteLn('OK (type=', str_type, ')');
  
  ASN1_STRING_free(str1);
  Result := True;
  PrintTestResult('ASN1_STRING Type-Specific', Result);
end;

// Test 6: ASN1_OCTET_STRING Operations
function Test_ASN1_OCTET_STRING: Boolean;
var
  oct1, oct2: PASN1_OCTET_STRING;
  test_data: array[0..9] of Byte = (1,2,3,4,5,6,7,8,9,10);
begin
  PrintTestHeader('ASN1_OCTET_STRING Operations');
  Result := False;
  
  Write('Creating ASN1_OCTET_STRING... ');
  oct1 := ASN1_OCTET_STRING_new();
  if oct1 = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting octet string data... ');
  if ASN1_OCTET_STRING_set(oct1, @test_data[0], Length(test_data)) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_OCTET_STRING_free(oct1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Duplicating octet string... ');
  oct2 := ASN1_OCTET_STRING_dup(oct1);
  if oct2 = nil then
  begin
    WriteLn('FAILED');
    ASN1_OCTET_STRING_free(oct1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Comparing octet strings... ');
  if ASN1_OCTET_STRING_cmp(oct1, oct2) <> 0 then
  begin
    WriteLn('FAILED (octet strings differ)');
    ASN1_OCTET_STRING_free(oct1);
    ASN1_OCTET_STRING_free(oct2);
    Exit;
  end;
  WriteLn('OK (octet strings are equal)');
  
  ASN1_OCTET_STRING_free(oct1);
  ASN1_OCTET_STRING_free(oct2);
  Result := True;
  PrintTestResult('ASN1_OCTET_STRING Operations', Result);
end;

// Test 7: ASN1_TIME Operations
function Test_ASN1_TIME: Boolean;
var
  time1: PASN1_TIME;
  current_time: time_t;
begin
  PrintTestHeader('ASN1_TIME Operations');
  Result := False;
  
  Write('Creating ASN1_TIME... ');
  time1 := ASN1_TIME_new();
  if time1 = nil then
  begin
    WriteLn('FAILED');
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting time to current... ');
  current_time := 0; // Use 0 for current time
  if ASN1_TIME_set(time1, current_time) = nil then
  begin
    WriteLn('FAILED');
    ASN1_TIME_free(time1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Checking time validity... ');
  if ASN1_TIME_check(time1) <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_TIME_free(time1);
    Exit;
  end;
  WriteLn('OK');
  
  Write('Setting time from string... ');
  if ASN1_TIME_set_string(time1, '20250101120000Z') <> 1 then
  begin
    WriteLn('FAILED');
    ASN1_TIME_free(time1);
    Exit;
  end;
  WriteLn('OK');
  
  ASN1_TIME_free(time1);
  Result := True;
  PrintTestResult('ASN1_TIME Operations', Result);
end;

var
  PassedTests, TotalTests: Integer;
  
begin
  WriteLn('ASN.1 Module Integration Test');
  WriteLn('==============================');
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
  
  // Load ASN1 functions
  Write('Loading ASN1 functions... ');
  if not LoadOpenSSLASN1(GetCryptoLibHandle) then
  begin
    WriteLn('FAILED');
    WriteLn('Error: Could not load ASN1 functions');
    UnloadOpenSSLCore;
    ExitCode := 1;
    Exit;
  end;
  WriteLn('OK');
  
  PassedTests := 0;
  TotalTests := 7;
  
  try
    // Run all tests
    if Test_ASN1_INTEGER_Basic then Inc(PassedTests);
    if Test_ASN1_INTEGER_Int64 then Inc(PassedTests);
    if Test_ASN1_INTEGER_DupCmp then Inc(PassedTests);
    if Test_ASN1_STRING_Basic then Inc(PassedTests);
    if Test_ASN1_STRING_Typed then Inc(PassedTests);
    if Test_ASN1_OCTET_STRING then Inc(PassedTests);
    if Test_ASN1_TIME then Inc(PassedTests);
    
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
  
  UnloadOpenSSLASN1;
  UnloadOpenSSLCore;
end.
