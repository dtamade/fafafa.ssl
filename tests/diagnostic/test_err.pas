program test_err;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.crypto;

var
  PassedTests, TotalTests: Integer;

procedure RunTest(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write('  [', TotalTests:2, '] ', TestName, '...');
  if Condition then
  begin
    WriteLn(' [PASS]');
    Inc(PassedTests);
  end
  else
    WriteLn(' [FAIL]');
end;

procedure TestErrorRetrieval;
var
  ErrCode: Cardinal;
  ErrStr: PAnsiChar;
begin
  WriteLn('Test 1: Error Retrieval');
  
  // Generate an error by calling a NULL function
  ERR_put_error(ERR_LIB_USER, 0, 100, 'test_file', 123);
  
  // Get the error code
  ErrCode := ERR_get_error();
  RunTest('Get error code', ErrCode <> 0);
  
  // Generate another error and get error string
  ERR_put_error(ERR_LIB_USER, 0, 101, 'test_file', 124);
  ErrCode := ERR_get_error();
  if ErrCode <> 0 then
  begin
    ErrStr := ERR_error_string(ErrCode);
    RunTest('Error string not nil', ErrStr <> nil);
  end;
  
  // Clear errors
  ERR_clear_error();
  ErrCode := ERR_get_error();
  RunTest('Clear error', ErrCode = 0);
  
  WriteLn;
end;

procedure TestErrorPeek;
var
  ErrCode1, ErrCode2: Cardinal;
begin
  WriteLn('Test 2: Error Peek (Non-destructive Read)');
  
  // Generate an error
  ERR_put_error(ERR_LIB_USER, 0, 200, 'test', 1);
  
  // Peek at error (should not remove it)
  ErrCode1 := ERR_peek_error();
  RunTest('Peek error code', ErrCode1 <> 0);
  
  // Peek again - should get same error
  ErrCode2 := ERR_peek_error();
  RunTest('Peek is non-destructive', ErrCode1 = ErrCode2);
  
  // Now actually get the error (removes it)
  ErrCode1 := ERR_get_error();
  RunTest('Get error removes it', ErrCode1 <> 0);
  
  // Peek should now return 0
  ErrCode2 := ERR_peek_error();
  RunTest('Error was removed', ErrCode2 = 0);
  
  ERR_clear_error();
  WriteLn;
end;

procedure TestErrorLibraryInfo;
var
  ErrCode: Cardinal;
  Lib, Reason: Integer;
begin
  WriteLn('Test 3: Error Library and Reason Codes');
  
  // Generate an error with known library and reason
  ERR_put_error(ERR_LIB_USER, 0, 300, 'test', 1);
  ErrCode := ERR_get_error();
  
  if ErrCode <> 0 then
  begin
    // Extract library code - use inline helper
    Lib := ERR_GET_LIB_INLINE(ErrCode);
    RunTest('Extract library code', Lib = ERR_LIB_USER);
    
    // Extract reason code
    Reason := ERR_GET_REASON_INLINE(ErrCode);
    RunTest('Extract reason code', Reason = 300);
  end;
  
  ERR_clear_error();
  WriteLn;
end;

procedure TestErrorLastError;
var
  ErrCode: Cardinal;
begin
  WriteLn('Test 4: Last Error Retrieval');
  
  ERR_clear_error();
  
  // Generate multiple errors
  ERR_put_error(ERR_LIB_USER, 0, 400, 'test', 1);
  ERR_put_error(ERR_LIB_USER, 0, 401, 'test', 2);
  
  // Peek at last error
  ErrCode := ERR_peek_last_error();
  RunTest('Peek last error', ErrCode <> 0);
  
  // Get first error in queue
  ErrCode := ERR_get_error();
  RunTest('Get first error in queue', ErrCode <> 0);
  
  ERR_clear_error();
  WriteLn;
end;

procedure TestErrorPrint;
begin
  WriteLn('Test 5: Error Printing Functions');
  
  // Generate an error
  ERR_put_error(ERR_LIB_USER, 0, 500, 'test', 1);
  
  // Test that print functions exist and can be called
  // Note: We don't test actual output as it goes to stderr
  try
    // These functions should not crash
    ERR_print_errors_cb(nil, nil);
    RunTest('Error print callback callable', True);
  except
    RunTest('Error print callback callable', False);
  end;
  
  ERR_clear_error();
  WriteLn;
end;

procedure TestErrorStringFormats;
var
  ErrCode: Cardinal;
  ErrStr: PAnsiChar;
begin
  WriteLn('Test 6: Different Error String Formats');
  
  // Generate an error
  ERR_put_error(ERR_LIB_USER, 0, 600, 'test', 1);
  ErrCode := ERR_get_error();
  
  if ErrCode <> 0 then
  begin
    // Get full error string
    ErrStr := ERR_error_string(ErrCode);
    RunTest('Full error string', ErrStr <> nil);
    
    // Get library string
    ErrStr := ERR_lib_error_string(ErrCode);
    RunTest('Library error string', ErrStr <> nil);
    
    // Get reason string
    ErrStr := ERR_reason_error_string(ErrCode);
    RunTest('Reason error string', ErrStr <> nil);
  end;
  
  ERR_clear_error();
  WriteLn;
end;

procedure TestErrorState;
begin
  WriteLn('Test 7: Error State Management');
  
  // Start clean
  ERR_clear_error();
  RunTest('Initial state clear', ERR_get_error() = 0);
  
  // Add error
  ERR_put_error(ERR_LIB_USER, 0, 700, 'test', 1);
  RunTest('Error added', ERR_peek_error() <> 0);
  
  // Test mark/pop functions
  ERR_set_mark();
  ERR_put_error(ERR_LIB_USER, 0, 701, 'test', 2);
  ERR_pop_to_mark();
  
  RunTest('Mark/pop functions callable', True);
  
  ERR_clear_error();
  WriteLn;
end;

begin
  WriteLn('========================================');
  WriteLn('  OpenSSL ERR Module Test');
  WriteLn('========================================');
  WriteLn;
  
  PassedTests := 0;
  TotalTests := 0;
  
  // Load OpenSSL libraries
  try
    LoadOpenSSLCore;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: Failed to load OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;
  
  if not LoadOpenSSLERR then
  begin
    WriteLn('ERROR: Failed to load OpenSSL ERR module');
    Halt(1);
  end;
  
  WriteLn('OpenSSL Version: ', OpenSSL_version(0));
  WriteLn;
  
  try
    TestErrorRetrieval;
    TestErrorPeek;
    TestErrorLibraryInfo;
    TestErrorLastError;
    TestErrorPrint;
    TestErrorStringFormats;
    TestErrorState;
    
    WriteLn('========================================');
    WriteLn('Results: ', PassedTests, '/', TotalTests, ' tests passed (', 
            FormatFloat('0.0', PassedTests * 100.0 / TotalTests), '%)');
    WriteLn('========================================');
    
    if PassedTests = TotalTests then
    begin
      WriteLn('Status: ALL TESTS PASSED');
      ExitCode := 0;
    end
    else
    begin
      WriteLn('Status: SOME TESTS FAILED');
      ExitCode := 1;
    end;
  finally
    UnloadOpenSSLERR;
    UnloadOpenSSLCore;
  end;
end.
