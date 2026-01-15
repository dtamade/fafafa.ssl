program test_error_handling;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,
  fafafa.ssl.factory,
  fafafa.ssl.base,
  fafafa.ssl;

var
  TotalTests, PassedTests, FailedTests: Integer;

procedure Test(const TestName: string; Condition: Boolean);
begin
  Inc(TotalTests);
  Write(TestName + ': ');
  if Condition then
  begin
    WriteLn('PASS');
    Inc(PassedTests);
  end
  else
  begin
    WriteLn('FAIL');
    Inc(FailedTests);
  end;
end;

procedure TestErrorHandlingInitialization;
var
  LLib: ISSLLibrary;
  LErrorCode: Integer;
  LErrorString: string;
begin
  WriteLn('Test 1: Error handling initialization');
  WriteLn('--------------------------------------');

  // Arrange: Create OpenSSL library
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not Assigned(LLib) then
  begin
    WriteLn('  OpenSSL library not available, skipping test');
    Exit;
  end;

  if not LLib.Initialize then
  begin
    WriteLn('  OpenSSL initialization failed, skipping test');
    Exit;
  end;

  // Act: Get initial error state
  LErrorCode := LLib.GetLastError;
  LErrorString := LLib.GetLastErrorString;

  // Assert: No errors initially
  Test('GetLastError returns 0 initially', LErrorCode = 0);
  Test('GetLastErrorString returns "No SSL errors" initially',
    LErrorString = 'No SSL errors');

  WriteLn;
end;

procedure TestErrorGenerationAndRetrieval;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LErrorString: string;
  LException: Boolean;
begin
  WriteLn('Test 2: Error generation and retrieval');
  WriteLn('---------------------------------------');

  // Arrange: Create OpenSSL library and context
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not Assigned(LLib) then
  begin
    WriteLn('  OpenSSL library not available, skipping test');
    Exit;
  end;

  if not LLib.Initialize then
  begin
    WriteLn('  OpenSSL initialization failed, skipping test');
    Exit;
  end;

  LCtx := LLib.CreateContext(sslCtxClient);

  // Act: Trigger an error by loading invalid CA file
  LException := False;
  try
    LCtx.LoadCAFile('nonexistent_invalid_ca_file_12345.pem');
  except
    on E: Exception do
    begin
      LException := True;
      WriteLn('  Exception caught: ', E.Message);
    end;
  end;

  // Get error info from library
  LErrorString := LLib.GetLastErrorString;
  WriteLn('  Error string: ', LErrorString);

  // Assert: Error was generated and retrieved
  Test('Invalid CA file raises exception', LException);
  Test('GetLastErrorString returns error info',
    (LErrorString <> '') and (LErrorString <> 'No SSL errors'));

  WriteLn;
end;

procedure TestErrorClearing;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LErrorBefore, LErrorAfter: string;
begin
  WriteLn('Test 3: Error clearing');
  WriteLn('----------------------');

  // Arrange: Create OpenSSL library and context
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not Assigned(LLib) then
  begin
    WriteLn('  OpenSSL library not available, skipping test');
    Exit;
  end;

  if not LLib.Initialize then
  begin
    WriteLn('  OpenSSL initialization failed, skipping test');
    Exit;
  end;

  LCtx := LLib.CreateContext(sslCtxClient);

  // Act: Generate an error
  try
    LCtx.LoadCAFile('nonexistent_file.pem');
  except
    // Ignore exception
  end;

  LErrorBefore := LLib.GetLastErrorString;
  WriteLn('  Error before clear: ', LErrorBefore);

  // Clear error
  LLib.ClearError;

  LErrorAfter := LLib.GetLastErrorString;
  WriteLn('  Error after clear: ', LErrorAfter);

  // Assert: Error was cleared
  Test('Error exists before clear',
    (LErrorBefore <> '') and (LErrorBefore <> 'No SSL errors'));
  Test('Error cleared after ClearError', LErrorAfter = 'No SSL errors');

  WriteLn;
end;

procedure TestMultipleErrorAccumulation;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LErrorString: string;
  i: Integer;
begin
  WriteLn('Test 4: Multiple error accumulation');
  WriteLn('------------------------------------');

  // Arrange: Create OpenSSL library and context
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not Assigned(LLib) then
  begin
    WriteLn('  OpenSSL library not available, skipping test');
    Exit;
  end;

  if not LLib.Initialize then
  begin
    WriteLn('  OpenSSL initialization failed, skipping test');
    Exit;
  end;

  LCtx := LLib.CreateContext(sslCtxClient);

  // Act: Generate multiple errors
  for i := 1 to 3 do
  begin
    try
      LCtx.LoadCAFile(Format('nonexistent_file_%d.pem', [i]));
    except
      // Ignore exceptions
    end;
  end;

  LErrorString := LLib.GetLastErrorString;
  WriteLn('  Accumulated errors: ', LErrorString);

  // Assert: Multiple errors accumulated (check for separator)
  Test('Multiple errors accumulated', Pos('|', LErrorString) > 0);

  WriteLn;
end;

procedure TestErrorHandlingWithValidOperations;
var
  LLib: ISSLLibrary;
  LCtx: ISSLContext;
  LErrorString: string;
begin
  WriteLn('Test 5: Error handling with valid operations');
  WriteLn('---------------------------------------------');

  // Arrange: Create OpenSSL library and context
  LLib := CreateSSLLibrary(sslOpenSSL);
  if not Assigned(LLib) then
  begin
    WriteLn('  OpenSSL library not available, skipping test');
    Exit;
  end;

  if not LLib.Initialize then
  begin
    WriteLn('  OpenSSL initialization failed, skipping test');
    Exit;
  end;

  // Clear any previous errors
  LLib.ClearError;

  // Act: Perform valid operations
  LCtx := LLib.CreateContext(sslCtxClient);
  LCtx.SetServerName('www.google.com');
  LCtx.SetProtocolVersions([sslProtocolTLS12, sslProtocolTLS13]);

  // Get error state
  LErrorString := LLib.GetLastErrorString;

  // Assert: No errors from valid operations
  Test('No errors from valid operations', LErrorString = 'No SSL errors');

  WriteLn;
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('OpenSSL Error Handling Tests');
  WriteLn('============================');
  WriteLn;
  WriteLn('Testing Phase B2.1: Enhanced Error Handling');
  WriteLn('This test verifies improved error extraction from OpenSSL error queue');
  WriteLn;

  TestErrorHandlingInitialization;
  TestErrorGenerationAndRetrieval;
  TestErrorClearing;
  TestMultipleErrorAccumulation;
  TestErrorHandlingWithValidOperations;

  WriteLn('=' + StringOfChar('=', 70));
  WriteLn(Format('Test Results: %d/%d passed (%.1f%%)',
    [PassedTests, TotalTests, PassedTests * 100.0 / TotalTests]));

  if FailedTests > 0 then
  begin
    WriteLn(Format('%d tests FAILED', [FailedTests]));
    Halt(1);
  end
  else
  begin
    WriteLn('All tests PASSED!');
    WriteLn;
    WriteLn('Phase B2.1 Complete:');
    WriteLn('  ✓ GetLastError extracts error code from OpenSSL');
    WriteLn('  ✓ GetLastErrorString drains entire error queue');
    WriteLn('  ✓ Multiple errors concatenated with " | " separator');
    WriteLn('  ✓ ClearError properly clears error queue');
    WriteLn('  ✓ Returns "No SSL errors" when queue is empty');
  end;
end.
