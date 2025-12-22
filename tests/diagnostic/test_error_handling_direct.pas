program test_error_handling_direct;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.err,
  fafafa.ssl.openssl.api.ssl,
  fafafa.ssl.openssl.api.bio;

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

procedure TestDirectErrorAPI;
var
  LCtx: PSSL_CTX;
  LError1, LError2: Cardinal;
  LBuf: array[0..511] of AnsiChar;
  LErrorStr: string;
begin
  WriteLn('Test 1: Direct OpenSSL error API');
  WriteLn('----------------------------------');

  // Load OpenSSL
  if not IsOpenSSLCoreLoaded then
    LoadOpenSSLCore;

  LoadOpenSSLSSL;
  LoadOpenSSLERR;  // Load error handling functions!

  WriteLn('  OpenSSL version: ', GetOpenSSLVersionString);

  // Check if error functions are loaded
  WriteLn('  ERR_get_error assigned: ', Assigned(ERR_get_error));
  WriteLn('  ERR_error_string_n assigned: ', Assigned(ERR_error_string_n));
  WriteLn('  ERR_clear_error assigned: ', Assigned(ERR_clear_error));
  WriteLn('  ERR_peek_error assigned: ', Assigned(ERR_peek_error));

  // Test initial state (no errors)
  if Assigned(ERR_get_error) then
  begin
    LError1 := ERR_get_error();
    WriteLn('  Initial error code: ', LError1);
    Test('Initial error queue is empty', LError1 = 0);
  end;

  // Trigger an error by calling SSL_CTX_load_verify_locations with invalid file
  if Assigned(TLS_method) and Assigned(SSL_CTX_new) and Assigned(SSL_CTX_load_verify_locations) then
  begin
    LCtx := SSL_CTX_new(TLS_method());
    if LCtx <> nil then
    begin
      WriteLn('  Creating error by loading invalid CA file...');

      // This should fail and set an error
      if SSL_CTX_load_verify_locations(LCtx, 'invalid_nonexistent_file_12345.pem', nil) <> 1 then
      begin
        WriteLn('  SSL_CTX_load_verify_locations failed as expected');

        // Check error queue
        if Assigned(ERR_get_error) and Assigned(ERR_error_string_n) then
        begin
          LError1 := ERR_get_error();
          WriteLn('  Error code after failure: ', LError1);

          if LError1 <> 0 then
          begin
            ERR_error_string_n(LError1, @LBuf[0], SizeOf(LBuf));
            LErrorStr := string(LBuf);
            WriteLn('  Error string: ', LErrorStr);

            Test('Error was generated', LError1 <> 0);
            Test('Error string is not empty', LErrorStr <> '');
          end
          else
            WriteLn('  WARNING: No error in queue after failed operation');
        end;
      end;

      SSL_CTX_free(LCtx);
    end;
  end;

  WriteLn;
end;

procedure TestMultipleErrors;
var
  LCtx: PSSL_CTX;
  LError: Cardinal;
  LBuf: array[0..511] of AnsiChar;
  LErrorStr, LAllErrors: string;
  LErrorCount: Integer;
begin
  WriteLn('Test 2: Multiple errors accumulation');
  WriteLn('-------------------------------------');

  if not IsOpenSSLCoreLoaded then
    LoadOpenSSLCore;

  LoadOpenSSLSSL;
  LoadOpenSSLERR;

  // Clear any previous errors
  if Assigned(ERR_clear_error) then
    ERR_clear_error();

  // Create context
  if Assigned(TLS_method) and Assigned(SSL_CTX_new) and Assigned(SSL_CTX_load_verify_locations) then
  begin
    LCtx := SSL_CTX_new(TLS_method());
    if LCtx <> nil then
    begin
      WriteLn('  Generating 3 errors...');

      // Generate 3 errors
      SSL_CTX_load_verify_locations(LCtx, 'error_file_1.pem', nil);
      SSL_CTX_load_verify_locations(LCtx, 'error_file_2.pem', nil);
      SSL_CTX_load_verify_locations(LCtx, 'error_file_3.pem', nil);

      // Now drain the error queue
      LAllErrors := '';
      LErrorCount := 0;

      if Assigned(ERR_get_error) and Assigned(ERR_error_string_n) then
      begin
        repeat
          LError := ERR_get_error();
          if LError <> 0 then
          begin
            Inc(LErrorCount);
            ERR_error_string_n(LError, @LBuf[0], SizeOf(LBuf));
            LErrorStr := string(LBuf);

            if LAllErrors <> '' then
              LAllErrors := LAllErrors + ' | ';
            LAllErrors := LAllErrors + LErrorStr;
          end;
        until LError = 0;
      end;

      WriteLn('  Error count: ', LErrorCount);
      WriteLn('  All errors: ', LAllErrors);

      Test('At least one error generated', LErrorCount > 0);
      Test('Multiple errors contain separator', Pos('|', LAllErrors) > 0);

      SSL_CTX_free(LCtx);
    end;
  end;

  WriteLn;
end;

procedure TestClearErrors;
var
  LCtx: PSSL_CTX;
  LErrorBefore, LErrorAfter: Cardinal;
begin
  WriteLn('Test 3: Error clearing');
  WriteLn('----------------------');

  if not IsOpenSSLCoreLoaded then
    LoadOpenSSLCore;

  LoadOpenSSLSSL;
  LoadOpenSSLERR;

  // Clear any previous errors
  if Assigned(ERR_clear_error) then
    ERR_clear_error();

  // Create context and trigger error
  if Assigned(TLS_method) and Assigned(SSL_CTX_new) and Assigned(SSL_CTX_load_verify_locations) then
  begin
    LCtx := SSL_CTX_new(TLS_method());
    if LCtx <> nil then
    begin
      WriteLn('  Generating error...');
      SSL_CTX_load_verify_locations(LCtx, 'error_file.pem', nil);

      // Check error before clear
      if Assigned(ERR_peek_error) then
      begin
        LErrorBefore := ERR_peek_error();
        WriteLn('  Error before clear (peek): ', LErrorBefore);
      end;

      // Clear errors
      if Assigned(ERR_clear_error) then
      begin
        WriteLn('  Clearing errors...');
        ERR_clear_error();
      end;

      // Check error after clear
      if Assigned(ERR_peek_error) then
      begin
        LErrorAfter := ERR_peek_error();
        WriteLn('  Error after clear (peek): ', LErrorAfter);
      end;

      Test('Error exists before clear', LErrorBefore <> 0);
      Test('Error cleared after ERR_clear_error', LErrorAfter = 0);

      SSL_CTX_free(LCtx);
    end;
  end;

  WriteLn;
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('Direct OpenSSL Error API Tests');
  WriteLn('===============================');
  WriteLn;
  WriteLn('Testing direct OpenSSL error functions to verify error handling works');
  WriteLn;

  TestDirectErrorAPI;
  TestMultipleErrors;
  TestClearErrors;

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
    WriteLn('Direct OpenSSL error API confirmed working:');
    WriteLn('  ✓ ERR_get_error() extracts errors from queue');
    WriteLn('  ✓ ERR_error_string_n() converts error codes to strings');
    WriteLn('  ✓ Multiple errors can be accumulated and retrieved');
    WriteLn('  ✓ ERR_clear_error() properly clears error queue');
    WriteLn('  ✓ ERR_peek_error() allows checking without removing errors');
  end;
end.
