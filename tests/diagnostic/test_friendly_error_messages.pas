program test_friendly_error_messages;

{$mode objfpc}{$H+}{$J-}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.openssl.types,
  fafafa.ssl.openssl.api.err;

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

{ Helper to create OpenSSL error code from library and reason }
function MakeError(ALib, AReason: Integer): Cardinal;
begin
  Result := Cardinal((ALib shl 24) or (AReason and $FFF));
end;

procedure TestErrorCodeZero;
var
  LMessage: string;
begin
  WriteLn('Test 1: Error code zero (no error)');
  WriteLn('-----------------------------------');

  LMessage := GetFriendlyErrorMessage(0);
  WriteLn('  Message: ', LMessage);

  Test('Error code 0 returns simple "No error" message', LMessage = 'No error');

  WriteLn;
end;

procedure TestCertificateError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 2: Certificate error message');
  WriteLn('----------------------------------');

  // Create a certificate-related error (X509 library)
  LError := MakeError(ERR_LIB_X509, 1);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message contains category tag', Pos('[X.509]', LMessage) > 0);
  Test('Message contains "Problem:" line', Pos('Problem:', LMessage) > 0);
  Test('Message contains "Details:" line', Pos('Details:', LMessage) > 0);
  Test('Message contains "Suggestion:" line', Pos('Suggestion:', LMessage) > 0);
  Test('Message contains certificate suggestion', Pos('certificate', LowerCase(LMessage)) > 0);

  WriteLn;
end;

procedure TestMemoryError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 3: Memory error message');
  WriteLn('-----------------------------');

  // Create a memory error
  LError := MakeError(ERR_LIB_CRYPTO, ERR_R_MALLOC_FAILURE);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message identified as memory error', Pos('内存', LMessage) > 0);
  Test('Message contains memory-related suggestion',
    (Pos('memory', LowerCase(LMessage)) > 0) or (Pos('资源', LMessage) > 0));

  WriteLn;
end;

procedure TestProtocolError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 4: Protocol error message');
  WriteLn('-------------------------------');

  // Create a protocol error (SSL library)
  LError := MakeError(ERR_LIB_SSL, 1);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message contains SSL/TLS category', Pos('SSL/TLS', LMessage) > 0);
  Test('Message contains protocol suggestion',
    (Pos('protocol', LowerCase(LMessage)) > 0) or (Pos('TLS', LMessage) > 0));

  WriteLn;
end;

procedure TestIOError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 5: I/O error message');
  WriteLn('--------------------------');

  // Create an I/O error (System library)
  LError := MakeError(ERR_LIB_SYS, 1);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message contains System category', Pos('System', LMessage) > 0);
  Test('Message contains I/O suggestion',
    (Pos('network', LowerCase(LMessage)) > 0) or (Pos('I/O', LMessage) > 0));

  WriteLn;
end;

procedure TestInvalidParamError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 6: Invalid parameter error message');
  WriteLn('----------------------------------------');

  // Create invalid parameter error
  LError := MakeError(ERR_LIB_EVP, ERR_R_PASSED_NULL_PARAMETER);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message identified as invalid parameter', Pos('参数', LMessage) > 0);
  Test('Message contains parameter verification suggestion',
    Pos('parameters', LowerCase(LMessage)) > 0);

  WriteLn;
end;

procedure TestMessageFormatting;
var
  LError: Cardinal;
  LMessage: string;
  LLines: TStringList;
begin
  WriteLn('Test 7: Message formatting consistency');
  WriteLn('--------------------------------------');

  // Test that all error types produce well-formatted messages
  LError := MakeError(ERR_LIB_X509, 1);
  LMessage := GetFriendlyErrorMessage(LError);

  LLines := TStringList.Create;
  try
    LLines.Text := LMessage;

    WriteLn('  Lines in message: ', LLines.Count);

    Test('Message has multiple lines', LLines.Count >= 3);
    Test('First line has category and type', Pos('[', LLines[0]) > 0);
    Test('Message has indented content', Pos('  ', LMessage) > 0);

  finally
    LLines.Free;
  end;

  WriteLn;
end;

procedure TestLibraryNotFoundError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 8: Library not found error');
  WriteLn('--------------------------------');

  // Create library loading error
  LError := MakeError(ERR_LIB_ENGINE, 1);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message contains Engine category', Pos('Engine', LMessage) > 0);
  Test('Message contains library installation suggestion',
    Pos('libraries', LowerCase(LMessage)) > 0);

  WriteLn;
end;

procedure TestUnsupportedFeatureError;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 9: Unsupported feature error');
  WriteLn('----------------------------------');

  // Create unsupported feature error
  LError := MakeError(ERR_LIB_RSA, ERR_R_DISABLED);
  LMessage := GetFriendlyErrorMessage(LError);

  WriteLn('  Error code: ', LError);
  WriteLn('  Message:');
  WriteLn(LMessage);
  WriteLn;

  Test('Message identified as unsupported', Pos('不支持', LMessage) > 0);
  Test('Message mentions version or build options',
    (Pos('version', LowerCase(LMessage)) > 0) or (Pos('build', LowerCase(LMessage)) > 0));

  WriteLn;
end;

procedure TestMultipleCertificateLibraries;
var
  LError: Cardinal;
  LMessage: string;
begin
  WriteLn('Test 10: Different certificate-related libraries');
  WriteLn('-------------------------------------------------');

  // Test PKCS7
  LError := MakeError(ERR_LIB_PKCS7, 1);
  LMessage := GetFriendlyErrorMessage(LError);
  Test('PKCS#7 library produces certificate error message', Pos('PKCS#7', LMessage) > 0);

  // Test PKCS12
  LError := MakeError(ERR_LIB_PKCS12, 1);
  LMessage := GetFriendlyErrorMessage(LError);
  Test('PKCS#12 library produces certificate error message', Pos('PKCS#12', LMessage) > 0);

  // Test OCSP
  LError := MakeError(ERR_LIB_OCSP, 1);
  LMessage := GetFriendlyErrorMessage(LError);
  Test('OCSP library produces certificate error message', Pos('OCSP', LMessage) > 0);

  WriteLn;
end;

begin
  TotalTests := 0;
  PassedTests := 0;
  FailedTests := 0;

  WriteLn('User-Friendly Error Messages Tests');
  WriteLn('===================================');
  WriteLn;
  WriteLn('Testing Phase B2.3: GetFriendlyErrorMessage()');
  WriteLn('This test verifies user-friendly error message generation');
  WriteLn;

  TestErrorCodeZero;
  TestCertificateError;
  TestMemoryError;
  TestProtocolError;
  TestIOError;
  TestInvalidParamError;
  TestMessageFormatting;
  TestLibraryNotFoundError;
  TestUnsupportedFeatureError;
  TestMultipleCertificateLibraries;

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
    WriteLn('Phase B2.3 Complete:');
    WriteLn('  ✓ GetFriendlyErrorMessage() generates well-formatted messages');
    WriteLn('  ✓ Messages include category, problem, details, and suggestions');
    WriteLn('  ✓ Context-specific guidance provided for each error type');
    WriteLn('  ✓ All error categories produce appropriate messages');
    WriteLn('  ✓ Message formatting is consistent and readable');
  end;
end.
