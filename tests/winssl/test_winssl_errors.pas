program test_winssl_errors;

{$mode objfpc}{$H+}

uses
  SysUtils, fafafa.ssl.winssl.errors, fafafa.ssl.winssl.base;

var
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;

procedure TestErrorMessage(const aTestName: string; aErrorCode: DWORD; const aExpected: string);
var
  LResult: string;
begin
  Inc(GTestCount);
  LResult := GetFriendlyErrorMessageCN(aErrorCode);
  
  if Pos(aExpected, LResult) > 0 then
  begin
    Inc(GPassCount);
    WriteLn('[PASS] ', aTestName);
  end
  else
  begin
    WriteLn('[FAIL] ', aTestName);
    WriteLn('       Expected to contain: ', aExpected);
    WriteLn('       Got: ', LResult);
  end;
end;

begin
  WriteLn('WinSSL Error Handling Test Suite');
  WriteLn;
  
  // 测试常见错误码映射
  TestErrorMessage('Test 1: SEC_E_OK', DWORD(SEC_E_OK), '成功');
  TestErrorMessage('Test 2: SEC_I_CONTINUE_NEEDED', DWORD(SEC_I_CONTINUE_NEEDED), '继续');
  TestErrorMessage('Test 3: SEC_E_INCOMPLETE_MESSAGE', DWORD(SEC_E_INCOMPLETE_MESSAGE), '不完整');
  TestErrorMessage('Test 4: CERT_E_EXPIRED', DWORD(CERT_E_EXPIRED), '过期');
  TestErrorMessage('Test 5: CERT_E_UNTRUSTEDROOT', DWORD(CERT_E_UNTRUSTEDROOT), '不受信任');
  TestErrorMessage('Test 6: CERT_E_REVOKED', DWORD(CERT_E_REVOKED), '吊销');
  
  // 测试未知错误码
  Inc(GTestCount);
  if Pos('未知错误', GetFriendlyErrorMessageCN($DEADBEEF)) > 0 then
  begin
    Inc(GPassCount);
    WriteLn('[PASS] Test 9: Unknown Error Code');
  end
  else
    WriteLn('[FAIL] Test 9: Unknown Error Code');
  
  WriteLn;
  WriteLn('=' + StringOfChar('=', 78));
  WriteLn(Format('Total: %d, Passed: %d, Failed: %d (%.1f%%)',
    [GTestCount, GPassCount, GTestCount - GPassCount,
     (GPassCount / GTestCount) * 100.0]));
  WriteLn('=' + StringOfChar('=', 78));
  
  if GPassCount < GTestCount then
    Halt(1);
end.

