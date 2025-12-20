program test_winssl_cert_verify_ex;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.base,
  fafafa.ssl.winssl.types;

type
  TTestResult = record
    TestName: string;
    Passed: Boolean;
    Message: string;
  end;

var
  GResults: array of TTestResult;
  GTestCount: Integer = 0;
  GPassCount: Integer = 0;

procedure AddTestResult(const aTestName: string; aPassed: Boolean; const aMessage: string);
begin
  SetLength(GResults, Length(GResults) + 1);
  with GResults[High(GResults)] do
  begin
    TestName := aTestName;
    Passed := aPassed;
    Message := aMessage;
  end;
  Inc(GTestCount);
  if aPassed then
    Inc(GPassCount);
end;

procedure PrintResults;
var
  i: Integer;
begin
  WriteLn;
  WriteLn('=' + StringOfChar('=', 78));
  WriteLn('WinSSL Certificate VerifyEx Test Results');
  WriteLn('=' + StringOfChar('=', 78));
  WriteLn;
  
  for i := 0 to High(GResults) do
  begin
    if GResults[i].Passed then
      Write('[PASS] ')
    else
      Write('[FAIL] ');
    WriteLn(GResults[i].TestName);
    if GResults[i].Message <> '' then
      WriteLn('       ', GResults[i].Message);
  end;
  
  WriteLn;
  WriteLn('=' + StringOfChar('=', 78));
  WriteLn(Format('Total: %d, Passed: %d, Failed: %d (%.1f%%)',
    [GTestCount, GPassCount, GTestCount - GPassCount,
     (GPassCount / GTestCount) * 100.0]));
  WriteLn('=' + StringOfChar('=', 78));
end;

// Test 1: 验证标志类型定义
procedure TestVerifyFlagTypes;
var
  LFlags: TSSLCertVerifyFlags;
begin
  try
    // 测试标志组合
    LFlags := [];
    LFlags := [sslCertVerifyDefault];
    LFlags := [sslCertVerifyCheckRevocation];
    LFlags := [sslCertVerifyCheckOCSP];
    LFlags := [sslCertVerifyCheckCRL];
    LFlags := [sslCertVerifyCheckRevocation, sslCertVerifyCheckCRL];
    
    AddTestResult('Test 1: Verify Flag Types', True,
      'All TSSLCertVerifyFlag types can be used correctly');
  except
    on E: Exception do
      AddTestResult('Test 1: Verify Flag Types', False, 'Exception: ' + E.Message);
  end;
end;

// Test 2: 验证标志枚举值
procedure TestVerifyFlagValues;
var
  LTestPassed: Boolean;
begin
  try
    // 测试所有枚举值
    LTestPassed := (Ord(sslCertVerifyDefault) = 0) and
                   (Ord(sslCertVerifyCheckRevocation) = 1) and
                   (Ord(sslCertVerifyCheckOCSP) = 2) and
                   (Ord(sslCertVerifyIgnoreExpiry) = 3) and
                   (Ord(sslCertVerifyIgnoreHostname) = 4) and
                   (Ord(sslCertVerifyAllowSelfSigned) = 5) and
                   (Ord(sslCertVerifyStrictChain) = 6) and
                   (Ord(sslCertVerifyCheckCRL) = 7);
    
    AddTestResult('Test 2: Verify Flag Values', LTestPassed,
      'All 8 verify flags are properly defined');
  except
    on E: Exception do
      AddTestResult('Test 2: Verify Flag Values', False, 'Exception: ' + E.Message);
  end;
end;

// Test 3: 验证结果结构
procedure TestVerifyResultStructure;
var
  LResult: TSSLCertVerifyResult;
begin
  try
    // 测试结构初始化
    FillChar(LResult, SizeOf(LResult), 0);
    LResult.Success := True;
    LResult.ErrorCode := 0;
    LResult.ErrorMessage := 'Test message';
    LResult.ChainStatus := 0;
    LResult.RevocationStatus := 0;
    LResult.DetailedInfo := 'Test detailed info';
    
    AddTestResult('Test 3: Verify Result Structure', True,
      'Structure fields can be accessed correctly');
  except
    on E: Exception do
      AddTestResult('Test 3: Verify Result Structure', False, 'Exception: ' + E.Message);
  end;
end;

// Test 4: 错误消息映射
procedure TestErrorMessageMapping;
var
  LTestPassed: Boolean;
begin
  try
    // 测试错误码常量定义
    LTestPassed := (CERT_E_EXPIRED <> 0) and
                   (CERT_E_UNTRUSTEDROOT <> 0) and
                   (CERT_E_REVOKED <> 0) and
                   (CERT_E_REVOCATION_FAILURE <> 0) and
                   (TRUST_E_CERT_SIGNATURE <> 0);
    
    AddTestResult('Test 4: Error Message Mapping', LTestPassed,
      'Error code constants are properly defined');
  except
    on E: Exception do
      AddTestResult('Test 4: Error Message Mapping', False, 'Exception: ' + E.Message);
  end;
end;

// Test 5: 证书链检查标志常量
procedure TestChainFlags;
var
  LTestPassed: Boolean;
begin
  try
    // 测试链检查标志常量定义
    LTestPassed := (CERT_CHAIN_REVOCATION_CHECK_END_CERT <> 0) and
                   (CERT_CHAIN_REVOCATION_CHECK_CHAIN <> 0) and
                   (CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT <> 0);
    
    AddTestResult('Test 5: Chain Flags', LTestPassed,
      Format('Revocation check flags: 0x%x, 0x%x, 0x%x',
        [CERT_CHAIN_REVOCATION_CHECK_END_CERT,
         CERT_CHAIN_REVOCATION_CHECK_CHAIN,
         CERT_CHAIN_REVOCATION_CHECK_CHAIN_EXCLUDE_ROOT]));
  except
    on E: Exception do
      AddTestResult('Test 5: Chain Flags', False, 'Exception: ' + E.Message);
  end;
end;

begin
  WriteLn('WinSSL Certificate VerifyEx Test Suite');
  WriteLn('Testing enhanced certificate verification functionality...');
  WriteLn;
  
  TestVerifyFlagTypes;
  TestVerifyFlagValues;
  TestVerifyResultStructure;
  TestErrorMessageMapping;
  TestChainFlags;
  
  PrintResults;
  
  if GPassCount < GTestCount then
    Halt(1);
end.

