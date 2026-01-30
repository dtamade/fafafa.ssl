program test_ct_sct_validation;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.ct.sct;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;

procedure StartTest(const TestName: string);
begin
  Inc(TotalTests);
  Write('[', TotalTests, '] ', TestName, '... ');
end;

procedure PassTest;
begin
  Inc(PassedTests);
  WriteLn('PASS');
end;

procedure FailTest(const Reason: string);
begin
  Inc(FailedTests);
  WriteLn('FAIL: ', Reason);
end;

procedure TestCreateDefaultValidationOptions;
var
  Options: TSCTValidationOptions;
begin
  StartTest('Create default validation options');
  try
    Options := CreateDefaultValidationOptions;
    
    if not Options.RequireValidSCTs then
      FailTest('RequireValidSCTs should be True')
    else if Options.MinimumSCTCount <> 2 then
      FailTest('MinimumSCTCount should be 2')
    else if Options.AllowUnknownLogs then
      FailTest('AllowUnknownLogs should be False')
    else if Options.ClockDriftTolerance <> 300000 then
      FailTest('ClockDriftTolerance should be 300000')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestSCTValidatorCreation;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
begin
  StartTest('Create SCT validator');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      if Validator = nil then
        FailTest('Validator is nil')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestGetSCTValidationStatusName;
begin
  StartTest('Get SCT validation status name');
  try
    if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_VALID) <> 'Valid' then
      FailTest('VALID status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_INVALID) <> 'Invalid' then
      FailTest('INVALID status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_UNKNOWN_LOG) <> 'Unknown Log' then
      FailTest('UNKNOWN_LOG status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_UNVERIFIED) <> 'Unverified' then
      FailTest('UNVERIFIED status name incorrect')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestFormatSCTTimestamp;
var
  Timestamp: UInt64;
  Formatted: string;
begin
  StartTest('Format SCT timestamp');
  try
    // 2024-01-01 00:00:00 UTC = 1704067200 seconds = 1704067200000 milliseconds
    Timestamp := 1704067200000;
    Formatted := FormatSCTTimestamp(Timestamp);
    
    if Formatted = '' then
      FailTest('Formatted timestamp is empty')
    else if Pos('2024', Formatted) = 0 then
      FailTest('Formatted timestamp does not contain year 2024')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLoadLogStore;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Success: Boolean;
begin
  StartTest('Load CT log store');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      // 尝试加载默认日志存储（可能失败，这是正常的）
      Success := Validator.LoadLogStore();
      
      // 无论成功与否，测试都应该通过（因为默认文件可能不存在）
      PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCheckPolicyWithNoSCTs;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy with no SCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 2;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 0);
      
      if Validator.CheckPolicy(Results) then
        FailTest('Policy should fail with no SCTs')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCheckPolicyWithInsufficientSCTs;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy with insufficient SCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 2;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 1);
      Results[0].IsValid := True;
      Results[0].Status := SCT_VALIDATION_STATUS_VALID;
      
      if Validator.CheckPolicy(Results) then
        FailTest('Policy should fail with only 1 SCT when 2 required')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCheckPolicyWithSufficientValidSCTs;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy with sufficient valid SCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 2;
    Options.RequireValidSCTs := True;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 2);
      Results[0].IsValid := True;
      Results[0].Status := SCT_VALIDATION_STATUS_VALID;
      Results[1].IsValid := True;
      Results[1].Status := SCT_VALIDATION_STATUS_VALID;
      
      if not Validator.CheckPolicy(Results) then
        FailTest('Policy should pass with 2 valid SCTs')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCheckPolicyWithMixedSCTs;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy with mixed valid/invalid SCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 2;
    Options.RequireValidSCTs := True;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 3);
      Results[0].IsValid := True;
      Results[0].Status := SCT_VALIDATION_STATUS_VALID;
      Results[1].IsValid := False;
      Results[1].Status := SCT_VALIDATION_STATUS_INVALID;
      Results[2].IsValid := True;
      Results[2].Status := SCT_VALIDATION_STATUS_VALID;
      
      if not Validator.CheckPolicy(Results) then
        FailTest('Policy should pass with 2 valid SCTs out of 3')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestValidateFromTLSWithNullSSL;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Validate from TLS with null SSL');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      Results := Validator.ValidateFromTLS(nil, nil, nil);
      
      if Length(Results) <> 0 then
        FailTest('Should return empty array for null SSL')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestValidateFromX509WithNullCert;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Validate from X509 with null cert');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      Results := Validator.ValidateFromX509(nil, nil);
      
      if Length(Results) <> 0 then
        FailTest('Should return empty array for null cert')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestValidateFromOCSPWithNullResp;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Validate from OCSP with null response');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      Results := Validator.ValidateFromOCSP(nil, nil, nil);
      
      if Length(Results) <> 0 then
        FailTest('Should return empty array for null OCSP response')
      else
        PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure PrintSummary;
begin
  WriteLn;
  WriteLn('=== Test Summary ===');
  WriteLn('Total tests: ', TotalTests);
  WriteLn('Passed: ', PassedTests);
  WriteLn('Failed: ', FailedTests);
  
  if FailedTests = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('Some tests failed!');
end;

begin
  WriteLn('=== SCT Validation Module Tests ===');
  WriteLn;
  
  // 加载 OpenSSL 函数
  try
    LoadOpenSSLCore;
    LoadOpenSSLX509;
    LoadOpenSSLBIO;
  except
    on E: Exception do
    begin
      WriteLn('ERROR: Failed to initialize OpenSSL: ', E.Message);
      Halt(1);
    end;
  end;
  
  // 运行测试
  TestCreateDefaultValidationOptions;
  TestSCTValidatorCreation;
  TestGetSCTValidationStatusName;
  TestFormatSCTTimestamp;
  TestLoadLogStore;
  TestCheckPolicyWithNoSCTs;
  TestCheckPolicyWithInsufficientSCTs;
  TestCheckPolicyWithSufficientValidSCTs;
  TestCheckPolicyWithMixedSCTs;
  TestValidateFromTLSWithNullSSL;
  TestValidateFromX509WithNullCert;
  TestValidateFromOCSPWithNullResp;
  
  PrintSummary;
  
  // 返回退出码
  if FailedTests > 0 then
    Halt(1)
  else
    Halt(0);
end.
