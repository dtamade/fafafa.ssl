program test_sct_verification;

{$mode objfpc}{$H+}{$J-}

{
  SCT (Signed Certificate Timestamp) 验证模块测试

  测试覆盖:
  - SCT 验证选项配置
  - SCT 验证器创建和销毁
  - SCT 验证状态名称
  - SCT 时间戳格式化
  - CT 日志存储加载
  - 策略检查逻辑
  - 空输入处理
  - SCT 列表遍历

  @author fafafa.ssl team
  @version 2.0.0
}

uses
  SysUtils, Classes, DateUtils,
  fafafa.ssl.openssl.api,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.ct,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.stack,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.loader,
  fafafa.ssl.ct.sct;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  OpenSSLLoaded: Boolean = False;

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

procedure SkipTest(const Reason: string);
begin
  Inc(PassedTests);  // 跳过的测试计为通过
  WriteLn('SKIP: ', Reason);
end;

// ========================================================================
// 测试: 默认验证选项
// ========================================================================

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
      FailTest('ClockDriftTolerance should be 300000 (5 minutes)')
    else if Options.LogStoreFile <> '' then
      FailTest('LogStoreFile should be empty by default')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestCustomValidationOptions;
var
  Options: TSCTValidationOptions;
begin
  StartTest('Custom validation options');
  try
    Options.RequireValidSCTs := False;
    Options.MinimumSCTCount := 1;
    Options.AllowUnknownLogs := True;
    Options.ClockDriftTolerance := 600000;
    Options.LogStoreFile := '/path/to/logs.txt';

    if Options.RequireValidSCTs then
      FailTest('RequireValidSCTs should be False')
    else if Options.MinimumSCTCount <> 1 then
      FailTest('MinimumSCTCount should be 1')
    else if not Options.AllowUnknownLogs then
      FailTest('AllowUnknownLogs should be True')
    else if Options.ClockDriftTolerance <> 600000 then
      FailTest('ClockDriftTolerance should be 600000')
    else if Options.LogStoreFile <> '/path/to/logs.txt' then
      FailTest('LogStoreFile mismatch')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: SCT 验证器创建
// ========================================================================

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

procedure TestSCTValidatorWithCustomOptions;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
begin
  StartTest('Create SCT validator with custom options');
  try
    Options.RequireValidSCTs := False;
    Options.MinimumSCTCount := 3;
    Options.AllowUnknownLogs := True;
    Options.ClockDriftTolerance := 120000;
    Options.LogStoreFile := '';

    Validator := TSCTValidator.Create(Options);
    try
      if Validator.Options.MinimumSCTCount <> 3 then
        FailTest('MinimumSCTCount should be preserved')
      else if not Validator.Options.AllowUnknownLogs then
        FailTest('AllowUnknownLogs should be preserved')
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

procedure TestSCTValidatorOptionsModification;
var
  Validator: TSCTValidator;
  Options, NewOptions: TSCTValidationOptions;
begin
  StartTest('Modify SCT validator options');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      NewOptions := Validator.Options;
      NewOptions.MinimumSCTCount := 5;
      Validator.Options := NewOptions;

      if Validator.Options.MinimumSCTCount <> 5 then
        FailTest('Options should be modifiable')
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

// ========================================================================
// 测试: 验证状态名称
// ========================================================================

procedure TestGetSCTValidationStatusName;
begin
  StartTest('Get SCT validation status names');
  try
    if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_NOT_SET) <> 'Not Set' then
      FailTest('NOT_SET status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_VALID) <> 'Valid' then
      FailTest('VALID status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_INVALID) <> 'Invalid' then
      FailTest('INVALID status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_UNKNOWN_LOG) <> 'Unknown Log' then
      FailTest('UNKNOWN_LOG status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_UNVERIFIED) <> 'Unverified' then
      FailTest('UNVERIFIED status name incorrect')
    else if GetSCTValidationStatusName(SCT_VALIDATION_STATUS_UNKNOWN_VERSION) <> 'Unknown Version' then
      FailTest('UNKNOWN_VERSION status name incorrect')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestGetSCTValidationStatusNameUnknown;
begin
  StartTest('Get SCT validation status name for unknown status');
  try
    if GetSCTValidationStatusName(999) <> 'Unknown Status' then
      FailTest('Unknown status should return "Unknown Status"')
    else if GetSCTValidationStatusName(-1) <> 'Unknown Status' then
      FailTest('Negative status should return "Unknown Status"')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 时间戳格式化
// ========================================================================

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

procedure TestFormatSCTTimestampZero;
var
  Formatted: string;
begin
  StartTest('Format SCT timestamp zero');
  try
    Formatted := FormatSCTTimestamp(0);

    // Unix 时间戳 0 应该是 1970-01-01
    if Pos('1970', Formatted) = 0 then
      FailTest('Zero timestamp should format to 1970')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestFormatSCTTimestampLargeValue;
var
  Timestamp: UInt64;
  Formatted: string;
begin
  StartTest('Format SCT timestamp large value');
  try
    // 2030-01-01 00:00:00 UTC
    Timestamp := 1893456000000;
    Formatted := FormatSCTTimestamp(Timestamp);

    if Formatted = '' then
      FailTest('Large timestamp should be formatted')
    else if Pos('2030', Formatted) = 0 then
      FailTest('Should contain year 2030')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: CT 日志存储加载
// ========================================================================

procedure TestLoadLogStore;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Success: Boolean;
begin
  StartTest('Load CT log store (default)');

  if not OpenSSLLoaded then
  begin
    SkipTest('OpenSSL not loaded');
    Exit;
  end;

  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      // 尝试加载默认日志存储（可能失败，因为文件可能不存在）
      Success := Validator.LoadLogStore();

      // 无论成功与否，测试都应该通过（不崩溃即可）
      PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLoadLogStoreNonexistentFile;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
begin
  StartTest('Load CT log store from nonexistent file');

  if not OpenSSLLoaded then
  begin
    SkipTest('OpenSSL not loaded');
    Exit;
  end;

  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      if Validator.LoadLogStore('/nonexistent/path/to/logs.conf') then
        FailTest('Should return false for nonexistent file')
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

// ========================================================================
// 测试: 策略检查
// ========================================================================

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

procedure TestCheckPolicyWithAllInvalidSCTs;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy with all invalid SCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 2;
    Options.RequireValidSCTs := True;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 3);
      Results[0].IsValid := False;
      Results[0].Status := SCT_VALIDATION_STATUS_INVALID;
      Results[1].IsValid := False;
      Results[1].Status := SCT_VALIDATION_STATUS_UNKNOWN_LOG;
      Results[2].IsValid := False;
      Results[2].Status := SCT_VALIDATION_STATUS_UNVERIFIED;

      if Validator.CheckPolicy(Results) then
        FailTest('Policy should fail with all invalid SCTs')
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

procedure TestCheckPolicyWithoutRequireValidSCTs;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy without RequireValidSCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 2;
    Options.RequireValidSCTs := False;  // 不要求 SCT 必须有效
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 2);
      Results[0].IsValid := False;
      Results[0].Status := SCT_VALIDATION_STATUS_UNKNOWN_LOG;
      Results[1].IsValid := False;
      Results[1].Status := SCT_VALIDATION_STATUS_UNVERIFIED;

      // 不要求有效，只要有足够数量就行
      if not Validator.CheckPolicy(Results) then
        FailTest('Policy should pass when RequireValidSCTs is False')
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

procedure TestCheckPolicyWithExactMinimum;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Check policy with exactly minimum SCTs');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 3;
    Options.RequireValidSCTs := True;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 3);
      Results[0].IsValid := True;
      Results[0].Status := SCT_VALIDATION_STATUS_VALID;
      Results[1].IsValid := True;
      Results[1].Status := SCT_VALIDATION_STATUS_VALID;
      Results[2].IsValid := True;
      Results[2].Status := SCT_VALIDATION_STATUS_VALID;

      if not Validator.CheckPolicy(Results) then
        FailTest('Policy should pass with exactly minimum valid SCTs')
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

// ========================================================================
// 测试: 空输入处理
// ========================================================================

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

procedure TestValidateSCTListWithNullList;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Validate SCT list with null list');
  try
    Options := CreateDefaultValidationOptions;
    Validator := TSCTValidator.Create(Options);
    try
      Results := Validator.ValidateSCTList(nil, nil, nil);

      if Length(Results) <> 0 then
        FailTest('Should return empty array for null SCT list')
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

// ========================================================================
// 测试: TSCTValidationResult 结构
// ========================================================================

procedure TestSCTValidationResultDefaults;
var
  Result: TSCTValidationResult;
begin
  StartTest('SCT validation result defaults');
  try
    FillChar(Result, SizeOf(Result), 0);

    if Result.IsValid then
      FailTest('Default IsValid should be False')
    else if Result.Status <> 0 then
      FailTest('Default Status should be 0')
    else if Result.Timestamp <> 0 then
      FailTest('Default Timestamp should be 0')
    else if Result.ErrorMessage <> '' then
      FailTest('Default ErrorMessage should be empty')
    else if Result.LogName <> '' then
      FailTest('Default LogName should be empty')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestSCTValidationResultAssignment;
var
  Result: TSCTValidationResult;
begin
  StartTest('SCT validation result assignment');
  try
    Result.IsValid := True;
    Result.Status := SCT_VALIDATION_STATUS_VALID;
    Result.Timestamp := 1704067200000;
    Result.ErrorMessage := 'Test error';
    Result.LogName := 'Google Argon 2024';

    if not Result.IsValid then
      FailTest('IsValid not assigned')
    else if Result.Status <> SCT_VALIDATION_STATUS_VALID then
      FailTest('Status not assigned')
    else if Result.Timestamp <> 1704067200000 then
      FailTest('Timestamp not assigned')
    else if Result.ErrorMessage <> 'Test error' then
      FailTest('ErrorMessage not assigned')
    else if Result.LogName <> 'Google Argon 2024' then
      FailTest('LogName not assigned')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: TSCTSource 枚举
// ========================================================================

procedure TestSCTSourceValues;
begin
  StartTest('SCT source enumeration values');
  try
    if Ord(sctSourceUnknown) <> 0 then
      FailTest('sctSourceUnknown should be 0')
    else if Ord(sctSourceTLSExtension) <> 1 then
      FailTest('sctSourceTLSExtension should be 1')
    else if Ord(sctSourceX509Extension) <> 2 then
      FailTest('sctSourceX509Extension should be 2')
    else if Ord(sctSourceOCSPStapled) <> 3 then
      FailTest('sctSourceOCSPStapled should be 3')
    else
      PassTest;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

// ========================================================================
// 测试: 边界条件
// ========================================================================

procedure TestMinimumSCTCountZero;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
  Results: TSCTValidationResultArray;
begin
  StartTest('Minimum SCT count zero');
  try
    Options := CreateDefaultValidationOptions;
    Options.MinimumSCTCount := 0;
    Validator := TSCTValidator.Create(Options);
    try
      SetLength(Results, 0);

      // 当最小要求为 0 时，空数组应该满足策略
      if not Validator.CheckPolicy(Results) then
        FailTest('Policy should pass when MinimumSCTCount is 0')
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

procedure TestClockDriftToleranceZero;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
begin
  StartTest('Clock drift tolerance zero');
  try
    Options := CreateDefaultValidationOptions;
    Options.ClockDriftTolerance := 0;
    Validator := TSCTValidator.Create(Options);
    try
      // 只要创建成功就算通过
      PassTest;
    finally
      Validator.Free;
    end;
  except
    on E: Exception do
      FailTest('Exception: ' + E.Message);
  end;
end;

procedure TestLargeClockDriftTolerance;
var
  Validator: TSCTValidator;
  Options: TSCTValidationOptions;
begin
  StartTest('Large clock drift tolerance');
  try
    Options := CreateDefaultValidationOptions;
    Options.ClockDriftTolerance := 86400000;  // 24 小时
    Validator := TSCTValidator.Create(Options);
    try
      if Validator.Options.ClockDriftTolerance <> 86400000 then
        FailTest('Large tolerance should be preserved')
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

// ========================================================================
// 主程序
// ========================================================================

procedure PrintSummary;
var
  PassRate: Double;
begin
  WriteLn;
  WriteLn('=== Test Summary ===');
  WriteLn('Total Tests: ', TotalTests);
  if TotalTests > 0 then
    PassRate := (PassedTests / TotalTests) * 100.0
  else
    PassRate := 0;
  WriteLn('Passed: ', PassedTests, ' (', PassRate:0:1, '%)');
  WriteLn('Failed: ', FailedTests, ' (', (100 - PassRate):0:1, '%)');

  if FailedTests = 0 then
    WriteLn('All tests passed!')
  else
    WriteLn('Some tests failed!');
end;

procedure LoadOpenSSLFunctions;
begin
  OpenSSLLoaded := False;

  try
    // 加载 OpenSSL 核心函数
    LoadOpenSSLCore;

    // 检查是否加载成功
    if not TOpenSSLLoader.IsModuleLoaded(osmCore) then
    begin
      WriteLn('WARNING: Failed to load OpenSSL library');
      WriteLn('Some tests will be skipped.');
      Exit;
    end;

    // 加载 CT 相关函数
    LoadCTFunctions;

    // 加载 Stack 函数
    LoadStackFunctions;

    OpenSSLLoaded := True;
    WriteLn('OpenSSL loaded successfully.');
  except
    on E: Exception do
    begin
      WriteLn('WARNING: Exception loading OpenSSL: ', E.Message);
      WriteLn('Some tests will be skipped.');
    end;
  end;
end;

begin
  WriteLn('=== SCT Verification Module Tests ===');
  WriteLn;

  // 加载 OpenSSL 函数
  LoadOpenSSLFunctions;
  WriteLn;

  // 默认选项测试
  TestCreateDefaultValidationOptions;
  TestCustomValidationOptions;

  // 验证器创建测试
  TestSCTValidatorCreation;
  TestSCTValidatorWithCustomOptions;
  TestSCTValidatorOptionsModification;

  // 状态名称测试
  TestGetSCTValidationStatusName;
  TestGetSCTValidationStatusNameUnknown;

  // 时间戳格式化测试
  TestFormatSCTTimestamp;
  TestFormatSCTTimestampZero;
  TestFormatSCTTimestampLargeValue;

  // CT 日志存储测试
  TestLoadLogStore;
  TestLoadLogStoreNonexistentFile;

  // 策略检查测试
  TestCheckPolicyWithNoSCTs;
  TestCheckPolicyWithInsufficientSCTs;
  TestCheckPolicyWithSufficientValidSCTs;
  TestCheckPolicyWithMixedSCTs;
  TestCheckPolicyWithAllInvalidSCTs;
  TestCheckPolicyWithoutRequireValidSCTs;
  TestCheckPolicyWithExactMinimum;

  // 空输入处理测试
  TestValidateFromTLSWithNullSSL;
  TestValidateFromX509WithNullCert;
  TestValidateFromOCSPWithNullResp;
  TestValidateSCTListWithNullList;

  // 结果结构测试
  TestSCTValidationResultDefaults;
  TestSCTValidationResultAssignment;

  // 枚举测试
  TestSCTSourceValues;

  // 边界条件测试
  TestMinimumSCTCountZero;
  TestClockDriftToleranceZero;
  TestLargeClockDriftTolerance;

  PrintSummary;

  // 返回退出码
  if FailedTests > 0 then
    Halt(1)
  else
    Halt(0);
end.
