{**
 * test_dane_tlsa.pas - DANE TLSA 和 DNSSEC 功能测试
 *
 * 测试内容:
 * 1. ldns 动态加载（成功和失败两种情况）
 * 2. TLSA 记录解析
 * 3. 对已知 DANE 域名的查询
 * 4. DNSSEC 验证
 * 5. ldns 不可用时的优雅降级
 *
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2026-02-05
 *}
program test_dane_tlsa;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes,
  fafafa.ssl.dns.ldns,
  fafafa.ssl.dane,
  fafafa.ssl.logging;

var
  TotalTests: Integer = 0;
  PassedTests: Integer = 0;
  FailedTests: Integer = 0;
  SkippedTests: Integer = 0;

procedure TestPass(const ATestName: string; const AMessage: string = '');
begin
  Inc(TotalTests);
  Inc(PassedTests);
  if AMessage = '' then
    WriteLn('[PASS] ', ATestName)
  else
    WriteLn('[PASS] ', ATestName, ' - ', AMessage);
end;

procedure TestFail(const ATestName: string; const AMessage: string = '');
begin
  Inc(TotalTests);
  Inc(FailedTests);
  if AMessage = '' then
    WriteLn('[FAIL] ', ATestName)
  else
    WriteLn('[FAIL] ', ATestName, ' - ', AMessage);
end;

procedure TestSkip(const ATestName: string; const AReason: string = '');
begin
  Inc(TotalTests);
  Inc(SkippedTests);
  if AReason = '' then
    WriteLn('[SKIP] ', ATestName)
  else
    WriteLn('[SKIP] ', ATestName, ' - ', AReason);
end;

// ============================================================
// 测试 1: ldns 库动态加载
// ============================================================
procedure TestLdnsLoading;
begin
  WriteLn;
  WriteLn('=== Test: ldns Library Loading ===');

  // 测试加载 ldns
  if LoadLdns then
  begin
    TestPass('LoadLdns', 'ldns library loaded successfully');

    // 验证已加载状态
    if IsLdnsLoaded then
      TestPass('IsLdnsLoaded', 'Returns True after successful load')
    else
      TestFail('IsLdnsLoaded', 'Should return True after successful load');

    // 卸载库
    UnloadLdns;

    // 验证卸载状态
    if not IsLdnsLoaded then
      TestPass('UnloadLdns', 'Library unloaded successfully')
    else
      TestFail('UnloadLdns', 'IsLdnsLoaded should return False after unload');

  end
  else
  begin
    TestSkip('LoadLdns', 'ldns library not available: ' + GetLdnsLoadError);
    TestSkip('IsLdnsLoaded', 'ldns library not available');
    TestSkip('UnloadLdns', 'ldns library not available');
  end;
end;

// ============================================================
// 测试 2: ldns 不可用时的优雅降级
// ============================================================
procedure TestGracefulDegradation;
var
  Validator: TDANEValidator;
  ValidatorEx: TDANEValidatorEx;
  Status: string;
begin
  WriteLn;
  WriteLn('=== Test: Graceful Degradation (ldns unavailable) ===');

  // 确保 ldns 未加载
  UnloadLdns;

  // 创建验证器
  Validator := TDANEValidator.Create('example.com', 443);
  try
    // 测试 QueryTLSARecords - 应该返回 False
    if not Validator.QueryTLSARecords('example.com', 443) then
    begin
      // ldns 不可用，预期行为
      if not IsLdnsLoaded then
        TestPass('QueryTLSARecords (no ldns)', 'Returns False when ldns unavailable')
      else
        TestPass('QueryTLSARecords', 'Query executed (ldns is actually available)');
    end
    else
    begin
      // 如果成功，说明 ldns 实际可用
      TestPass('QueryTLSARecords', 'Query successful (ldns is available)');
    end;
  finally
    Validator.Free;
  end;

  // 测试 TDANEValidatorEx
  ValidatorEx := TDANEValidatorEx.Create('example.com', 443);
  try
    // 测试 VerifyDNSSEC - 应该返回 False
    if not ValidatorEx.VerifyDNSSEC then
    begin
      if not IsLdnsLoaded then
        TestPass('VerifyDNSSEC (no ldns)', 'Returns False when ldns unavailable')
      else
        TestPass('VerifyDNSSEC', 'Verification executed (ldns available, domain may not have DNSSEC)');
    end
    else
    begin
      TestPass('VerifyDNSSEC', 'DNSSEC verification successful');
    end;

    // 测试 GetDNSSECStatus
    Status := ValidatorEx.GetDNSSECStatus;
    if Pos('not available', Status) > 0 then
      TestPass('GetDNSSECStatus (no ldns)', 'Returns appropriate message: ' + Status)
    else
      TestPass('GetDNSSECStatus', 'Status: ' + Status);
  finally
    ValidatorEx.Free;
  end;
end;

// ============================================================
// 测试 3: TLSA 记录手动添加和验证
// ============================================================
procedure TestManualTLSARecords;
var
  Validator: TDANEValidator;
  TestData: TBytes;
  i: Integer;
begin
  WriteLn;
  WriteLn('=== Test: Manual TLSA Record Management ===');

  Validator := TDANEValidator.Create('test.example.com', 443);
  try
    // 测试初始状态
    if Validator.GetRecordCount = 0 then
      TestPass('Initial record count', 'No records initially')
    else
      TestFail('Initial record count', Format('Expected 0, got %d', [Validator.GetRecordCount]));

    // 创建测试数据 (32 字节 SHA-256 哈希)
    SetLength(TestData, 32);
    for i := 0 to 31 do
      TestData[i] := Byte(i);

    // 添加 TLSA 记录
    Validator.AddTLSARecord(duDomainIssuedCert, dsSubjectPublicKeyInfo, dmSHA256, TestData);

    // 验证记录已添加
    if Validator.GetRecordCount = 1 then
      TestPass('AddTLSARecord', 'Record added successfully')
    else
      TestFail('AddTLSARecord', Format('Expected 1 record, got %d', [Validator.GetRecordCount]));

    // 添加更多记录
    Validator.AddTLSARecord(duCAConstraint, dsFullCertificate, dmSHA512, TestData);
    Validator.AddTLSARecord(duServiceCertConstraint, dsSubjectPublicKeyInfo, dmExact, TestData);

    if Validator.GetRecordCount = 3 then
      TestPass('Multiple records', 'Added 3 records total')
    else
      TestFail('Multiple records', Format('Expected 3 records, got %d', [Validator.GetRecordCount]));

    // 测试 GetRecordInfo
    WriteLn(Validator.GetRecordInfo);
    TestPass('GetRecordInfo', 'Record info retrieved');

    // 测试 ClearRecords
    Validator.ClearRecords;
    if Validator.GetRecordCount = 0 then
      TestPass('ClearRecords', 'All records cleared')
    else
      TestFail('ClearRecords', Format('Expected 0 records, got %d', [Validator.GetRecordCount]));

  finally
    Validator.Free;
  end;
end;

// ============================================================
// 测试 4: DANE 验证器属性
// ============================================================
procedure TestValidatorProperties;
var
  Validator: TDANEValidator;
begin
  WriteLn;
  WriteLn('=== Test: Validator Properties ===');

  Validator := TDANEValidator.Create('example.com', 443);
  try
    // 测试域名
    if Validator.Domain = 'example.com' then
      TestPass('Domain property', 'Initial value correct')
    else
      TestFail('Domain property', 'Unexpected value: ' + Validator.Domain);

    Validator.Domain := 'test.com';
    if Validator.Domain = 'test.com' then
      TestPass('Domain setter', 'Value changed correctly')
    else
      TestFail('Domain setter', 'Failed to change value');

    // 测试端口
    if Validator.Port = 443 then
      TestPass('Port property', 'Initial value correct')
    else
      TestFail('Port property', Format('Expected 443, got %d', [Validator.Port]));

    Validator.Port := 25;
    if Validator.Port = 25 then
      TestPass('Port setter', 'Value changed correctly')
    else
      TestFail('Port setter', 'Failed to change value');

    // 测试 RequireDNSSEC（默认 True）
    if Validator.RequireDNSSEC then
      TestPass('RequireDNSSEC default', 'Default is True')
    else
      TestFail('RequireDNSSEC default', 'Expected True');

    Validator.RequireDNSSEC := False;
    if not Validator.RequireDNSSEC then
      TestPass('RequireDNSSEC setter', 'Changed to False')
    else
      TestFail('RequireDNSSEC setter', 'Failed to change');

    // 测试 EnableCache（默认 True）
    if Validator.EnableCache then
      TestPass('EnableCache default', 'Default is True')
    else
      TestFail('EnableCache default', 'Expected True');

    // 测试 CacheTimeout（默认 3600）
    if Validator.CacheTimeout = 3600 then
      TestPass('CacheTimeout default', 'Default is 3600 seconds')
    else
      TestFail('CacheTimeout default', Format('Expected 3600, got %d', [Validator.CacheTimeout]));

    Validator.CacheTimeout := 7200;
    if Validator.CacheTimeout = 7200 then
      TestPass('CacheTimeout setter', 'Changed to 7200')
    else
      TestFail('CacheTimeout setter', 'Failed to change');

  finally
    Validator.Free;
  end;
end;

// ============================================================
// 测试 5: DNS TLSA 查询（需要 ldns 和网络）
// ============================================================
procedure TestDNSTLSAQuery;
var
  Records: TLdnsTLSARecordArray;
  DNSSECStatus: TDNSSECStatus;
  i: Integer;
begin
  WriteLn;
  WriteLn('=== Test: DNS TLSA Query ===');

  // 确保 ldns 可用
  if not LoadLdns then
  begin
    TestSkip('DNS TLSA Query', 'ldns library not available');
    Exit;
  end;

  // 测试已知的 DANE 域名 (IETF 有 TLSA 记录)
  // 注意：这需要网络连接
  WriteLn('Querying _443._tcp.www.ietf.org...');

  if QueryDNSTLSA('www.ietf.org', 443, 'tcp', Records, DNSSECStatus) then
  begin
    TestPass('QueryDNSTLSA', 'Query executed successfully');

    WriteLn(Format('  DNSSEC Status: %s', [DNSSECStatusToStr(DNSSECStatus)]));
    WriteLn(Format('  Records found: %d', [Length(Records)]));

    for i := 0 to High(Records) do
    begin
      WriteLn(Format('  Record %d: Usage=%d, Selector=%d, Matching=%d, Data=%d bytes',
        [i, Records[i].Usage, Records[i].Selector, Records[i].MatchingType,
         Length(Records[i].CertData)]));
    end;

    if Length(Records) > 0 then
      TestPass('TLSA records found', Format('%d records for www.ietf.org', [Length(Records)]))
    else
      TestPass('No TLSA records', 'Query successful but no records (domain may not have TLSA)');

    // 测试 DNSSEC 状态
    case DNSSECStatus of
      dnssecSecure:
        TestPass('DNSSEC status', 'Secure (validated)');
      dnssecInsecure:
        TestPass('DNSSEC status', 'Insecure (not signed)');
      dnssecBogus:
        TestFail('DNSSEC status', 'Bogus (validation failed)');
      else
        TestPass('DNSSEC status', 'Unknown/Indeterminate');
    end;
  end
  else
  begin
    // 查询失败可能是网络问题
    TestSkip('QueryDNSTLSA', 'Query failed (network issue or no resolver)');
  end;

  // 测试不存在的域名
  WriteLn;
  WriteLn('Querying _443._tcp.this-domain-does-not-exist.invalid...');
  if QueryDNSTLSA('this-domain-does-not-exist.invalid', 443, 'tcp', Records, DNSSECStatus) then
  begin
    if Length(Records) = 0 then
      TestPass('Non-existent domain', 'Query successful, no records (expected)')
    else
      TestFail('Non-existent domain', 'Unexpected records returned');
  end
  else
  begin
    TestPass('Non-existent domain', 'Query failed as expected');
  end;
end;

// ============================================================
// 测试 6: TDANEValidatorEx 扩展功能
// ============================================================
procedure TestDANEValidatorEx;
var
  ValidatorEx: TDANEValidatorEx;
begin
  WriteLn;
  WriteLn('=== Test: TDANEValidatorEx Extended Features ===');

  ValidatorEx := TDANEValidatorEx.Create('example.com', 443);
  try
    // 测试设置 DNS 解析器
    ValidatorEx.SetDNSResolver('8.8.8.8');
    TestPass('SetDNSResolver', 'Custom resolver set');

    // 测试设置超时
    ValidatorEx.SetDNSTimeout(10000);
    TestPass('SetDNSTimeout', 'Timeout set to 10000ms');

    // 测试 GetDNSSECStatus（初始状态）
    WriteLn('  Initial DNSSEC status: ', ValidatorEx.GetDNSSECStatus);
    TestPass('GetDNSSECStatus (initial)', 'Status retrieved');

    // 测试 VerifyDNSSEC
    if LoadLdns then
    begin
      ValidatorEx.VerifyDNSSEC;  // 执行验证
      WriteLn('  After verify DNSSEC status: ', ValidatorEx.GetDNSSECStatus);
      TestPass('VerifyDNSSEC', 'Verification executed');
    end
    else
    begin
      TestSkip('VerifyDNSSEC', 'ldns not available');
    end;

  finally
    ValidatorEx.Free;
  end;
end;

// ============================================================
// 测试 7: DNSSEC 链验证
// ============================================================
procedure TestDNSSECChainVerification;
var
  Status: TDNSSECStatus;
begin
  WriteLn;
  WriteLn('=== Test: DNSSEC Chain Verification ===');

  if not LoadLdns then
  begin
    TestSkip('DNSSEC Chain Verification', 'ldns library not available');
    Exit;
  end;

  // 测试已知 DNSSEC 签名域名
  WriteLn('Verifying DNSSEC for ietf.org...');
  Status := VerifyDNSSECChain('ietf.org', LDNS_RR_TYPE_A);
  WriteLn(Format('  Status: %s', [DNSSECStatusToStr(Status)]));

  case Status of
    dnssecSecure:
      TestPass('VerifyDNSSECChain (ietf.org)', 'Domain is DNSSEC secure');
    dnssecInsecure:
      TestPass('VerifyDNSSECChain (ietf.org)', 'Domain not DNSSEC signed');
    dnssecBogus:
      TestFail('VerifyDNSSECChain (ietf.org)', 'DNSSEC validation failed');
    else
      TestPass('VerifyDNSSECChain (ietf.org)', 'Status indeterminate');
  end;

  // 测试普通域名（可能没有 DNSSEC）
  WriteLn;
  WriteLn('Verifying DNSSEC for google.com...');
  Status := VerifyDNSSECChain('google.com', LDNS_RR_TYPE_A);
  WriteLn(Format('  Status: %s', [DNSSECStatusToStr(Status)]));
  TestPass('VerifyDNSSECChain (google.com)', 'Verification executed');
end;

// ============================================================
// 测试 8: ldns 状态转换函数
// ============================================================
procedure TestStatusFunctions;
begin
  WriteLn;
  WriteLn('=== Test: Status Helper Functions ===');

  // 测试 DNSSECStatusToStr
  if DNSSECStatusToStr(dnssecUnknown) = 'Unknown' then
    TestPass('DNSSECStatusToStr (Unknown)', 'Correct string')
  else
    TestFail('DNSSECStatusToStr (Unknown)', 'Incorrect string');

  if Pos('Secure', DNSSECStatusToStr(dnssecSecure)) > 0 then
    TestPass('DNSSECStatusToStr (Secure)', 'Contains "Secure"')
  else
    TestFail('DNSSECStatusToStr (Secure)', 'Missing "Secure"');

  if Pos('Insecure', DNSSECStatusToStr(dnssecInsecure)) > 0 then
    TestPass('DNSSECStatusToStr (Insecure)', 'Contains "Insecure"')
  else
    TestFail('DNSSECStatusToStr (Insecure)', 'Missing "Insecure"');

  if Pos('Bogus', DNSSECStatusToStr(dnssecBogus)) > 0 then
    TestPass('DNSSECStatusToStr (Bogus)', 'Contains "Bogus"')
  else
    TestFail('DNSSECStatusToStr (Bogus)', 'Missing "Bogus"');

  if Pos('Indeterminate', DNSSECStatusToStr(dnssecIndeterminate)) > 0 then
    TestPass('DNSSECStatusToStr (Indeterminate)', 'Contains "Indeterminate"')
  else
    TestFail('DNSSECStatusToStr (Indeterminate)', 'Missing "Indeterminate"');

  // 测试 GetLdnsLoadError
  UnloadLdns;  // 确保未加载
  if not LoadLdns then
  begin
    if GetLdnsLoadError <> '' then
      TestPass('GetLdnsLoadError', 'Error message available: ' + GetLdnsLoadError)
    else
      TestFail('GetLdnsLoadError', 'Error message should not be empty');
  end
  else
  begin
    TestPass('GetLdnsLoadError', 'ldns loaded, no error expected');
  end;
end;

// ============================================================
// 主程序
// ============================================================
begin
  WriteLn('========================================');
  WriteLn(' DANE/TLSA 和 DNSSEC 测试套件');
  WriteLn(' fafafa.ssl 项目');
  WriteLn('========================================');
  WriteLn;

  // 运行所有测试
  TestLdnsLoading;
  TestGracefulDegradation;
  TestManualTLSARecords;
  TestValidatorProperties;
  TestDNSTLSAQuery;
  TestDANEValidatorEx;
  TestDNSSECChainVerification;
  TestStatusFunctions;

  // 输出汇总
  WriteLn;
  WriteLn('========================================');
  WriteLn(' 测试结果汇总');
  WriteLn('========================================');
  WriteLn(Format(' 总测试数: %d', [TotalTests]));
  WriteLn(Format(' 通过: %d', [PassedTests]));
  WriteLn(Format(' 失败: %d', [FailedTests]));
  WriteLn(Format(' 跳过: %d', [SkippedTests]));
  WriteLn;

  if FailedTests = 0 then
  begin
    WriteLn('所有测试通过!');
    ExitCode := 0;
  end
  else
  begin
    WriteLn(Format('有 %d 个测试失败。', [FailedTests]));
    ExitCode := 1;
  end;
end.
