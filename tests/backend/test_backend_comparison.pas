program test_backend_comparison;

{$mode ObjFPC}{$H+}

{
  后端对照测试 - OpenSSL vs WinSSL 能力对比

  目标：
  1. 对比 OpenSSL 和 WinSSL 后端能力
  2. 生成差异报告
  3. 识别兼容性问题
  4. 建议降级策略
}

uses
  SysUtils, Classes, TypInfo,
  fafafa.ssl.base,
  fafafa.ssl.factory,
  fafafa.ssl.capability.diff,
  fafafa.ssl.openssl.base,
  fafafa.ssl.openssl.api.core,
  fafafa.ssl.openssl.api.x509,
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.loader;

procedure PrintSeparator(const ATitle: string = '');
begin
  WriteLn;
  WriteLn('================================================================================');
  if ATitle <> '' then
    WriteLn('  ', ATitle);
  WriteLn('================================================================================');
  WriteLn;
end;

procedure TestBackendComparison;
var
  LOpenSSLLib, LWinSSLLib: ISSLLibrary;
  LOpenSSLCaps, LWinSSLCaps: TSSLBackendCapabilities;
  LDiff: TCapabilityDiffResult;
  LReport: string;
  i: Integer;
begin
  PrintSeparator('Backend Comparison Test: OpenSSL vs WinSSL');

  // 初始化 OpenSSL
  WriteLn('1. Initializing OpenSSL backend...');
  try
    LoadOpenSSLCore;
    LoadOpenSSLBIO;
    LoadOpenSSLX509;
    LOpenSSLLib := TSSLFactory.CreateLibrary(sslOpenSSL);
    LOpenSSLLib.Initialize;
    WriteLn('   ✅ OpenSSL initialized: ', LOpenSSLLib.GetVersionString);
  except
    on E: Exception do
    begin
      WriteLn('   ❌ OpenSSL initialization failed: ', E.Message);
      Exit;
    end;
  end;

  // 获取 OpenSSL 能力
  WriteLn('2. Getting OpenSSL capabilities...');
  LOpenSSLCaps := LOpenSSLLib.GetCapabilities;
  WriteLn('   ✅ OpenSSL capabilities retrieved');

  {$IFDEF WINDOWS}
  // 初始化 WinSSL (仅 Windows)
  WriteLn('3. Initializing WinSSL backend...');
  try
    LWinSSLLib := TSSLFactory.CreateLibrary(sslWinSSL);
    LWinSSLLib.Initialize;
    WriteLn('   ✅ WinSSL initialized: ', LWinSSLLib.GetVersionString);
  except
    on E: Exception do
    begin
      WriteLn('   ❌ WinSSL initialization failed: ', E.Message);
      LWinSSLLib := nil;
    end;
  end;

  if LWinSSLLib <> nil then
  begin
    // 获取 WinSSL 能力
    WriteLn('4. Getting WinSSL capabilities...');
    LWinSSLCaps := LWinSSLLib.GetCapabilities;
    WriteLn('   ✅ WinSSL capabilities retrieved');

    // 对比能力
    WriteLn('5. Comparing capabilities...');
    LDiff := CompareCapabilities(LOpenSSLCaps, LWinSSLCaps);
    WriteLn('   ✅ Comparison complete');

    // 生成报告
    PrintSeparator('Capability Comparison Report');

    WriteLn('Difference Level: ', GetEnumName(TypeInfo(TCapabilityDifference), Ord(LDiff.DifferenceLevel)));
    WriteLn;

    if Length(LDiff.AddedFeatures) > 0 then
    begin
      WriteLn('Features in WinSSL but not in OpenSSL:');
      for i := 0 to High(LDiff.AddedFeatures) do
        WriteLn('  + ', LDiff.AddedFeatures[i]);
      WriteLn;
    end;

    if Length(LDiff.RemovedFeatures) > 0 then
    begin
      WriteLn('Features in OpenSSL but not in WinSSL:');
      for i := 0 to High(LDiff.RemovedFeatures) do
        WriteLn('  - ', LDiff.RemovedFeatures[i]);
      WriteLn;
    end;

    if Length(LDiff.ChangedFields) > 0 then
    begin
      WriteLn('Changed Fields:');
      for i := 0 to High(LDiff.ChangedFields) do
        WriteLn('  ~ ', LDiff.ChangedFields[i].FieldName, ': ',
          LDiff.ChangedFields[i].OldValue, ' → ', LDiff.ChangedFields[i].NewValue);
      WriteLn;
    end;

    WriteLn('Score Differences:');
    WriteLn('  Security:      ', LDiff.SecurityScoreDiff:+4);
    WriteLn('  Performance:   ', LDiff.PerformanceScoreDiff:+4);
    WriteLn('  Compatibility: ', LDiff.CompatibilityLevelDiff:+4);
    WriteLn;

    WriteLn('Summary:');
    WriteLn('  ', LDiff.Summary);
    WriteLn;

    // 生成文本报告
    PrintSeparator('Detailed Text Report');
    LReport := GenerateDiffReport(LDiff, 'text');
    WriteLn(LReport);

    // 生成 JSON 报告
    PrintSeparator('JSON Report');
    LReport := GenerateDiffReport(LDiff, 'json');
    WriteLn(LReport);

    // 清理
    LWinSSLLib.Finalize;
  end
  else
  begin
    WriteLn('3. WinSSL not available (not on Windows or initialization failed)');
    WriteLn('   Using theoretical comparison...');

    // 使用 CompareTwoBackends 进行理论对比
    WriteLn('4. Comparing backends theoretically...');
    LDiff := CompareTwoBackends(sslOpenSSL, sslWinSSL);
    WriteLn('   ✅ Theoretical comparison complete');

    PrintSeparator('Theoretical Capability Comparison Report');

    WriteLn('Difference Level: ', GetEnumName(TypeInfo(TCapabilityDifference), Ord(LDiff.DifferenceLevel)));
    WriteLn;

    WriteLn('Summary:');
    WriteLn('  ', LDiff.Summary);
    WriteLn;

    LReport := GenerateDiffReport(LDiff, 'text');
    WriteLn(LReport);
  end;
  {$ELSE}
  // 非 Windows 平台 - 理论对比
  WriteLn('3. Not on Windows - using theoretical comparison...');
  WriteLn('4. Comparing backends theoretically...');
  LDiff := CompareTwoBackends(sslOpenSSL, sslWinSSL);
  WriteLn('   ✅ Theoretical comparison complete');

  PrintSeparator('Theoretical Capability Comparison Report');

  WriteLn('Difference Level: ', GetEnumName(TypeInfo(TCapabilityDifference), Ord(LDiff.DifferenceLevel)));
  WriteLn;

  if Length(LDiff.AddedFeatures) > 0 then
  begin
    WriteLn('Features in WinSSL but not in OpenSSL:');
    for i := 0 to High(LDiff.AddedFeatures) do
      WriteLn('  + ', LDiff.AddedFeatures[i]);
    WriteLn;
  end;

  if Length(LDiff.RemovedFeatures) > 0 then
  begin
    WriteLn('Features in OpenSSL but not in WinSSL:');
    for i := 0 to High(LDiff.RemovedFeatures) do
      WriteLn('  - ', LDiff.RemovedFeatures[i]);
    WriteLn;
  end;

  if Length(LDiff.ChangedFields) > 0 then
  begin
    WriteLn('Changed Fields:');
    for i := 0 to High(LDiff.ChangedFields) do
      WriteLn('  ~ ', LDiff.ChangedFields[i].FieldName, ': ',
        LDiff.ChangedFields[i].OldValue, ' → ', LDiff.ChangedFields[i].NewValue);
    WriteLn;
  end;

  WriteLn('Score Differences:');
  WriteLn('  Security:      ', LDiff.SecurityScoreDiff:+4);
  WriteLn('  Performance:   ', LDiff.PerformanceScoreDiff:+4);
  WriteLn('  Compatibility: ', LDiff.CompatibilityLevelDiff:+4);
  WriteLn;

  WriteLn('Summary:');
  WriteLn('  ', LDiff.Summary);
  WriteLn;

  PrintSeparator('Detailed Text Report');
  LReport := GenerateDiffReport(LDiff, 'text');
  WriteLn(LReport);

  PrintSeparator('JSON Report (for automation)');
  LReport := GenerateDiffReport(LDiff, 'json');
  WriteLn(LReport);
  {$ENDIF}

  // 清理 OpenSSL
  LOpenSSLLib.Finalize;
end;

procedure TestCapabilityAnalysis;
var
  LLib: ISSLLibrary;
  LCaps: TSSLBackendCapabilities;
begin
  PrintSeparator('OpenSSL Capability Analysis');

  WriteLn('Analyzing OpenSSL capabilities in detail...');
  WriteLn;

  LoadOpenSSLCore;
  LoadOpenSSLBIO;
  LoadOpenSSLX509;

  LLib := TSSLFactory.CreateLibrary(sslOpenSSL);
  LLib.Initialize;

  LCaps := LLib.GetCapabilities;

  WriteLn('TLS/SSL Features:');
  WriteLn('  TLS 1.3:              ', LCaps.SupportsTLS13);
  WriteLn('  ALPN:                 ', LCaps.SupportsALPN);
  WriteLn('  SNI:                  ', LCaps.SupportsSNI);
  WriteLn('  Session Tickets:      ', LCaps.SupportsSessionTickets);
  WriteLn('  OCSP Stapling:        ', LCaps.SupportsOCSPStapling);
  WriteLn('  Certificate Transparency: ', LCaps.SupportsCertificateTransparency);
  WriteLn;

  WriteLn('Cryptographic Features:');
  WriteLn('  ECDHE:                ', LCaps.SupportsECDHE);
  WriteLn('  ChaCha20-Poly1305:    ', LCaps.SupportsChaChaPoly);
  WriteLn;

  WriteLn('Protocol Versions:');
  WriteLn('  Min TLS:              ', GetEnumName(TypeInfo(TSSLProtocolVersion), Ord(LCaps.MinTLSVersion)));
  WriteLn('  Max TLS:              ', GetEnumName(TypeInfo(TSSLProtocolVersion), Ord(LCaps.MaxTLSVersion)));
  WriteLn;

  LLib.Finalize;
end;

begin
  try
    TestCapabilityAnalysis;
    TestBackendComparison;

    PrintSeparator('Test Complete');
    WriteLn('✅ Backend comparison test completed successfully');
    WriteLn;
    WriteLn('Next steps:');
    WriteLn('  1. Review capability differences');
    WriteLn('  2. Identify compatibility issues');
    WriteLn('  3. Implement fallback strategies if needed');
    WriteLn('  4. Run on Windows to get actual WinSSL comparison');
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn('❌ Error: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
