program test_backend_comparison_simple;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes,
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

procedure TestTheoreticalComparison;
var
  LDiff: TCapabilityDiffResult;
  LReport: string;
  i: Integer;
begin
  PrintSeparator('Backend Theoretical Comparison: OpenSSL vs WinSSL');

  WriteLn('Comparing OpenSSL and WinSSL capabilities...');
  WriteLn('(Theoretical comparison - does not require both backends to be available)');
  WriteLn;

  try
    LDiff := CompareTwoBackends(sslOpenSSL, sslWinSSL);
    WriteLn('✅ Comparison complete');
    WriteLn;

    PrintSeparator('Comparison Results');

    WriteLn('Difference Level: ');
    case LDiff.DifferenceLevel of
      cdIdentical:    WriteLn('  ✅ IDENTICAL - Backends have same capabilities');
      cdMinor:        WriteLn('  ℹ️  MINOR - Small differences, fully compatible');
      cdMajor:        WriteLn('  ⚠️  MAJOR - Significant differences, may need adaptation');
      cdIncompatible: WriteLn('  ❌ INCOMPATIBLE - Cannot be used interchangeably');
    end;
    WriteLn;

    if Length(LDiff.AddedFeatures) > 0 then
    begin
      WriteLn('Features in WinSSL but NOT in OpenSSL:');
      for i := 0 to High(LDiff.AddedFeatures) do
        WriteLn('  + ', LDiff.AddedFeatures[i]);
      WriteLn;
    end;

    if Length(LDiff.RemovedFeatures) > 0 then
    begin
      WriteLn('Features in OpenSSL but NOT in WinSSL:');
      for i := 0 to High(LDiff.RemovedFeatures) do
        WriteLn('  - ', LDiff.RemovedFeatures[i]);
      WriteLn;
    end;

    if Length(LDiff.ChangedFields) > 0 then
    begin
      WriteLn('Changed Configuration:');
      for i := 0 to High(LDiff.ChangedFields) do
        WriteLn('  ~ ', LDiff.ChangedFields[i].FieldName, ': ',
          LDiff.ChangedFields[i].OldValue, ' → ', LDiff.ChangedFields[i].NewValue);
      WriteLn;
    end;

    WriteLn('Score Differences (WinSSL vs OpenSSL):');
    WriteLn('  Security:      ', LDiff.SecurityScoreDiff:+4, ' points');
    WriteLn('  Performance:   ', LDiff.PerformanceScoreDiff:+4, ' points');
    WriteLn('  Compatibility: ', LDiff.CompatibilityLevelDiff:+4, ' points');
    WriteLn;

    WriteLn('Summary:');
    WriteLn('  ', LDiff.Summary);
    WriteLn;

    // 生成详细文本报告
    PrintSeparator('Detailed Text Report');
    LReport := GenerateDiffReport(LDiff, 'text');
    WriteLn(LReport);

    // 生成JSON报告
    PrintSeparator('JSON Report (for automation)');
    LReport := GenerateDiffReport(LDiff, 'json');
    WriteLn(LReport);

  except
    on E: Exception do
    begin
      WriteLn('❌ Error during comparison: ', E.Message);
      Halt(1);
    end;
  end;
end;

begin
  try
    // 初始化 OpenSSL
    WriteLn('Initializing OpenSSL...');
    try
      LoadOpenSSLCore;
      LoadOpenSSLBIO;
      LoadOpenSSLX509;
      WriteLn('✅ OpenSSL loaded: ', GetOpenSSLVersionString);
    except
      on E: Exception do
      begin
        WriteLn('❌ OpenSSL initialization failed: ', E.Message);
        Halt(1);
      end;
    end;
    WriteLn;

    TestTheoreticalComparison;

    PrintSeparator('Test Complete');
    WriteLn('✅ Backend comparison completed successfully');
    WriteLn;
    WriteLn('Recommendations:');
    WriteLn('  1. Review capability differences above');
    WriteLn('  2. Test critical features on both backends');
    WriteLn('  3. Implement fallback for missing features');
    WriteLn('  4. Document platform-specific behavior');
    WriteLn;

  except
    on E: Exception do
    begin
      WriteLn('❌ Fatal error: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
