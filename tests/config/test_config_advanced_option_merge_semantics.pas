program test_config_advanced_option_merge_semantics;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder;

var
  GTestsPassed: Integer = 0;
  GTestsFailed: Integer = 0;

procedure Assert(ACondition: Boolean; const AMessage: string);
begin
  if ACondition then
  begin
    Inc(GTestsPassed);
    WriteLn('  ✓ ', AMessage);
  end
  else
  begin
    Inc(GTestsFailed);
    WriteLn('  ✗ ', AMessage);
  end;
end;

procedure AssertEqualStr(const AExpected, AActual, AMessage: string);
begin
  Assert(AExpected = AActual,
    AMessage + ' (expected="' + AExpected + '" actual="' + AActual + '")');
end;

procedure TestHeader(const ATestName: string);
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  ', ATestName);
  WriteLn('═══════════════════════════════════════════════════════════');
end;

procedure AssertBuilderParity(const AMergedBuilder,
  AExpectedBuilder: ISSLContextBuilder; const AMessage: string);
var
  LMergedJSON, LExpectedJSON: string;
  LMergedINI, LExpectedINI: string;
begin
  LMergedJSON := AMergedBuilder.ExportToJSON;
  LExpectedJSON := AExpectedBuilder.ExportToJSON;
  AssertEqualStr(LExpectedJSON, LMergedJSON, AMessage + ' JSON parity');

  LMergedINI := AMergedBuilder.ExportToINI;
  LExpectedINI := AExpectedBuilder.ExportToINI;
  AssertEqualStr(LExpectedINI, LMergedINI, AMessage + ' INI parity');
end;

procedure Test_Merge_EmptyServerName_ClearsFieldAndOption;
var
  LBase, LSource: ISSLContextBuilder;
begin
  TestHeader('Test 1: Merge Empty Server Name Clears Field And Option');

  LBase := TSSLContextBuilder.Create
    .WithSNI('api.example.com');
  LSource := TSSLContextBuilder.Create
    .WithoutOption(ssoEnableSNI)
    .ImportFromJSON('{"server_name":""}');

  LBase.Merge(LSource);

  AssertBuilderParity(
    LBase,
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI),
    'Merge should clear server_name when source snapshot carries empty value and SNI disabled');
end;

procedure Test_Merge_EmptyALPN_ClearsFieldAndOption;
var
  LBase, LSource: ISSLContextBuilder;
begin
  TestHeader('Test 2: Merge Empty ALPN Clears Field And Option');

  LBase := TSSLContextBuilder.Create
    .WithALPN('h2,http/1.1');
  LSource := TSSLContextBuilder.Create
    .WithoutOption(ssoEnableALPN)
    .ImportFromJSON('{"alpn_protocols":""}');

  LBase.Merge(LSource);

  AssertBuilderParity(
    LBase,
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN),
    'Merge should clear alpn_protocols when source snapshot carries empty value and ALPN disabled');
end;

procedure Test_Merge_EmptyOptionsArray_ClearsOptionSet;
var
  LBase, LSource: ISSLContextBuilder;
begin
  TestHeader('Test 3: Merge Empty Options Array Clears Option Set');

  LBase := TSSLContextBuilder.Create
    .WithOption(ssoEnableSessionTickets)
    .WithOption(ssoEnableSessionCache)
    .WithOption(ssoEnableSNI)
    .WithOption(ssoEnableALPN)
    .WithOption(ssoDisableCompression)
    .WithOption(ssoDisableRenegotiation)
    .WithOption(ssoEnableOCSPStapling)
    .WithOption(ssoRequireOCSPStapling)
    .WithOption(ssoEnableCertVerifyCache)
    .WithOption(ssoSkipCertVerifyCacheValidHitRefresh);

  LSource := TSSLContextBuilder.Create
    .WithoutOption(ssoEnableSessionTickets)
    .WithoutOption(ssoEnableSessionCache)
    .WithoutOption(ssoEnableSNI)
    .WithoutOption(ssoEnableALPN)
    .WithoutOption(ssoDisableCompression)
    .WithoutOption(ssoDisableRenegotiation)
    .WithoutOption(ssoEnableOCSPStapling)
    .WithoutOption(ssoRequireOCSPStapling)
    .WithoutOption(ssoEnableCertVerifyCache)
    .WithoutOption(ssoSkipCertVerifyCacheValidHitRefresh);

  LBase.Merge(LSource);

  AssertBuilderParity(
    LBase,
    LSource,
    'Merge should preserve an explicitly empty source option set');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Advanced Option Merge Semantics Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Merge_EmptyServerName_ClearsFieldAndOption;
    Test_Merge_EmptyALPN_ClearsFieldAndOption;
    Test_Merge_EmptyOptionsArray_ClearsOptionSet;

    WriteLn;
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn('  Test Summary');
    WriteLn('═══════════════════════════════════════════════════════════');
    WriteLn('  Tests Passed: ', GTestsPassed);
    WriteLn('  Tests Failed: ', GTestsFailed);
    WriteLn('  Total Tests:  ', GTestsPassed + GTestsFailed);
    WriteLn;

    if GTestsFailed = 0 then
    begin
      WriteLn('  ✓ ALL TESTS PASSED!');
      WriteLn;
      ExitCode := 0;
    end
    else
    begin
      WriteLn('  ✗ SOME TESTS FAILED!');
      WriteLn;
      ExitCode := 1;
    end;
  except
    on E: Exception do
    begin
      WriteLn;
      WriteLn('═══════════════════════════════════════════════════════════');
      WriteLn('  FATAL ERROR');
      WriteLn('═══════════════════════════════════════════════════════════');
      WriteLn('  Class: ', E.ClassName);
      WriteLn('  Message: ', E.Message);
      WriteLn;
      ExitCode := 2;
    end;
  end;
end.
