program test_config_advanced_option_empty_value_semantics;

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
    WriteLn('  ✗ FAILED: ', AMessage);
  end;
end;

procedure AssertEqualStr(const AExpected, AActual, AMessage: string);
begin
  Assert(AExpected = AActual,
    Format('%s (expected="%s" actual="%s")', [AMessage, AExpected, AActual]));
end;

procedure TestHeader(const ATestName: string);
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  ', ATestName);
  WriteLn('═══════════════════════════════════════════════════════════');
end;

procedure AssertBuilderParity(const ALeft,
  ARight: ISSLContextBuilder; const AMessage: string);
var
  LLeftJSON, LRightJSON: string;
  LLeftINI, LRightINI: string;
begin
  LLeftJSON := ALeft.ExportToJSON;
  LRightJSON := ARight.ExportToJSON;
  AssertEqualStr(LRightJSON, LLeftJSON, AMessage + ' JSON parity');

  LLeftINI := ALeft.ExportToINI;
  LRightINI := ARight.ExportToINI;
  AssertEqualStr(LRightINI, LLeftINI, AMessage + ' INI parity');
end;

procedure Test_Override_EmptyServerName_StillEnablesSNI;
begin
  TestHeader('Test 1: Override Empty Server Name Still Enables SNI');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .Override('server_name', ''),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithSNI(''),
    'Override empty server_name keeps existing builder-method semantics');
end;

procedure Test_Override_EmptyALPN_StillEnablesALPN;
begin
  TestHeader('Test 2: Override Empty ALPN Still Enables ALPN');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .Override('alpn_protocols', ''),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .WithALPN(''),
    'Override empty alpn_protocols keeps existing builder-method semantics');
end;

procedure Test_ImportFromJSON_EmptyServerName_DoesNotEnableSNIWithoutOptions;
const
  CJSON = '{"server_name":""}';
begin
  TestHeader('Test 3: ImportFromJSON Empty Server Name Does Not Enable SNI');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .ImportFromJSON(CJSON),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI),
    'Field-only JSON import keeps SNI disabled for empty server_name');
end;

procedure Test_ImportFromJSON_EmptyALPN_DoesNotEnableALPNWithoutOptions;
const
  CJSON = '{"alpn_protocols":""}';
begin
  TestHeader('Test 4: ImportFromJSON Empty ALPN Does Not Enable ALPN');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .ImportFromJSON(CJSON),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN),
    'Field-only JSON import keeps ALPN disabled for empty alpn_protocols');
end;

procedure Test_ImportFromINI_EmptyAdvancedFields_DoNotEnableOptions;
var
  LINI: string;
begin
  TestHeader('Test 5: ImportFromINI Empty Advanced Fields Do Not Enable Options');

  LINI :=
    '[Advanced]' + LineEnding +
    'server_name=' + LineEnding +
    'alpn_protocols=' + LineEnding;

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .ImportFromINI(LINI),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN),
    'Field-only INI import keeps SNI/ALPN disabled for empty values');
end;

procedure Test_ImportFromJSON_ExplicitOptionsStillWinOnEmptyValues;
const
  CJSON = '{"server_name":"","alpn_protocols":"","options":[0,1,4,5]}';
begin
  TestHeader('Test 6: ImportFromJSON Explicit Options Still Win On Empty Values');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .ImportFromJSON(CJSON),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .WithSNI('')
      .WithALPN(''),
    'Explicit JSON options preserve enabled SNI/ALPN even when values are empty');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Advanced Option Empty Value Semantics Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Override_EmptyServerName_StillEnablesSNI;
    Test_Override_EmptyALPN_StillEnablesALPN;
    Test_ImportFromJSON_EmptyServerName_DoesNotEnableSNIWithoutOptions;
    Test_ImportFromJSON_EmptyALPN_DoesNotEnableALPNWithoutOptions;
    Test_ImportFromINI_EmptyAdvancedFields_DoNotEnableOptions;
    Test_ImportFromJSON_ExplicitOptionsStillWinOnEmptyValues;

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
