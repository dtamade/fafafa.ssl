program test_config_advanced_option_import_parity;

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

procedure AssertBuilderParity(const AImportedBuilder,
  AMethodBuilder: ISSLContextBuilder; const AMessage: string);
var
  LImportedJSON, LMethodJSON: string;
  LImportedINI, LMethodINI: string;
begin
  LImportedJSON := AImportedBuilder.ExportToJSON;
  LMethodJSON := AMethodBuilder.ExportToJSON;
  AssertEqualStr(LMethodJSON, LImportedJSON, AMessage + ' JSON parity');

  LImportedINI := AImportedBuilder.ExportToINI;
  LMethodINI := AMethodBuilder.ExportToINI;
  AssertEqualStr(LMethodINI, LImportedINI, AMessage + ' INI parity');
end;

procedure Test_ImportFromJSON_ServerName_SyncsSNI;
const
  CJSON = '{"server_name":"api.example.com"}';
begin
  TestHeader('Test 1: ImportFromJSON Server Name Syncs SNI');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .ImportFromJSON(CJSON),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithSNI('api.example.com'),
    'ImportFromJSON server_name matches WithSNI when options are omitted');
end;

procedure Test_ImportFromJSON_ALPNProtocols_SyncsALPN;
const
  CJSON = '{"alpn_protocols":"h2,http/1.1"}';
begin
  TestHeader('Test 2: ImportFromJSON ALPN Protocols Syncs ALPN');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .ImportFromJSON(CJSON),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .WithALPN('h2,http/1.1'),
    'ImportFromJSON alpn_protocols matches WithALPN when options are omitted');
end;

procedure Test_ImportFromJSON_SessionCacheEnabled_SyncsOption;
const
  CJSON = '{"session_cache_enabled":true}';
begin
  TestHeader('Test 3: ImportFromJSON Session Cache Enabled Syncs Option');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithSessionCache(False)
      .ImportFromJSON(CJSON),
    TSSLContextBuilder.Create
      .WithSessionCache(False)
      .WithSessionCache(True),
    'ImportFromJSON session_cache_enabled matches WithSessionCache when options are omitted');
end;

procedure Test_ImportFromINI_CombinedAdvancedFields_SyncsOptions;
var
  LINI: string;
begin
  TestHeader('Test 4: ImportFromINI Combined Advanced Fields Syncs Options');

  LINI :=
    '[Advanced]' + LineEnding +
    'server_name=cdn.example.com' + LineEnding +
    'alpn_protocols=h2' + LineEnding +
    'session_cache_enabled=true' + LineEnding;

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .WithSessionCache(False)
      .ImportFromINI(LINI),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .WithSessionCache(False)
      .WithSNI('cdn.example.com')
      .WithALPN('h2')
      .WithSessionCache(True),
    'ImportFromINI advanced field-only input matches builder methods');
end;

procedure Test_ImportFromINI_SessionCacheDisabled_ClearsOption;
var
  LINI: string;
begin
  TestHeader('Test 5: ImportFromINI Session Cache Disabled Clears Option');

  LINI :=
    '[Advanced]' + LineEnding +
    'session_cache_enabled=false' + LineEnding;

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithSessionCache(True)
      .ImportFromINI(LINI),
    TSSLContextBuilder.Create
      .WithSessionCache(True)
      .WithSessionCache(False),
    'ImportFromINI session_cache_enabled=false matches WithSessionCache(false)');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Advanced Option Import Parity Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_ImportFromJSON_ServerName_SyncsSNI;
    Test_ImportFromJSON_ALPNProtocols_SyncsALPN;
    Test_ImportFromJSON_SessionCacheEnabled_SyncsOption;
    Test_ImportFromINI_CombinedAdvancedFields_SyncsOptions;
    Test_ImportFromINI_SessionCacheDisabled_ClearsOption;

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
