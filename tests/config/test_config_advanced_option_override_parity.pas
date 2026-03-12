program test_config_advanced_option_override_parity;

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

procedure AssertBuilderParity(const AOverrideBuilder,
  AMethodBuilder: ISSLContextBuilder; const AMessage: string);
var
  LOverrideJSON, LMethodJSON: string;
  LOverrideINI, LMethodINI: string;
begin
  LOverrideJSON := AOverrideBuilder.ExportToJSON;
  LMethodJSON := AMethodBuilder.ExportToJSON;
  AssertEqualStr(LMethodJSON, LOverrideJSON, AMessage + ' JSON parity');

  LOverrideINI := AOverrideBuilder.ExportToINI;
  LMethodINI := AMethodBuilder.ExportToINI;
  AssertEqualStr(LMethodINI, LOverrideINI, AMessage + ' INI parity');
end;

procedure Test_Override_ServerName_MatchesWithSNI;
begin
  TestHeader('Test 1: Override Server Name Matches WithSNI');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .Override('server_name', 'api.example.com'),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithSNI('api.example.com'),
    'Override server_name matches WithSNI');
end;

procedure Test_Override_ALPNProtocols_MatchesWithALPN;
begin
  TestHeader('Test 2: Override ALPN Protocols Matches WithALPN');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .Override('alpn_protocols', 'h2,http/1.1'),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableALPN)
      .WithALPN('h2,http/1.1'),
    'Override alpn_protocols matches WithALPN');
end;

procedure Test_Override_SessionCacheEnabled_MatchesWithSessionCache;
begin
  TestHeader('Test 3: Override Session Cache Enabled Matches WithSessionCache');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithSessionCache(False)
      .Override('session_cache_enabled', 'true'),
    TSSLContextBuilder.Create
      .WithSessionCache(False)
      .WithSessionCache(True),
    'Override session_cache_enabled=true matches WithSessionCache(true)');
end;

procedure Test_Override_SessionCacheDisabled_ClearsOption;
begin
  TestHeader('Test 4: Override Session Cache Disabled Clears Option');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .Override('session_cache_enabled', 'true')
      .Override('session_cache_enabled', 'false'),
    TSSLContextBuilder.Create
      .WithSessionCache(True)
      .WithSessionCache(False),
    'Override session_cache_enabled last-write-wins matches WithSessionCache');
end;

procedure Test_Override_AdvancedFields_CaseInsensitiveCombined;
begin
  TestHeader('Test 5: Override Advanced Fields Case Insensitive Combined');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .WithSessionCache(False)
      .Override('SERVER_NAME', 'cdn.example.com')
      .Override('ALPN_PROTOCOLS', 'h2')
      .Override('SESSION_CACHE_ENABLED', 'false'),
    TSSLContextBuilder.Create
      .WithoutOption(ssoEnableSNI)
      .WithoutOption(ssoEnableALPN)
      .WithSessionCache(False)
      .WithSNI('cdn.example.com')
      .WithALPN('h2')
      .WithSessionCache(False),
    'Override advanced option-coupled fields remain case-insensitive');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Advanced Option Override Parity Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Override_ServerName_MatchesWithSNI;
    Test_Override_ALPNProtocols_MatchesWithALPN;
    Test_Override_SessionCacheEnabled_MatchesWithSessionCache;
    Test_Override_SessionCacheDisabled_ClearsOption;
    Test_Override_AdvancedFields_CaseInsensitiveCombined;

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
