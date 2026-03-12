program test_config_cert_verify_cache_override_parity;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
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

procedure Test_Override_CertVerifyCache_Enable;
begin
  TestHeader('Test 1: Override Cert Verify Cache Enable');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .Override('cert_verify_cache', 'true'),
    TSSLContextBuilder.Create
      .WithCertVerifyCache(True),
    'Override cert_verify_cache=true matches builder method');
end;

procedure Test_Override_CertVerifyCache_Disable;
begin
  TestHeader('Test 2: Override Cert Verify Cache Disable');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .Override('cert_verify_cache', 'true')
      .Override('cert_verify_cache', 'false'),
    TSSLContextBuilder.Create
      .WithCertVerifyCache(True)
      .WithCertVerifyCache(False),
    'Override cert_verify_cache last-write-wins matches builder method');
end;

procedure Test_Override_CertVerifyCacheSkipValidHitRefresh_Enable;
begin
  TestHeader('Test 3: Override Cert Verify Cache Skip Valid Hit Refresh Enable');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .Override('cert_verify_cache_skip_valid_hit_refresh', 'true'),
    TSSLContextBuilder.Create
      .WithCertVerifyCacheSkipValidHitRefresh(True),
    'Override cert_verify_cache_skip_valid_hit_refresh=true matches builder method');
end;

procedure Test_Override_CertVerifyCache_Combined;
begin
  TestHeader('Test 4: Override Cert Verify Cache Combined Options');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .Override('cert_verify_cache', 'true')
      .Override('cert_verify_cache_skip_valid_hit_refresh', 'true'),
    TSSLContextBuilder.Create
      .WithCertVerifyCache(True)
      .WithCertVerifyCacheSkipValidHitRefresh(True),
    'Override combined cert verify cache options match builder methods');
end;

procedure Test_Override_CertVerifyCache_CaseInsensitive;
begin
  TestHeader('Test 5: Override Cert Verify Cache Case Insensitive');

  AssertBuilderParity(
    TSSLContextBuilder.Create
      .Override('CERT_VERIFY_CACHE', 'true')
      .Override('CERT_VERIFY_CACHE_SKIP_VALID_HIT_REFRESH', 'true'),
    TSSLContextBuilder.Create
      .WithCertVerifyCache(True)
      .WithCertVerifyCacheSkipValidHitRefresh(True),
    'Override cert verify cache fields remain case-insensitive');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Cert Verify Cache Override Parity Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Override_CertVerifyCache_Enable;
    Test_Override_CertVerifyCache_Disable;
    Test_Override_CertVerifyCacheSkipValidHitRefresh_Enable;
    Test_Override_CertVerifyCache_Combined;
    Test_Override_CertVerifyCache_CaseInsensitive;

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
