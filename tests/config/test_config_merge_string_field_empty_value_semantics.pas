program test_config_merge_string_field_empty_value_semantics;

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

procedure Test_Merge_DefaultSnapshot_ClearsCertificateMaterialStrings;
var
  LBase, LSource: ISSLContextBuilder;
begin
  TestHeader('Test 1: Merge Default Snapshot Clears Certificate Material Strings');

  LBase := TSSLContextBuilder.Create
    .WithCertificate('/tmp/server.crt')
    .WithCertificatePEM('CERT-PEM')
    .WithPrivateKey('/tmp/server.key')
    .WithPrivateKeyPEM('KEY-PEM');
  LSource := TSSLContextBuilder.Create;

  LBase.Merge(LSource);

  AssertBuilderParity(
    LBase,
    TSSLContextBuilder.Create,
    'Merge default snapshot clears certificate and private-key string fields');
end;

procedure Test_Merge_DefaultSnapshot_ClearsTrustAndCipherStrings;
var
  LBase, LSource: ISSLContextBuilder;
begin
  TestHeader('Test 2: Merge Default Snapshot Clears Trust And Cipher Strings');

  LBase := TSSLContextBuilder.Create
    .WithCAFile('/tmp/ca.pem')
    .WithCAPath('/tmp/ca')
    .WithCipherList('ECDHE+AESGCM')
    .WithTLS13CipherSuites('TLS_AES_128_GCM_SHA256');
  LSource := TSSLContextBuilder.Create;

  LBase.Merge(LSource);

  AssertBuilderParity(
    LBase,
    TSSLContextBuilder.Create,
    'Merge default snapshot clears CA and cipher string fields');
end;

procedure Test_Merge_DefaultSnapshot_ClearsPKCS11Strings;
var
  LBase, LSource: ISSLContextBuilder;
begin
  TestHeader('Test 3: Merge Default Snapshot Clears PKCS11 Strings');

  LBase := TSSLContextBuilder.Create
    .UsePKCS11('pkcs11:token=test;object=server-key;type=private')
    .WithPKCS11PIN('2468')
    .Override('pkcs11_pin_method', 'interactive');
  LSource := TSSLContextBuilder.Create;

  LBase.Merge(LSource);

  AssertBuilderParity(
    LBase,
    TSSLContextBuilder.Create,
    'Merge default snapshot clears PKCS11 uri/pin and restores pin method');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Merge String Field Empty Value Semantics Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Merge_DefaultSnapshot_ClearsCertificateMaterialStrings;
    Test_Merge_DefaultSnapshot_ClearsTrustAndCipherStrings;
    Test_Merge_DefaultSnapshot_ClearsPKCS11Strings;

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
