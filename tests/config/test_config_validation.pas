program test_config_validation;

{$mode objfpc}{$H+}

{**
 * Test suite for Phase 2.1.2 - Configuration Validation
 *
 * Tests the validation functionality:
 * 1. ValidateClient - Client configuration validation
 * 2. ValidateServer - Server configuration validation
 * 3. BuildWithValidation - Build with automatic validation
 * 4. Warning and error detection
 *}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils,
  fafafa.ssl.exceptions,
  fafafa.ssl.freepascal.lib;

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

procedure TestHeader(const ATestName: string);
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  ', ATestName);
  WriteLn('═══════════════════════════════════════════════════════════');
end;

function HasWarningContaining(const AResult: TBuildValidationResult; const AText: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AResult.WarningCount - 1 do
    if Pos(AText, AResult.Warnings[I]) > 0 then
      Exit(True);
end;

function HasErrorContaining(const AResult: TBuildValidationResult; const AText: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to AResult.ErrorCount - 1 do
    if Pos(AText, AResult.Errors[I]) > 0 then
      Exit(True);
end;

{ Test 1: Valid client configuration }
procedure Test_ValidClient;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 1: Valid Client Configuration');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSystemRoots;

  LResult := LBuilder.ValidateClient;

  Assert(LResult.IsValid, 'Valid client configuration passes validation');
  Assert(not LResult.HasErrors, 'No errors for valid configuration');
end;

{ Test 2: Client with warnings }
procedure Test_ClientWithWarnings;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 2: Client With Warnings');

  // Configuration with deprecated TLS 1.0 and no verification
  LBuilder := TSSLContextBuilder.Create
    .WithProtocols([sslProtocolTLS10, sslProtocolTLS12])
    .WithVerifyNone;

  LResult := LBuilder.ValidateClient;

  Assert(LResult.IsValid, 'Configuration is valid (warnings don''t block)');
  Assert(LResult.HasWarnings, 'Warnings are detected');
  Assert(LResult.WarningCount >= 2, 'At least 2 warnings (TLS 1.0 + no verification)');

  WriteLn('  Warnings detected:');
  if LResult.HasWarnings then
    WriteLn('    - ', LResult.Warnings[0]);
end;

{ Test 3: Client with insecure protocols }
procedure Test_ClientInsecureProtocols;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 3: Client With Insecure Protocols');

  // Try to use SSL 2.0 (should produce error)
  LBuilder := TSSLContextBuilder.Create
    .WithProtocols([sslProtocolSSL2, sslProtocolTLS12]);

  LResult := LBuilder.ValidateClient;

  Assert(not LResult.IsValid, 'SSL 2.0 makes configuration invalid');
  Assert(LResult.HasErrors, 'Error is detected');
  Assert(LResult.ErrorCount >= 1, 'At least 1 error for SSL 2.0');

  WriteLn('  Errors detected:');
  if LResult.HasErrors then
    WriteLn('    - ', LResult.Errors[0]);
end;

{ Test 4: Server without certificate }
procedure Test_ServerNoCertificate;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 4: Server Without Certificate');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13;

  LResult := LBuilder.ValidateServer;

  Assert(not LResult.IsValid, 'Server without certificate is invalid');
  Assert(LResult.HasErrors, 'Missing certificate is an error');
  Assert(LResult.ErrorCount >= 2, 'At least 2 errors (cert + key)');

  WriteLn('  Errors detected:');
  if LResult.HasErrors then
  begin
    WriteLn('    - ', LResult.Errors[0]);
    if LResult.ErrorCount > 1 then
      WriteLn('    - ', LResult.Errors[1]);
  end;
end;

{ Test 5: Valid server configuration }
procedure Test_ValidServer;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 5: Valid Server Configuration');

  // Generate test certificate
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'test.local', 'Test Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'Valid server configuration passes validation');
  Assert(not LResult.HasErrors, 'No errors for valid configuration');
end;

{ Test 6: Insecure cipher configuration }
procedure Test_InsecureCiphers;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 6: Insecure Cipher Configuration');

  // NULL cipher provides no encryption
  LBuilder := TSSLContextBuilder.Create
    .WithTLS12
    .WithCipherList('NULL-SHA');

  LResult := LBuilder.ValidateClient;

  Assert(not LResult.IsValid, 'NULL cipher makes configuration invalid');
  Assert(LResult.HasErrors, 'NULL cipher is an error');

  WriteLn('  Errors detected:');
  if LResult.HasErrors then
    WriteLn('    - ', LResult.Errors[0]);
end;

{ Test 7: Weak cipher warning }
procedure Test_WeakCiphers;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 7: Weak Cipher Warning');

  // RC4 is weak but not fatal
  LBuilder := TSSLContextBuilder.Create
    .WithTLS12
    .WithCipherList('RC4-SHA');

  LResult := LBuilder.ValidateClient;

  Assert(LResult.IsValid, 'Weak cipher is valid (warning only)');
  Assert(LResult.HasWarnings, 'Weak cipher produces warning');

  WriteLn('  Warnings detected:');
  if LResult.HasWarnings then
    WriteLn('    - ', LResult.Warnings[0]);
end;

{ Test 8: BuildWithValidation success }
procedure Test_BuildWithValidation_Success;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LValidation: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 8: BuildWithValidation Success');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'valid.local', 'Valid Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Production
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  try
    LContext := LBuilder.BuildServerWithValidation(LValidation);

    Assert(LValidation.IsValid, 'Validation passes');
    Assert(LContext <> nil, 'Context is created');
    Assert(not LValidation.HasErrors, 'No errors');

    if LValidation.HasWarnings then
      WriteLn('  Note: ', LValidation.WarningCount, ' warning(s) present');
  except
    on E: Exception do
    begin
      WriteLn('  ✗ Unexpected exception: ', E.Message);
      Inc(GTestsFailed);
    end;
  end;
end;

{ Test 9: BuildWithValidation failure }
procedure Test_BuildWithValidation_Failure;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LValidation: TBuildValidationResult;
  LFailed: Boolean;
begin
  TestHeader('Test 9: BuildWithValidation Failure');

  LBuilder := TSSLContextBuilder.Create
    .WithProtocols([sslProtocolSSL2]);  // Invalid protocol

  LFailed := False;
  try
    LContext := LBuilder.BuildClientWithValidation(LValidation);
  except
    on E: ESSLConfigurationException do
    begin
      LFailed := True;
      Assert(True, 'Build fails with configuration exception');
      WriteLn('  Exception message: ', E.Message);
    end;
  end;

  Assert(LFailed, 'BuildWithValidation throws exception on invalid config');
end;

{ Test 10: Preset validation }
procedure Test_PresetValidation;
var
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 10: Preset Validation');

  // Development preset - should have warnings
  LResult := TSSLContextBuilder.Development.ValidateClient;
  Assert(LResult.IsValid, 'Development preset is valid');
  Assert(LResult.HasWarnings, 'Development preset has warnings (no verification)');

  // Production preset - should be clean
  LResult := TSSLContextBuilder.Production.ValidateClient;
  Assert(LResult.IsValid, 'Production preset is valid');

  // StrictSecurity preset - should be clean
  LResult := TSSLContextBuilder.StrictSecurity.ValidateClient;
  Assert(LResult.IsValid, 'StrictSecurity preset is valid');

  // LegacyCompatibility preset - should have warnings
  LResult := TSSLContextBuilder.LegacyCompatibility.ValidateClient;
  Assert(LResult.IsValid, 'LegacyCompatibility preset is valid');
  Assert(LResult.HasWarnings, 'LegacyCompatibility preset has warnings (old TLS)');
end;

{ Test 11: Multiple errors }
procedure Test_MultipleErrors;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  I: Integer;
begin
  TestHeader('Test 11: Multiple Errors');

  // Configuration with multiple issues
  LBuilder := TSSLContextBuilder.Create
    .WithProtocols([sslProtocolSSL2, sslProtocolSSL3])
    .WithCipherList('NULL-SHA')
    .WithSessionTimeout(-100);

  LResult := LBuilder.ValidateClient;

  Assert(not LResult.IsValid, 'Configuration with multiple errors is invalid');
  Assert(LResult.ErrorCount >= 3, 'At least 3 errors detected');

  WriteLn('  Errors detected (', LResult.ErrorCount, ' total):');
  for I := 0 to LResult.ErrorCount - 1 do
    WriteLn('    ', I + 1, '. ', LResult.Errors[I]);
end;

{ Test 12: Server PKCS#11 key validation }
procedure Test_ServerPKCS11Validation;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 12: Server PKCS#11 Key Validation');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'pkcs11.local', 'PKCS11 Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .UsePKCS11('pkcs11:token=test;object=server-key;type=private');

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'PKCS#11 private key counts as valid server key source');
  Assert(not LResult.HasErrors, 'No missing private key error when PKCS#11 URI is present');
end;

{ Test 12.1: Server PKCS#11 override parity }
procedure Test_ServerPKCS11Validation_ViaOverride;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 12.1: Server PKCS#11 Validation Via Override');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'pkcs11-override.local', 'PKCS11 Override Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .Override('pkcs11_uri', 'pkcs11:token=test;object=server-key;type=private');

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'Override pkcs11_uri counts as valid server key source');
  Assert(not LResult.HasErrors,
    'Override pkcs11_uri suppresses missing private key validation error');
end;

{ Test 12.2: Server PKCS#11 precedence warning with file key }
procedure Test_ServerPKCS11Validation_WarnsWhenFileKeyAlsoPresent;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 12.2: Server PKCS11 Warning With File Key');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'pkcs11-file-warning.local', 'PKCS11 File Warning Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKey('/nonexistent/server.key')
    .UsePKCS11('pkcs11:token=test;object=server-key;type=private');

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'PKCS#11 + file private key remains a valid server config');
  Assert(HasWarningContaining(LResult, 'PKCS#11 will be used'),
    'Validation warns that PKCS#11 takes precedence over file private key');
end;

{ Test 12.3: Server PKCS#11 precedence warning via override with PEM key }
procedure Test_ServerPKCS11Validation_ViaOverrideWarnsWhenPEMKeyAlsoPresent;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 12.3: Server PKCS11 Warning Via Override With PEM Key');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'pkcs11-pem-warning.local', 'PKCS11 PEM Warning Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey)
    .Override('pkcs11_uri', 'pkcs11:token=test;object=server-key;type=private');

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'Override pkcs11_uri + PEM private key remains a valid server config');
  Assert(HasWarningContaining(LResult, 'PKCS#11 will be used'),
    'Override path warns that PKCS#11 takes precedence over PEM private key');
end;

{ Test 12.4: Server PKCS#11 still requires certificate }
procedure Test_ServerPKCS11WithoutCertificateStillFails;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 12.4: Server PKCS11 Still Requires Certificate');

  LBuilder := TSSLContextBuilder.Create
    .UsePKCS11('pkcs11:token=test;object=server-key;type=private');

  LResult := LBuilder.ValidateServer;

  Assert(not LResult.IsValid, 'PKCS#11 without certificate is invalid for server config');
  Assert(HasErrorContaining(LResult, 'requires a certificate'),
    'Validation still requires an explicit certificate when PKCS#11 provides the private key');
  Assert(not HasErrorContaining(LResult, 'requires a private key'),
    'PKCS#11 key source should satisfy the private key requirement');
end;

{ Test 13: Server system roots satisfy CA validation }
procedure Test_ServerSystemRootsValidation;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 13: Server System Roots Validation');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'mTLS.local', 'Mutual TLS Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey)
    .WithVerifyPeer
    .WithSystemRoots;

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'Server config with system roots remains valid');
  Assert(not HasWarningContaining(LResult, 'no CA certificates configured'),
    'System roots suppress missing CA warning for server validation');
end;

{ Test 13.1: Server system roots override parity }
procedure Test_ServerSystemRootsValidation_ViaOverride;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 13.1: Server System Roots Validation Via Override');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'mtls-override.local', 'Mutual TLS Override Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey)
    .WithVerifyPeer
    .Override('use_system_roots', 'true');

  LResult := LBuilder.ValidateServer;

  Assert(LResult.IsValid, 'Override use_system_roots keeps server config valid');
  Assert(not HasWarningContaining(LResult, 'no CA certificates configured'),
    'Override use_system_roots suppresses missing CA warning for server validation');
end;


{ Test 13.2: Server build preserves configured ServerName }
procedure Test_ServerBuildWithValidation_PreservesServerName;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LValidation: TBuildValidationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 13.2: Server Build Preserves ServerName');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'sni-server.local', 'SNI Server Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey)
    .WithSNI('configured.server.local');

  try
    LContext := LBuilder.BuildServerWithValidation(LValidation);
    Assert(LValidation.IsValid, 'Validation passes for server build with SNI');
    Assert(LContext <> nil, 'Context is created for server build with SNI');
    {$PUSH}{$WARN SYMBOL_DEPRECATED OFF}
    Assert(LContext.GetServerName = 'configured.server.local',
      'BuildServer preserves configured ServerName on context');
    {$POP}
  except
    on E: Exception do
    begin
      WriteLn('  ✗ Unexpected exception: ', E.Message);
      Inc(GTestsFailed);
    end;
  end;
end;

{ Test 14: Session timeout validation }
procedure Test_SessionTimeout;
var
  LBuilder: ISSLContextBuilder;
  LResult: TBuildValidationResult;
begin
  TestHeader('Test 12: Session Timeout Validation');

  // Negative timeout
  LBuilder := TSSLContextBuilder.Create
    .WithSessionTimeout(-1);
  LResult := LBuilder.ValidateClient;
  Assert(not LResult.IsValid, 'Negative timeout is invalid');

  // Very long timeout
  LBuilder := TSSLContextBuilder.Create
    .WithSessionTimeout(100000);
  LResult := LBuilder.ValidateClient;
  Assert(LResult.IsValid, 'Very long timeout is valid');
  Assert(LResult.HasWarnings, 'Very long timeout produces warning');
end;

{ Main Test Runner }
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Phase 2.1.2 Configuration Validation Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('Testing validation functionality:');
  WriteLn('  1. Client validation');
  WriteLn('  2. Server validation');
  WriteLn('  3. Build with validation');
  WriteLn('  4. Error and warning detection');
  WriteLn;

  try
    // Run all tests
    Test_ValidClient;
    Test_ClientWithWarnings;
    Test_ClientInsecureProtocols;
    Test_ServerNoCertificate;
    Test_ValidServer;
    Test_InsecureCiphers;
    Test_WeakCiphers;
    Test_BuildWithValidation_Success;
    Test_BuildWithValidation_Failure;
    Test_PresetValidation;
    Test_MultipleErrors;
    Test_ServerPKCS11Validation;
    Test_ServerPKCS11Validation_ViaOverride;
    Test_ServerPKCS11Validation_WarnsWhenFileKeyAlsoPresent;
    Test_ServerPKCS11Validation_ViaOverrideWarnsWhenPEMKeyAlsoPresent;
    Test_ServerPKCS11WithoutCertificateStillFails;
    Test_ServerSystemRootsValidation;
    Test_ServerSystemRootsValidation_ViaOverride;
    Test_ServerBuildWithValidation_PreservesServerName;
    Test_SessionTimeout;

    // Print summary
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
