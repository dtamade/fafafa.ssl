program test_config_backend_selection_mode_normalization;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fpjson,
  jsonparser,
  fafafa.ssl.base,
  fafafa.ssl.backend.selector,
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

procedure AssertEqualInt(AExpected, AActual: Integer; const AMessage: string);
begin
  Assert(AExpected = AActual,
    Format('%s (expected=%d actual=%d)', [AMessage, AExpected, AActual]));
end;

procedure AssertEqualBool(AExpected, AActual: Boolean; const AMessage: string);
begin
  Assert(AExpected = AActual,
    Format('%s (expected=%s actual=%s)', [
      AMessage,
      BoolToStr(AExpected, True),
      BoolToStr(AActual, True)
    ]));
end;

procedure TestHeader(const ATestName: string);
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  ', ATestName);
  WriteLn('═══════════════════════════════════════════════════════════');
end;

function CreateCustomRequirements: TSSLRequirements;
begin
  Result := CreateDefaultRequirements(optSize);
  Result.RequiredProtocols := [sslProtocolTLS12];
  Result.RequiredCiphers := [sslCipherAES128GCM];
  Result.RequiredHashes := [sslHashSHA384];
  Result.RequiredKeyExchanges := [sslKexECDHE_RSA];
  Result.RequiredFeatures := [sslFeatSNI, sslFeatALPN];
  Result.PreferredCiphers := [sslCipherCHACHA20_POLY1305];
  Result.PreferredHashes := [sslHashSHA256];
  Result.MinSecurityScore := 77;
  Result.MinPerformanceScore := 66;
  Result.MinCompatibilityLevel := 55;
  Result.PlatformPreferences.PreferOSNative := True;
  Result.PlatformPreferences.PreferHardwareAccel := True;
  Result.PlatformPreferences.PreferFIPSCompliant := True;
  Result.PlatformPreferences.RequirePKCS11 := True;
  Result.PlatformPreferences.RequireTPM := True;
  Result.PlatformPreferences.RequireSystemCertStore := True;
  Result.OptimizationTarget := optSize;
end;

function ParseJSONObject(const AJSON: string): TJSONObject;
var
  LData: TJSONData;
begin
  LData := GetJSON(AJSON);
  if not (LData is TJSONObject) then
  begin
    LData.Free;
    raise Exception.Create('JSON root is not object');
  end;
  Result := TJSONObject(LData);
end;

procedure AssertBackendRequirementsCleared(AObj: TJSONObject; const AMessagePrefix: string);
begin
  AssertEqualInt(0, AObj.Arrays['backend_required_protocols'].Count,
    AMessagePrefix + ' clears backend_required_protocols');
  AssertEqualInt(0, AObj.Arrays['backend_required_ciphers'].Count,
    AMessagePrefix + ' clears backend_required_ciphers');
  AssertEqualInt(0, AObj.Arrays['backend_required_hashes'].Count,
    AMessagePrefix + ' clears backend_required_hashes');
  AssertEqualInt(0, AObj.Arrays['backend_required_key_exchanges'].Count,
    AMessagePrefix + ' clears backend_required_key_exchanges');
  AssertEqualInt(0, AObj.Arrays['backend_required_features'].Count,
    AMessagePrefix + ' clears backend_required_features');
  AssertEqualInt(0, AObj.Arrays['backend_preferred_ciphers'].Count,
    AMessagePrefix + ' clears backend_preferred_ciphers');
  AssertEqualInt(0, AObj.Arrays['backend_preferred_hashes'].Count,
    AMessagePrefix + ' clears backend_preferred_hashes');
  AssertEqualInt(0, AObj.Integers['backend_min_security_score'],
    AMessagePrefix + ' clears backend_min_security_score');
  AssertEqualInt(0, AObj.Integers['backend_min_performance_score'],
    AMessagePrefix + ' clears backend_min_performance_score');
  AssertEqualInt(0, AObj.Integers['backend_min_compatibility_level'],
    AMessagePrefix + ' clears backend_min_compatibility_level');
  AssertEqualBool(False, AObj.Booleans['backend_prefer_os_native'],
    AMessagePrefix + ' clears backend_prefer_os_native');
  AssertEqualBool(False, AObj.Booleans['backend_prefer_hardware_accel'],
    AMessagePrefix + ' clears backend_prefer_hardware_accel');
  AssertEqualBool(False, AObj.Booleans['backend_prefer_fips_compliant'],
    AMessagePrefix + ' clears backend_prefer_fips_compliant');
  AssertEqualBool(False, AObj.Booleans['backend_require_pkcs11'],
    AMessagePrefix + ' clears backend_require_pkcs11');
  AssertEqualBool(False, AObj.Booleans['backend_require_tpm'],
    AMessagePrefix + ' clears backend_require_tpm');
  AssertEqualBool(False, AObj.Booleans['backend_require_system_cert_store'],
    AMessagePrefix + ' clears backend_require_system_cert_store');
  AssertEqualInt(Ord(optBalanced), AObj.Integers['backend_optimization_target'],
    AMessagePrefix + ' clears backend_optimization_target');
end;

procedure Test_WithAutoBackendSelection_ClearsStaleExplicitState;
var
  LJSON: string;
  LObj: TJSONObject;
begin
  TestHeader('Test 1: WithAutoBackendSelection Clears Stale Explicit State');

  LJSON := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL)
    .WithAutoBackendSelection(CreateCustomRequirements)
    .ExportToJSON;

  LObj := ParseJSONObject(LJSON);
  try
    AssertEqualBool(True, LObj.Booleans['backend_auto_select'],
      'Switching to auto backend enables backend_auto_select');
    AssertEqualBool(False, LObj.Booleans['backend_explicit_library_set'],
      'Switching to auto backend clears backend_explicit_library_set');
    AssertEqualInt(Ord(sslOpenSSL), LObj.Integers['backend_explicit_library'],
      'Switching to auto backend resets inert explicit backend value');
    AssertEqualInt(1, LObj.Arrays['backend_required_ciphers'].Count,
      'Switching to auto backend preserves backend requirements');
  finally
    LObj.Free;
  end;
end;

procedure Test_WithBackend_ClearsStaleAutoRequirements;
var
  LJSON: string;
  LObj: TJSONObject;
begin
  TestHeader('Test 2: WithBackend Clears Stale Auto Requirements');

  LJSON := TSSLContextBuilder.Create
    .WithAutoBackendSelection(CreateCustomRequirements)
    .WithBackend(sslWinSSL)
    .ExportToJSON;

  LObj := ParseJSONObject(LJSON);
  try
    AssertEqualBool(False, LObj.Booleans['backend_auto_select'],
      'Switching to explicit backend clears backend_auto_select');
    AssertEqualBool(True, LObj.Booleans['backend_explicit_library_set'],
      'Switching to explicit backend keeps backend_explicit_library_set');
    AssertEqualInt(Ord(sslWinSSL), LObj.Integers['backend_explicit_library'],
      'Switching to explicit backend preserves selected library');
    AssertBackendRequirementsCleared(LObj,
      'Switching to explicit backend');
  finally
    LObj.Free;
  end;
end;

procedure Test_RequireTLS13_AfterExplicitBackend_MatchesPureAutoMode;
var
  LFromExplicitJSON: string;
  LPureAutoJSON: string;
begin
  TestHeader('Test 3: RequireTLS13 After Explicit Backend Matches Pure Auto Mode');

  LFromExplicitJSON := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL)
    .RequireTLS13
    .ExportToJSON;

  LPureAutoJSON := TSSLContextBuilder.Create
    .RequireTLS13
    .ExportToJSON;

  AssertEqualStr(LPureAutoJSON, LFromExplicitJSON,
    'RequireTLS13 after explicit backend normalizes to pure auto-mode snapshot');
end;

procedure Test_ImportFromJSON_NormalizesConflictingBackendModes;
const
  CJSON = '{' +
    '"backend_auto_select":true,' +
    '"backend_explicit_library":4,' +
    '"backend_explicit_library_set":true,' +
    '"backend_required_protocols":[6],' +
    '"backend_min_security_score":77,' +
    '"backend_prefer_os_native":true' +
  '}';
var
  LJSON: string;
  LObj: TJSONObject;
begin
  TestHeader('Test 4: ImportFromJSON Normalizes Conflicting Backend Modes');

  LJSON := TSSLContextBuilder.Create
    .ImportFromJSON(CJSON)
    .ExportToJSON;

  LObj := ParseJSONObject(LJSON);
  try
    AssertEqualBool(True, LObj.Booleans['backend_auto_select'],
      'Import keeps auto mode active when backend_auto_select=true');
    AssertEqualBool(False, LObj.Booleans['backend_explicit_library_set'],
      'Import clears conflicting explicit backend flag');
    AssertEqualInt(Ord(sslOpenSSL), LObj.Integers['backend_explicit_library'],
      'Import resets conflicting explicit backend value');
    AssertEqualInt(1, LObj.Arrays['backend_required_protocols'].Count,
      'Import preserves required protocol requirements');
    AssertEqualInt(6, LObj.Arrays['backend_required_protocols'].Integers[0],
      'Import preserves required protocol entry');
    AssertEqualInt(77, LObj.Integers['backend_min_security_score'],
      'Import preserves min security score');
    AssertEqualBool(True, LObj.Booleans['backend_prefer_os_native'],
      'Import preserves platform preferences in auto mode');
  finally
    LObj.Free;
  end;
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Backend Selection Mode Normalization Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_WithAutoBackendSelection_ClearsStaleExplicitState;
    Test_WithBackend_ClearsStaleAutoRequirements;
    Test_RequireTLS13_AfterExplicitBackend_MatchesPureAutoMode;
    Test_ImportFromJSON_NormalizesConflictingBackendModes;

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
