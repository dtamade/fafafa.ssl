program test_config_backend_selection_snapshot_semantics;

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

procedure AssertJSONHasBackendSelectionFields(const AJSON, AMessagePrefix: string);
const
  CFields: array[0..19] of string = (
    'backend_auto_select',
    'backend_explicit_library',
    'backend_explicit_library_set',
    'backend_required_protocols',
    'backend_required_ciphers',
    'backend_required_hashes',
    'backend_required_key_exchanges',
    'backend_required_features',
    'backend_preferred_ciphers',
    'backend_preferred_hashes',
    'backend_min_security_score',
    'backend_min_performance_score',
    'backend_min_compatibility_level',
    'backend_prefer_os_native',
    'backend_prefer_hardware_accel',
    'backend_prefer_fips_compliant',
    'backend_require_pkcs11',
    'backend_require_tpm',
    'backend_require_system_cert_store',
    'backend_optimization_target'
  );
var
  LRoot: TJSONData;
  LObj: TJSONObject;
  I: Integer;
begin
  LRoot := GetJSON(AJSON);
  try
    Assert(LRoot is TJSONObject, AMessagePrefix + ' JSON root is object');
    LObj := TJSONObject(LRoot);
    for I := Low(CFields) to High(CFields) do
      Assert(LObj.IndexOfName(CFields[I]) >= 0,
        AMessagePrefix + ' contains ' + CFields[I]);
  finally
    LRoot.Free;
  end;
end;

procedure Test_JSONRoundTrip_PreservesExplicitBackendSelection;
var
  LBuilder: ISSLContextBuilder;
  LJSON1, LJSON2: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 1: JSON Round Trip Preserves Explicit Backend Selection');

  LBuilder := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL);

  LJSON1 := LBuilder.ExportToJSON;
  AssertJSONHasBackendSelectionFields(LJSON1, 'Explicit backend export');

  LRoot := GetJSON(LJSON1);
  try
    LObj := TJSONObject(LRoot);
    AssertEqualBool(False, LObj.Booleans['backend_auto_select'],
      'JSON exports backend_auto_select=false for explicit backend');
    AssertEqualBool(True, LObj.Booleans['backend_explicit_library_set'],
      'JSON exports backend_explicit_library_set=true for explicit backend');
    AssertEqualInt(Ord(sslWinSSL), LObj.Integers['backend_explicit_library'],
      'JSON exports explicit backend library value');
  finally
    LRoot.Free;
  end;

  LJSON2 := TSSLContextBuilder.Create
    .ImportFromJSON(LJSON1)
    .ExportToJSON;

  AssertEqualStr(LJSON1, LJSON2,
    'JSON round trip preserves explicit backend selection state');
end;

procedure Test_INIRoundTrip_PreservesAutoBackendRequirements;
var
  LBuilder: ISSLContextBuilder;
  LINI1, LINI2: string;
begin
  TestHeader('Test 2: INI Round Trip Preserves Auto Backend Requirements');

  LBuilder := TSSLContextBuilder.Create
    .WithAutoBackendSelection(CreateCustomRequirements);

  LINI1 := LBuilder.ExportToINI;
  Assert(Pos('[Backend Selection]', LINI1) > 0,
    'INI exports backend selection section');
  Assert(Pos('backend_auto_select=true', LINI1) > 0,
    'INI exports backend_auto_select=true');
  Assert(Pos('backend_require_pkcs11=true', LINI1) > 0,
    'INI exports backend_require_pkcs11=true');
  Assert(Pos('backend_prefer_os_native=true', LINI1) > 0,
    'INI exports backend_prefer_os_native=true');
  Assert(Pos('backend_optimization_target=' + IntToStr(Ord(optSize)), LINI1) > 0,
    'INI exports backend_optimization_target');

  LINI2 := TSSLContextBuilder.Create
    .ImportFromINI(LINI1)
    .ExportToINI;

  AssertEqualStr(LINI1, LINI2,
    'INI round trip preserves auto backend requirements');
end;

procedure Test_Clone_PreservesBackendSelectionState;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON1, LJSON2: string;
begin
  TestHeader('Test 3: Clone Preserves Backend Selection State');

  LBuilder1 := TSSLContextBuilder.Create
    .WithAutoBackendSelection(CreateCustomRequirements);
  LBuilder2 := LBuilder1.Clone;

  LJSON1 := LBuilder1.ExportToJSON;
  LJSON2 := LBuilder2.ExportToJSON;

  AssertJSONHasBackendSelectionFields(LJSON2, 'Clone export');
  AssertEqualStr(LJSON1, LJSON2,
    'Clone preserves backend-selection snapshot state');
end;

procedure Test_Merge_SourceBackendSelection_OverridesTarget;
var
  LBase, LSource: ISSLContextBuilder;
  LExpectedJSON, LActualJSON: string;
begin
  TestHeader('Test 4: Merge Source Backend Selection Overrides Target');

  LBase := TSSLContextBuilder.Create
    .WithBackend(sslWinSSL);
  LSource := TSSLContextBuilder.Create
    .WithAutoBackendSelection(CreateCustomRequirements);

  LExpectedJSON := LSource.ExportToJSON;
  LBase.Merge(LSource);
  LActualJSON := LBase.ExportToJSON;

  AssertJSONHasBackendSelectionFields(LActualJSON, 'Merged export');
  AssertEqualStr(LExpectedJSON, LActualJSON,
    'Merge adopts source backend-selection state');
end;

procedure Test_Reset_RestoresBackendSelectionDefaults;
var
  LBuilder: ISSLContextBuilder;
  LDefaultJSON, LModifiedJSON, LResetJSON: string;
begin
  TestHeader('Test 5: Reset Restores Backend Selection Defaults');

  LDefaultJSON := TSSLContextBuilder.Create.ExportToJSON;

  LBuilder := TSSLContextBuilder.Create
    .WithAutoBackendSelection(CreateCustomRequirements);
  LModifiedJSON := LBuilder.ExportToJSON;
  Assert(LModifiedJSON <> LDefaultJSON,
    'Backend selection changes snapshot before Reset');

  LBuilder.Reset;
  LResetJSON := LBuilder.ExportToJSON;

  AssertEqualStr(LDefaultJSON, LResetJSON,
    'Reset restores backend-selection defaults');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Backend Selection Snapshot Semantics Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_JSONRoundTrip_PreservesExplicitBackendSelection;
    Test_INIRoundTrip_PreservesAutoBackendRequirements;
    Test_Clone_PreservesBackendSelectionState;
    Test_Merge_SourceBackendSelection_OverridesTarget;
    Test_Reset_RestoresBackendSelectionDefaults;

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
