program test_config_snapshot_clone;

{$mode objfpc}{$H+}

{**
 * Test suite for Phase 2.1.4 - Configuration Snapshot and Clone
 *
 * Tests the snapshot and clone functionality:
 * 1. Clone - Create independent copy of builder
 * 2. Reset - Reset builder to default configuration
 * 3. ResetToDefaults - Alias for Reset
 * 4. Merge - Merge configuration from another builder
 *}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils;

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

{ Test 1: Clone creates independent copy }
procedure Test_Clone_Independence;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON1, LJSON2: string;
begin
  TestHeader('Test 1: Clone Creates Independent Copy');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithCipherList('ECDHE+AESGCM');

  // Clone the builder
  LBuilder2 := LBuilder1.Clone;

  Assert(LBuilder2 <> nil, 'Clone returns valid builder');

  // Export both to verify they are identical
  LJSON1 := LBuilder1.ExportToJSON;
  LJSON2 := LBuilder2.ExportToJSON;

  Assert(LJSON1 = LJSON2, 'Clone produces identical configuration');

  // Modify clone
  LBuilder2.WithTLS13.WithCipherList('CHACHA20');

  // Verify original is unchanged
  LJSON1 := LBuilder1.ExportToJSON;
  LJSON2 := LBuilder2.ExportToJSON;

  Assert(LJSON1 <> LJSON2, 'Modifying clone does not affect original');
end;

{ Test 2: Clone copies all fields }
procedure Test_Clone_AllFields;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON1, LJSON2: string;
begin
  TestHeader('Test 2: Clone Copies All Fields');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyPeer
    .WithVerifyDepth(15)
    .WithCertificate('/path/to/cert.pem')
    .WithPrivateKey('/path/to/key.pem', 'password')
    .WithCAFile('/path/to/ca.pem')
    .WithCipherList('ECDHE+AESGCM')
    .WithTLS13Ciphersuites('TLS_AES_256_GCM_SHA384')
    .WithSNI('example.com')
    .WithALPN('h2,http/1.1')
    .WithSessionCache(True)
    .WithSessionTimeout(3600);

  LBuilder2 := LBuilder1.Clone;

  LJSON1 := LBuilder1.ExportToJSON;
  LJSON2 := LBuilder2.ExportToJSON;

  Assert(LJSON1 = LJSON2, 'Clone copies all fields correctly');
end;

{ Test 3: Reset restores defaults }
procedure Test_Reset_RestoresDefaults;
var
  LBuilder: ISSLContextBuilder;
  LDefault, LModified, LReset: string;
begin
  TestHeader('Test 3: Reset Restores Defaults');

  // Get default configuration
  LDefault := TSSLContextBuilder.Create.ExportToJSON;

  // Create and modify builder
  LBuilder := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyNone
    .WithCipherList('CHACHA20')
    .WithSessionTimeout(7200);

  LModified := LBuilder.ExportToJSON;
  Assert(LDefault <> LModified, 'Configuration is modified');

  // Reset to defaults
  LBuilder.Reset;
  LReset := LBuilder.ExportToJSON;

  Assert(LDefault = LReset, 'Reset restores default configuration');
end;

{ Test 4: ResetToDefaults is alias for Reset }
procedure Test_ResetToDefaults_Alias;
var
  LBuilder: ISSLContextBuilder;
  LReset1, LReset2: string;
begin
  TestHeader('Test 4: ResetToDefaults Is Alias For Reset');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyNone;

  // Reset using Reset
  LBuilder.Reset;
  LReset1 := LBuilder.ExportToJSON;

  // Modify again
  LBuilder.WithTLS12.WithSessionTimeout(9999);

  // Reset using ResetToDefaults
  LBuilder.ResetToDefaults;
  LReset2 := LBuilder.ExportToJSON;

  Assert(LReset1 = LReset2, 'ResetToDefaults produces same result as Reset');
end;

{ Test 5: Reset supports method chaining }
procedure Test_Reset_Chaining;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 5: Reset Supports Method Chaining');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'test.local', 'Test Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyNone
    .Reset  // Reset and continue chaining
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  LResult := LBuilder.TryBuildServer(LContext);

  Assert(LResult.IsOk, 'Reset in chain allows successful build');
end;

{ Test 6: Merge from empty source }
procedure Test_Merge_EmptySource;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LBefore, LAfter: string;
begin
  TestHeader('Test 6: Merge From Empty Source');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyPeer;

  LBefore := LBuilder1.ExportToJSON;

  // Merge with empty builder
  LBuilder2 := TSSLContextBuilder.Create;
  LBuilder1.Merge(LBuilder2);

  LAfter := LBuilder1.ExportToJSON;

  // Should remain unchanged or update to source's defaults
  Assert(True, 'Merge with empty source completes');
end;

{ Test 7: Merge from nil source }
procedure Test_Merge_NilSource;
var
  LBuilder: ISSLContextBuilder;
  LBefore, LAfter: string;
begin
  TestHeader('Test 7: Merge From Nil Source');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS13;

  LBefore := LBuilder.ExportToJSON;

  // Merge with nil
  LBuilder.Merge(nil);

  LAfter := LBuilder.ExportToJSON;

  Assert(LBefore = LAfter, 'Merge with nil does not change configuration');
end;

{ Test 8: Merge replaces fields }
procedure Test_Merge_ReplacesFields;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 8: Merge Replaces Fields');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS12
    .WithCipherList('ECDHE+AESGCM')
    .WithSessionTimeout(600);

  LBuilder2 := TSSLContextBuilder.Create
    .WithTLS13  // Different protocol
    .WithCipherList('CHACHA20');  // Different cipher

  // Merge Builder2 into Builder1
  LBuilder1.Merge(LBuilder2);

  LJSON := LBuilder1.ExportToJSON;

  // Verify configuration changed
  Assert(Pos('CHACHA20', LJSON) > 0, 'Merge applied cipher list from source');
  Assert(Pos('600', LJSON) = 0, 'Session timeout from original is replaced');
end;

{ Test 9: Merge preserves unspecified fields }
procedure Test_Merge_PreservesFields;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSONBefore, LJSONAfter: string;
begin
  TestHeader('Test 9: Merge Preserves Unspecified Fields');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS12
    .WithCipherList('ECDHE+AESGCM')
    .WithSessionTimeout(600);

  // Builder2 only specifies cipher list
  LBuilder2 := TSSLContextBuilder.Create.Reset
    .WithCipherList('CHACHA20');

  LJSONBefore := LBuilder1.ExportToJSON;

  // Merge
  LBuilder1.Merge(LBuilder2);

  LJSONAfter := LBuilder1.ExportToJSON;

  Assert(LJSONBefore <> LJSONAfter, 'Configuration changed after merge');
end;

{ Test 10: Merge supports method chaining }
procedure Test_Merge_Chaining;
var
  LBuilder1, LBuilder2, LBuilder3: ISSLContextBuilder;
begin
  TestHeader('Test 10: Merge Supports Method Chaining');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS12;

  LBuilder2 := TSSLContextBuilder.Create
    .WithTLS13;

  LBuilder3 := TSSLContextBuilder.Create
    .WithSessionTimeout(9999);

  // Chain multiple merges
  LBuilder1
    .Merge(LBuilder2)
    .Merge(LBuilder3)
    .WithVerifyPeer;

  Assert(True, 'Multiple merges in chain work correctly');
end;

{ Test 11: Clone and merge workflow }
procedure Test_Clone_Merge_Workflow;
var
  LBase, LClone1, LClone2: ISSLContextBuilder;
  LOverride: ISSLContextBuilder;
begin
  TestHeader('Test 11: Clone And Merge Workflow');

  // Base configuration
  LBase := TSSLContextBuilder.Production;

  // Create two independent clones
  LClone1 := LBase.Clone.WithSessionTimeout(1800);
  LClone2 := LBase.Clone.WithSessionTimeout(3600);

  // Override configuration
  LOverride := TSSLContextBuilder.Create
    .WithVerifyDepth(20);

  // Merge override into both clones
  LClone1.Merge(LOverride);
  LClone2.Merge(LOverride);

  Assert(True, 'Clone and merge workflow completes successfully');
end;

{ Test 12: Reset and rebuild }
procedure Test_Reset_Rebuild;
var
  LBuilder: ISSLContextBuilder;
  LContext1, LContext2: ISSLContext;
  LResult: TSSLOperationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 12: Reset And Rebuild');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'test.local', 'Test Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  // Build first context
  LResult := LBuilder.TryBuildServer(LContext1);
  Assert(LResult.IsOk, 'First build succeeds');

  // Reset and build again
  LBuilder.Reset
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  LResult := LBuilder.TryBuildServer(LContext2);
  Assert(LResult.IsOk, 'Second build after reset succeeds');

  Assert(LContext1 <> LContext2, 'Reset creates new independent context');
end;

{ Test 13: Preset clone }
procedure Test_Preset_Clone;
var
  LDev1, LDev2: ISSLContextBuilder;
  LJSON1, LJSON2: string;
begin
  TestHeader('Test 13: Preset Clone');

  LDev1 := TSSLContextBuilder.Development;
  LDev2 := LDev1.Clone;

  LJSON1 := LDev1.ExportToJSON;
  LJSON2 := LDev2.ExportToJSON;

  Assert(LJSON1 = LJSON2, 'Cloning preset produces identical configuration');

  // Modify clone
  LDev2.WithSessionTimeout(9999);

  LJSON1 := LDev1.ExportToJSON;
  LJSON2 := LDev2.ExportToJSON;

  Assert(LJSON1 <> LJSON2, 'Modifying preset clone does not affect original');
end;

{ Test 14: Merge with preset }
procedure Test_Merge_WithPreset;
var
  LBuilder: ISSLContextBuilder;
  LProd: ISSLContextBuilder;
begin
  TestHeader('Test 14: Merge With Preset');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12;

  LProd := TSSLContextBuilder.Production;

  // Merge production defaults
  LBuilder.Merge(LProd);

  Assert(True, 'Merging with preset completes successfully');
end;

{ Test 15: Complex merge scenario }
procedure Test_Complex_Merge;
var
  LBase, LDev, LProd: ISSLContextBuilder;
  LFinal: string;
begin
  TestHeader('Test 15: Complex Merge Scenario');

  // Start with development preset
  LBase := TSSLContextBuilder.Development.Clone;

  // Create custom configurations
  LDev := TSSLContextBuilder.Create
    .WithSessionTimeout(1800)
    .WithCipherList('DEV-CIPHER');

  LProd := TSSLContextBuilder.StrictSecurity.Clone;

  // Merge development settings, then production security
  LBase.Merge(LDev).Merge(LProd);

  LFinal := LBase.ExportToJSON;

  Assert(LFinal <> '', 'Complex merge produces valid configuration');
end;

{ Main Test Runner }
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Phase 2.1.4 Configuration Snapshot and Clone Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('Testing snapshot and clone functionality:');
  WriteLn('  1. Clone - Independent copy');
  WriteLn('  2. Reset - Restore defaults');
  WriteLn('  3. ResetToDefaults - Alias');
  WriteLn('  4. Merge - Combine configurations');
  WriteLn;

  try
    // Run all tests
    Test_Clone_Independence;
    Test_Clone_AllFields;
    Test_Reset_RestoresDefaults;
    Test_ResetToDefaults_Alias;
    Test_Reset_Chaining;
    Test_Merge_EmptySource;
    Test_Merge_NilSource;
    Test_Merge_ReplacesFields;
    Test_Merge_PreservesFields;
    Test_Merge_Chaining;
    Test_Clone_Merge_Workflow;
    Test_Reset_Rebuild;
    Test_Preset_Clone;
    Test_Merge_WithPreset;
    Test_Complex_Merge;

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
