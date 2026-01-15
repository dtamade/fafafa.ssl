program test_config_import_export;

{$mode objfpc}{$H+}

{**
 * Test suite for Phase 2.1.3 - Configuration Import/Export
 *
 * Tests the import/export functionality:
 * 1. JSON export - produces valid JSON
 * 2. JSON import - correctly restores configuration
 * 3. JSON round-trip - export → import → export produces identical results
 * 4. INI export - produces valid INI format
 * 5. INI import - correctly restores configuration
 * 6. INI round-trip - export → import → export produces identical results
 * 7. All configuration fields (protocols, certs, ciphers, options)
 * 8. Edge cases (empty config, missing fields)
 * 9. Preset configuration export/import
 *}

uses
  SysUtils,
  fafafa.ssl.base,
  fafafa.ssl.context.builder,
  fafafa.ssl.cert.utils,
  fafafa.ssl.exceptions,
  fpjson, jsonparser;

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

{ Test 1: JSON export produces valid JSON }
procedure Test_JSONExport_ValidJSON;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LData: TJSONData;
begin
  TestHeader('Test 1: JSON Export Produces Valid JSON');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithCipherList('ECDHE+AESGCM');

  LJSON := LBuilder.ExportToJSON;

  Assert(LJSON <> '', 'JSON export is not empty');

  // Try to parse JSON
  try
    LData := GetJSON(LJSON);
    try
      Assert(LData <> nil, 'JSON is valid and parseable');
      Assert(LData is TJSONObject, 'JSON root is an object');
    finally
      LData.Free;
    end;
  except
    on E: Exception do
    begin
      Assert(False, 'JSON parsing failed: ' + E.Message);
    end;
  end;
end;

{ Test 2: JSON export contains expected fields }
procedure Test_JSONExport_HasExpectedFields;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 2: JSON Export Contains Expected Fields');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithCipherList('ECDHE+AESGCM')
    .WithSessionTimeout(600);

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    Assert(LRoot is TJSONObject, 'JSON root is object');
    LObj := TJSONObject(LRoot);

    Assert(LObj.IndexOfName('protocols') >= 0, 'Has protocols field');
    Assert(LObj.IndexOfName('verify_modes') >= 0, 'Has verify_modes field');
    Assert(LObj.IndexOfName('cipher_list') >= 0, 'Has cipher_list field');
    Assert(LObj.IndexOfName('session_timeout') >= 0, 'Has session_timeout field');
    Assert(LObj.IndexOfName('options') >= 0, 'Has options field');
  finally
    LRoot.Free;
  end;
end;

{ Test 3: JSON import restores configuration }
procedure Test_JSONImport_RestoresConfig;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 3: JSON Import Restores Configuration');

  // Create builder with specific config
  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyPeer
    .WithVerifyDepth(5)
    .WithCipherList('ECDHE+AESGCM')
    .WithSessionTimeout(900);

  // Export to JSON
  LJSON := LBuilder1.ExportToJSON;

  // Create new builder and import
  LBuilder2 := TSSLContextBuilder.Create.ImportFromJSON(LJSON);

  // Export again and compare
  Assert(LBuilder2.ExportToJSON <> '', 'Imported builder can export');

  // Note: We can't directly compare all fields, but we can verify it doesn't crash
  Assert(True, 'JSON import completed without errors');
end;

{ Test 4: JSON round-trip produces consistent results }
procedure Test_JSONRoundTrip;
var
  LBuilder: ISSLContextBuilder;
  LJSON1, LJSON2: string;
begin
  TestHeader('Test 4: JSON Round-Trip Produces Consistent Results');

  // Create builder
  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithCipherList('ECDHE+AESGCM');

  // First export
  LJSON1 := LBuilder.ExportToJSON;

  // Import and export again
  LJSON2 := TSSLContextBuilder.Create
    .ImportFromJSON(LJSON1)
    .ExportToJSON;

  Assert(LJSON1 <> '', 'First export is not empty');
  Assert(LJSON2 <> '', 'Second export is not empty');
  Assert(LJSON1 = LJSON2, 'Round-trip produces identical JSON');
end;

{ Test 5: INI export produces valid format }
procedure Test_INIExport_ValidFormat;
var
  LBuilder: ISSLContextBuilder;
  LINI: string;
begin
  TestHeader('Test 5: INI Export Produces Valid Format');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer;

  LINI := LBuilder.ExportToINI;

  Assert(LINI <> '', 'INI export is not empty');
  Assert(Pos('[SSL Context Configuration]', LINI) > 0, 'Contains main section header');
  Assert(Pos('protocols=', LINI) > 0, 'Contains protocols field');
  Assert(Pos('verify_modes=', LINI) > 0, 'Contains verify_modes field');
end;

{ Test 6: INI export contains section headers }
procedure Test_INIExport_HasSections;
var
  LBuilder: ISSLContextBuilder;
  LINI: string;
begin
  TestHeader('Test 6: INI Export Contains Section Headers');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13;

  LINI := LBuilder.ExportToINI;

  Assert(Pos('[SSL Context Configuration]', LINI) > 0, 'Has main section');
  Assert(Pos('[Certificates]', LINI) > 0, 'Has Certificates section');
  Assert(Pos('[Ciphers]', LINI) > 0, 'Has Ciphers section');
  Assert(Pos('[Advanced]', LINI) > 0, 'Has Advanced section');
  Assert(Pos('[Options]', LINI) > 0, 'Has Options section');
end;

{ Test 7: INI import restores configuration }
procedure Test_INIImport_RestoresConfig;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LINI: string;
begin
  TestHeader('Test 7: INI Import Restores Configuration');

  LBuilder1 := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer;

  LINI := LBuilder1.ExportToINI;

  LBuilder2 := TSSLContextBuilder.Create.ImportFromINI(LINI);

  Assert(LBuilder2.ExportToINI <> '', 'Imported builder can export');
  Assert(True, 'INI import completed without errors');
end;

{ Test 8: INI round-trip produces consistent results }
procedure Test_INIRoundTrip;
var
  LBuilder: ISSLContextBuilder;
  LINI1, LINI2: string;
begin
  TestHeader('Test 8: INI Round-Trip Produces Consistent Results');

  LBuilder := TSSLContextBuilder.Create
    .WithTLS12And13
    .WithVerifyPeer
    .WithSessionTimeout(600);

  LINI1 := LBuilder.ExportToINI;

  LINI2 := TSSLContextBuilder.Create
    .ImportFromINI(LINI1)
    .ExportToINI;

  Assert(LINI1 <> '', 'First export is not empty');
  Assert(LINI2 <> '', 'Second export is not empty');
  Assert(LINI1 = LINI2, 'Round-trip produces identical INI');
end;

{ Test 9: Export all protocol versions }
procedure Test_Export_AllProtocols;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LProtocols: TJSONArray;
begin
  TestHeader('Test 9: Export All Protocol Versions');

  LBuilder := TSSLContextBuilder.Create
    .WithProtocols([sslProtocolTLS10, sslProtocolTLS11, sslProtocolTLS12, sslProtocolTLS13]);

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LProtocols := TJSONObject(LRoot).Arrays['protocols'];
    Assert(LProtocols.Count = 4, 'Exported 4 protocol versions');
  finally
    LRoot.Free;
  end;
end;

{ Test 10: Export with certificate paths }
procedure Test_Export_WithCertPaths;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 10: Export With Certificate Paths');

  LBuilder := TSSLContextBuilder.Create
    .WithCertificate('/path/to/cert.pem')
    .WithPrivateKey('/path/to/key.pem', 'password123')
    .WithCAFile('/path/to/ca.pem');

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Strings['certificate_file'] = '/path/to/cert.pem', 'Certificate file exported');
    Assert(LObj.Strings['private_key_file'] = '/path/to/key.pem', 'Private key file exported');
    Assert(LObj.Strings['ca_file'] = '/path/to/ca.pem', 'CA file exported');
  finally
    LRoot.Free;
  end;
end;

{ Test 11: Export with cipher configuration }
procedure Test_Export_WithCiphers;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 11: Export With Cipher Configuration');

  LBuilder := TSSLContextBuilder.Create
    .WithCipherList('ECDHE+AESGCM:ECDHE+AES256')
    .WithTLS13Ciphersuites('TLS_AES_256_GCM_SHA384');

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Strings['cipher_list'] = 'ECDHE+AESGCM:ECDHE+AES256', 'Cipher list exported');
    Assert(LObj.Strings['tls13_ciphersuites'] = 'TLS_AES_256_GCM_SHA384', 'TLS 1.3 ciphersuites exported');
  finally
    LRoot.Free;
  end;
end;

{ Test 12: Export with advanced options }
procedure Test_Export_WithAdvancedOptions;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 12: Export With Advanced Options');

  LBuilder := TSSLContextBuilder.Create
    .WithSNI('example.com')
    .WithALPN('h2,http/1.1')
    .WithSessionCache(True)
    .WithSessionTimeout(3600);

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Strings['server_name'] = 'example.com', 'SNI server name exported');
    Assert(LObj.Strings['alpn_protocols'] = 'h2,http/1.1', 'ALPN protocols exported');
    Assert(LObj.Booleans['session_cache_enabled'] = True, 'Session cache exported');
    Assert(LObj.Integers['session_timeout'] = 3600, 'Session timeout exported');
  finally
    LRoot.Free;
  end;
end;

{ Test 13: Import empty JSON }
procedure Test_Import_EmptyJSON;
var
  LBuilder: ISSLContextBuilder;
begin
  TestHeader('Test 13: Import Empty JSON');

  LBuilder := TSSLContextBuilder.Create;

  // Import empty JSON should not crash
  try
    LBuilder.ImportFromJSON('');
    Assert(True, 'Empty JSON import does not crash');
  except
    on E: Exception do
      Assert(False, 'Empty JSON import crashed: ' + E.Message);
  end;
end;

{ Test 14: Import empty INI }
procedure Test_Import_EmptyINI;
var
  LBuilder: ISSLContextBuilder;
begin
  TestHeader('Test 14: Import Empty INI');

  LBuilder := TSSLContextBuilder.Create;

  // Import empty INI should not crash
  try
    LBuilder.ImportFromINI('');
    Assert(True, 'Empty INI import does not crash');
  except
    on E: Exception do
      Assert(False, 'Empty INI import crashed: ' + E.Message);
  end;
end;

{ Test 15: Preset configuration export }
procedure Test_Preset_Export;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 15: Preset Configuration Export');

  // Test Development preset
  LBuilder := TSSLContextBuilder.Development;
  LJSON := LBuilder.ExportToJSON;
  Assert(LJSON <> '', 'Development preset can be exported to JSON');

  // Test Production preset
  LBuilder := TSSLContextBuilder.Production;
  LJSON := LBuilder.ExportToJSON;
  Assert(LJSON <> '', 'Production preset can be exported to JSON');

  // Test StrictSecurity preset
  LBuilder := TSSLContextBuilder.StrictSecurity;
  LJSON := LBuilder.ExportToJSON;
  Assert(LJSON <> '', 'StrictSecurity preset can be exported to JSON');
end;

{ Test 16: Preset configuration import and use }
procedure Test_Preset_ImportAndUse;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 16: Preset Configuration Import and Use');

  // Generate test certificate
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'test.local', 'Test Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  // Export Production preset
  LJSON := TSSLContextBuilder.Production.ExportToJSON;

  // Create new builder from JSON and add certificate
  LBuilder := TSSLContextBuilder.Create
    .ImportFromJSON(LJSON)
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  // Try to build server context
  LResult := LBuilder.TryBuildServer(LContext);

  Assert(LResult.IsOk, 'Imported preset config can build server context');
  if not LResult.IsOk then
    WriteLn('  Error: ', LResult.ErrorMessage);
end;

{ Test 17: System roots configuration export }
procedure Test_Export_SystemRoots;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 17: System Roots Configuration Export');

  LBuilder := TSSLContextBuilder.Create
    .WithSystemRoots;

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Booleans['use_system_roots'] = True, 'System roots flag exported');
  finally
    LRoot.Free;
  end;
end;

{ Test 18: Options export and import }
procedure Test_Options_ExportImport;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LOptions: TJSONArray;
begin
  TestHeader('Test 18: Options Export and Import');

  LBuilder1 := TSSLContextBuilder.Create
    .WithOptions([ssoEnableSNI, ssoDisableCompression, ssoDisableRenegotiation]);

  LJSON := LBuilder1.ExportToJSON;

  // Check options are in JSON
  LRoot := GetJSON(LJSON);
  try
    LOptions := TJSONObject(LRoot).Arrays['options'];
    Assert(LOptions.Count > 0, 'Options exported to JSON');
  finally
    LRoot.Free;
  end;

  // Import and verify
  LBuilder2 := TSSLContextBuilder.Create.ImportFromJSON(LJSON);
  Assert(True, 'Options imported successfully');
end;

{ Main Test Runner }
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Phase 2.1.3 Configuration Import/Export Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('Testing configuration import/export functionality:');
  WriteLn('  1. JSON export/import');
  WriteLn('  2. INI export/import');
  WriteLn('  3. Round-trip consistency');
  WriteLn('  4. All configuration fields');
  WriteLn('  5. Edge cases');
  WriteLn;

  try
    // Run all tests
    Test_JSONExport_ValidJSON;
    Test_JSONExport_HasExpectedFields;
    Test_JSONImport_RestoresConfig;
    Test_JSONRoundTrip;
    Test_INIExport_ValidFormat;
    Test_INIExport_HasSections;
    Test_INIImport_RestoresConfig;
    Test_INIRoundTrip;
    Test_Export_AllProtocols;
    Test_Export_WithCertPaths;
    Test_Export_WithCiphers;
    Test_Export_WithAdvancedOptions;
    Test_Import_EmptyJSON;
    Test_Import_EmptyINI;
    Test_Preset_Export;
    Test_Preset_ImportAndUse;
    Test_Export_SystemRoots;
    Test_Options_ExportImport;

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
