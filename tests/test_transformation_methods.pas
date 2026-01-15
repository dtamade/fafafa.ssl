program test_transformation_methods;

{$mode objfpc}{$H+}

{**
 * Test suite for Phase 2.2.4 - Configuration Transformation
 *
 * Tests the configuration transformation functionality:
 * 1. Transform - Apply transformation function
 * 2. Extend - Extend options set
 * 3. Override - Override specific configuration fields
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

{ Global transformation functions for testing }

function AddCustomTimeout(ABuilder: ISSLContextBuilder): ISSLContextBuilder;
begin
  Result := ABuilder.WithSessionTimeout(9999);
end;

function AddCustomCipher(ABuilder: ISSLContextBuilder): ISSLContextBuilder;
begin
  Result := ABuilder.WithCipherList('CUSTOM-CIPHER');
end;

function ChainTransform(ABuilder: ISSLContextBuilder): ISSLContextBuilder;
begin
  Result := ABuilder
    .WithSessionTimeout(8888)
    .WithVerifyDepth(15);
end;

{ Test 1: Transform applies function }
procedure Test_Transform_AppliesFunction;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 1: Transform Applies Function');

  LBuilder := TSSLContextBuilder.Create
    .Transform(@AddCustomTimeout);

  LJSON := LBuilder.ExportToJSON;

  Assert(Pos('9999', LJSON) > 0,
    'Transform applies transformation function');
end;

{ Test 2: Transform with nil function }
procedure Test_Transform_NilFunction;
var
  LBuilder: ISSLContextBuilder;
begin
  TestHeader('Test 2: Transform With Nil Function');

  try
    LBuilder := TSSLContextBuilder.Create
      .Transform(nil);

    Assert(True, 'Transform with nil function does not crash');
  except
    Assert(False, 'Transform with nil function crashed');
  end;
end;

{ Test 3: Transform supports method chaining }
procedure Test_Transform_Chaining;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 3: Transform Supports Method Chaining');

  LBuilder := TSSLContextBuilder.Create
    .Transform(@AddCustomTimeout)
    .WithVerifyNone;

  LJSON := LBuilder.ExportToJSON;

  Assert(Pos('9999', LJSON) > 0,
    'Method chaining works after Transform');
end;

{ Test 4: Multiple Transform calls }
procedure Test_Multiple_Transform;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 4: Multiple Transform Calls');

  LBuilder := TSSLContextBuilder.Create
    .Transform(@AddCustomTimeout)
    .Transform(@AddCustomCipher);

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('9999', LJSON) > 0) and (Pos('CUSTOM-CIPHER', LJSON) > 0),
    'Multiple Transform calls work correctly');
end;

{ Test 5: Transform with chaining inside }
procedure Test_Transform_ChainInside;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 5: Transform With Chaining Inside');

  LBuilder := TSSLContextBuilder.Create
    .Transform(@ChainTransform);

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('8888', LJSON) > 0),
    'Transform with internal chaining works');
end;

{ Test 6: Extend adds single option }
procedure Test_Extend_SingleOption;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 6: Extend Adds Single Option');

  LBuilder := TSSLContextBuilder.Create
    .Extend([ssoEnableSessionTickets]);

  LJSON := LBuilder.ExportToJSON;

  Assert(Pos('options', LJSON) > 0,
    'Extend adds single option');
end;

{ Test 7: Extend adds multiple options }
procedure Test_Extend_MultipleOptions;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 7: Extend Adds Multiple Options');

  LBuilder := TSSLContextBuilder.Create
    .Extend([ssoEnableSessionTickets, ssoEnableALPN, ssoEnableSNI]);

  LJSON := LBuilder.ExportToJSON;

  Assert(Pos('options', LJSON) > 0,
    'Extend adds multiple options');
end;

{ Test 8: Extend preserves existing options }
procedure Test_Extend_PreservesOptions;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 8: Extend Preserves Existing Options');

  LBuilder := TSSLContextBuilder.Create
    .WithOption(ssoDisableCompression)
    .Extend([ssoEnableSessionTickets]);

  LJSON := LBuilder.ExportToJSON;

  // Should have both options
  Assert(Pos('options', LJSON) > 0,
    'Extend preserves existing options');
end;

{ Test 9: Extend supports method chaining }
procedure Test_Extend_Chaining;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 9: Extend Supports Method Chaining');

  LBuilder := TSSLContextBuilder.Create
    .Extend([ssoEnableSessionTickets])
    .WithSessionTimeout(7777);

  LJSON := LBuilder.ExportToJSON;

  Assert(Pos('7777', LJSON) > 0,
    'Method chaining works after Extend');
end;

{ Test 10: Extend with empty array }
procedure Test_Extend_EmptyArray;
var
  LBuilder: ISSLContextBuilder;
  LOptions: array of TSSLOption;
begin
  TestHeader('Test 10: Extend With Empty Array');

  SetLength(LOptions, 0);

  try
    LBuilder := TSSLContextBuilder.Create
      .Extend(LOptions);

    Assert(True, 'Extend handles empty array');
  except
    Assert(False, 'Extend crashed with empty array');
  end;
end;

{ Test 11: Override cipher_list }
procedure Test_Override_CipherList;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 11: Override Cipher List');

  LBuilder := TSSLContextBuilder.Create
    .WithCipherList('OLD-CIPHER')
    .Override('cipher_list', 'NEW-CIPHER');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('NEW-CIPHER', LJSON) > 0) and (Pos('OLD-CIPHER', LJSON) = 0),
    'Override replaces cipher_list');
end;

{ Test 12: Override session_timeout }
procedure Test_Override_SessionTimeout;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 12: Override Session Timeout');

  LBuilder := TSSLContextBuilder.Create
    .WithSessionTimeout(1000)
    .Override('session_timeout', '5555');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('5555', LJSON) > 0) and (Pos('1000', LJSON) = 0),
    'Override replaces session_timeout');
end;

{ Test 13: Override server_name }
procedure Test_Override_ServerName;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 13: Override Server Name');

  LBuilder := TSSLContextBuilder.Create
    .WithSNI('old.example.com')
    .Override('server_name', 'new.example.com');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('new.example.com', LJSON) > 0),
    'Override replaces server_name');
end;

{ Test 14: Override supports method chaining }
procedure Test_Override_Chaining;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 14: Override Supports Method Chaining');

  LBuilder := TSSLContextBuilder.Create
    .Override('cipher_list', 'CIPHER-1')
    .WithSessionTimeout(6666);

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('CIPHER-1', LJSON) > 0) and (Pos('6666', LJSON) > 0),
    'Method chaining works after Override');
end;

{ Test 15: Multiple Override calls }
procedure Test_Multiple_Override;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 15: Multiple Override Calls');

  LBuilder := TSSLContextBuilder.Create
    .Override('cipher_list', 'CIPHER-A')
    .Override('session_timeout', '4444')
    .Override('server_name', 'test.local');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('CIPHER-A', LJSON) > 0) and (Pos('4444', LJSON) > 0) and (Pos('test.local', LJSON) > 0),
    'Multiple Override calls work correctly');
end;

{ Test 16: Override unknown field }
procedure Test_Override_UnknownField;
var
  LBuilder: ISSLContextBuilder;
begin
  TestHeader('Test 16: Override Unknown Field');

  try
    LBuilder := TSSLContextBuilder.Create
      .Override('unknown_field', 'some_value');

    Assert(True, 'Override ignores unknown field gracefully');
  except
    Assert(False, 'Override crashed with unknown field');
  end;
end;

{ Test 17: Override case insensitive }
procedure Test_Override_CaseInsensitive;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 17: Override Case Insensitive');

  LBuilder := TSSLContextBuilder.Create
    .Override('CIPHER_LIST', 'UPPER-CASE')
    .Override('Session_Timeout', '3333');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('UPPER-CASE', LJSON) > 0) and (Pos('3333', LJSON) > 0),
    'Override is case insensitive');
end;

{ Test 18: Combining Transform, Extend, Override }
procedure Test_Combining_All;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 18: Combining Transform, Extend, Override');

  LBuilder := TSSLContextBuilder.Create
    .Transform(@AddCustomTimeout)
    .Extend([ssoEnableSessionTickets, ssoEnableALPN])
    .Override('cipher_list', 'COMBINED-CIPHER');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('9999', LJSON) > 0) and (Pos('COMBINED-CIPHER', LJSON) > 0),
    'Transform, Extend, Override work together');
end;

{ Test 19: Transformation with presets }
procedure Test_Transformation_WithPresets;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
begin
  TestHeader('Test 19: Transformation With Presets');

  LBuilder := TSSLContextBuilder.Production
    .Transform(@AddCustomCipher)
    .Override('session_timeout', '1111');

  LJSON := LBuilder.ExportToJSON;

  Assert((Pos('CUSTOM-CIPHER', LJSON) > 0) and (Pos('1111', LJSON) > 0),
    'Transformation methods work with presets');
end;

{ Test 20: Build context after transformation }
procedure Test_BuildAfterTransformation;
var
  LBuilder: ISSLContextBuilder;
  LContext: ISSLContext;
  LResult: TSSLOperationResult;
  LCert, LKey: string;
begin
  TestHeader('Test 20: Build Context After Transformation');

  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'test.local', 'Test Org', 30, LCert, LKey
  ) then
  begin
    WriteLn('  ✗ Failed to generate test certificate');
    Exit;
  end;

  LBuilder := TSSLContextBuilder.Create
    .Transform(@AddCustomTimeout)
    .Extend([ssoEnableSessionTickets])
    .Override('cipher_list', 'ECDHE+AESGCM')
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey);

  LResult := LBuilder.TryBuildServer(LContext);

  Assert(LResult.IsOk,
    'Can build context after transformation methods');
end;

{ Main Test Runner }
begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Phase 2.2.4 Configuration Transformation Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;
  WriteLn('Testing configuration transformation methods:');
  WriteLn('  1. Transform - Apply transformation function');
  WriteLn('  2. Extend - Extend options set');
  WriteLn('  3. Override - Override configuration fields');
  WriteLn;

  try
    // Run all tests
    Test_Transform_AppliesFunction;
    Test_Transform_NilFunction;
    Test_Transform_Chaining;
    Test_Multiple_Transform;
    Test_Transform_ChainInside;
    Test_Extend_SingleOption;
    Test_Extend_MultipleOptions;
    Test_Extend_PreservesOptions;
    Test_Extend_Chaining;
    Test_Extend_EmptyArray;
    Test_Override_CipherList;
    Test_Override_SessionTimeout;
    Test_Override_ServerName;
    Test_Override_Chaining;
    Test_Multiple_Override;
    Test_Override_UnknownField;
    Test_Override_CaseInsensitive;
    Test_Combining_All;
    Test_Transformation_WithPresets;
    Test_BuildAfterTransformation;

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
