program test_config_private_key_password_snapshot_semantics;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fpjson,
  jsonparser,
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

function GetJSONStringOrDefault(AObj: TJSONObject; const AName, ADefault: string): string;
begin
  if AObj.IndexOfName(AName) >= 0 then
    Result := AObj.Strings[AName]
  else
    Result := ADefault;
end;

procedure Test_JSONRoundTrip_PreservesPrivateKeyPassword;
var
  LBuilder: ISSLContextBuilder;
  LJSON1, LJSON2: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 1: JSON Round Trip Preserves Private Key Password');

  LBuilder := TSSLContextBuilder.Create
    .WithPrivateKey('/path/to/key.pem', 'password123');

  LJSON1 := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON1);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.IndexOfName('private_key_password') >= 0,
      'JSON exports private_key_password field');
    AssertEqualStr('password123', GetJSONStringOrDefault(LObj, 'private_key_password', ''),
      'JSON exports private_key_password value');
  finally
    LRoot.Free;
  end;

  LJSON2 := TSSLContextBuilder.Create
    .ImportFromJSON(LJSON1)
    .ExportToJSON;

  LRoot := GetJSON(LJSON2);
  try
    LObj := TJSONObject(LRoot);
    AssertEqualStr('password123', GetJSONStringOrDefault(LObj, 'private_key_password', ''),
      'JSON import restores private_key_password');
  finally
    LRoot.Free;
  end;
end;

procedure Test_INIRoundTrip_PreservesPrivateKeyPassword;
var
  LBuilder: ISSLContextBuilder;
  LINI1, LINI2: string;
begin
  TestHeader('Test 2: INI Round Trip Preserves Private Key Password');

  LBuilder := TSSLContextBuilder.Create
    .WithPrivateKeyPEM('KEY-PEM', 'pem-secret');

  LINI1 := LBuilder.ExportToINI;
  Assert(Pos('private_key_password=pem-secret', LINI1) > 0,
    'INI exports private_key_password line');

  LINI2 := TSSLContextBuilder.Create
    .ImportFromINI(LINI1)
    .ExportToINI;

  Assert(Pos('private_key_password=pem-secret', LINI2) > 0,
    'INI import restores private_key_password line');
end;

procedure Test_Merge_SourcePassword_OverridesTarget;
var
  LBase, LSource: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 3: Merge Source Password Overrides Target');

  LBase := TSSLContextBuilder.Create
    .WithPrivateKey('/path/to/key.pem', 'old-secret');
  LSource := TSSLContextBuilder.Create
    .WithPrivateKey('/path/to/key.pem', 'new-secret');

  LBase.Merge(LSource);
  LJSON := LBase.ExportToJSON;

  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    AssertEqualStr('new-secret', GetJSONStringOrDefault(LObj, 'private_key_password', ''),
      'Merge carries source private_key_password');
  finally
    LRoot.Free;
  end;
end;

procedure Test_Merge_EmptyPassword_ClearsTarget;
var
  LBase, LSource: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 4: Merge Empty Password Clears Target');

  LBase := TSSLContextBuilder.Create
    .WithPrivateKey('/path/to/key.pem', 'old-secret');
  LSource := TSSLContextBuilder.Create
    .WithPrivateKey('/path/to/key.pem', '');

  LBase.Merge(LSource);
  LJSON := LBase.ExportToJSON;

  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.IndexOfName('private_key_password') >= 0,
      'Merge export keeps private_key_password field even when empty');
    AssertEqualStr('', GetJSONStringOrDefault(LObj, 'private_key_password', '__missing__'),
      'Merge can clear private_key_password to empty');
  finally
    LRoot.Free;
  end;
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  Private Key Password Snapshot Semantics Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_JSONRoundTrip_PreservesPrivateKeyPassword;
    Test_INIRoundTrip_PreservesPrivateKeyPassword;
    Test_Merge_SourcePassword_OverridesTarget;
    Test_Merge_EmptyPassword_ClearsTarget;

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
