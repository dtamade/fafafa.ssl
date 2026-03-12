program test_config_pkcs11_json_parity;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  fpjson,
  jsonparser,
  fafafa.ssl.context.builder,
  fafafa.ssl.pkcs11.types;

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

procedure Test_Override_PKCS11PIN_ExportsValueMethod;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
  LURI: string;
begin
  TestHeader('Test 1: Override PKCS11 PIN Exports Value Method');

  LURI := 'pkcs11:token=test;object=server-key;type=private';
  LBuilder := TSSLContextBuilder.Create
    .Override('pkcs11_uri', LURI)
    .Override('pkcs11_pin', '2468');

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.IndexOfName('pkcs11_uri') >= 0, 'JSON exports pkcs11_uri field');
    Assert(LObj.IndexOfName('pkcs11_pin') >= 0, 'JSON exports pkcs11_pin field');
    Assert(LObj.IndexOfName('pkcs11_pin_method') >= 0, 'JSON exports pkcs11_pin_method field');
    AssertEqualStr(LURI, GetJSONStringOrDefault(LObj, 'pkcs11_uri', ''), 'Override pkcs11_uri is exported');
    AssertEqualStr('2468', GetJSONStringOrDefault(LObj, 'pkcs11_pin', ''), 'Override pkcs11_pin is exported');
    AssertEqualStr('value', GetJSONStringOrDefault(LObj, 'pkcs11_pin_method', ''),
      'Override pkcs11_pin implies value pin method');
  finally
    LRoot.Free;
  end;
end;

procedure Test_Override_PKCS11PINMethod_StringParse;
var
  LBuilder: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 2: Override PKCS11 PIN Method String Parse');

  LBuilder := TSSLContextBuilder.Create
    .Override('pkcs11_pin_method', 'Interactive');

  LJSON := LBuilder.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    AssertEqualStr('interactive', GetJSONStringOrDefault(LObj, 'pkcs11_pin_method', ''),
      'Override pkcs11_pin_method parses case-insensitive string value');
  finally
    LRoot.Free;
  end;
end;

procedure Test_JSONRoundTrip_PKCS11Fields;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LJSON1, LJSON2: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 3: JSON Round Trip Preserves PKCS11 Fields');

  LBuilder1 := TSSLContextBuilder.Create
    .UsePKCS11('pkcs11:token=test;object=server-key;type=private')
    .WithPKCS11PIN('1357')
    .WithPKCS11PINMethod(pmInteractive);

  LJSON1 := LBuilder1.ExportToJSON;
  LBuilder2 := TSSLContextBuilder.Create.ImportFromJSON(LJSON1);
  LJSON2 := LBuilder2.ExportToJSON;

  LRoot := GetJSON(LJSON2);
  try
    LObj := TJSONObject(LRoot);
    AssertEqualStr('pkcs11:token=test;object=server-key;type=private',
      GetJSONStringOrDefault(LObj, 'pkcs11_uri', ''), 'JSON import restores pkcs11_uri');
    AssertEqualStr('1357', GetJSONStringOrDefault(LObj, 'pkcs11_pin', ''),
      'JSON import restores pkcs11_pin');
    AssertEqualStr('interactive', GetJSONStringOrDefault(LObj, 'pkcs11_pin_method', ''),
      'JSON import restores pkcs11_pin_method');
  finally
    LRoot.Free;
  end;

  Assert(LJSON1 = LJSON2, 'JSON round trip preserves PKCS11 fields exactly');
end;

procedure Test_Merge_Preserves_PKCS11Fields;
var
  LBase, LSource: ISSLContextBuilder;
  LJSON: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 4: Merge Preserves PKCS11 Fields');

  LBase := TSSLContextBuilder.Create
    .WithSessionTimeout(1200);
  LSource := TSSLContextBuilder.Create
    .UsePKCS11('pkcs11:token=test;object=merge-key;type=private')
    .WithPKCS11PIN('9999')
    .WithPKCS11PINMethod(pmInteractive);

  LBase.Merge(LSource);
  LJSON := LBase.ExportToJSON;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    AssertEqualStr('pkcs11:token=test;object=merge-key;type=private',
      GetJSONStringOrDefault(LObj, 'pkcs11_uri', ''), 'Merge keeps pkcs11_uri');
    AssertEqualStr('9999', GetJSONStringOrDefault(LObj, 'pkcs11_pin', ''), 'Merge keeps pkcs11_pin');
    AssertEqualStr('interactive', GetJSONStringOrDefault(LObj, 'pkcs11_pin_method', ''),
      'Merge keeps pkcs11_pin_method');
  finally
    LRoot.Free;
  end;
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  PKCS11 Override JSON Parity Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Override_PKCS11PIN_ExportsValueMethod;
    Test_Override_PKCS11PINMethod_StringParse;
    Test_JSONRoundTrip_PKCS11Fields;
    Test_Merge_Preserves_PKCS11Fields;

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
