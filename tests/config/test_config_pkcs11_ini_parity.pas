program test_config_pkcs11_ini_parity;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
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

function GetINIValue(const AINI, AKey: string): string;
var
  LLines: TStringList;
  I, LPos: Integer;
  LLine: string;
begin
  Result := '';
  LLines := TStringList.Create;
  try
    LLines.Text := AINI;
    for I := 0 to LLines.Count - 1 do
    begin
      LLine := Trim(LLines[I]);
      if Pos(AKey + '=', LLine) = 1 then
      begin
        LPos := Pos('=', LLine);
        Exit(Copy(LLine, LPos + 1, MaxInt));
      end;
    end;
  finally
    LLines.Free;
  end;
end;

procedure Test_ExportToINI_PKCS11Fields;
var
  LBuilder: ISSLContextBuilder;
  LINI: string;
  LURI: string;
begin
  TestHeader('Test 1: ExportToINI PKCS11 Fields');

  LURI := 'pkcs11:token=test;object=server-key;type=private';
  LBuilder := TSSLContextBuilder.Create
    .Override('pkcs11_uri', LURI)
    .Override('pkcs11_pin', '2468')
    .Override('pkcs11_pin_method', 'Interactive');

  LINI := LBuilder.ExportToINI;

  Assert(Pos('pkcs11_uri=', LINI) > 0, 'INI exports pkcs11_uri field');
  Assert(Pos('pkcs11_pin=', LINI) > 0, 'INI exports pkcs11_pin field');
  Assert(Pos('pkcs11_pin_method=', LINI) > 0, 'INI exports pkcs11_pin_method field');
  AssertEqualStr(LURI, GetINIValue(LINI, 'pkcs11_uri'), 'Exported INI keeps pkcs11_uri');
  AssertEqualStr('2468', GetINIValue(LINI, 'pkcs11_pin'), 'Exported INI keeps pkcs11_pin');
  AssertEqualStr('interactive', GetINIValue(LINI, 'pkcs11_pin_method'),
    'Exported INI keeps pkcs11_pin_method');
end;

procedure Test_ImportFromINI_PKCS11Fields;
var
  LBuilder: ISSLContextBuilder;
  LINI, LOut: string;
begin
  TestHeader('Test 2: ImportFromINI PKCS11 Fields');

  LINI :=
    '[PKCS11]' + LineEnding +
    'pkcs11_uri=pkcs11:token=test;object=import-key;type=private' + LineEnding +
    'pkcs11_pin=1357' + LineEnding +
    'pkcs11_pin_method=Callback' + LineEnding;

  LBuilder := TSSLContextBuilder.Create.ImportFromINI(LINI);
  LOut := LBuilder.ExportToINI;

  AssertEqualStr('pkcs11:token=test;object=import-key;type=private',
    GetINIValue(LOut, 'pkcs11_uri'), 'ImportFromINI restores pkcs11_uri');
  AssertEqualStr('1357', GetINIValue(LOut, 'pkcs11_pin'), 'ImportFromINI restores pkcs11_pin');
  AssertEqualStr('callback', GetINIValue(LOut, 'pkcs11_pin_method'),
    'ImportFromINI parses pkcs11_pin_method case-insensitively');
end;

procedure Test_INIRoundTrip_PKCS11Fields;
var
  LBuilder1, LBuilder2: ISSLContextBuilder;
  LINI1, LINI2: string;
begin
  TestHeader('Test 3: INI Round Trip Preserves PKCS11 Fields');

  LBuilder1 := TSSLContextBuilder.Create
    .UsePKCS11('pkcs11:token=test;object=roundtrip-key;type=private')
    .WithPKCS11PIN('9999')
    .WithPKCS11PINMethod(pmEnvironment);

  LINI1 := LBuilder1.ExportToINI;
  LBuilder2 := TSSLContextBuilder.Create.ImportFromINI(LINI1);
  LINI2 := LBuilder2.ExportToINI;

  AssertEqualStr('pkcs11:token=test;object=roundtrip-key;type=private',
    GetINIValue(LINI2, 'pkcs11_uri'), 'INI round trip preserves pkcs11_uri');
  AssertEqualStr('9999', GetINIValue(LINI2, 'pkcs11_pin'), 'INI round trip preserves pkcs11_pin');
  AssertEqualStr('environment', GetINIValue(LINI2, 'pkcs11_pin_method'),
    'INI round trip preserves pkcs11_pin_method');
  Assert(LINI1 = LINI2, 'INI round trip preserves PKCS11 fields exactly');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  PKCS11 INI Parity Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_ExportToINI_PKCS11Fields;
    Test_ImportFromINI_PKCS11Fields;
    Test_INIRoundTrip_PKCS11Fields;

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
