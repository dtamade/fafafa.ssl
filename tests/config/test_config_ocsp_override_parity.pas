program test_config_ocsp_override_parity;

{$mode ObjFPC}{$H+}

uses
  SysUtils,
  Classes,
  fpjson,
  jsonparser,
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

procedure Test_Override_OCSPEnabled_ExportsToJSONAndINI;
var
  LBuilder: ISSLContextBuilder;
  LJSON, LINI: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 1: Override OCSP Enabled Exports To JSON And INI');

  LBuilder := TSSLContextBuilder.Create
    .Override('ocsp_stapling_enabled', 'true');

  LJSON := LBuilder.ExportToJSON;
  LINI := LBuilder.ExportToINI;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Booleans['ocsp_stapling_enabled'],
      'Override ocsp_stapling_enabled exports enabled=true to JSON');
    Assert(not LObj.Booleans['ocsp_stapling_required'],
      'Override ocsp_stapling_enabled alone keeps required=false in JSON');
  finally
    LRoot.Free;
  end;

  Assert(GetINIValue(LINI, 'ocsp_stapling_enabled') = 'true',
    'Override ocsp_stapling_enabled exports enabled=true to INI');
  Assert(GetINIValue(LINI, 'ocsp_stapling_required') = 'false',
    'Override ocsp_stapling_enabled alone keeps required=false in INI');
end;

procedure Test_Override_OCSPRequired_EnablesStapling;
var
  LBuilder: ISSLContextBuilder;
  LJSON, LINI: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 2: Override OCSP Required Enables Stapling');

  LBuilder := TSSLContextBuilder.Create
    .Override('ocsp_stapling_required', 'true');

  LJSON := LBuilder.ExportToJSON;
  LINI := LBuilder.ExportToINI;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Booleans['ocsp_stapling_enabled'],
      'Override ocsp_stapling_required implies enabled=true in JSON');
    Assert(LObj.Booleans['ocsp_stapling_required'],
      'Override ocsp_stapling_required exports required=true in JSON');
  finally
    LRoot.Free;
  end;

  Assert(GetINIValue(LINI, 'ocsp_stapling_enabled') = 'true',
    'Override ocsp_stapling_required implies enabled=true in INI');
  Assert(GetINIValue(LINI, 'ocsp_stapling_required') = 'true',
    'Override ocsp_stapling_required exports required=true in INI');
end;

procedure Test_Override_OCSPRequiredFalse_ClearsRequiredButKeepsEnabled;
var
  LBuilder: ISSLContextBuilder;
  LJSON, LINI: string;
  LRoot: TJSONData;
  LObj: TJSONObject;
begin
  TestHeader('Test 3: Override OCSP Required False Clears Required But Keeps Enabled');

  LBuilder := TSSLContextBuilder.Create
    .Override('ocsp_stapling_required', 'true')
    .Override('ocsp_stapling_required', 'false');

  LJSON := LBuilder.ExportToJSON;
  LINI := LBuilder.ExportToINI;
  LRoot := GetJSON(LJSON);
  try
    LObj := TJSONObject(LRoot);
    Assert(LObj.Booleans['ocsp_stapling_enabled'],
      'Clearing required keeps enabled=true in JSON, matching builder method semantics');
    Assert(not LObj.Booleans['ocsp_stapling_required'],
      'Clearing required resets required=false in JSON');
  finally
    LRoot.Free;
  end;

  Assert(GetINIValue(LINI, 'ocsp_stapling_enabled') = 'true',
    'Clearing required keeps enabled=true in INI');
  Assert(GetINIValue(LINI, 'ocsp_stapling_required') = 'false',
    'Clearing required resets required=false in INI');
end;

begin
  WriteLn;
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn('  OCSP Override Parity Test Suite');
  WriteLn('═══════════════════════════════════════════════════════════');
  WriteLn;

  try
    Test_Override_OCSPEnabled_ExportsToJSONAndINI;
    Test_Override_OCSPRequired_EnablesStapling;
    Test_Override_OCSPRequiredFalse_ClearsRequiredButKeepsEnabled;

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
