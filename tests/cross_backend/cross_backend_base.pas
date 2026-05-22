unit cross_backend_base;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes,
  fafafa.ssl.base;

type
  TBackendUnderTest = (butOpenSSL, butFreePascal);
  TBackendSet = set of TBackendUnderTest;

  TCrossBackendTestRunner = class
  private
    FPassed: Integer;
    FFailed: Integer;
    FSkipped: Integer;
    FCurrentBackend: string;
  public
    constructor Create;

    procedure BeginBackend(ABackend: TBackendUnderTest);
    procedure Check(const AName: string; ACondition: Boolean; const ADetails: string = '');
    procedure Skip(const AName: string; const AReason: string);
    procedure AssertEqual(const AName: string; const AExpected, AActual: string);
    procedure AssertEqualInt(const AName: string; AExpected, AActual: Integer);
    procedure AssertBothSucceed(const AName: string; AResultA, AResultB: Boolean;
      const ABackendA, ABackendB: string);

    procedure PrintSummary;
    function ExitCode: Integer;

    property Passed: Integer read FPassed;
    property Failed: Integer read FFailed;
    property Skipped: Integer read FSkipped;
  end;

function BackendName(ABackend: TBackendUnderTest): string;
function CreateLibraryForBackend(ABackend: TBackendUnderTest): ISSLLibrary;
function IsBackendAvailable(ABackend: TBackendUnderTest): Boolean;

implementation

uses
  fafafa.ssl.openssl.backed,
  fafafa.ssl.freepascal.lib;

function BackendName(ABackend: TBackendUnderTest): string;
begin
  case ABackend of
    butOpenSSL: Result := 'OpenSSL';
    butFreePascal: Result := 'FreePascal';
  end;
end;

function CreateLibraryForBackend(ABackend: TBackendUnderTest): ISSLLibrary;
begin
  case ABackend of
    butOpenSSL: Result := TOpenSSLLibrary.Create;
    butFreePascal: Result := CreateFreePascalSSLLibrary;
  end;
end;

function IsBackendAvailable(ABackend: TBackendUnderTest): Boolean;
var
  Lib: ISSLLibrary;
begin
  Result := False;
  try
    Lib := CreateLibraryForBackend(ABackend);
    if Lib <> nil then
      Result := Lib.Initialize;
    if Result and (Lib <> nil) then
      Lib.Finalize;
  except
    Result := False;
  end;
end;

{ TCrossBackendTestRunner }

constructor TCrossBackendTestRunner.Create;
begin
  inherited Create;
  FPassed := 0;
  FFailed := 0;
  FSkipped := 0;
  FCurrentBackend := '';
end;

procedure TCrossBackendTestRunner.BeginBackend(ABackend: TBackendUnderTest);
begin
  FCurrentBackend := BackendName(ABackend);
  WriteLn('');
  WriteLn('=== Backend: ', FCurrentBackend, ' ===');
end;

procedure TCrossBackendTestRunner.Check(const AName: string; ACondition: Boolean;
  const ADetails: string);
var
  Prefix: string;
begin
  if FCurrentBackend <> '' then
    Prefix := '[' + FCurrentBackend + '] '
  else
    Prefix := '';
  if ACondition then
  begin
    WriteLn('  [PASS] ', Prefix, AName);
    Inc(FPassed);
  end
  else
  begin
    WriteLn('  [FAIL] ', Prefix, AName);
    if ADetails <> '' then
      WriteLn('         ', ADetails);
    Inc(FFailed);
  end;
end;

procedure TCrossBackendTestRunner.Skip(const AName: string; const AReason: string);
begin
  WriteLn('  [SKIP] ', AName, ' -- ', AReason);
  Inc(FSkipped);
end;

procedure TCrossBackendTestRunner.AssertEqual(const AName: string;
  const AExpected, AActual: string);
begin
  Check(AName, AExpected = AActual,
    Format('expected "%s", got "%s"', [AExpected, AActual]));
end;

procedure TCrossBackendTestRunner.AssertEqualInt(const AName: string;
  AExpected, AActual: Integer);
begin
  Check(AName, AExpected = AActual,
    Format('expected %d, got %d', [AExpected, AActual]));
end;

procedure TCrossBackendTestRunner.AssertBothSucceed(const AName: string;
  AResultA, AResultB: Boolean; const ABackendA, ABackendB: string);
begin
  if AResultA = AResultB then
    Check(AName + ' (consistent)', True)
  else
    Check(AName + ' (inconsistent)',  False,
      Format('%s=%s, %s=%s', [ABackendA, BoolToStr(AResultA, True),
                              ABackendB, BoolToStr(AResultB, True)]));
end;

procedure TCrossBackendTestRunner.PrintSummary;
begin
  WriteLn('');
  WriteLn('==========================================');
  WriteLn('Cross-Backend Test Summary');
  WriteLn('==========================================');
  WriteLn(Format('  Passed:  %d', [FPassed]));
  WriteLn(Format('  Failed:  %d', [FFailed]));
  WriteLn(Format('  Skipped: %d', [FSkipped]));
  WriteLn(Format('  Total:   %d', [FPassed + FFailed + FSkipped]));
  WriteLn('==========================================');
  if FFailed = 0 then
    WriteLn('[PASS] All cross-backend checks passed')
  else
    WriteLn('[FAIL] ', FFailed, ' cross-backend check(s) failed');
end;

function TCrossBackendTestRunner.ExitCode: Integer;
begin
  Result := FFailed;
end;

end.
