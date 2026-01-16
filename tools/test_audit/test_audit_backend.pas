{
  Test Audit Backend Consistency - 后端一致性审计器

  检查 OpenSSL 和 WinSSL 后端的测试一致性
}
unit test_audit_backend;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 后端一致性审计器 }
  TBackendAuditor = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestCases: TTestCaseInfoArray;
    
    function IsBackendSpecificFunction(const AFunc: TFunctionInfo): Boolean;
    function GetBackend(const AUnitName: string): string;
    function HasCounterpartTest(const AFuncName, ABackend: string): Boolean;
    function HasConsistencyTest(const AFuncName: string): Boolean;
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
    function AuditBackendConsistency: TBackendTestGapArray;
    function GetBackendScore: Integer;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TBackendAuditor.Create;
begin
  inherited Create;
  FVerbose := False;
end;

procedure TBackendAuditor.SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
begin
  FFunctions := AFunctions;
  FTestCases := ATestCases;
end;

function TBackendAuditor.IsBackendSpecificFunction(const AFunc: TFunctionInfo): Boolean;
var
  LowerUnit: string;
begin
  LowerUnit := LowerCase(AFunc.UnitName);
  Result := (Pos('openssl', LowerUnit) > 0) or
            (Pos('winssl', LowerUnit) > 0) or
            (Pos('schannel', LowerUnit) > 0) or
            (Pos('backend', LowerUnit) > 0);
end;

function TBackendAuditor.GetBackend(const AUnitName: string): string;
var
  LowerUnit: string;
begin
  LowerUnit := LowerCase(AUnitName);
  if (Pos('openssl', LowerUnit) > 0) then
    Result := 'OpenSSL'
  else if (Pos('winssl', LowerUnit) > 0) or (Pos('schannel', LowerUnit) > 0) then
    Result := 'WinSSL'
  else
    Result := 'Unknown';
end;

function TBackendAuditor.HasCounterpartTest(const AFuncName, ABackend: string): Boolean;
var
  I: Integer;
  LowerFunc, LowerTest, OtherBackend: string;
begin
  Result := False;
  LowerFunc := LowerCase(AFuncName);
  
  if ABackend = 'OpenSSL' then
    OtherBackend := 'winssl'
  else
    OtherBackend := 'openssl';
  
  for I := 0 to High(FTestCases) do
  begin
    LowerTest := LowerCase(FTestCases[I].TestFile);
    if (Pos(OtherBackend, LowerTest) > 0) and
       (Pos(LowerFunc, LowerCase(FTestCases[I].Name)) > 0) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TBackendAuditor.HasConsistencyTest(const AFuncName: string): Boolean;
var
  I: Integer;
  LowerFunc, LowerTest: string;
begin
  Result := False;
  LowerFunc := LowerCase(AFuncName);
  
  for I := 0 to High(FTestCases) do
  begin
    LowerTest := LowerCase(FTestCases[I].Name);
    if (Pos(LowerFunc, LowerTest) > 0) and
       ((Pos('consistency', LowerTest) > 0) or
        (Pos('compare', LowerTest) > 0) or
        (Pos('both', LowerTest) > 0) or
        (Pos('cross', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TBackendAuditor.AuditBackendConsistency: TBackendTestGapArray;
var
  I, GapCount: Integer;
  Backend: string;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    if IsBackendSpecificFunction(FFunctions[I]) then
    begin
      Backend := GetBackend(FFunctions[I].UnitName);
      
      // Check for counterpart test in other backend
      if not HasCounterpartTest(FFunctions[I].Name, Backend) then
      begin
        SetLength(Result, GapCount + 1);
        Result[GapCount].FunctionName := FFunctions[I].Name;
        Result[GapCount].UnitName := FFunctions[I].UnitName;
        Result[GapCount].Backend := Backend;
        Result[GapCount].GapType := bgtMissingCounterpart;
        Result[GapCount].Recommendation := Format('Add counterpart test for %s in other backend', [FFunctions[I].Name]);
        Inc(GapCount);
      end;
      
      // Check for consistency test
      if not HasConsistencyTest(FFunctions[I].Name) then
      begin
        SetLength(Result, GapCount + 1);
        Result[GapCount].FunctionName := FFunctions[I].Name;
        Result[GapCount].UnitName := FFunctions[I].UnitName;
        Result[GapCount].Backend := Backend;
        Result[GapCount].GapType := bgtMissingConsistencyTest;
        Result[GapCount].Recommendation := Format('Add cross-backend consistency test for %s', [FFunctions[I].Name]);
        Inc(GapCount);
      end;
    end;
  end;
  
  if FVerbose then
    WriteLn(Format('  Found %d backend consistency gaps', [GapCount]));
end;

function TBackendAuditor.GetBackendScore: Integer;
var
  BackendFuncs, GapsCount, I: Integer;
begin
  BackendFuncs := 0;
  for I := 0 to High(FFunctions) do
    if IsBackendSpecificFunction(FFunctions[I]) then
      Inc(BackendFuncs);
  
  GapsCount := Length(AuditBackendConsistency);
  
  if BackendFuncs = 0 then
    Result := 100
  else
    Result := Round(100 - (GapsCount / (BackendFuncs * 2) * 100));
  
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

end.
