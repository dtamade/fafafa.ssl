{
  Test Audit Thread Safety - 线程安全审计器

  检查线程安全相关的测试覆盖
}
unit test_audit_thread;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 线程安全审计器 }
  TThreadAuditor = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestCases: TTestCaseInfoArray;
    
    function IsThreadSafeFunction(const AFunc: TFunctionInfo): Boolean;
    function HasConcurrentTest(const AFuncName: string): Boolean;
    function HasRaceTest(const AFuncName: string): Boolean;
    function HasMultiThreadTest(const AFuncName: string): Boolean;
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
    function AuditAllFunctions: TThreadTestGapArray;
    function GetThreadScore: Integer;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TThreadAuditor.Create;
begin
  inherited Create;
  FVerbose := False;
end;

procedure TThreadAuditor.SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
begin
  FFunctions := AFunctions;
  FTestCases := ATestCases;
end;

function TThreadAuditor.IsThreadSafeFunction(const AFunc: TFunctionInfo): Boolean;
var
  LowerName, LowerUnit: string;
begin
  LowerName := LowerCase(AFunc.Name);
  LowerUnit := LowerCase(AFunc.UnitName);
  
  Result := (Pos('thread', LowerName) > 0) or
            (Pos('lock', LowerName) > 0) or
            (Pos('mutex', LowerName) > 0) or
            (Pos('critical', LowerName) > 0) or
            (Pos('atomic', LowerName) > 0) or
            (Pos('sync', LowerName) > 0) or
            (Pos('concurrent', LowerName) > 0) or
            (Pos('parallel', LowerName) > 0) or
            (Pos('thread', LowerUnit) > 0) or
            (Pos('sync', LowerUnit) > 0);
end;

function TThreadAuditor.HasConcurrentTest(const AFuncName: string): Boolean;
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
       ((Pos('concurrent', LowerTest) > 0) or
        (Pos('parallel', LowerTest) > 0) or
        (Pos('multithread', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TThreadAuditor.HasRaceTest(const AFuncName: string): Boolean;
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
       ((Pos('race', LowerTest) > 0) or
        (Pos('deadlock', LowerTest) > 0) or
        (Pos('contention', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TThreadAuditor.HasMultiThreadTest(const AFuncName: string): Boolean;
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
       ((Pos('thread', LowerTest) > 0) or
        (Pos('stress', LowerTest) > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TThreadAuditor.AuditAllFunctions: TThreadTestGapArray;
var
  I, GapCount: Integer;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    if IsThreadSafeFunction(FFunctions[I]) then
    begin
      if not HasConcurrentTest(FFunctions[I].Name) then
      begin
        SetLength(Result, GapCount + 1);
        Result[GapCount].FunctionName := FFunctions[I].Name;
        Result[GapCount].UnitName := FFunctions[I].UnitName;
        Result[GapCount].GapType := tgtMissingConcurrentTest;
        Result[GapCount].Recommendation := Format('Add concurrent access test for %s', [FFunctions[I].Name]);
        Inc(GapCount);
      end;
      
      if not HasRaceTest(FFunctions[I].Name) then
      begin
        SetLength(Result, GapCount + 1);
        Result[GapCount].FunctionName := FFunctions[I].Name;
        Result[GapCount].UnitName := FFunctions[I].UnitName;
        Result[GapCount].GapType := tgtMissingRaceTest;
        Result[GapCount].Recommendation := Format('Add race condition test for %s', [FFunctions[I].Name]);
        Inc(GapCount);
      end;
    end;
  end;
  
  if FVerbose then
    WriteLn(Format('  Found %d thread safety gaps', [GapCount]));
end;

function TThreadAuditor.GetThreadScore: Integer;
var
  ThreadFuncs, GapsCount, I: Integer;
begin
  ThreadFuncs := 0;
  for I := 0 to High(FFunctions) do
    if IsThreadSafeFunction(FFunctions[I]) then
      Inc(ThreadFuncs);
  
  GapsCount := Length(AuditAllFunctions);
  
  if ThreadFuncs = 0 then
    Result := 100
  else
    Result := Round(100 - (GapsCount / (ThreadFuncs * 2) * 100));
  
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

end.
