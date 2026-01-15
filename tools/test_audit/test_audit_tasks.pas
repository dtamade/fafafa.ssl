{
  Test Audit Task Generator - 改进任务生成器

  根据审计结果生成具体的改进任务
}
unit test_audit_tasks;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 任务生成器 }
  TTaskGenerator = class
  private
    FVerbose: Boolean;
    FTaskCounter: Integer;
    
    function GenerateTaskID(ACategory: TTaskCategory): string;
    function EstimateEffort(ACategory: TTaskCategory; AGapCount: Integer): string;
    function DeterminePriority(ACategory: TTaskCategory; const AUnitName: string): TTaskPriority;
    function GenerateCodeTemplate(ACategory: TTaskCategory; const AFuncName, AUnitName: string): string;
  public
    constructor Create;
    
    function GenerateCoverageTasks(const ACoverageResults: TCoverageResultArray): TImprovementTaskArray;
    function GenerateBoundaryTasks(const ABoundaryGaps: TBoundaryGapArray): TImprovementTaskArray;
    function GenerateErrorTasks(const AErrorGaps: TErrorTestGapArray): TImprovementTaskArray;
    function GenerateCryptoTasks(const ACryptoGaps: TCryptoTestGapArray): TImprovementTaskArray;
    function GenerateThreadTasks(const AThreadGaps: TThreadTestGapArray): TImprovementTaskArray;
    function GenerateResourceTasks(const AResourceGaps: TResourceTestGapArray): TImprovementTaskArray;
    function GenerateBackendTasks(const ABackendGaps: TBackendTestGapArray): TImprovementTaskArray;
    
    function GenerateAllTasks(const AResults: TAuditResults): TImprovementTaskArray;
    function SortByPriority(const ATasks: TImprovementTaskArray): TImprovementTaskArray;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TTaskGenerator.Create;
begin
  inherited Create;
  FVerbose := False;
  FTaskCounter := 0;
end;

function TTaskGenerator.GenerateTaskID(ACategory: TTaskCategory): string;
begin
  Inc(FTaskCounter);
  case ACategory of
    tcCoverage: Result := Format('COV-%03d', [FTaskCounter]);
    tcBoundary: Result := Format('BND-%03d', [FTaskCounter]);
    tcError: Result := Format('ERR-%03d', [FTaskCounter]);
    tcCrypto: Result := Format('CRY-%03d', [FTaskCounter]);
    tcThread: Result := Format('THR-%03d', [FTaskCounter]);
    tcResource: Result := Format('RES-%03d', [FTaskCounter]);
    tcBackend: Result := Format('BCK-%03d', [FTaskCounter]);
  else
    Result := Format('TSK-%03d', [FTaskCounter]);
  end;
end;

function TTaskGenerator.EstimateEffort(ACategory: TTaskCategory; AGapCount: Integer): string;
begin
  case ACategory of
    tcCoverage:
      if AGapCount > 10 then Result := 'large'
      else if AGapCount > 3 then Result := 'medium'
      else Result := 'small';
    tcCrypto, tcThread:
      Result := 'large';
    tcBoundary, tcError:
      if AGapCount > 5 then Result := 'medium'
      else Result := 'small';
  else
    Result := 'medium';
  end;
end;

function TTaskGenerator.DeterminePriority(ACategory: TTaskCategory; const AUnitName: string): TTaskPriority;
var
  LowerUnit: string;
begin
  LowerUnit := LowerCase(AUnitName);
  
  // Security-critical modules get higher priority
  if (Pos('crypto', LowerUnit) > 0) or (Pos('cipher', LowerUnit) > 0) or
     (Pos('key', LowerUnit) > 0) or (Pos('auth', LowerUnit) > 0) or
     (Pos('sign', LowerUnit) > 0) or (Pos('hash', LowerUnit) > 0) then
  begin
    case ACategory of
      tcCrypto: Result := tpCritical;
      tcError, tcBoundary: Result := tpHigh;
    else
      Result := tpMedium;
    end;
    Exit;
  end;
  
  // Thread safety is always important
  if ACategory = tcThread then
  begin
    Result := tpHigh;
    Exit;
  end;
  
  // Default priorities by category
  case ACategory of
    tcCrypto: Result := tpCritical;
    tcError: Result := tpHigh;
    tcBoundary: Result := tpMedium;
    tcResource: Result := tpMedium;
    tcBackend: Result := tpMedium;
    tcCoverage: Result := tpLow;
  else
    Result := tpLow;
  end;
end;

function TTaskGenerator.GenerateCodeTemplate(ACategory: TTaskCategory; const AFuncName, AUnitName: string): string;
begin
  case ACategory of
    tcCoverage:
      Result := Format(
        'procedure Test_%s;'#13#10 +
        'begin'#13#10 +
        '  // TODO: Add test for %s'#13#10 +
        '  // AssertTrue(...):'#13#10 +
        'end;', [AFuncName, AFuncName]);
    tcBoundary:
      Result := Format(
        'procedure Test_%s_Boundary;'#13#10 +
        'begin'#13#10 +
        '  // Test with zero/empty/nil values'#13#10 +
        '  // Test with max/min values'#13#10 +
        '  // Test with boundary conditions'#13#10 +
        'end;', [AFuncName]);
    tcError:
      Result := Format(
        'procedure Test_%s_ErrorHandling;'#13#10 +
        'begin'#13#10 +
        '  // Test error return codes'#13#10 +
        '  // Test exception handling'#13#10 +
        '  // Test recovery paths'#13#10 +
        'end;', [AFuncName]);
    tcCrypto:
      Result := Format(
        'procedure Test_%s_KAT;'#13#10 +
        'begin'#13#10 +
        '  // Known Answer Test with NIST/RFC vectors'#13#10 +
        '  // Verify against standard test vectors'#13#10 +
        'end;', [AFuncName]);
    tcThread:
      Result := Format(
        'procedure Test_%s_ThreadSafety;'#13#10 +
        'begin'#13#10 +
        '  // Test concurrent access from multiple threads'#13#10 +
        '  // Test for race conditions'#13#10 +
        '  // Use at least 4 threads'#13#10 +
        'end;', [AFuncName]);
    tcResource:
      Result := Format(
        'procedure Test_%s_ResourceManagement;'#13#10 +
        'begin'#13#10 +
        '  // Test for memory leaks'#13#10 +
        '  // Test proper cleanup'#13#10 +
        '  // Test double-free protection'#13#10 +
        'end;', [AFuncName]);
    tcBackend:
      Result := Format(
        'procedure Test_%s_BackendConsistency;'#13#10 +
        'begin'#13#10 +
        '  // Test same behavior on OpenSSL and WinSSL'#13#10 +
        '  // Compare outputs for consistency'#13#10 +
        'end;', [AFuncName]);
  else
    Result := '';
  end;
end;

function TTaskGenerator.GenerateCoverageTasks(const ACoverageResults: TCoverageResultArray): TImprovementTaskArray;
var
  I, J, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(ACoverageResults) do
  begin
    if ACoverageResults[I].CoveragePercent < 80 then
    begin
      for J := 0 to High(ACoverageResults[I].UntestedFunctions) do
      begin
        SetLength(Result, TaskCount + 1);
        Result[TaskCount].ID := GenerateTaskID(tcCoverage);
        Result[TaskCount].Title := Format('Add test for %s', [ACoverageResults[I].UntestedFunctions[J]]);
        Result[TaskCount].Description := Format('Function %s in %s has no test coverage.',
          [ACoverageResults[I].UntestedFunctions[J], ACoverageResults[I].UnitName]);
        Result[TaskCount].Category := tcCoverage;
        Result[TaskCount].Priority := DeterminePriority(tcCoverage, ACoverageResults[I].UnitName);
        Result[TaskCount].AffectedUnit := ACoverageResults[I].UnitName;
        Result[TaskCount].AffectedFunction := ACoverageResults[I].UntestedFunctions[J];
        Result[TaskCount].EstimatedEffort := 'small';
        Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcCoverage,
          ACoverageResults[I].UntestedFunctions[J], ACoverageResults[I].UnitName);
        Inc(TaskCount);
        
        // Limit tasks per module
        if J >= 4 then Break;
      end;
    end;
  end;
end;

function TTaskGenerator.GenerateBoundaryTasks(const ABoundaryGaps: TBoundaryGapArray): TImprovementTaskArray;
var
  I, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(ABoundaryGaps) do
  begin
    SetLength(Result, TaskCount + 1);
    Result[TaskCount].ID := GenerateTaskID(tcBoundary);
    Result[TaskCount].Title := Format('Add boundary test for %s.%s',
      [ABoundaryGaps[I].FunctionName, ABoundaryGaps[I].ParameterName]);
    Result[TaskCount].Description := Format('%s - %s',
      [ABoundaryGaps[I].Recommendation, ABoundaryGaps[I].ParameterType]);
    Result[TaskCount].Category := tcBoundary;
    Result[TaskCount].Priority := DeterminePriority(tcBoundary, ABoundaryGaps[I].UnitName);
    Result[TaskCount].AffectedUnit := ABoundaryGaps[I].UnitName;
    Result[TaskCount].AffectedFunction := ABoundaryGaps[I].FunctionName;
    Result[TaskCount].EstimatedEffort := 'small';
    Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcBoundary,
      ABoundaryGaps[I].FunctionName, ABoundaryGaps[I].UnitName);
    Inc(TaskCount);
    
    if TaskCount >= 20 then Break;
  end;
end;

function TTaskGenerator.GenerateErrorTasks(const AErrorGaps: TErrorTestGapArray): TImprovementTaskArray;
var
  I, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(AErrorGaps) do
  begin
    SetLength(Result, TaskCount + 1);
    Result[TaskCount].ID := GenerateTaskID(tcError);
    Result[TaskCount].Title := Format('Add error handling test for %s', [AErrorGaps[I].FunctionName]);
    Result[TaskCount].Description := AErrorGaps[I].Recommendation;
    Result[TaskCount].Category := tcError;
    Result[TaskCount].Priority := DeterminePriority(tcError, AErrorGaps[I].UnitName);
    Result[TaskCount].AffectedUnit := AErrorGaps[I].UnitName;
    Result[TaskCount].AffectedFunction := AErrorGaps[I].FunctionName;
    Result[TaskCount].EstimatedEffort := 'small';
    Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcError,
      AErrorGaps[I].FunctionName, AErrorGaps[I].UnitName);
    Inc(TaskCount);
    
    if TaskCount >= 20 then Break;
  end;
end;

function TTaskGenerator.GenerateCryptoTasks(const ACryptoGaps: TCryptoTestGapArray): TImprovementTaskArray;
var
  I, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(ACryptoGaps) do
  begin
    SetLength(Result, TaskCount + 1);
    Result[TaskCount].ID := GenerateTaskID(tcCrypto);
    Result[TaskCount].Title := Format('Add crypto test for %s', [ACryptoGaps[I].FunctionName]);
    Result[TaskCount].Description := ACryptoGaps[I].Recommendation;
    Result[TaskCount].Category := tcCrypto;
    Result[TaskCount].Priority := tpCritical;
    Result[TaskCount].AffectedUnit := ACryptoGaps[I].UnitName;
    Result[TaskCount].AffectedFunction := ACryptoGaps[I].FunctionName;
    Result[TaskCount].EstimatedEffort := 'medium';
    Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcCrypto,
      ACryptoGaps[I].FunctionName, ACryptoGaps[I].UnitName);
    Inc(TaskCount);
    
    if TaskCount >= 20 then Break;
  end;
end;

function TTaskGenerator.GenerateThreadTasks(const AThreadGaps: TThreadTestGapArray): TImprovementTaskArray;
var
  I, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(AThreadGaps) do
  begin
    SetLength(Result, TaskCount + 1);
    Result[TaskCount].ID := GenerateTaskID(tcThread);
    Result[TaskCount].Title := Format('Add thread safety test for %s', [AThreadGaps[I].FunctionName]);
    Result[TaskCount].Description := AThreadGaps[I].Recommendation;
    Result[TaskCount].Category := tcThread;
    Result[TaskCount].Priority := tpHigh;
    Result[TaskCount].AffectedUnit := AThreadGaps[I].UnitName;
    Result[TaskCount].AffectedFunction := AThreadGaps[I].FunctionName;
    Result[TaskCount].EstimatedEffort := 'large';
    Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcThread,
      AThreadGaps[I].FunctionName, AThreadGaps[I].UnitName);
    Inc(TaskCount);
    
    if TaskCount >= 10 then Break;
  end;
end;

function TTaskGenerator.GenerateResourceTasks(const AResourceGaps: TResourceTestGapArray): TImprovementTaskArray;
var
  I, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(AResourceGaps) do
  begin
    SetLength(Result, TaskCount + 1);
    Result[TaskCount].ID := GenerateTaskID(tcResource);
    Result[TaskCount].Title := Format('Add resource test for %s', [AResourceGaps[I].FunctionName]);
    Result[TaskCount].Description := AResourceGaps[I].Recommendation;
    Result[TaskCount].Category := tcResource;
    Result[TaskCount].Priority := DeterminePriority(tcResource, AResourceGaps[I].UnitName);
    Result[TaskCount].AffectedUnit := AResourceGaps[I].UnitName;
    Result[TaskCount].AffectedFunction := AResourceGaps[I].FunctionName;
    Result[TaskCount].EstimatedEffort := 'medium';
    Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcResource,
      AResourceGaps[I].FunctionName, AResourceGaps[I].UnitName);
    Inc(TaskCount);
    
    if TaskCount >= 15 then Break;
  end;
end;

function TTaskGenerator.GenerateBackendTasks(const ABackendGaps: TBackendTestGapArray): TImprovementTaskArray;
var
  I, TaskCount: Integer;
begin
  SetLength(Result, 0);
  TaskCount := 0;
  
  for I := 0 to High(ABackendGaps) do
  begin
    SetLength(Result, TaskCount + 1);
    Result[TaskCount].ID := GenerateTaskID(tcBackend);
    Result[TaskCount].Title := Format('Add backend test for %s (%s)', [ABackendGaps[I].FunctionName, ABackendGaps[I].Backend]);
    Result[TaskCount].Description := ABackendGaps[I].Recommendation;
    Result[TaskCount].Category := tcBackend;
    Result[TaskCount].Priority := DeterminePriority(tcBackend, ABackendGaps[I].UnitName);
    Result[TaskCount].AffectedUnit := ABackendGaps[I].UnitName;
    Result[TaskCount].AffectedFunction := ABackendGaps[I].FunctionName;
    Result[TaskCount].EstimatedEffort := 'medium';
    Result[TaskCount].CodeTemplate := GenerateCodeTemplate(tcBackend,
      ABackendGaps[I].FunctionName, ABackendGaps[I].UnitName);
    Inc(TaskCount);
    
    if TaskCount >= 15 then Break;
  end;
end;

function TTaskGenerator.GenerateAllTasks(const AResults: TAuditResults): TImprovementTaskArray;
var
  CovTasks, BndTasks, ErrTasks, CryTasks, ThrTasks, ResTasks, BckTasks: TImprovementTaskArray;
  I, TotalCount: Integer;
begin
  FTaskCounter := 0;
  SetLength(Result, 0);
  TotalCount := 0;
  
  CovTasks := GenerateCoverageTasks(AResults.CoverageResults);
  BndTasks := GenerateBoundaryTasks(AResults.BoundaryGaps);
  ErrTasks := GenerateErrorTasks(AResults.ErrorGaps);
  CryTasks := GenerateCryptoTasks(AResults.CryptoGaps);
  ThrTasks := GenerateThreadTasks(AResults.ThreadGaps);
  ResTasks := GenerateResourceTasks(AResults.ResourceGaps);
  BckTasks := GenerateBackendTasks(AResults.BackendGaps);
  
  // Combine all tasks
  for I := 0 to High(CryTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := CryTasks[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(ErrTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := ErrTasks[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(ThrTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := ThrTasks[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(BndTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := BndTasks[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(ResTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := ResTasks[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(BckTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := BckTasks[I];
    Inc(TotalCount);
  end;
  
  for I := 0 to High(CovTasks) do
  begin
    SetLength(Result, TotalCount + 1);
    Result[TotalCount] := CovTasks[I];
    Inc(TotalCount);
  end;
  
  Result := SortByPriority(Result);
  
  if FVerbose then
    WriteLn(Format('  Generated %d improvement tasks', [TotalCount]));
end;

function TTaskGenerator.SortByPriority(const ATasks: TImprovementTaskArray): TImprovementTaskArray;
var
  I, J: Integer;
  Temp: TImprovementTask;
begin
  Result := ATasks;
  
  // Simple bubble sort by priority
  for I := 0 to High(Result) - 1 do
  begin
    for J := I + 1 to High(Result) do
    begin
      if Ord(Result[J].Priority) < Ord(Result[I].Priority) then
      begin
        Temp := Result[I];
        Result[I] := Result[J];
        Result[J] := Temp;
      end;
    end;
  end;
end;

end.
