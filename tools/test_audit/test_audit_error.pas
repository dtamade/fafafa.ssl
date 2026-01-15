{
  Test Audit Error Handler - 错误处理审计器

  检查错误处理路径的测试覆盖
}
unit test_audit_error;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 错误处理审计器 }
  TErrorAuditor = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestCases: TTestCaseInfoArray;
    
    function IsErrorReturningFunction(const AFunc: TFunctionInfo): Boolean;
    function HasExceptionHandling(const AFunc: TFunctionInfo): Boolean;
    function FindErrorTests(const AFuncName: string): Integer;
    function GenerateErrorRecommendation(AGapType: TErrorGapType; const AFuncName: string): string;
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
    function IdentifyErrorPaths(const AFunc: TFunctionInfo): TStringArray;
    function CheckErrorTests(const AFunc: TFunctionInfo): TErrorTestGapArray;
    function AuditAllFunctions: TErrorTestGapArray;
    function GetErrorScore: Integer;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TErrorAuditor.Create;
begin
  inherited Create;
  FVerbose := False;
end;

procedure TErrorAuditor.SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
begin
  FFunctions := AFunctions;
  FTestCases := ATestCases;
end;

function TErrorAuditor.IsErrorReturningFunction(const AFunc: TFunctionInfo): Boolean;
var
  LowerReturn, LowerName: string;
begin
  LowerReturn := LowerCase(AFunc.ReturnType);
  LowerName := LowerCase(AFunc.Name);
  
  Result := (Pos('boolean', LowerReturn) > 0) or
            (Pos('integer', LowerReturn) > 0) or
            (Pos('result', LowerReturn) > 0) or
            (Pos('error', LowerName) > 0) or
            (Pos('try', LowerName) > 0) or
            (Pos('validate', LowerName) > 0) or
            (Pos('check', LowerName) > 0) or
            (Pos('verify', LowerName) > 0) or
            AFunc.HasErrorReturn;
end;

function TErrorAuditor.HasExceptionHandling(const AFunc: TFunctionInfo): Boolean;
var
  LowerName: string;
begin
  LowerName := LowerCase(AFunc.Name);
  Result := (Pos('raise', LowerName) > 0) or
            (Pos('exception', LowerName) > 0) or
            (Pos('throw', LowerName) > 0);
end;

function TErrorAuditor.FindErrorTests(const AFuncName: string): Integer;
var
  I: Integer;
  LowerFunc, LowerTest: string;
begin
  Result := 0;
  LowerFunc := LowerCase(AFuncName);
  
  for I := 0 to High(FTestCases) do
  begin
    LowerTest := LowerCase(FTestCases[I].Name);
    if (Pos(LowerFunc, LowerTest) > 0) and
       ((Pos('error', LowerTest) > 0) or
        (Pos('fail', LowerTest) > 0) or
        (Pos('invalid', LowerTest) > 0) or
        (Pos('exception', LowerTest) > 0) or
        (Pos('negative', LowerTest) > 0)) then
      Inc(Result);
  end;
end;

function TErrorAuditor.GenerateErrorRecommendation(AGapType: TErrorGapType; const AFuncName: string): string;
begin
  case AGapType of
    egtMissingErrorCodeTest:
      Result := Format('Add test for error return codes in %s', [AFuncName]);
    egtMissingExceptionTest:
      Result := Format('Add exception handling test for %s', [AFuncName]);
    egtMissingPreconditionTest:
      Result := Format('Add precondition validation test for %s', [AFuncName]);
    egtMissingRecoveryTest:
      Result := Format('Add error recovery test for %s', [AFuncName]);
  else
    Result := Format('Add error handling test for %s', [AFuncName]);
  end;
end;

function TErrorAuditor.IdentifyErrorPaths(const AFunc: TFunctionInfo): TStringArray;
var
  Count: Integer;
begin
  SetLength(Result, 0);
  Count := 0;
  
  if IsErrorReturningFunction(AFunc) then
  begin
    SetLength(Result, Count + 1);
    Result[Count] := 'Error return code path';
    Inc(Count);
  end;
  
  if HasExceptionHandling(AFunc) then
  begin
    SetLength(Result, Count + 1);
    Result[Count] := 'Exception handling path';
    Inc(Count);
  end;
  
  // Check for validation functions
  if (Pos('validate', LowerCase(AFunc.Name)) > 0) or
     (Pos('check', LowerCase(AFunc.Name)) > 0) then
  begin
    SetLength(Result, Count + 1);
    Result[Count] := 'Validation failure path';
    Inc(Count);
  end;
end;

function TErrorAuditor.CheckErrorTests(const AFunc: TFunctionInfo): TErrorTestGapArray;
var
  ErrorTestCount: Integer;
  GapCount: Integer;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  if not IsErrorReturningFunction(AFunc) and not HasExceptionHandling(AFunc) then
    Exit;
  
  ErrorTestCount := FindErrorTests(AFunc.Name);
  
  if IsErrorReturningFunction(AFunc) and (ErrorTestCount = 0) then
  begin
    SetLength(Result, GapCount + 1);
    Result[GapCount].FunctionName := AFunc.Name;
    Result[GapCount].UnitName := AFunc.UnitName;
    Result[GapCount].GapType := egtMissingErrorCodeTest;
    Result[GapCount].ErrorCondition := 'Error return code';
    Result[GapCount].HasTest := False;
    Result[GapCount].Recommendation := GenerateErrorRecommendation(egtMissingErrorCodeTest, AFunc.Name);
    Inc(GapCount);
  end;
  
  if HasExceptionHandling(AFunc) and (ErrorTestCount = 0) then
  begin
    SetLength(Result, GapCount + 1);
    Result[GapCount].FunctionName := AFunc.Name;
    Result[GapCount].UnitName := AFunc.UnitName;
    Result[GapCount].GapType := egtMissingExceptionTest;
    Result[GapCount].ErrorCondition := 'Exception handling';
    Result[GapCount].HasTest := False;
    Result[GapCount].Recommendation := GenerateErrorRecommendation(egtMissingExceptionTest, AFunc.Name);
    Inc(GapCount);
  end;
end;

function TErrorAuditor.AuditAllFunctions: TErrorTestGapArray;
var
  I, J, TotalCount: Integer;
  FuncGaps: TErrorTestGapArray;
begin
  SetLength(Result, 0);
  TotalCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    FuncGaps := CheckErrorTests(FFunctions[I]);
    for J := 0 to High(FuncGaps) do
    begin
      SetLength(Result, TotalCount + 1);
      Result[TotalCount] := FuncGaps[J];
      Inc(TotalCount);
    end;
  end;
  
  if FVerbose then
    WriteLn(Format('  Found %d error handling gaps', [TotalCount]));
end;

function TErrorAuditor.GetErrorScore: Integer;
var
  ErrorFuncs, GapsCount, I: Integer;
begin
  ErrorFuncs := 0;
  for I := 0 to High(FFunctions) do
  begin
    if IsErrorReturningFunction(FFunctions[I]) or HasExceptionHandling(FFunctions[I]) then
      Inc(ErrorFuncs);
  end;
  
  GapsCount := Length(AuditAllFunctions);
  
  if ErrorFuncs = 0 then
    Result := 100
  else
    Result := Round(100 - (GapsCount / ErrorFuncs * 100));
  
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

end.
