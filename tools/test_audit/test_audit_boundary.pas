{
  Test Audit Boundary Checker - 边界条件检查器

  检查测试是否覆盖了各种边界条件
}
unit test_audit_boundary;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types;

type
  { 边界条件检查器 }
  TBoundaryChecker = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestCases: TTestCaseInfoArray;
    
    function GetRequiredBoundaries(ACategory: TParamCategory): TBoundaryTypeSet;
    function CheckTestForBoundary(const ATestName: string; ABoundary: TBoundaryType): Boolean;
    function FindTestsForFunction(const AFuncName: string): TTestCaseInfoArray;
    function GenerateBoundaryRecommendation(ABoundary: TBoundaryType; const AParamName, AParamType: string): string;
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
    function CheckFunction(const AFunc: TFunctionInfo): TBoundaryGapArray;
    function CheckAllFunctions: TBoundaryGapArray;
    function GetBoundaryScore: Integer;
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TBoundaryChecker.Create;
begin
  inherited Create;
  FVerbose := False;
end;

procedure TBoundaryChecker.SetData(const AFunctions: TFunctionInfoArray; const ATestCases: TTestCaseInfoArray);
begin
  FFunctions := AFunctions;
  FTestCases := ATestCases;
end;

function TBoundaryChecker.GetRequiredBoundaries(ACategory: TParamCategory): TBoundaryTypeSet;
begin
  Result := [];
  case ACategory of
    pcNumeric:
      Result := [btNumericZero, btNumericNegative, btNumericMax, btNumericMin];
    pcString:
      Result := [btStringEmpty, btStringNull, btStringLong];
    pcArray:
      Result := [btArrayEmpty, btArraySingle, btArrayBoundary];
    pcPointer:
      Result := [btPointerNil];
  end;
end;

function TBoundaryChecker.CheckTestForBoundary(const ATestName: string; ABoundary: TBoundaryType): Boolean;
var
  LowerName: string;
begin
  LowerName := LowerCase(ATestName);
  Result := False;
  
  case ABoundary of
    btNumericZero:
      Result := (Pos('zero', LowerName) > 0) or (Pos('_0', LowerName) > 0);
    btNumericNegative:
      Result := (Pos('negative', LowerName) > 0) or (Pos('neg_', LowerName) > 0);
    btNumericMax:
      Result := (Pos('max', LowerName) > 0) or (Pos('maximum', LowerName) > 0);
    btNumericMin:
      Result := (Pos('min', LowerName) > 0) or (Pos('minimum', LowerName) > 0);
    btNumericOverflow:
      Result := (Pos('overflow', LowerName) > 0);
    btStringEmpty:
      Result := (Pos('empty', LowerName) > 0) or (Pos('emptystr', LowerName) > 0);
    btStringNull:
      Result := (Pos('null', LowerName) > 0) or (Pos('nil', LowerName) > 0);
    btStringLong:
      Result := (Pos('long', LowerName) > 0) or (Pos('large', LowerName) > 0);
    btStringSpecialChars:
      Result := (Pos('special', LowerName) > 0) or (Pos('unicode', LowerName) > 0);
    btArrayEmpty:
      Result := (Pos('empty', LowerName) > 0) or (Pos('emptyarray', LowerName) > 0);
    btArraySingle:
      Result := (Pos('single', LowerName) > 0) or (Pos('one', LowerName) > 0);
    btArrayBoundary:
      Result := (Pos('boundary', LowerName) > 0) or (Pos('edge', LowerName) > 0);
    btArrayOversized:
      Result := (Pos('oversized', LowerName) > 0) or (Pos('large', LowerName) > 0);
    btPointerNil:
      Result := (Pos('nil', LowerName) > 0) or (Pos('null', LowerName) > 0);
  end;
end;

function TBoundaryChecker.FindTestsForFunction(const AFuncName: string): TTestCaseInfoArray;
var
  I, Count: Integer;
  LowerFunc: string;
begin
  SetLength(Result, 0);
  Count := 0;
  LowerFunc := LowerCase(AFuncName);
  
  for I := 0 to High(FTestCases) do
  begin
    if (Pos(LowerFunc, LowerCase(FTestCases[I].Name)) > 0) or
       (Pos(LowerFunc, LowerCase(FTestCases[I].TestedFunction)) > 0) then
    begin
      SetLength(Result, Count + 1);
      Result[Count] := FTestCases[I];
      Inc(Count);
    end;
  end;
end;

function TBoundaryChecker.GenerateBoundaryRecommendation(ABoundary: TBoundaryType; 
  const AParamName, AParamType: string): string;
begin
  case ABoundary of
    btNumericZero:
      Result := Format('Add test with %s = 0', [AParamName]);
    btNumericNegative:
      Result := Format('Add test with %s = -1 or negative value', [AParamName]);
    btNumericMax:
      Result := Format('Add test with %s = MaxInt or type maximum', [AParamName]);
    btNumericMin:
      Result := Format('Add test with %s = MinInt or type minimum', [AParamName]);
    btNumericOverflow:
      Result := Format('Add overflow test for %s', [AParamName]);
    btStringEmpty:
      Result := Format('Add test with %s = ''''', [AParamName]);
    btStringNull:
      Result := Format('Add test with %s = nil', [AParamName]);
    btStringLong:
      Result := Format('Add test with very long string for %s', [AParamName]);
    btStringSpecialChars:
      Result := Format('Add test with special/unicode chars for %s', [AParamName]);
    btArrayEmpty:
      Result := Format('Add test with empty array for %s', [AParamName]);
    btArraySingle:
      Result := Format('Add test with single element array for %s', [AParamName]);
    btArrayBoundary:
      Result := Format('Add boundary size test for %s', [AParamName]);
    btArrayOversized:
      Result := Format('Add oversized array test for %s', [AParamName]);
    btPointerNil:
      Result := Format('Add test with %s = nil', [AParamName]);
  else
    Result := Format('Add boundary test for %s', [AParamName]);
  end;
end;

function TBoundaryChecker.CheckFunction(const AFunc: TFunctionInfo): TBoundaryGapArray;
var
  Tests: TTestCaseInfoArray;
  RequiredBoundaries, MissingBoundaries: TBoundaryTypeSet;
  I, J, GapCount: Integer;
  BT: TBoundaryType;
  Found: Boolean;
begin
  SetLength(Result, 0);
  GapCount := 0;
  
  Tests := FindTestsForFunction(AFunc.Name);
  
  for I := 0 to High(AFunc.Parameters) do
  begin
    RequiredBoundaries := GetRequiredBoundaries(AFunc.Parameters[I].Category);
    MissingBoundaries := [];
    
    for BT := Low(TBoundaryType) to High(TBoundaryType) do
    begin
      if BT in RequiredBoundaries then
      begin
        Found := False;
        for J := 0 to High(Tests) do
        begin
          if CheckTestForBoundary(Tests[J].Name, BT) then
          begin
            Found := True;
            Break;
          end;
        end;
        if not Found then
          MissingBoundaries := MissingBoundaries + [BT];
      end;
    end;
    
    if MissingBoundaries <> [] then
    begin
      SetLength(Result, GapCount + 1);
      Result[GapCount].FunctionName := AFunc.Name;
      Result[GapCount].UnitName := AFunc.UnitName;
      Result[GapCount].ParameterName := AFunc.Parameters[I].Name;
      Result[GapCount].ParameterType := AFunc.Parameters[I].TypeName;
      Result[GapCount].MissingBoundaries := MissingBoundaries;
      
      // Generate recommendation for first missing boundary
      for BT := Low(TBoundaryType) to High(TBoundaryType) do
      begin
        if BT in MissingBoundaries then
        begin
          Result[GapCount].Recommendation := GenerateBoundaryRecommendation(
            BT, AFunc.Parameters[I].Name, AFunc.Parameters[I].TypeName);
          Break;
        end;
      end;
      
      Inc(GapCount);
    end;
  end;
end;

function TBoundaryChecker.CheckAllFunctions: TBoundaryGapArray;
var
  I, J, TotalCount: Integer;
  FuncGaps: TBoundaryGapArray;
begin
  SetLength(Result, 0);
  TotalCount := 0;
  
  for I := 0 to High(FFunctions) do
  begin
    if Length(FFunctions[I].Parameters) > 0 then
    begin
      FuncGaps := CheckFunction(FFunctions[I]);
      for J := 0 to High(FuncGaps) do
      begin
        SetLength(Result, TotalCount + 1);
        Result[TotalCount] := FuncGaps[J];
        Inc(TotalCount);
      end;
    end;
  end;
  
  if FVerbose then
    WriteLn(Format('  Found %d boundary gaps', [TotalCount]));
end;

function TBoundaryChecker.GetBoundaryScore: Integer;
var
  TotalParams, GapsCount: Integer;
  I: Integer;
begin
  TotalParams := 0;
  for I := 0 to High(FFunctions) do
    TotalParams := TotalParams + Length(FFunctions[I].Parameters);
  
  GapsCount := Length(CheckAllFunctions);
  
  if TotalParams = 0 then
    Result := 100
  else
    Result := Round(100 - (GapsCount / TotalParams * 50));
  
  if Result < 0 then Result := 0;
  if Result > 100 then Result := 100;
end;

end.
