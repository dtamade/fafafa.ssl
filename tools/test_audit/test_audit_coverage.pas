{
  Test Audit Coverage Analyzer - 覆盖率分析器

  分析测试覆盖率，识别未测试的函数和模块
}
unit test_audit_coverage;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, test_audit_types, test_audit_test_scanner;

type
  { 覆盖率分析器 }
  TCoverageAnalyzer = class
  private
    FVerbose: Boolean;
    FFunctions: TFunctionInfoArray;
    FTestMap: TTestFunctionMapArray;
    FCoverageResults: TCoverageResultArray;
    FOverallCoverage: Double;
    FCoverageThreshold: Double;
    
    procedure CalculateUnitCoverage(const AUnitName: string; out AResult: TCoverageResult);
  public
    constructor Create;
    
    procedure SetData(const AFunctions: TFunctionInfoArray;
      const ATestMap: TTestFunctionMapArray);
    
    function AnalyzeUnit(const AUnitName: string): TCoverageResult;
    function AnalyzeProject: TCoverageResultArray;
    function GetOverallCoverage: Double;
    function GetHighPriorityGaps: TStringArray;
    function GetUntestedFunctions: TFunctionInfoArray;
    
    property Verbose: Boolean read FVerbose write FVerbose;
    property CoverageThreshold: Double read FCoverageThreshold write FCoverageThreshold;
    property CoverageResults: TCoverageResultArray read FCoverageResults;
    property OverallCoverage: Double read FOverallCoverage;
  end;

implementation

constructor TCoverageAnalyzer.Create;
begin
  inherited Create;
  FVerbose := False;
  FCoverageThreshold := 80.0;
  FOverallCoverage := 0.0;
  SetLength(FFunctions, 0);
  SetLength(FTestMap, 0);
  SetLength(FCoverageResults, 0);
end;

procedure TCoverageAnalyzer.SetData(const AFunctions: TFunctionInfoArray;
  const ATestMap: TTestFunctionMapArray);
begin
  FFunctions := AFunctions;
  FTestMap := ATestMap;
end;

procedure TCoverageAnalyzer.CalculateUnitCoverage(const AUnitName: string;
  out AResult: TCoverageResult);
var
  I: Integer;
  TotalFuncs, TestedFuncs: Integer;
  UntestedList, PartialList: TStringList;
begin
  AResult.UnitName := AUnitName;
  AResult.FileName := '';
  TotalFuncs := 0;
  TestedFuncs := 0;
  
  UntestedList := TStringList.Create;
  PartialList := TStringList.Create;
  try
    // 遍历所有函数
    for I := 0 to High(FFunctions) do
    begin
      if FFunctions[I].UnitName = AUnitName then
      begin
        Inc(TotalFuncs);
        AResult.FileName := FFunctions[I].FileName;
        
        // 检查是否有测试
        if (I <= High(FTestMap)) and (FTestMap[I].TestCount > 0) then
        begin
          Inc(TestedFuncs);
          // 检查是否只有部分测试（少于 2 个测试用例）
          if FTestMap[I].TestCount < 2 then
            PartialList.Add(FFunctions[I].Name);
        end
        else
          UntestedList.Add(FFunctions[I].Name);
      end;
    end;
    
    AResult.TotalFunctions := TotalFuncs;
    AResult.TestedFunctions := TestedFuncs;
    
    if TotalFuncs > 0 then
      AResult.CoveragePercent := (TestedFuncs / TotalFuncs) * 100.0
    else
      AResult.CoveragePercent := 100.0;
      
    // 转换为数组
    SetLength(AResult.UntestedFunctions, UntestedList.Count);
    for I := 0 to UntestedList.Count - 1 do
      AResult.UntestedFunctions[I] := UntestedList[I];
      
    SetLength(AResult.PartiallyTestedFunctions, PartialList.Count);
    for I := 0 to PartialList.Count - 1 do
      AResult.PartiallyTestedFunctions[I] := PartialList[I];
  finally
    UntestedList.Free;
    PartialList.Free;
  end;
end;

function TCoverageAnalyzer.AnalyzeUnit(const AUnitName: string): TCoverageResult;
begin
  CalculateUnitCoverage(AUnitName, Result);
end;

function TCoverageAnalyzer.AnalyzeProject: TCoverageResultArray;
var
  UnitNames: TStringList;
  I: Integer;
  UnitResult: TCoverageResult;
  TotalFuncs, TotalTested: Integer;
begin
  SetLength(Result, 0);
  TotalFuncs := 0;
  TotalTested := 0;
  
  // 收集所有单元名称
  UnitNames := TStringList.Create;
  UnitNames.Sorted := True;
  UnitNames.Duplicates := dupIgnore;
  try
    for I := 0 to High(FFunctions) do
      UnitNames.Add(FFunctions[I].UnitName);
      
    // 分析每个单元
    SetLength(Result, UnitNames.Count);
    for I := 0 to UnitNames.Count - 1 do
    begin
      CalculateUnitCoverage(UnitNames[I], UnitResult);
      Result[I] := UnitResult;
      
      TotalFuncs := TotalFuncs + UnitResult.TotalFunctions;
      TotalTested := TotalTested + UnitResult.TestedFunctions;
      
      if FVerbose then
        WriteLn(Format('  %s: %.1f%% (%d/%d)',
          [UnitResult.UnitName, UnitResult.CoveragePercent,
           UnitResult.TestedFunctions, UnitResult.TotalFunctions]));
    end;
    
    // 计算总体覆盖率
    if TotalFuncs > 0 then
      FOverallCoverage := (TotalTested / TotalFuncs) * 100.0
    else
      FOverallCoverage := 100.0;
      
    FCoverageResults := Result;
  finally
    UnitNames.Free;
  end;
end;

function TCoverageAnalyzer.GetOverallCoverage: Double;
begin
  Result := FOverallCoverage;
end;

function TCoverageAnalyzer.GetHighPriorityGaps: TStringArray;
var
  I: Integer;
  GapList: TStringList;
begin
  SetLength(Result, 0);
  GapList := TStringList.Create;
  try
    for I := 0 to High(FCoverageResults) do
    begin
      if FCoverageResults[I].CoveragePercent < FCoverageThreshold then
        GapList.Add(Format('%s (%.1f%%)',
          [FCoverageResults[I].UnitName, FCoverageResults[I].CoveragePercent]));
    end;
    
    // 按覆盖率排序（最低的在前）
    GapList.Sort;
    
    SetLength(Result, GapList.Count);
    for I := 0 to GapList.Count - 1 do
      Result[I] := GapList[I];
  finally
    GapList.Free;
  end;
end;

function TCoverageAnalyzer.GetUntestedFunctions: TFunctionInfoArray;
var
  I: Integer;
begin
  SetLength(Result, 0);
  
  for I := 0 to High(FFunctions) do
  begin
    // 检查是否有测试
    if (I > High(FTestMap)) or (FTestMap[I].TestCount = 0) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := FFunctions[I];
    end;
  end;
end;

end.
