{
  Test Audit Reporter - 质量报告生成器

  生成 Markdown 和 JSON 格式的审计报告
}
unit test_audit_reporter;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes, Math, StrUtils, fpjson, test_audit_types;

type
  { 质量报告生成器 }
  TQualityReporter = class
  private
    FVerbose: Boolean;
    
    function CalculateCoverageScore(ACoverage: Double): Integer;
    function CalculateBoundaryScore(AGapCount, ATotalParams: Integer): Integer;
    function CalculateErrorScore(AGapCount, ATotalFuncs: Integer): Integer;
    function PriorityToString(APriority: TTaskPriority): string;
    function CategoryToString(ACategory: TTaskCategory): string;
  public
    constructor Create;
    
    function GenerateMarkdownReport(const AResults: TAuditResults): string;
    function GenerateJSONReport(const AResults: TAuditResults): string;
    function GenerateTaskList(const AResults: TAuditResults): string;
    function CalculateScore(const AResults: TAuditResults): TQualityScore;
    
    procedure SaveMarkdownReport(const AResults: TAuditResults; const AFileName: string);
    procedure SaveJSONReport(const AResults: TAuditResults; const AFileName: string);
    
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

implementation

constructor TQualityReporter.Create;
begin
  inherited Create;
  FVerbose := False;
end;

function TQualityReporter.CalculateCoverageScore(ACoverage: Double): Integer;
begin
  Result := Round(ACoverage);
  if Result > 100 then Result := 100;
  if Result < 0 then Result := 0;
end;

function TQualityReporter.CalculateBoundaryScore(AGapCount, ATotalParams: Integer): Integer;
begin
  if ATotalParams = 0 then
    Result := 100
  else
    Result := Round(100 - (AGapCount / ATotalParams * 100));
  if Result < 0 then Result := 0;
end;

function TQualityReporter.CalculateErrorScore(AGapCount, ATotalFuncs: Integer): Integer;
begin
  if ATotalFuncs = 0 then
    Result := 100
  else
    Result := Round(100 - (AGapCount / ATotalFuncs * 100));
  if Result < 0 then Result := 0;
end;

function TQualityReporter.PriorityToString(APriority: TTaskPriority): string;
begin
  case APriority of
    tpCritical: Result := 'Critical';
    tpHigh: Result := 'High';
    tpMedium: Result := 'Medium';
    tpLow: Result := 'Low';
  else
    Result := 'Unknown';
  end;
end;

function TQualityReporter.CategoryToString(ACategory: TTaskCategory): string;
begin
  case ACategory of
    tcCoverage: Result := 'Coverage';
    tcBoundary: Result := 'Boundary';
    tcError: Result := 'Error Handling';
    tcCrypto: Result := 'Cryptography';
    tcThread: Result := 'Thread Safety';
    tcResource: Result := 'Resource Management';
    tcBackend: Result := 'Backend Consistency';
  else
    Result := 'Unknown';
  end;
end;

function TQualityReporter.CalculateScore(const AResults: TAuditResults): TQualityScore;
begin
  Result.Coverage := CalculateCoverageScore(AResults.OverallCoverage);
  Result.BoundaryTesting := AResults.BoundaryScore;
  Result.ErrorHandling := AResults.ErrorScore;
  Result.CryptoTesting := AResults.CryptoScore;
  Result.ThreadSafety := AResults.ThreadScore;
  Result.ResourceManagement := AResults.ResourceScore;
  Result.BackendConsistency := AResults.BackendScore;
  
  // 计算总体评分（加权平均）
  Result.Overall := Round(
    Result.Coverage * 0.25 +
    Result.BoundaryTesting * 0.15 +
    Result.ErrorHandling * 0.15 +
    Result.CryptoTesting * 0.15 +
    Result.ThreadSafety * 0.10 +
    Result.ResourceManagement * 0.10 +
    Result.BackendConsistency * 0.10
  );
end;

function TQualityReporter.GenerateMarkdownReport(const AResults: TAuditResults): string;
var
  SL: TStringList;
  I, J: Integer;
  Score: TQualityScore;
begin
  Score := CalculateScore(AResults);
  
  SL := TStringList.Create;
  try
    // 标题
    SL.Add('# Test Quality Audit Report');
    SL.Add('');
    SL.Add(Format('**Generated:** %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', AResults.Timestamp)]));
    SL.Add(Format('**Project:** %s', [AResults.ProjectPath]));
    SL.Add('');
    
    // 总体评分
    SL.Add('## Overall Health Score');
    SL.Add('');
    SL.Add(Format('**Score: %d/100**', [Score.Overall]));
    SL.Add('');
    SL.Add('| Category | Score |');
    SL.Add('|----------|-------|');
    SL.Add(Format('| Coverage | %d%% |', [Score.Coverage]));
    SL.Add(Format('| Boundary Testing | %d%% |', [Score.BoundaryTesting]));
    SL.Add(Format('| Error Handling | %d%% |', [Score.ErrorHandling]));
    SL.Add(Format('| Crypto Testing | %d%% |', [Score.CryptoTesting]));
    SL.Add(Format('| Thread Safety | %d%% |', [Score.ThreadSafety]));
    SL.Add(Format('| Resource Management | %d%% |', [Score.ResourceManagement]));
    SL.Add(Format('| Backend Consistency | %d%% |', [Score.BackendConsistency]));
    SL.Add('');
    
    // 统计摘要
    SL.Add('## Summary Statistics');
    SL.Add('');
    SL.Add(Format('- **Source Files:** %d', [AResults.TotalSourceFiles]));
    SL.Add(Format('- **Test Files:** %d', [AResults.TotalTestFiles]));
    SL.Add(Format('- **Total Functions:** %d', [AResults.TotalFunctions]));
    SL.Add(Format('- **Total Test Cases:** %d', [AResults.TotalTestCases]));
    SL.Add(Format('- **Overall Coverage:** %.1f%%', [AResults.OverallCoverage]));
    SL.Add('');
    
    // 覆盖率详情
    SL.Add('## Coverage by Module');
    SL.Add('');
    SL.Add('| Module | Coverage | Tested | Total | Status |');
    SL.Add('|--------|----------|--------|-------|--------|');
    for I := 0 to High(AResults.CoverageResults) do
    begin
      SL.Add(Format('| %s | %.1f%% | %d | %d | %s |',
        [AResults.CoverageResults[I].UnitName,
         AResults.CoverageResults[I].CoveragePercent,
         AResults.CoverageResults[I].TestedFunctions,
         AResults.CoverageResults[I].TotalFunctions,
         IfThen(AResults.CoverageResults[I].CoveragePercent >= 80, '✅', '⚠️')]));
    end;
    SL.Add('');
    
    // 未测试函数
    SL.Add('## Untested Functions');
    SL.Add('');
    for I := 0 to High(AResults.CoverageResults) do
    begin
      if Length(AResults.CoverageResults[I].UntestedFunctions) > 0 then
      begin
        SL.Add(Format('### %s', [AResults.CoverageResults[I].UnitName]));
        SL.Add('');
        for J := 0 to High(AResults.CoverageResults[I].UntestedFunctions) do
          SL.Add(Format('- `%s`', [AResults.CoverageResults[I].UntestedFunctions[J]]));
        SL.Add('');
      end;
    end;
    
    // 边界测试缺口
    if Length(AResults.BoundaryGaps) > 0 then
    begin
      SL.Add('## Boundary Test Gaps');
      SL.Add('');
      SL.Add('| Function | Parameter | Missing Tests | Recommendation |');
      SL.Add('|----------|-----------|---------------|----------------|');
      for I := 0 to Min(High(AResults.BoundaryGaps), 19) do
      begin
        SL.Add(Format('| %s | %s | %s | %s |',
          [AResults.BoundaryGaps[I].FunctionName,
           AResults.BoundaryGaps[I].ParameterName,
           AResults.BoundaryGaps[I].ParameterType,
           AResults.BoundaryGaps[I].Recommendation]));
      end;
      if Length(AResults.BoundaryGaps) > 20 then
        SL.Add(Format('| ... | ... | ... | (%d more) |', [Length(AResults.BoundaryGaps) - 20]));
      SL.Add('');
    end;
    
    // 改进任务
    if Length(AResults.ImprovementTasks) > 0 then
    begin
      SL.Add('## Improvement Tasks');
      SL.Add('');
      for I := 0 to High(AResults.ImprovementTasks) do
      begin
        SL.Add(Format('### %s: %s',
          [AResults.ImprovementTasks[I].ID, AResults.ImprovementTasks[I].Title]));
        SL.Add('');
        SL.Add(Format('- **Priority:** %s', [PriorityToString(AResults.ImprovementTasks[I].Priority)]));
        SL.Add(Format('- **Category:** %s', [CategoryToString(AResults.ImprovementTasks[I].Category)]));
        SL.Add(Format('- **Effort:** %s', [AResults.ImprovementTasks[I].EstimatedEffort]));
        SL.Add(Format('- **Unit:** %s', [AResults.ImprovementTasks[I].AffectedUnit]));
        SL.Add('');
        SL.Add(AResults.ImprovementTasks[I].Description);
        SL.Add('');
        if AResults.ImprovementTasks[I].CodeTemplate <> '' then
        begin
          SL.Add('```pascal');
          SL.Add(AResults.ImprovementTasks[I].CodeTemplate);
          SL.Add('```');
          SL.Add('');
        end;
      end;
    end;
    
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;


function TQualityReporter.GenerateJSONReport(const AResults: TAuditResults): string;
var
  RootObj, ScoreObj, StatsObj, CovObj, TaskObj: TJSONObject;
  CovArr, GapsArr, TasksArr, UntestedArr: TJSONArray;
  I, J: Integer;
  Score: TQualityScore;
begin
  Score := CalculateScore(AResults);
  
  RootObj := TJSONObject.Create;
  try
    // 元数据
    RootObj.Add('timestamp', FormatDateTime('yyyy-mm-dd"T"hh:nn:ss', AResults.Timestamp));
    RootObj.Add('projectPath', AResults.ProjectPath);
    
    // 评分
    ScoreObj := TJSONObject.Create;
    ScoreObj.Add('overall', Score.Overall);
    ScoreObj.Add('coverage', Score.Coverage);
    ScoreObj.Add('boundaryTesting', Score.BoundaryTesting);
    ScoreObj.Add('errorHandling', Score.ErrorHandling);
    ScoreObj.Add('cryptoTesting', Score.CryptoTesting);
    ScoreObj.Add('threadSafety', Score.ThreadSafety);
    ScoreObj.Add('resourceManagement', Score.ResourceManagement);
    ScoreObj.Add('backendConsistency', Score.BackendConsistency);
    RootObj.Add('scores', ScoreObj);
    
    // 统计
    StatsObj := TJSONObject.Create;
    StatsObj.Add('totalSourceFiles', AResults.TotalSourceFiles);
    StatsObj.Add('totalTestFiles', AResults.TotalTestFiles);
    StatsObj.Add('totalFunctions', AResults.TotalFunctions);
    StatsObj.Add('totalTestCases', AResults.TotalTestCases);
    StatsObj.Add('overallCoverage', AResults.OverallCoverage);
    RootObj.Add('statistics', StatsObj);
    
    // 覆盖率结果
    CovArr := TJSONArray.Create;
    for I := 0 to High(AResults.CoverageResults) do
    begin
      CovObj := TJSONObject.Create;
      CovObj.Add('unitName', AResults.CoverageResults[I].UnitName);
      CovObj.Add('fileName', AResults.CoverageResults[I].FileName);
      CovObj.Add('totalFunctions', AResults.CoverageResults[I].TotalFunctions);
      CovObj.Add('testedFunctions', AResults.CoverageResults[I].TestedFunctions);
      CovObj.Add('coveragePercent', AResults.CoverageResults[I].CoveragePercent);
      
      UntestedArr := TJSONArray.Create;
      for J := 0 to High(AResults.CoverageResults[I].UntestedFunctions) do
        UntestedArr.Add(AResults.CoverageResults[I].UntestedFunctions[J]);
      CovObj.Add('untestedFunctions', UntestedArr);
      
      CovArr.Add(CovObj);
    end;
    RootObj.Add('coverageResults', CovArr);
    
    // 边界测试缺口
    GapsArr := TJSONArray.Create;
    for I := 0 to High(AResults.BoundaryGaps) do
    begin
      CovObj := TJSONObject.Create;
      CovObj.Add('functionName', AResults.BoundaryGaps[I].FunctionName);
      CovObj.Add('unitName', AResults.BoundaryGaps[I].UnitName);
      CovObj.Add('parameterName', AResults.BoundaryGaps[I].ParameterName);
      CovObj.Add('parameterType', AResults.BoundaryGaps[I].ParameterType);
      CovObj.Add('recommendation', AResults.BoundaryGaps[I].Recommendation);
      GapsArr.Add(CovObj);
    end;
    RootObj.Add('boundaryGaps', GapsArr);
    
    // 改进任务
    TasksArr := TJSONArray.Create;
    for I := 0 to High(AResults.ImprovementTasks) do
    begin
      TaskObj := TJSONObject.Create;
      TaskObj.Add('id', AResults.ImprovementTasks[I].ID);
      TaskObj.Add('title', AResults.ImprovementTasks[I].Title);
      TaskObj.Add('description', AResults.ImprovementTasks[I].Description);
      TaskObj.Add('category', CategoryToString(AResults.ImprovementTasks[I].Category));
      TaskObj.Add('priority', PriorityToString(AResults.ImprovementTasks[I].Priority));
      TaskObj.Add('affectedUnit', AResults.ImprovementTasks[I].AffectedUnit);
      TaskObj.Add('affectedFunction', AResults.ImprovementTasks[I].AffectedFunction);
      TaskObj.Add('estimatedEffort', AResults.ImprovementTasks[I].EstimatedEffort);
      TaskObj.Add('codeTemplate', AResults.ImprovementTasks[I].CodeTemplate);
      TasksArr.Add(TaskObj);
    end;
    RootObj.Add('improvementTasks', TasksArr);
    
    Result := RootObj.FormatJSON;
  finally
    RootObj.Free;
  end;
end;

function TQualityReporter.GenerateTaskList(const AResults: TAuditResults): string;
var
  SL: TStringList;
  I: Integer;
begin
  SL := TStringList.Create;
  try
    SL.Add('# Test Improvement Tasks');
    SL.Add('');
    SL.Add(Format('Generated: %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', AResults.Timestamp)]));
    SL.Add('');
    
    // 按优先级分组
    SL.Add('## Critical Priority');
    SL.Add('');
    for I := 0 to High(AResults.ImprovementTasks) do
      if AResults.ImprovementTasks[I].Priority = tpCritical then
        SL.Add(Format('- [ ] %s: %s (%s)',
          [AResults.ImprovementTasks[I].ID,
           AResults.ImprovementTasks[I].Title,
           AResults.ImprovementTasks[I].EstimatedEffort]));
    SL.Add('');
    
    SL.Add('## High Priority');
    SL.Add('');
    for I := 0 to High(AResults.ImprovementTasks) do
      if AResults.ImprovementTasks[I].Priority = tpHigh then
        SL.Add(Format('- [ ] %s: %s (%s)',
          [AResults.ImprovementTasks[I].ID,
           AResults.ImprovementTasks[I].Title,
           AResults.ImprovementTasks[I].EstimatedEffort]));
    SL.Add('');
    
    SL.Add('## Medium Priority');
    SL.Add('');
    for I := 0 to High(AResults.ImprovementTasks) do
      if AResults.ImprovementTasks[I].Priority = tpMedium then
        SL.Add(Format('- [ ] %s: %s (%s)',
          [AResults.ImprovementTasks[I].ID,
           AResults.ImprovementTasks[I].Title,
           AResults.ImprovementTasks[I].EstimatedEffort]));
    SL.Add('');
    
    SL.Add('## Low Priority');
    SL.Add('');
    for I := 0 to High(AResults.ImprovementTasks) do
      if AResults.ImprovementTasks[I].Priority = tpLow then
        SL.Add(Format('- [ ] %s: %s (%s)',
          [AResults.ImprovementTasks[I].ID,
           AResults.ImprovementTasks[I].Title,
           AResults.ImprovementTasks[I].EstimatedEffort]));
    
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TQualityReporter.SaveMarkdownReport(const AResults: TAuditResults;
  const AFileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := GenerateMarkdownReport(AResults);
    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

procedure TQualityReporter.SaveJSONReport(const AResults: TAuditResults;
  const AFileName: string);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := GenerateJSONReport(AResults);
    SL.SaveToFile(AFileName);
  finally
    SL.Free;
  end;
end;

end.
