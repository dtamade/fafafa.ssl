{
  Test Audit Main - 测试审计系统主程序

  用于分析 fafafa.ssl 项目的测试质量
  
  用法:
    test_audit [options]
    
  选项:
    -c, --config <file>   指定配置文件 (默认: audit_config.json)
    -s, --source <dir>    源码目录 (默认: src)
    -t, --tests <dir>     测试目录 (默认: tests)
    -o, --output <dir>    输出目录 (默认: reports/audit)
    -v, --verbose         详细输出
    -i, --incremental     增量分析模式
    -b, --baseline <file> 基线文件
    -h, --help            显示帮助
}
program test_audit_main;

{$mode objfpc}{$H+}{$J-}

uses
  SysUtils, Classes,
  test_audit_types,
  test_audit_config,
  test_audit_source_scanner,
  test_audit_test_scanner,
  test_audit_coverage,
  test_audit_boundary,
  test_audit_error,
  test_audit_crypto,
  test_audit_thread,
  test_audit_resource,
  test_audit_backend,
  test_audit_tasks,
  test_audit_reporter;

const
  VERSION = '1.1.0';

var
  GConfig: TAuditConfig;
  GExitCode: Integer;

procedure PrintUsage;
begin
  WriteLn('Test Audit Tool v', VERSION);
  WriteLn('Analyze test quality for fafafa.ssl project');
  WriteLn;
  WriteLn('Usage: test_audit [options]');
  WriteLn;
  WriteLn('Options:');
  WriteLn('  -c, --config <file>   Config file (default: audit_config.json)');
  WriteLn('  -s, --source <dir>    Source directory (default: src)');
  WriteLn('  -t, --tests <dir>     Test directory (default: tests)');
  WriteLn('  -o, --output <dir>    Output directory (default: reports/audit)');
  WriteLn('  -v, --verbose         Verbose output');
  WriteLn('  -i, --incremental     Incremental analysis mode');
  WriteLn('  -b, --baseline <file> Baseline file');
  WriteLn('  -h, --help            Show help');
end;

procedure ParseCommandLine;
var
  I: Integer;
  Param, Value: string;
begin
  I := 1;
  while I <= ParamCount do
  begin
    Param := ParamStr(I);
    
    if (Param = '-h') or (Param = '--help') then
    begin
      PrintUsage;
      Halt(0);
    end
    else if (Param = '-v') or (Param = '--verbose') then
      GConfig.Verbose := True
    else if (Param = '-i') or (Param = '--incremental') then
      GConfig.IncrementalMode := True
    else if (Param = '-c') or (Param = '--config') then
    begin
      Inc(I);
      if I <= ParamCount then
      begin
        Value := ParamStr(I);
        if FileExists(Value) then
          GConfig.LoadFromFile(Value)
        else
          WriteLn('Warning: Config file not found: ', Value);
      end;
    end
    else if (Param = '-s') or (Param = '--source') then
    begin
      Inc(I);
      if I <= ParamCount then
        GConfig.SourceDir := ParamStr(I);
    end
    else if (Param = '-t') or (Param = '--tests') then
    begin
      Inc(I);
      if I <= ParamCount then
        GConfig.TestDir := ParamStr(I);
    end
    else if (Param = '-o') or (Param = '--output') then
    begin
      Inc(I);
      if I <= ParamCount then
        GConfig.OutputDir := ParamStr(I);
    end
    else if (Param = '-b') or (Param = '--baseline') then
    begin
      Inc(I);
      if I <= ParamCount then
        GConfig.BaselineFile := ParamStr(I);
    end;
    
    Inc(I);
  end;
end;

procedure RunAudit;
var
  SourceScanner: TSourceScanner;
  TestScanner: TTestScanner;
  CoverageAnalyzer: TCoverageAnalyzer;
  BoundaryChecker: TBoundaryChecker;
  ErrorAuditor: TErrorAuditor;
  CryptoAuditor: TCryptoAuditor;
  ThreadAuditor: TThreadAuditor;
  ResourceAuditor: TResourceAuditor;
  BackendAuditor: TBackendAuditor;
  TaskGenerator: TTaskGenerator;
  Reporter: TQualityReporter;
  Results: TAuditResults;
  Functions: TFunctionInfoArray;
  TestCases: TTestCaseInfoArray;
  TestMap: TTestFunctionMapArray;
  OutputDir, ReportFile: string;
  Score: TQualityScore;
begin
  WriteLn('Test Quality Audit Tool v', VERSION);
  WriteLn('================================');
  WriteLn;
  
  // 初始化结果
  Results.Timestamp := Now;
  Results.ProjectPath := GetCurrentDir;
  
  // 创建所有组件
  SourceScanner := TSourceScanner.Create;
  TestScanner := TTestScanner.Create;
  CoverageAnalyzer := TCoverageAnalyzer.Create;
  BoundaryChecker := TBoundaryChecker.Create;
  ErrorAuditor := TErrorAuditor.Create;
  CryptoAuditor := TCryptoAuditor.Create;
  ThreadAuditor := TThreadAuditor.Create;
  ResourceAuditor := TResourceAuditor.Create;
  BackendAuditor := TBackendAuditor.Create;
  TaskGenerator := TTaskGenerator.Create;
  Reporter := TQualityReporter.Create;
  try
    // 设置 verbose 模式
    SourceScanner.Verbose := GConfig.Verbose;
    TestScanner.Verbose := GConfig.Verbose;
    CoverageAnalyzer.Verbose := GConfig.Verbose;
    BoundaryChecker.Verbose := GConfig.Verbose;
    ErrorAuditor.Verbose := GConfig.Verbose;
    CryptoAuditor.Verbose := GConfig.Verbose;
    ThreadAuditor.Verbose := GConfig.Verbose;
    ResourceAuditor.Verbose := GConfig.Verbose;
    BackendAuditor.Verbose := GConfig.Verbose;
    TaskGenerator.Verbose := GConfig.Verbose;
    CoverageAnalyzer.CoverageThreshold := GConfig.CoverageThreshold;
    
    // 扫描源码
    WriteLn('Scanning source files in: ', GConfig.SourceDir);
    Functions := SourceScanner.ScanDirectory(GConfig.SourceDir);
    Results.TotalSourceFiles := Length(Functions);
    Results.TotalFunctions := Length(Functions);
    WriteLn('  Found ', Length(Functions), ' public functions');
    WriteLn;
    
    // 扫描测试
    WriteLn('Scanning test files in: ', GConfig.TestDir);
    TestCases := TestScanner.ScanTestDirectory(GConfig.TestDir);
    Results.TotalTestFiles := Length(TestCases);
    Results.TotalTestCases := Length(TestCases);
    WriteLn('  Found ', Length(TestCases), ' test cases');
    WriteLn;
    
    // 映射测试到函数
    WriteLn('Mapping tests to functions...');
    TestMap := TestScanner.MapTestsToFunctions(TestCases, Functions);
    WriteLn;
    
    // 分析覆盖率
    WriteLn('Analyzing coverage...');
    CoverageAnalyzer.SetData(Functions, TestMap);
    Results.CoverageResults := CoverageAnalyzer.AnalyzeProject;
    Results.OverallCoverage := CoverageAnalyzer.GetOverallCoverage;
    WriteLn(Format('  Overall coverage: %.1f%%', [Results.OverallCoverage]));
    WriteLn;
    
    // 边界条件检查
    WriteLn('Checking boundary conditions...');
    BoundaryChecker.SetData(Functions, TestCases);
    Results.BoundaryGaps := BoundaryChecker.CheckAllFunctions;
    Results.BoundaryScore := BoundaryChecker.GetBoundaryScore;
    WriteLn(Format('  Boundary score: %d%%', [Results.BoundaryScore]));
    WriteLn;
    
    // 错误处理审计
    WriteLn('Auditing error handling...');
    ErrorAuditor.SetData(Functions, TestCases);
    Results.ErrorGaps := ErrorAuditor.AuditAllFunctions;
    Results.ErrorScore := ErrorAuditor.GetErrorScore;
    WriteLn(Format('  Error handling score: %d%%', [Results.ErrorScore]));
    WriteLn;
    
    // 加密功能审计
    WriteLn('Auditing crypto functions...');
    CryptoAuditor.SetData(Functions, TestCases);
    Results.CryptoGaps := CryptoAuditor.AuditAllCryptoFunctions;
    Results.CryptoScore := CryptoAuditor.GetCryptoScore;
    WriteLn(Format('  Crypto testing score: %d%%', [Results.CryptoScore]));
    WriteLn;
    
    // 线程安全审计
    WriteLn('Auditing thread safety...');
    ThreadAuditor.SetData(Functions, TestCases);
    Results.ThreadGaps := ThreadAuditor.AuditAllFunctions;
    Results.ThreadScore := ThreadAuditor.GetThreadScore;
    WriteLn(Format('  Thread safety score: %d%%', [Results.ThreadScore]));
    WriteLn;
    
    // 资源管理审计
    WriteLn('Auditing resource management...');
    ResourceAuditor.SetData(Functions, TestCases);
    Results.ResourceGaps := ResourceAuditor.AuditAllFunctions;
    Results.ResourceScore := ResourceAuditor.GetResourceScore;
    WriteLn(Format('  Resource management score: %d%%', [Results.ResourceScore]));
    WriteLn;
    
    // 后端一致性审计
    WriteLn('Auditing backend consistency...');
    BackendAuditor.SetData(Functions, TestCases);
    Results.BackendGaps := BackendAuditor.AuditBackendConsistency;
    Results.BackendScore := BackendAuditor.GetBackendScore;
    WriteLn(Format('  Backend consistency score: %d%%', [Results.BackendScore]));
    WriteLn;
    
    // 生成改进任务
    WriteLn('Generating improvement tasks...');
    Results.ImprovementTasks := TaskGenerator.GenerateAllTasks(Results);
    WriteLn(Format('  Generated %d tasks', [Length(Results.ImprovementTasks)]));
    WriteLn;
    
    // 计算总体评分
    Score := Reporter.CalculateScore(Results);
    Results.OverallScore := Score;
    
    // 生成报告
    OutputDir := GConfig.OutputDir;
    if not DirectoryExists(OutputDir) then
      ForceDirectories(OutputDir);
      
    ReportFile := 'audit_' + FormatDateTime('yyyymmdd_hhnnss', Now);
    
    WriteLn('Generating reports...');
    Reporter.SaveMarkdownReport(Results, 
      IncludeTrailingPathDelimiter(OutputDir) + ReportFile + '.md');
    Reporter.SaveJSONReport(Results,
      IncludeTrailingPathDelimiter(OutputDir) + ReportFile + '.json');
    WriteLn('  Reports saved to: ', OutputDir);
    WriteLn;
    
    // 输出摘要
    WriteLn('================================');
    WriteLn('Audit Summary');
    WriteLn('================================');
    WriteLn(Format('Overall Score: %d/100', [Score.Overall]));
    WriteLn;
    WriteLn('Category Scores:');
    WriteLn(Format('  Coverage:            %d%%', [Score.Coverage]));
    WriteLn(Format('  Boundary Testing:    %d%%', [Score.BoundaryTesting]));
    WriteLn(Format('  Error Handling:      %d%%', [Score.ErrorHandling]));
    WriteLn(Format('  Crypto Testing:      %d%%', [Score.CryptoTesting]));
    WriteLn(Format('  Thread Safety:       %d%%', [Score.ThreadSafety]));
    WriteLn(Format('  Resource Management: %d%%', [Score.ResourceManagement]));
    WriteLn(Format('  Backend Consistency: %d%%', [Score.BackendConsistency]));
    WriteLn;
    WriteLn(Format('Improvement Tasks: %d', [Length(Results.ImprovementTasks)]));
    WriteLn;
    
    // 设置退出码
    if Score.Overall < Round(GConfig.OverallThreshold) then
    begin
      WriteLn('WARNING: Quality score below threshold!');
      GExitCode := 1;
    end
    else
    begin
      WriteLn('Quality check passed.');
      GExitCode := 0;
    end;
    
  finally
    SourceScanner.Free;
    TestScanner.Free;
    CoverageAnalyzer.Free;
    BoundaryChecker.Free;
    ErrorAuditor.Free;
    CryptoAuditor.Free;
    ThreadAuditor.Free;
    ResourceAuditor.Free;
    BackendAuditor.Free;
    TaskGenerator.Free;
    Reporter.Free;
  end;
end;

begin
  GExitCode := 0;
  GConfig := TAuditConfig.Create;
  try
    // 尝试加载默认配置
    if FileExists('tools/test_audit/audit_config.json') then
      GConfig.LoadFromFile('tools/test_audit/audit_config.json')
    else if FileExists('audit_config.json') then
      GConfig.LoadFromFile('audit_config.json');
      
    ParseCommandLine;
    RunAudit;
  finally
    GConfig.Free;
  end;
  
  Halt(GExitCode);
end.
