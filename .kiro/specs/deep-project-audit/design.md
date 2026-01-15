# Design Document: Deep Project Audit

## Overview

本设计文档描述了 fafafa.ssl 项目深度审查系统的架构和实现方案。该系统将全面扫描项目的源码、测试、文档和示例，识别缺失或不完整的部分，并生成改进任务清单。

系统采用模块化设计，每个审查维度由独立的分析器负责，最终由报告生成器汇总所有发现。

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Deep Audit System                         │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐       │
│  │ Compilation  │  │  Coverage    │  │    API       │       │
│  │  Validator   │  │  Analyzer    │  │  Validator   │       │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘       │
│         │                 │                 │                │
│  ┌──────┴───────┐  ┌──────┴───────┐  ┌──────┴───────┐       │
│  │    Doc       │  │  Security    │  │   Quality    │       │
│  │   Scanner    │  │   Checker    │  │   Analyzer   │       │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘       │
│         │                 │                 │                │
│         └─────────────────┼─────────────────┘                │
│                           │                                  │
│                    ┌──────┴───────┐                         │
│                    │   Report     │                         │
│                    │  Generator   │                         │
│                    └──────────────┘                         │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

## Components and Interfaces

### 1. Compilation Validator (编译验证器)

负责验证所有源码模块的编译状态。

```pascal
type
  TCompilationStatus = (csSuccess, csWarning, csError);
  
  TCompilationResult = record
    ModulePath: string;
    Status: TCompilationStatus;
    ErrorMessage: string;
    ErrorLine: Integer;
    ErrorType: string;  // syntax, type, dependency
  end;
  
  TCompilationValidator = class
  public
    function ValidateModule(const APath: string): TCompilationResult;
    function ValidateDirectory(const ADir: string): TArray<TCompilationResult>;
    function GetCompilationReport: string;
  end;
```

### 2. Coverage Analyzer (覆盖率分析器)

分析测试覆盖率，识别未测试的公共函数。

```pascal
type
  TFunctionInfo = record
    Name: string;
    ModulePath: string;
    LineNumber: Integer;
    IsTested: Boolean;
    TestFiles: TArray<string>;
  end;
  
  TModuleCoverage = record
    ModulePath: string;
    TotalFunctions: Integer;
    TestedFunctions: Integer;
    CoveragePercent: Double;
    IsHighPriority: Boolean;
    UntestedFunctions: TArray<TFunctionInfo>;
  end;
  
  TCoverageAnalyzer = class
  public
    function AnalyzeModule(const APath: string): TModuleCoverage;
    function AnalyzeProject: TArray<TModuleCoverage>;
    function GetUntestedFunctions: TArray<TFunctionInfo>;
  end;
```

### 3. API Validator (API 验证器)

验证 OpenSSL 和 WinSSL 后端的 API 一致性。

```pascal
type
  TAPIDiscrepancy = record
    InterfaceName: string;
    MethodName: string;
    DiscrepancyType: string;  // missing, signature_mismatch, error_code_mismatch
    OpenSSLSignature: string;
    WinSSLSignature: string;
    Severity: string;
  end;
  
  TAPIValidator = class
  public
    function CompareInterfaces: TArray<TAPIDiscrepancy>;
    function CheckErrorCodeMapping: TArray<TAPIDiscrepancy>;
    function GetConsistencyScore: Double;
  end;
```

### 4. Doc Scanner (文档扫描器)

检查文档完整性和准确性。

```pascal
type
  TDocIssue = record
    IssueType: string;  // missing_doc, outdated, broken_example
    Location: string;
    Description: string;
    Suggestion: string;
  end;
  
  TDocScanner = class
  public
    function ScanPublicAPIs: TArray<TDocIssue>;
    function ValidateReadme: TArray<TDocIssue>;
    function ValidateAPIReference: TArray<TDocIssue>;
    function ValidateExamples: TArray<TDocIssue>;
  end;
```

### 5. Security Checker (安全检查器)

检查安全最佳实践。

```pascal
type
  TSeverity = (svCritical, svHigh, svMedium, svLow);
  
  TSecurityIssue = record
    IssueType: string;
    Location: string;
    Description: string;
    Severity: TSeverity;
    Recommendation: string;
  end;
  
  TSecurityChecker = class
  public
    function CheckMemoryClearing: TArray<TSecurityIssue>;
    function CheckConstantTimeOps: TArray<TSecurityIssue>;
    function CheckSecureRandom: TArray<TSecurityIssue>;
    function CheckTLSDefaults: TArray<TSecurityIssue>;
    function GetSecurityScore: Double;
  end;
```

### 6. Quality Analyzer (代码质量分析器)

分析代码质量问题。

```pascal
type
  TQualityIssue = record
    IssueType: string;  // unused, complexity, duplicate, naming, cleanup
    Location: string;
    Description: string;
    Impact: string;
    Suggestion: string;
  end;
  
  TQualityAnalyzer = class
  public
    function CheckUnusedCode: TArray<TQualityIssue>;
    function CheckComplexity: TArray<TQualityIssue>;
    function CheckDuplicates: TArray<TQualityIssue>;
    function CheckNamingConventions: TArray<TQualityIssue>;
    function CheckResourceCleanup: TArray<TQualityIssue>;
  end;
```

### 7. Report Generator (报告生成器)

汇总所有发现并生成报告。

```pascal
type
  TAuditReport = record
    Timestamp: TDateTime;
    HealthScore: Double;
    CompilationResults: TArray<TCompilationResult>;
    CoverageResults: TArray<TModuleCoverage>;
    APIDiscrepancies: TArray<TAPIDiscrepancy>;
    DocIssues: TArray<TDocIssue>;
    SecurityIssues: TArray<TSecurityIssue>;
    QualityIssues: TArray<TQualityIssue>;
    ImprovementTasks: TArray<TImprovementTask>;
  end;
  
  TReportGenerator = class
  public
    function GenerateMarkdownReport(const AReport: TAuditReport): string;
    function GenerateJSONReport(const AReport: TAuditReport): string;
    function CalculateHealthScore(const AReport: TAuditReport): Double;
    function GenerateImprovementTasks(const AReport: TAuditReport): TArray<TImprovementTask>;
  end;
```

## Data Models

### Audit Configuration

```pascal
type
  TAuditConfig = record
    SourceDir: string;           // 默认: src/
    TestDir: string;             // 默认: tests/
    ExamplesDir: string;         // 默认: examples/
    DocsDir: string;             // 默认: docs/
    OutputDir: string;           // 默认: reports/audit/
    CoverageThreshold: Double;   // 默认: 60%
    ComplexityThreshold: Integer; // 默认: 10
    SkipModules: TArray<string>; // 跳过的模块
  end;
```

### Improvement Task

```pascal
type
  TTaskPriority = (tpCritical, tpHigh, tpMedium, tpLow);
  TTaskCategory = (tcCompilation, tcCoverage, tcAPI, tcDoc, tcSecurity, tcQuality);
  
  TImprovementTask = record
    ID: string;
    Title: string;
    Description: string;
    Category: TTaskCategory;
    Priority: TTaskPriority;
    AffectedFiles: TArray<string>;
    EstimatedEffort: string;  // small, medium, large
    Requirements: TArray<string>;  // 关联的需求编号
  end;
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Compilation Analysis Correctness

*For any* set of Pascal source files, the Compilation Validator SHALL correctly identify compilation status (success/warning/error) for each file, and for files with errors, SHALL correctly extract error message, line number, and error type.

**Validates: Requirements 1.1, 1.2, 1.3, 1.4**

### Property 2: Coverage Analysis Correctness

*For any* source module with known public functions and corresponding test files, the Coverage Analyzer SHALL correctly identify all public functions, map tests to functions, calculate coverage percentage as (tested/total)*100, and flag modules below 60% as high priority.

**Validates: Requirements 2.1, 2.2, 2.3, 2.4, 2.5**

### Property 3: API Consistency Detection

*For any* interface implemented by both OpenSSL and WinSSL backends, the API Validator SHALL detect missing methods, signature mismatches, and error code mapping inconsistencies between the two implementations.

**Validates: Requirements 3.4, 3.5, 3.6**

### Property 4: Documentation Completeness Detection

*For any* public function/procedure in the codebase, the Doc Scanner SHALL correctly identify whether it has documentation comments, and SHALL flag undocumented APIs for documentation.

**Validates: Requirements 4.1, 4.3, 4.4, 4.5**

### Property 5: Security Issue Detection

*For any* code handling sensitive data (keys, passwords, authentication), the Security Checker SHALL verify proper memory clearing, constant-time comparison usage, and secure random generation, categorizing issues by severity.

**Validates: Requirements 5.1, 5.2, 5.3, 5.4, 5.5**

### Property 6: Dependency Analysis Correctness

*For any* module with uses clauses, the Audit System SHALL verify all referenced units exist and SHALL detect circular dependencies between modules.

**Validates: Requirements 6.1, 6.2, 6.5**

### Property 7: Code Quality Analysis Correctness

*For any* source module, the Quality Analyzer SHALL correctly identify unused code, functions with cyclomatic complexity > 10, duplicate code blocks, naming convention violations, and missing resource cleanup patterns.

**Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5, 7.6**

### Property 8: Test Quality Analysis Correctness

*For any* test file, the Audit System SHALL verify presence of assertions, detect tests without real validation, check for error handling tests, boundary tests, and verify property-based tests use 100+ iterations.

**Validates: Requirements 8.1, 8.2, 8.3, 8.4, 8.5, 8.6**

### Property 9: Example Validation Correctness

*For any* example file in examples/ directory, the Audit System SHALL correctly determine compilation status and detect usage of outdated APIs.

**Validates: Requirements 9.1, 9.2, 9.4**

### Property 10: Report Generation Correctness

*For any* audit run, the Report Generator SHALL produce a Markdown report containing all findings categorized by area, prioritized by severity, with actionable improvement tasks and an overall health score between 0-100.

**Validates: Requirements 10.1, 10.2, 10.3, 10.4, 10.5, 10.6**

## Error Handling

### Error Categories

1. **Compilation Errors**: 编译失败时记录详细错误信息，继续处理其他模块
2. **Parse Errors**: 解析源码失败时记录并跳过该文件
3. **File Access Errors**: 文件访问失败时记录并继续
4. **Configuration Errors**: 配置无效时使用默认值并警告

### Error Recovery Strategy

```pascal
// 错误处理示例
function TCompilationValidator.ValidateModule(const APath: string): TCompilationResult;
begin
  Result.ModulePath := APath;
  try
    // 尝试编译
    if CompileModule(APath, ErrorMsg, ErrorLine) then
      Result.Status := csSuccess
    else
    begin
      Result.Status := csError;
      Result.ErrorMessage := ErrorMsg;
      Result.ErrorLine := ErrorLine;
      Result.ErrorType := CategorizeError(ErrorMsg);
    end;
  except
    on E: Exception do
    begin
      Result.Status := csError;
      Result.ErrorMessage := 'Exception: ' + E.Message;
      Result.ErrorType := 'exception';
    end;
  end;
end;
```

## Testing Strategy

### Unit Tests

- 每个分析器组件的独立测试
- 使用已知输入验证输出正确性
- 边界条件测试（空目录、无效文件等）

### Property-Based Tests

使用 FreePascal 的测试框架实现属性测试，每个属性测试运行 100+ 次迭代。

测试框架: FPCUnit + 自定义属性测试生成器

### Integration Tests

- 端到端审查流程测试
- 报告生成验证
- 与现有 test_audit 工具的集成测试

### Test Configuration

```pascal
const
  PBT_ITERATIONS = 100;  // 属性测试最小迭代次数
  
// 属性测试标注格式
// **Feature: deep-project-audit, Property N: [Property Title]**
// **Validates: Requirements X.Y**
```
