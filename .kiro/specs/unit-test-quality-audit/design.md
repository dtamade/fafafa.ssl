# Design Document: Unit Test Quality Audit

## Overview

本设计文档描述了 fafafa.ssl 项目单元测试质量审计系统的架构和实现方案。该系统将分析现有测试的覆盖率、边界条件、错误处理等方面，生成质量报告和改进建议。

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Test Quality Audit System                     │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐  │
│  │   Source    │  │    Test     │  │      Audit Engine       │  │
│  │   Scanner   │  │   Scanner   │  │  ┌───────────────────┐  │  │
│  │             │  │             │  │  │ Coverage Analyzer │  │  │
│  │ - Parse .pas│  │ - Parse test│  │  │ Boundary Checker  │  │  │
│  │ - Extract   │  │ - Extract   │  │  │ Error Auditor     │  │  │
│  │   functions │  │   test cases│  │  │ Crypto Auditor    │  │  │
│  │ - Build AST │  │ - Map to src│  │  │ Thread Auditor    │  │  │
│  └──────┬──────┘  └──────┬──────┘  │  │ Resource Auditor  │  │  │
│         │                │         │  │ Backend Auditor   │  │  │
│         └────────┬───────┘         │  └───────────────────┘  │  │
│                  │                 └────────────┬────────────┘  │
│                  ▼                              │               │
│         ┌────────────────┐                     │               │
│         │  Symbol Table  │◄────────────────────┘               │
│         │  & Test Map    │                                     │
│         └────────┬───────┘                                     │
│                  │                                             │
│                  ▼                                             │
│         ┌────────────────┐                                     │
│         │ Quality Report │                                     │
│         │ Generator      │                                     │
│         │ - Markdown     │                                     │
│         │ - JSON         │                                     │
│         │ - Task List    │                                     │
│         └────────────────┘                                     │
└─────────────────────────────────────────────────────────────────┘
```

## Components and Interfaces

### 1. Source Scanner (源码扫描器)

```pascal
type
  TFunctionInfo = record
    Name: string;
    UnitName: string;
    Visibility: TVisibility;  // public, private, protected
    Parameters: array of TParamInfo;
    ReturnType: string;
    LineNumber: Integer;
    IsMethod: Boolean;
    ClassName: string;
    HasErrorReturn: Boolean;  // 是否可能返回错误
  end;

  TSourceScanner = class
  public
    function ScanDirectory(const APath: string): TArray<TFunctionInfo>;
    function ScanUnit(const AFilePath: string): TArray<TFunctionInfo>;
    function ExtractPublicAPI(const AUnit: string): TArray<TFunctionInfo>;
  end;
```

### 2. Test Scanner (测试扫描器)

```pascal
type
  TTestCaseInfo = record
    Name: string;
    TestFile: string;
    TestedFunction: string;
    TestType: TTestType;  // unit, integration, property, boundary, error
    HasSetup: Boolean;
    HasTeardown: Boolean;
    Assertions: Integer;
    LineNumber: Integer;
  end;

  TTestScanner = class
  public
    function ScanTestDirectory(const APath: string): TArray<TTestCaseInfo>;
    function ScanTestFile(const AFilePath: string): TArray<TTestCaseInfo>;
    function MapTestsToFunctions(const ATests: TArray<TTestCaseInfo>;
      const AFunctions: TArray<TFunctionInfo>): TTestFunctionMap;
  end;
```

### 3. Coverage Analyzer (覆盖率分析器)

```pascal
type
  TCoverageResult = record
    UnitName: string;
    TotalFunctions: Integer;
    TestedFunctions: Integer;
    CoveragePercent: Double;
    UntestedFunctions: TArray<string>;
    PartiallyTestedFunctions: TArray<string>;
  end;

  TCoverageAnalyzer = class
  public
    function AnalyzeUnit(const AUnit: string): TCoverageResult;
    function AnalyzeProject: TArray<TCoverageResult>;
    function GetOverallCoverage: Double;
    function GetHighPriorityGaps: TArray<string>;  // 覆盖率 < 80%
  end;
```

### 4. Boundary Checker (边界检查器)

```pascal
type
  TBoundaryType = (
    btNumericZero,
    btNumericNegative,
    btNumericMax,
    btNumericMin,
    btNumericOverflow,
    btStringEmpty,
    btStringNull,
    btStringLong,
    btStringSpecialChars,
    btArrayEmpty,
    btArraySingle,
    btArrayBoundary,
    btArrayOversized,
    btPointerNil
  );

  TBoundaryGap = record
    FunctionName: string;
    ParameterName: string;
    MissingBoundaries: set of TBoundaryType;
    Recommendation: string;
  end;

  TBoundaryChecker = class
  public
    function CheckFunction(const AFunc: TFunctionInfo;
      const ATests: TArray<TTestCaseInfo>): TArray<TBoundaryGap>;
    function CheckUnit(const AUnit: string): TArray<TBoundaryGap>;
    function GenerateRecommendations: TArray<string>;
  end;
```

### 5. Error Handling Auditor (错误处理审计器)

```pascal
type
  TErrorTestGap = record
    FunctionName: string;
    ErrorCondition: string;
    HasTest: Boolean;
    Recommendation: string;
  end;

  TErrorAuditor = class
  public
    function IdentifyErrorPaths(const AFunc: TFunctionInfo): TArray<string>;
    function CheckErrorTests(const AFunc: TFunctionInfo;
      const ATests: TArray<TTestCaseInfo>): TArray<TErrorTestGap>;
    function AuditExceptionHandling(const AUnit: string): TArray<TErrorTestGap>;
  end;
```

### 6. Crypto Auditor (加密审计器)

```pascal
type
  TCryptoTestGap = record
    FunctionName: string;
    GapType: TCryptoGapType;  // MissingKAT, MissingRoundTrip, MissingWeakKeyTest
    StandardReference: string;  // NIST, RFC 编号
    Recommendation: string;
  end;

  TCryptoAuditor = class
  public
    function CheckKnownAnswerTests(const AUnit: string): TArray<TCryptoTestGap>;
    function CheckRoundTripTests(const AUnit: string): TArray<TCryptoTestGap>;
    function CheckWeakKeyTests(const AUnit: string): TArray<TCryptoTestGap>;
    function VerifyTestVectors(const AUnit: string): TArray<TCryptoTestGap>;
  end;
```

### 7. Quality Reporter (质量报告生成器)

```pascal
type
  TQualityScore = record
    Overall: Integer;  // 0-100
    Coverage: Integer;
    BoundaryTesting: Integer;
    ErrorHandling: Integer;
    CryptoTesting: Integer;
    ThreadSafety: Integer;
    ResourceManagement: Integer;
  end;

  TQualityReporter = class
  public
    function GenerateMarkdownReport(const AResults: TAuditResults): string;
    function GenerateJSONReport(const AResults: TAuditResults): string;
    function GenerateTaskList(const AResults: TAuditResults): string;
    function CalculateScore(const AResults: TAuditResults): TQualityScore;
  end;
```

## Data Models

### Audit Results Structure

```pascal
type
  TAuditResults = record
    Timestamp: TDateTime;
    ProjectPath: string;
    
    // 覆盖率结果
    CoverageResults: TArray<TCoverageResult>;
    OverallCoverage: Double;
    
    // 边界测试结果
    BoundaryGaps: TArray<TBoundaryGap>;
    BoundaryScore: Integer;
    
    // 错误处理结果
    ErrorGaps: TArray<TErrorTestGap>;
    ErrorScore: Integer;
    
    // 加密测试结果
    CryptoGaps: TArray<TCryptoTestGap>;
    CryptoScore: Integer;
    
    // 线程安全结果
    ThreadGaps: TArray<TThreadTestGap>;
    ThreadScore: Integer;
    
    // 资源管理结果
    ResourceGaps: TArray<TResourceTestGap>;
    ResourceScore: Integer;
    
    // 跨后端一致性结果
    BackendGaps: TArray<TBackendTestGap>;
    BackendScore: Integer;
    
    // 总体评分
    OverallScore: TQualityScore;
    
    // 改进任务
    ImprovementTasks: TArray<TImprovementTask>;
  end;
```

### Improvement Task Structure

```pascal
type
  TTaskPriority = (tpCritical, tpHigh, tpMedium, tpLow);
  TTaskCategory = (tcCoverage, tcBoundary, tcError, tcCrypto, tcThread, tcResource, tcBackend);

  TImprovementTask = record
    ID: string;
    Title: string;
    Description: string;
    Category: TTaskCategory;
    Priority: TTaskPriority;
    AffectedUnit: string;
    AffectedFunction: string;
    EstimatedEffort: string;  // 'small', 'medium', 'large'
    CodeTemplate: string;
    Requirements: TArray<string>;
  end;
```



## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*


### Property 1: Coverage Analysis Correctness

*For any* source directory containing Pascal units with public functions, the Coverage_Analyzer SHALL correctly identify all public functions, map tests to functions, flag untested functions, calculate accurate coverage percentages, and identify modules below 80% coverage as high priority.

**Validates: Requirements 1.1, 1.2, 1.3, 1.4, 1.5**

### Property 2: Boundary Detection Correctness

*For any* function with typed parameters, the Boundary_Checker SHALL correctly identify missing boundary tests for numeric (zero, negative, max, min, overflow), string (empty, null, long, special chars), array (empty, single, boundary, oversized), and pointer (nil) parameters, and generate appropriate recommendations.

**Validates: Requirements 2.1, 2.2, 2.3, 2.4, 2.5**

### Property 3: Test Classification Correctness

*For any* test file, the Test_Classifier SHALL correctly categorize tests by type (unit, integration, performance, security, stress, fuzz), flag mixed-type files, identify naming convention violations, verify test structure (setup/execution/assertion), and flag external dependencies.

**Validates: Requirements 3.1, 3.2, 3.3, 3.4, 3.5**

### Property 4: Error Handling Audit Correctness

*For any* function that can return errors or raise exceptions, the Test_Auditor SHALL verify tests exist for each error condition, exception handling, precondition failures, and recovery paths.

**Validates: Requirements 4.1, 4.2, 4.3, 4.5**

### Property 5: Crypto Audit Correctness

*For any* cryptographic function, the Test_Auditor SHALL verify known-answer tests with standard vectors exist, hash tests use NIST/RFC vectors, key generation tests verify randomness and strength, signature tests include round-trips, and edge case tests exist for weak keys and invalid parameters.

**Validates: Requirements 5.1, 5.2, 5.3, 5.4, 5.5**

### Property 6: Thread Safety Audit Correctness

*For any* module claiming thread-safety, the Test_Auditor SHALL verify concurrent access tests, race condition tests, deadlock prevention tests, multi-thread tests (minimum 4 threads), and stress tests exist.

**Validates: Requirements 6.1, 6.2, 6.3, 6.4, 6.5**

### Property 7: Resource Management Audit Correctness

*For any* code that allocates resources, the Test_Auditor SHALL verify memory leak tests, cleanup tests for handles/contexts, resource exhaustion tests, destructor behavior tests, and double-free/use-after-free prevention tests exist.

**Validates: Requirements 7.1, 7.2, 7.3, 7.4, 7.5**

### Property 8: Backend Consistency Audit Correctness

*For any* feature supported by both OpenSSL and WinSSL backends, the Test_Auditor SHALL verify identical tests exist for both, output consistency tests exist, error code mapping tests exist, graceful degradation tests exist for backend-specific features, and API contract tests exist.

**Validates: Requirements 8.1, 8.2, 8.3, 8.4, 8.5**

### Property 9: Report Generation Correctness

*For any* audit results, the Quality_Reporter SHALL generate a health score (0-100), list untested functions sorted by risk, list missing boundary tests, provide specific recommendations, and output in both Markdown and JSON formats.

**Validates: Requirements 9.1, 9.2, 9.3, 9.4, 9.5**

### Property 10: Task Generation Correctness

*For any* identified issues, the Test_Auditor SHALL generate specific coding tasks, prioritize by security/usage/complexity, estimate effort, group related tasks, and provide code templates.

**Validates: Requirements 10.1, 10.2, 10.3, 10.4, 10.5**

### Property 11: CI Integration Correctness

*For any* execution mode, the Test_Auditor SHALL support command-line execution with exit codes, return non-zero for quality below threshold, support incremental analysis, output machine-readable results, and support configurable quality gates.

**Validates: Requirements 11.1, 11.2, 11.3, 11.4, 11.5**

## Error Handling

### Scanner Errors

| Error | Handling |
|-------|----------|
| File not found | Log warning, skip file, continue scanning |
| Parse error | Log error with line number, skip file, continue |
| Invalid encoding | Try UTF-8 and ANSI, log if both fail |
| Permission denied | Log error, skip file, continue |

### Analysis Errors

| Error | Handling |
|-------|----------|
| Unknown parameter type | Classify as "unanalyzable", log warning |
| Circular dependency | Detect and break cycle, log warning |
| Missing test directory | Create empty result, log warning |
| Timeout during analysis | Return partial results, log timeout |

### Report Errors

| Error | Handling |
|-------|----------|
| Output directory not writable | Return error, suggest alternative |
| JSON serialization failure | Fall back to simplified format |
| Template not found | Use default template, log warning |

## Testing Strategy

### Unit Tests

1. **Scanner Tests**
   - Test parsing of various Pascal constructs
   - Test extraction of function signatures
   - Test handling of malformed files

2. **Analyzer Tests**
   - Test coverage calculation accuracy
   - Test boundary detection for each parameter type
   - Test error path identification

3. **Reporter Tests**
   - Test Markdown format correctness
   - Test JSON schema compliance
   - Test score calculation accuracy

### Property-Based Tests

1. **Coverage Calculation Property**
   - Generate random function lists and test maps
   - Verify coverage = tested / total * 100

2. **Boundary Detection Property**
   - Generate random parameter types
   - Verify all boundary types are checked

3. **Report Consistency Property**
   - Generate random audit results
   - Verify Markdown and JSON contain same data

### Integration Tests

1. **End-to-End Audit**
   - Run full audit on test project
   - Verify all components work together

2. **CI Integration**
   - Test command-line interface
   - Test exit codes
   - Test incremental analysis

