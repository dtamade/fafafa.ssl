# Design Document: Critical Issues Audit

## Overview

本设计文档描述了对 fafafa.ssl 项目进行深度审查的系统架构。系统将执行编译验证、运行时测试、静态分析、安全审计，并自动修复可修复的问题，最终生成详细报告。

## Architecture

系统采用多阶段审查架构：

```
┌─────────────────────────────────────────────────────────────┐
│                    Critical Issues Audit                     │
├─────────────────────────────────────────────────────────────┤
│  Phase 1: Compilation Validation                             │
│  ├── Source File Compiler                                    │
│  ├── Test File Compiler                                      │
│  └── Error Collector                                         │
├─────────────────────────────────────────────────────────────┤
│  Phase 2: Runtime Testing                                    │
│  ├── Test Executor                                           │
│  ├── Result Collector                                        │
│  └── Failure Analyzer                                        │
├─────────────────────────────────────────────────────────────┤
│  Phase 3: Static Analysis                                    │
│  ├── Memory Safety Analyzer                                  │
│  ├── Resource Leak Detector                                  │
│  ├── Error Handling Checker                                  │
│  └── Code Quality Scanner                                    │
├─────────────────────────────────────────────────────────────┤
│  Phase 4: Security Audit                                     │
│  ├── Crypto Usage Auditor                                    │
│  ├── Sensitive Data Handler                                  │
│  └── Configuration Validator                                 │
├─────────────────────────────────────────────────────────────┤
│  Phase 5: Auto-Fix & Report                                  │
│  ├── Issue Fixer                                             │
│  ├── Report Generator                                        │
│  └── Score Calculator                                        │
└─────────────────────────────────────────────────────────────┘
```

## Components and Interfaces

### 1. Compilation Validator

负责编译所有源码和测试文件，收集编译错误。

```pascal
type
  TCompilationResult = record
    FilePath: string;
    Success: Boolean;
    ErrorMessage: string;
    ErrorLine: Integer;
    ErrorColumn: Integer;
    SuggestedFix: string;
  end;

  TCompilationValidator = class
  public
    function CompileSourceFiles(const ASourceDir: string): TArray<TCompilationResult>;
    function CompileTestFiles(const ATestDir: string): TArray<TCompilationResult>;
    function GetFailedFiles: TArray<string>;
    function GetSuccessRate: Double;
  end;
```

### 2. Runtime Tester

执行测试程序并收集结果。

```pascal
type
  TTestResult = record
    TestFile: string;
    TestName: string;
    Passed: Boolean;
    Duration: Integer;  // milliseconds
    Output: string;
    ErrorMessage: string;
  end;

  TRuntimeTester = class
  public
    function ExecuteTest(const ATestPath: string): TTestResult;
    function ExecuteAllTests(const ATestDir: string): TArray<TTestResult>;
    function GetPassRate: Double;
    function GetFailedTests: TArray<TTestResult>;
  end;
```

### 3. Memory Safety Analyzer

分析内存分配和释放模式。

```pascal
type
  TMemoryIssue = record
    FilePath: string;
    LineNumber: Integer;
    IssueType: TMemoryIssueType;  // Leak, DoubleFree, UseAfterFree, UninitializedRead
    Description: string;
    Severity: TSeverity;
    CanAutoFix: Boolean;
    FixCode: string;
  end;

  TMemorySafetyAnalyzer = class
  public
    function AnalyzeFile(const AFilePath: string): TArray<TMemoryIssue>;
    function AnalyzeDirectory(const ADir: string): TArray<TMemoryIssue>;
    function FindUnmatchedAllocations: TArray<TMemoryIssue>;
    function CheckTryFinallyPatterns: TArray<TMemoryIssue>;
  end;
```

### 4. Security Auditor

检测安全漏洞。

```pascal
type
  TSecurityIssue = record
    FilePath: string;
    LineNumber: Integer;
    IssueType: TSecurityIssueType;
    Description: string;
    Severity: TSeverity;
    Recommendation: string;
  end;

  TSecurityAuditor = class
  public
    function AuditCryptoUsage(const ADir: string): TArray<TSecurityIssue>;
    function AuditSensitiveDataHandling(const ADir: string): TArray<TSecurityIssue>;
    function AuditTLSConfiguration(const ADir: string): TArray<TSecurityIssue>;
    function FindHardcodedSecrets(const ADir: string): TArray<TSecurityIssue>;
  end;
```

### 5. Issue Fixer

自动修复可修复的问题。

```pascal
type
  TFixResult = record
    FilePath: string;
    IssueDescription: string;
    FixApplied: Boolean;
    BackupPath: string;
    BeforeCode: string;
    AfterCode: string;
  end;

  TIssueFixer = class
  public
    function FixCompilationError(const AResult: TCompilationResult): TFixResult;
    function FixMemoryIssue(const AIssue: TMemoryIssue): TFixResult;
    function FixResourceLeak(const AIssue: TResourceIssue): TFixResult;
    function CreateBackup(const AFilePath: string): string;
    function RestoreBackup(const ABackupPath: string): Boolean;
  end;
```

## Data Models

### Issue Severity Levels

```pascal
type
  TSeverity = (
    svCritical,   // 必须立即修复，可能导致安全漏洞或崩溃
    svHigh,       // 应尽快修复，可能导致功能问题
    svMedium,     // 应该修复，影响代码质量
    svLow         // 可选修复，改进建议
  );
```

### Issue Categories

```pascal
type
  TIssueCategory = (
    icCompilation,      // 编译错误
    icRuntime,          // 运行时错误
    icMemorySafety,     // 内存安全
    icSecurity,         // 安全漏洞
    icResourceLeak,     // 资源泄漏
    icErrorHandling,    // 错误处理
    icAPIConsistency,   // API 一致性
    icCodeQuality       // 代码质量
  );
```

### Audit Report Structure

```pascal
type
  TAuditReport = record
    Timestamp: TDateTime;
    ProjectPath: string;
    
    // Summary
    TotalIssues: Integer;
    CriticalCount: Integer;
    HighCount: Integer;
    MediumCount: Integer;
    LowCount: Integer;
    
    // By Category
    CompilationIssues: TArray<TCompilationResult>;
    RuntimeFailures: TArray<TTestResult>;
    MemoryIssues: TArray<TMemoryIssue>;
    SecurityIssues: TArray<TSecurityIssue>;
    ResourceLeaks: TArray<TResourceIssue>;
    
    // Fixes Applied
    FixesApplied: TArray<TFixResult>;
    
    // Score
    PreviousScore: Integer;
    CurrentScore: Integer;
  end;
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Compilation Validation Completeness

*For any* source directory containing Pascal files, the Compilation_Validator SHALL attempt to compile every .pas file and record a result (success or failure with details) for each file.

**Validates: Requirements 1.1, 1.2, 1.3**

### Property 2: Test Execution Isolation

*For any* test execution, the Runtime_Tester SHALL execute the test in a separate process such that failures in one test do not affect other tests.

**Validates: Requirements 2.1, 2.3**

### Property 3: Memory Allocation Tracking

*For any* memory allocation call (GetMem, New, Create) found in source code, the Memory_Auditor SHALL identify and track it, and verify a corresponding deallocation exists in the same scope or a finally block.

**Validates: Requirements 3.1, 3.2, 3.3, 3.4**

### Property 4: Security Pattern Detection

*For any* cryptographic comparison operation, the Security_Auditor SHALL verify it uses constant-time comparison functions to prevent timing attacks.

**Validates: Requirements 4.1**

### Property 5: Auto-Fix Safety

*For any* automatic fix applied, the Issue_Fixer SHALL create a backup of the original file before modification, and the fix SHALL not introduce new compilation errors.

**Validates: Requirements 9.1, 9.4**

### Property 6: Report Completeness

*For any* audit execution, the Report_Generator SHALL produce a report containing all discovered issues categorized by severity and type, with counts matching the actual issues found.

**Validates: Requirements 10.1, 10.2, 10.3**

## Error Handling

### Compilation Errors

- 捕获 FPC 编译器输出
- 解析错误消息提取文件、行号、错误类型
- 对常见错误提供修复建议

### Runtime Errors

- 使用超时机制防止测试挂起
- 捕获异常和崩溃信息
- 记录标准输出和标准错误

### Analysis Errors

- 对无法解析的文件记录警告
- 继续处理其他文件
- 在报告中标注跳过的文件

## Testing Strategy

### Unit Tests

- 测试编译器输出解析
- 测试内存模式检测
- 测试安全模式检测
- 测试修复代码生成

### Integration Tests

- 端到端审计流程测试
- 报告生成验证
- 自动修复验证

### Property-Based Tests

- 使用 QuickCheck 风格测试验证属性
- 最少 100 次迭代
- 测试边界条件和异常情况

