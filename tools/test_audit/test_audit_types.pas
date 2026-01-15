{
  Test Audit Types - 测试审计系统类型定义

  定义审计系统使用的所有数据类型和枚举
}
unit test_audit_types;

{$mode objfpc}{$H+}{$J-}

interface

uses
  SysUtils, Classes;

type
  { 可见性枚举 }
  TVisibility = (vsPublic, vsPrivate, vsProtected, vsPublished);

  { 参数类型分类 }
  TParamCategory = (
    pcNumeric,      // 数值类型: Integer, Int64, Cardinal, etc.
    pcString,       // 字符串类型: string, AnsiString, PChar
    pcArray,        // 数组类型: array of, TBytes
    pcPointer,      // 指针类型: Pointer, PByte, ^T
    pcRecord,       // 记录类型
    pcClass,        // 类类型
    pcInterface,    // 接口类型
    pcOther         // 其他类型
  );

  { 参数信息 }
  TParamInfo = record
    Name: string;
    TypeName: string;
    Category: TParamCategory;
    IsVar: Boolean;      // var 参数
    IsConst: Boolean;    // const 参数
    IsOut: Boolean;      // out 参数
    HasDefault: Boolean; // 有默认值
  end;
  TParamInfoArray = array of TParamInfo;

  { 函数信息 }
  TFunctionInfo = record
    Name: string;
    UnitName: string;
    FileName: string;
    Visibility: TVisibility;
    Parameters: TParamInfoArray;
    ReturnType: string;
    LineNumber: Integer;
    IsMethod: Boolean;
    IsConstructor: Boolean;
    IsDestructor: Boolean;
    ClassName: string;
    HasErrorReturn: Boolean;  // 可能返回错误
    IsOverload: Boolean;
  end;
  TFunctionInfoArray = array of TFunctionInfo;


  { 测试类型枚举 }
  TTestType = (
    ttUnit,         // 单元测试
    ttIntegration,  // 集成测试
    ttPerformance,  // 性能测试
    ttSecurity,     // 安全测试
    ttStress,       // 压力测试
    ttFuzz,         // 模糊测试
    ttProperty,     // 属性测试
    ttBoundary,     // 边界测试
    ttError,        // 错误处理测试
    ttUnknown       // 未知类型
  );
  TTestTypeSet = set of TTestType;

  { 测试用例信息 }
  TTestCaseInfo = record
    Name: string;
    TestFile: string;
    TestedFunction: string;
    TestType: TTestType;
    HasSetup: Boolean;
    HasTeardown: Boolean;
    AssertionCount: Integer;
    LineNumber: Integer;
    UsesClause: TStringArray;
    CalledFunctions: TStringArray;
  end;
  TTestCaseInfoArray = array of TTestCaseInfo;

  { 边界类型枚举 }
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
  TBoundaryTypeSet = set of TBoundaryType;

  { 边界测试缺口 }
  TBoundaryGap = record
    FunctionName: string;
    UnitName: string;
    ParameterName: string;
    ParameterType: string;
    MissingBoundaries: TBoundaryTypeSet;
    Recommendation: string;
  end;
  TBoundaryGapArray = array of TBoundaryGap;

  { 覆盖率结果 }
  TCoverageResult = record
    UnitName: string;
    FileName: string;
    TotalFunctions: Integer;
    TestedFunctions: Integer;
    CoveragePercent: Double;
    UntestedFunctions: TStringArray;
    PartiallyTestedFunctions: TStringArray;
  end;
  TCoverageResultArray = array of TCoverageResult;

  { 错误测试缺口类型 }
  TErrorGapType = (
    egtMissingErrorCodeTest,
    egtMissingExceptionTest,
    egtMissingPreconditionTest,
    egtMissingRecoveryTest
  );

  { 错误测试缺口 }
  TErrorTestGap = record
    FunctionName: string;
    UnitName: string;
    GapType: TErrorGapType;
    ErrorCondition: string;
    HasTest: Boolean;
    Recommendation: string;
  end;
  TErrorTestGapArray = array of TErrorTestGap;


  { 加密测试缺口类型 }
  TCryptoGapType = (
    cgtMissingKAT,           // 缺少已知答案测试
    cgtMissingRoundTrip,     // 缺少往返测试
    cgtMissingWeakKeyTest,   // 缺少弱密钥测试
    cgtMissingVectorTest,    // 缺少标准向量测试
    cgtMissingEdgeCase       // 缺少边缘情况测试
  );

  { 加密测试缺口 }
  TCryptoTestGap = record
    FunctionName: string;
    UnitName: string;
    GapType: TCryptoGapType;
    StandardReference: string;  // NIST, RFC 编号
    Recommendation: string;
  end;
  TCryptoTestGapArray = array of TCryptoTestGap;

  { 线程安全测试缺口类型 }
  TThreadGapType = (
    tgtMissingConcurrentTest,
    tgtMissingRaceTest,
    tgtMissingDeadlockTest,
    tgtMissingMultiThreadTest,
    tgtMissingStressTest
  );

  { 线程安全测试缺口 }
  TThreadTestGap = record
    FunctionName: string;
    UnitName: string;
    GapType: TThreadGapType;
    Recommendation: string;
  end;
  TThreadTestGapArray = array of TThreadTestGap;

  { 资源管理测试缺口类型 }
  TResourceGapType = (
    rgtMissingLeakTest,
    rgtMissingCleanupTest,
    rgtMissingExhaustionTest,
    rgtMissingDestructorTest,
    rgtMissingDoubleFreeTest
  );

  { 资源管理测试缺口 }
  TResourceTestGap = record
    FunctionName: string;
    UnitName: string;
    GapType: TResourceGapType;
    ResourceType: string;
    Recommendation: string;
  end;
  TResourceTestGapArray = array of TResourceTestGap;

  { 后端一致性测试缺口类型 }
  TBackendGapType = (
    bgtMissingCounterpart,     // 缺少对应后端测试
    bgtMissingConsistencyTest, // 缺少一致性测试
    bgtMissingErrorMapping,    // 缺少错误码映射测试
    bgtMissingDegradation,     // 缺少降级测试
    bgtMissingContractTest     // 缺少契约测试
  );

  { 后端一致性测试缺口 }
  TBackendTestGap = record
    FunctionName: string;
    UnitName: string;
    Backend: string;           // 'OpenSSL' or 'WinSSL'
    GapType: TBackendGapType;
    Recommendation: string;
  end;
  TBackendTestGapArray = array of TBackendTestGap;

  { 任务优先级 }
  TTaskPriority = (tpCritical, tpHigh, tpMedium, tpLow);

  { 任务类别 }
  TTaskCategory = (
    tcCoverage,
    tcBoundary,
    tcError,
    tcCrypto,
    tcThread,
    tcResource,
    tcBackend
  );

  { 改进任务 }
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
  end;
  TImprovementTaskArray = array of TImprovementTask;


  { 质量评分 }
  TQualityScore = record
    Overall: Integer;           // 0-100
    Coverage: Integer;
    BoundaryTesting: Integer;
    ErrorHandling: Integer;
    CryptoTesting: Integer;
    ThreadSafety: Integer;
    ResourceManagement: Integer;
    BackendConsistency: Integer;
  end;

  { 审计结果汇总 }
  TAuditResults = record
    Timestamp: TDateTime;
    ProjectPath: string;
    
    // 覆盖率结果
    CoverageResults: TCoverageResultArray;
    OverallCoverage: Double;
    
    // 边界测试结果
    BoundaryGaps: TBoundaryGapArray;
    BoundaryScore: Integer;
    
    // 错误处理结果
    ErrorGaps: TErrorTestGapArray;
    ErrorScore: Integer;
    
    // 加密测试结果
    CryptoGaps: TCryptoTestGapArray;
    CryptoScore: Integer;
    
    // 线程安全结果
    ThreadGaps: TThreadTestGapArray;
    ThreadScore: Integer;
    
    // 资源管理结果
    ResourceGaps: TResourceTestGapArray;
    ResourceScore: Integer;
    
    // 后端一致性结果
    BackendGaps: TBackendTestGapArray;
    BackendScore: Integer;
    
    // 总体评分
    OverallScore: TQualityScore;
    
    // 改进任务
    ImprovementTasks: TImprovementTaskArray;
    
    // 统计信息
    TotalSourceFiles: Integer;
    TotalTestFiles: Integer;
    TotalFunctions: Integer;
    TotalTestCases: Integer;
  end;

{ 辅助函数 }
function VisibilityToStr(V: TVisibility): string;
function TestTypeToStr(T: TTestType): string;
function BoundaryTypeToStr(B: TBoundaryType): string;
function TaskPriorityToStr(P: TTaskPriority): string;
function TaskCategoryToStr(C: TTaskCategory): string;
function ParamCategoryToStr(C: TParamCategory): string;

implementation

function VisibilityToStr(V: TVisibility): string;
begin
  case V of
    vsPublic: Result := 'public';
    vsPrivate: Result := 'private';
    vsProtected: Result := 'protected';
    vsPublished: Result := 'published';
  else
    Result := 'unknown';
  end;
end;

function TestTypeToStr(T: TTestType): string;
begin
  case T of
    ttUnit: Result := 'unit';
    ttIntegration: Result := 'integration';
    ttPerformance: Result := 'performance';
    ttSecurity: Result := 'security';
    ttStress: Result := 'stress';
    ttFuzz: Result := 'fuzz';
    ttProperty: Result := 'property';
    ttBoundary: Result := 'boundary';
    ttError: Result := 'error';
    ttUnknown: Result := 'unknown';
  else
    Result := 'unknown';
  end;
end;

function BoundaryTypeToStr(B: TBoundaryType): string;
begin
  case B of
    btNumericZero: Result := 'numeric_zero';
    btNumericNegative: Result := 'numeric_negative';
    btNumericMax: Result := 'numeric_max';
    btNumericMin: Result := 'numeric_min';
    btNumericOverflow: Result := 'numeric_overflow';
    btStringEmpty: Result := 'string_empty';
    btStringNull: Result := 'string_null';
    btStringLong: Result := 'string_long';
    btStringSpecialChars: Result := 'string_special';
    btArrayEmpty: Result := 'array_empty';
    btArraySingle: Result := 'array_single';
    btArrayBoundary: Result := 'array_boundary';
    btArrayOversized: Result := 'array_oversized';
    btPointerNil: Result := 'pointer_nil';
  else
    Result := 'unknown';
  end;
end;

function TaskPriorityToStr(P: TTaskPriority): string;
begin
  case P of
    tpCritical: Result := 'critical';
    tpHigh: Result := 'high';
    tpMedium: Result := 'medium';
    tpLow: Result := 'low';
  else
    Result := 'unknown';
  end;
end;

function TaskCategoryToStr(C: TTaskCategory): string;
begin
  case C of
    tcCoverage: Result := 'coverage';
    tcBoundary: Result := 'boundary';
    tcError: Result := 'error';
    tcCrypto: Result := 'crypto';
    tcThread: Result := 'thread';
    tcResource: Result := 'resource';
    tcBackend: Result := 'backend';
  else
    Result := 'unknown';
  end;
end;

function ParamCategoryToStr(C: TParamCategory): string;
begin
  case C of
    pcNumeric: Result := 'numeric';
    pcString: Result := 'string';
    pcArray: Result := 'array';
    pcPointer: Result := 'pointer';
    pcRecord: Result := 'record';
    pcClass: Result := 'class';
    pcInterface: Result := 'interface';
    pcOther: Result := 'other';
  else
    Result := 'unknown';
  end;
end;

end.
