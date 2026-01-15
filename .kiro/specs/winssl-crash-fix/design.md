# Design Document: WinSSL Library Crash Fix

## Overview

本设计文档描述修复 `fafafa.ssl.winssl.lib.pas` 在 Windows 环境下启动时崩溃 (STATUS_ENTRYPOINT_NOT_FOUND) 的解决方案。

根据分析，崩溃发生在程序导入 WinSSL 库模块时，在 initialization 段执行之前。问题的根本原因是 Pascal 编译器在链接时需要解析所有 implementation 段中引用的类型，即使这些类型只在方法体内使用。

## Architecture

### 问题分析

```
┌─────────────────────────────────────────────────────────────────┐
│                    程序启动流程                                   │
├─────────────────────────────────────────────────────────────────┤
│  1. 加载 EXE                                                     │
│  2. 解析 DLL 依赖 (Windows Loader)                               │
│  3. 解析单元依赖 (FPC Runtime)                                   │
│     ├── fafafa.ssl.winssl.lib.pas                               │
│     │   └── implementation uses:                                 │
│     │       ├── fafafa.ssl.winssl.context    ← 可能的问题点      │
│     │       ├── fafafa.ssl.winssl.certificate ← 可能的问题点     │
│     │       ├── fafafa.ssl.winssl.certstore  ← 可能的问题点      │
│     │       └── fafafa.ssl.factory           ← 可能的问题点      │
│  4. 执行 initialization 段                                       │
│  5. 执行 main 程序                                               │
└─────────────────────────────────────────────────────────────────┘
```

### 崩溃原因假设

STATUS_ENTRYPOINT_NOT_FOUND 通常表示：
1. 某个 DLL 导出函数找不到
2. 某个单元引用了不存在的符号
3. 循环依赖导致初始化顺序问题

在 WinSSL 场景中，可能的原因：
1. `fafafa.ssl.factory` 模块间接依赖了 OpenSSL 模块
2. `TWinSSLContext` 等类的构造函数引用了未初始化的资源
3. Windows API 函数在某些 Windows 版本上不可用

### 解决方案架构

```
┌─────────────────────────────────────────────────────────────────┐
│                    修复后的架构                                   │
├─────────────────────────────────────────────────────────────────┤
│  fafafa.ssl.winssl.lib.pas (修复版)                              │
│  ├── interface section:                                          │
│  │   └── 只引用基础类型 (fafafa.ssl.base, winssl.base, etc.)     │
│  │                                                               │
│  ├── implementation section:                                     │
│  │   └── 延迟引用实现单元 (使用条件编译或动态加载)                │
│  │                                                               │
│  └── initialization section:                                     │
│      └── 安全注册 (检查 factory 可用性)                          │
└─────────────────────────────────────────────────────────────────┘
```

## Components and Interfaces

### 1. TWinSSLLibrary 类 (修复版)

```pascal
type
  TWinSSLLibrary = class(TInterfacedObject, ISSLLibrary)
  private
    // 延迟加载标志
    FContextLoaded: Boolean;
    FCertificateLoaded: Boolean;
    FCertStoreLoaded: Boolean;
    
    // 延迟加载方法
    procedure EnsureContextLoaded;
    procedure EnsureCertificateLoaded;
    procedure EnsureCertStoreLoaded;
    
  public
    // ISSLLibrary 工厂方法 (使用延迟加载)
    function CreateContext(AType: TSSLContextType): ISSLContext;
    function CreateCertificate: ISSLCertificate;
    function CreateCertificateStore: ISSLCertificateStore;
  end;
```

### 2. 安全注册机制

```pascal
procedure RegisterWinSSLBackend;
begin
  {$IFDEF WINDOWS}
  try
    // 检查 factory 模块是否可用
    if not Assigned(TSSLFactory) then
      Exit;
      
    TSSLFactory.RegisterLibrary(sslWinSSL, TWinSSLLibrary,
      'Windows Schannel (Native SSL/TLS)', 200);
  except
    // 注册失败时静默处理，不影响程序启动
    on E: Exception do
      ; // 可选：记录日志
  end;
  {$ENDIF}
end;
```

### 3. 延迟加载实现

```pascal
// 方案 A: 使用 GetClass 动态获取类
function TWinSSLLibrary.CreateContext(AType: TSSLContextType): ISSLContext;
var
  ContextClass: TClass;
begin
  ContextClass := GetClass('TWinSSLContext');
  if ContextClass <> nil then
    Result := TInterfacedObject(ContextClass.Create) as ISSLContext
  else
    Result := nil;
end;

// 方案 B: 使用条件编译和单独的实现单元
// fafafa.ssl.winssl.lib.impl.pas - 包含实际实现
// fafafa.ssl.winssl.lib.pas - 只包含接口和延迟加载逻辑
```

## Data Models

### 错误状态

```pascal
type
  TWinSSLLoadState = (
    wlsNotLoaded,      // 未加载
    wlsLoading,        // 正在加载
    wlsLoaded,         // 已加载
    wlsLoadFailed      // 加载失败
  );
  
  TWinSSLLoadError = record
    State: TWinSSLLoadState;
    ErrorCode: Integer;
    ErrorMessage: string;
  end;
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system-essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Module Load Safety
*For any* Windows program that imports `fafafa.ssl.winssl.lib.pas`, the module SHALL load without STATUS_ENTRYPOINT_NOT_FOUND error, regardless of whether OpenSSL DLLs are present.
**Validates: Requirements 2.1, 5.6**

### Property 2: Lazy Loading Correctness
*For any* call to CreateContext, CreateCertificate, or CreateCertificateStore, the corresponding implementation unit SHALL only be loaded at the time of the call, not at module initialization.
**Validates: Requirements 3.1, 3.2, 3.3**

### Property 3: Factory Registration Safety
*For any* initialization of the WinSSL library, if the factory module is unavailable or registration fails, the library SHALL continue to function (returning nil for factory-dependent operations) without crashing.
**Validates: Requirements 2.2, 4.1, 4.2**

### Property 4: Backward Compatibility
*For any* existing code that uses the WinSSL library API, the fixed version SHALL produce the same results as the original version when the original version works correctly.
**Validates: Requirements 2.4**

## Error Handling

### 错误处理策略

| 错误场景 | 处理方式 |
|----------|----------|
| Factory 不可用 | 静默跳过注册，CreateXxx 返回 nil |
| 实现单元加载失败 | 设置错误状态，返回 nil |
| Windows API 不可用 | 检测 Windows 版本，返回适当错误 |
| 循环依赖 | 通过延迟加载打破循环 |

### 错误码定义

```pascal
const
  WINSSL_ERR_FACTORY_UNAVAILABLE = -1001;
  WINSSL_ERR_CONTEXT_LOAD_FAILED = -1002;
  WINSSL_ERR_CERT_LOAD_FAILED = -1003;
  WINSSL_ERR_STORE_LOAD_FAILED = -1004;
```

## Testing Strategy

### 单元测试

1. **最小导入测试**: 只导入 `fafafa.ssl.winssl.lib.pas`，验证不崩溃
2. **工厂注册测试**: 验证 WinSSL 后端成功注册到工厂
3. **CreateContext 测试**: 验证上下文创建功能
4. **CreateCertificate 测试**: 验证证书创建功能
5. **CreateCertificateStore 测试**: 验证证书存储创建功能

### 属性测试

1. **模块加载安全性**: 在不同 Windows 版本上测试模块加载
2. **延迟加载正确性**: 验证实现单元只在需要时加载
3. **工厂注册安全性**: 模拟工厂不可用场景

### 测试框架

使用 FPCUnit 或简单的断言测试：

```pascal
program test_winssl_load;
uses
  fafafa.ssl.winssl.lib;  // 只导入，不使用
begin
  WriteLn('SUCCESS: Module loaded without crash');
end.
```

## Implementation Notes

### 修复步骤

1. **诊断阶段**
   - 创建最小测试用例隔离问题
   - 确定具体是哪个 uses 导致崩溃

2. **修复阶段**
   - 重构 implementation uses 为延迟加载
   - 添加安全的工厂注册机制
   - 添加错误处理

3. **验证阶段**
   - 运行所有测试用例
   - 在不同 Windows 版本上测试
   - 验证与 OpenSSL 后端的兼容性

### 兼容性考虑

- 保持现有 API 不变
- 保持与 OpenSSL 后端的行为一致
- 支持 Windows 7/8/10/11

