# fafafa.ssl 代码风格指南

## 概述

本文档定义了 fafafa.ssl 项目的代码风格和最佳实践。

## 通用规则

### 文件编码
- 使用 UTF-8 编码
- Windows代码页指令：`{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}`

### 缩进和空格
- 使用 **2个空格** 缩进，不使用Tab
- 行尾不留空格
- 文件末尾保留一个空行

### 行长度
- 最大行长度：120个字符
- 超过时适当换行

## Pascal代码风格

### 命名约定

#### 类型名称
```pascal
// 类：T前缀，帕斯卡命名法
TMyClassName = class
THTTPSClient = class

// 接口：I前缀，帕斯卡命名法
IConnection = interface
ISSLContext = interface

// 记录：T前缀，帕斯卡命名法
TOptions = record
TCertificateInfo = record

// 枚举：T前缀，帕斯卡命名法
TLogLevel = (llDebug, llInfo, llError)
THTTPMethod = (httpGET, httpPOST)

// 枚举值：类型前缀缩写
TLogLevel = (
  llDebug,    // ll = LogLevel
  llInfo,
  llError
)
```

#### 变量名称
```pascal
// 局部变量：L前缀，帕斯卡命名法
var
  LConnection: ISSLConnection;
  LBuffer: array of Byte;
  LCount: Integer;

// 参数：A前缀，帕斯卡命名法
procedure DoSomething(AValue: Integer; const AName: string);

// 私有字段：F前缀，帕斯卡命名法
private
  FConnection: ISSLConnection;
  FTimeout: Integer;

// 全局变量：G前缀，帕斯卡命名法
var
  GLogger: ILogger;
  GConfig: TConfiguration;
```

#### 常量名称
```pascal
// 全大写，下划线分隔
const
  DEFAULT_TIMEOUT = 30000;
  MAX_BUFFER_SIZE = 16384;
  USER_AGENT = 'fafafa.ssl/1.0';
```

### 代码结构

#### 单元结构
```pascal
unit unit.name;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ 单元说明
  
  功能描述
}

interface

uses
  SysUtils, Classes,  // 标准单元
  fafafa.ssl.base;    // 项目单元

type
  // 类型定义

const
  // 公开常量

// 公开函数/过程

implementation

uses
  // 实现部分需要的单元

const
  // 私有常量

// 私有函数/过程

// 公开函数/过程实现

end.
```

#### 类定义
```pascal
type
  TMyClass = class(TInterfacedObject, IMyInterface)
  private
    FField1: Integer;
    FField2: string;
    
    procedure PrivateMethod;
  protected
    procedure ProtectedMethod; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure PublicMethod;
    property Field1: Integer read FField1 write FField1;
  end;
```

### 注释

#### 单元注释
```pascal
{ fafafa.ssl 简化HTTP客户端
  
  功能：
  - 一行代码实现HTTPS请求
  - 支持GET/POST/PUT/DELETE
  - 文件上传下载
  
  示例：
    LResponse := TSimpleHTTPSClient.Get('https://example.com');
}
```

#### 类注释
```pascal
{ 简化HTTPS客户端
  
  提供高层API，简化常见HTTPS操作
}
TSimpleHTTPSClient = class
```

#### 方法注释
```pascal
{ 发送GET请求
  
  @param AURL 目标URL
  @return 响应内容
  @raises EHTTPSClientException 请求失败时
}
class function Get(const AURL: string): string;
```

#### 行内注释
```pascal
LTimeout := 30000;  // 30秒超时
```

### 代码格式

#### 控制结构
```pascal
// if语句
if LCondition then
begin
  DoSomething;
  DoAnotherThing;
end
else
begin
  DoElse;
end;

// 单行if
if LCondition then
  DoSomething;

// case语句
case LValue of
  1: HandleOne;
  2: HandleTwo;
  3..10: HandleRange;
else
  HandleDefault;
end;

// for循环
for i := 0 to Count - 1 do
begin
  Process(i);
end;

// while循环
while LCondition do
begin
  Process;
end;

// try-finally
try
  DoSomething;
  DoMore;
finally
  Cleanup;
end;

// try-except
try
  RiskyOperation;
except
  on E: ESpecificException do
    HandleSpecific(E);
  on E: Exception do
    HandleGeneric(E);
end;
```

#### 方法定义
```pascal
// 简单方法
function Calculate(A, B: Integer): Integer;
begin
  Result := A + B;
end;

// 复杂方法
function ComplexCalculation(
  const AFirstParameter: string;
  ASecondParameter: Integer;
  AThirdParameter: Boolean = False): TResult;
var
  LTemp: Integer;
begin
  // 实现
  Result := Default(TResult);
end;
```

## 最佳实践

### 内存管理

#### 使用接口自动管理
```pascal
// 推荐：使用接口
var
  LConnection: ISSLConnection;
begin
  LConnection := LContext.CreateConnection;
  // 自动释放
end;

// 避免：手动管理
var
  LConnection: TSSLConnection;
begin
  LConnection := TSSLConnection.Create;
  try
    // 使用
  finally
    LConnection.Free;  // 容易忘记
  end;
end;
```

#### 正确的资源清理
```pascal
procedure ProcessFile(const AFileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    // 使用流
  finally
    LStream.Free;
  end;
end;
```

### 错误处理

#### 明确的异常类型
```pascal
// 推荐
try
  DoRiskyOperation;
except
  on E: ESSLException do
    HandleSSLError(E);
  on E: EIOException do
    HandleIOError(E);
  on E: Exception do
    HandleGenericError(E);
end;

// 避免
try
  DoRiskyOperation;
except
  // 捕获所有异常但不处理
end;
```

#### 提供有用的错误信息
```pascal
// 推荐
if not FileExists(AFileName) then
  raise EFileNotFoundException.CreateFmt(
    '文件不存在: %s', [AFileName]);

// 避免
if not FileExists(AFileName) then
  raise Exception.Create('Error');
```

### 日志记录

```pascal
// 使用结构化日志
LLogger.Info('开始处理请求');
LLogger.Debug(Format('URL: %s, Method: %s', [LURL, LMethod]));

try
  ProcessRequest;
  LLogger.Info('请求处理成功');
except
  on E: Exception do
  begin
    LLogger.Error(Format('请求处理失败: %s', [E.Message]));
    raise;
  end;
end;
```

### 参数传递

```pascal
// 输入参数：const
procedure Process(const AData: string);

// 输出参数：out
procedure GetValues(out AValue1, AValue2: Integer);

// 输入输出参数：var
procedure Modify(var AData: TData);

// 大型结构：const
procedure HandleRecord(const ARec: TLargeRecord);
```

## 文档

### 代码注释
- 所有公开接口都应有注释
- 复杂算法添加说明
- 使用中文注释

### 文件头
```pascal
unit fafafa.ssl.example;

{$mode objfpc}{$H+}
{$IFDEF WINDOWS}{$CODEPAGE UTF8}{$ENDIF}

{ fafafa.ssl 示例单元
  
  作者：Your Name
  日期：2025-11-05
  
  功能：
  - 功能1
  - 功能2
  
  使用示例：
    LExample := TExample.Create;
    try
      LExample.DoSomething;
    finally
      LExample.Free;
    end;
}
```

## Git提交

### 提交信息格式
```
类型: 简短描述（50字符以内）

详细描述（可选）
- 变更点1
- 变更点2

相关Issue: #123
```

### 提交类型
- `feat`: 新功能
- `fix`: 修复Bug
- `docs`: 文档更新
- `style`: 代码格式（不影响功能）
- `refactor`: 重构
- `perf`: 性能优化
- `test`: 测试相关
- `chore`: 构建/工具相关

### 示例
```
feat: 添加简化HTTP客户端

- 实现TSimpleHTTPSClient类
- 支持GET/POST方法
- 添加文件下载功能

相关Issue: #42
```

## 工具

### EditorConfig
项目包含 `.editorconfig` 文件，确保编辑器配置正确。

### 代码检查
使用 Free Pascal 内置检查：
```bash
fpc -vh -vw your_file.pas
```

## 参考

- [Object Pascal Style Guide](http://edn.embarcadero.com/article/10280)
- [Free Pascal Documentation](https://www.freepascal.org/docs.html)
- [Clean Code principles](https://github.com/ryanmcdermott/clean-code-javascript)

---

遵循这些规范，保持代码清晰、一致、易维护！


