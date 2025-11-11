# TSimpleHTTPSClient API参考

**单元**: `fafafa.ssl.http.simple`

## 概述

`TSimpleHTTPSClient` 是最简单的HTTPS客户端API，让你用一行代码完成HTTPS请求。

**代码量减少**: 95% （从~20行减少到1行）

---

## 快速示例

```pascal
uses
  fafafa.ssl.http.simple;

var
  LResponse: string;
begin
  // GET请求
  LResponse := TSimpleHTTPSClient.Get('https://api.example.com/data');
  
  // POST请求
  LResponse := TSimpleHTTPSClient.Post(
    'https://api.example.com/data',
    '{"name":"test"}'
  );
end;
```

---

## 类定义

```pascal
TSimpleHTTPSClient = class
public
  // 默认选项
  class function DefaultOptions: THTTPSOptions;
  
  // 简化方法
  class function Get(const AURL: string): string;
  class function Post(const AURL: string; const AData: string): string;
  
  // 完整方法
  class function GetEx(const AURL: string; 
    const AOptions: THTTPSOptions): THTTPResponse;
  class function PostEx(const AURL, AData: string;
    const AOptions: THTTPSOptions): THTTPResponse;
  class function PutEx(const AURL, AData: string;
    const AOptions: THTTPSOptions): THTTPResponse;
  class function DeleteEx(const AURL: string;
    const AOptions: THTTPSOptions): THTTPResponse;
  
  // 文件操作
  class function Download(const AURL, AFilePath: string;
    AProgressCallback: TProgressCallback = nil): Boolean;
  class function Upload(const AURL, AFilePath: string): Boolean;
end;
```

---

## 简化方法

### Get

**功能**: 发送GET请求（最简单）

**签名**:
```pascal
class function Get(const AURL: string): string;
```

**参数**:
- `AURL`: 目标URL（必须以`https://`开头）

**返回值**: 响应体字符串

**异常**: 
- `EHTTPSClientException` - HTTP错误
- `ESSLException` - SSL错误

**示例**:
```pascal
var
  LResponse: string;
begin
  LResponse := TSimpleHTTPSClient.Get('https://api.github.com/users/octocat');
  WriteLn(LResponse);
end;
```

---

### Post

**功能**: 发送POST请求（最简单）

**签名**:
```pascal
class function Post(const AURL: string; const AData: string): string;
```

**参数**:
- `AURL`: 目标URL
- `AData`: POST数据（JSON、表单数据等）

**返回值**: 响应体字符串

**示例**:
```pascal
var
  LJSON, LResponse: string;
begin
  LJSON := '{"name":"John","age":30}';
  LResponse := TSimpleHTTPSClient.Post('https://httpbin.org/post', LJSON);
  WriteLn(LResponse);
end;
```

---

## 完整方法

### GetEx

**功能**: 发送GET请求（自定义选项）

**签名**:
```pascal
class function GetEx(const AURL: string; 
  const AOptions: THTTPSOptions): THTTPResponse;
```

**参数**:
- `AURL`: 目标URL
- `AOptions`: 请求选项

**返回值**: `THTTPResponse` 记录

**示例**:
```pascal
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  LOptions.Timeout := 10000;  // 10秒
  LOptions.Headers.Add('Accept: application/json');
  LOptions.Headers.Add('Authorization: Bearer token123');
  
  LResponse := TSimpleHTTPSClient.GetEx('https://api.example.com/data', LOptions);
  
  if LResponse.Success then
    WriteLn('状态码: ', LResponse.StatusCode)
  else
    WriteLn('错误: ', LResponse.ErrorMessage);
    
  // 清理
  LOptions.Headers.Free;
  LResponse.Headers.Free;
end;
```

---

### PostEx

**功能**: 发送POST请求（自定义选项）

**签名**:
```pascal
class function PostEx(const AURL, AData: string;
  const AOptions: THTTPSOptions): THTTPResponse;
```

**参数**:
- `AURL`: 目标URL
- `AData`: POST数据
- `AOptions`: 请求选项

**示例**:
```pascal
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  LOptions.Headers.Add('Content-Type: application/json');
  
  LResponse := TSimpleHTTPSClient.PostEx(
    'https://api.example.com/users',
    '{"name":"Alice"}',
    LOptions
  );
  
  WriteLn('状态码: ', LResponse.StatusCode);
  WriteLn('响应: ', LResponse.Body);
  
  LOptions.Headers.Free;
  LResponse.Headers.Free;
end;
```

---

## 文件操作

### Download

**功能**: 下载文件

**签名**:
```pascal
class function Download(const AURL, AFilePath: string;
  AProgressCallback: TProgressCallback = nil): Boolean;
```

**参数**:
- `AURL`: 文件URL
- `AFilePath`: 本地保存路径
- `AProgressCallback`: 进度回调（可选）

**返回值**: 成功返回`True`

**示例**:
```pascal
begin
  if TSimpleHTTPSClient.Download(
    'https://example.com/file.zip',
    'local_file.zip'
  ) then
    WriteLn('下载成功')
  else
    WriteLn('下载失败');
end;
```

---

### Upload

**功能**: 上传文件

**签名**:
```pascal
class function Upload(const AURL, AFilePath: string): Boolean;
```

**参数**:
- `AURL`: 上传URL
- `AFilePath`: 本地文件路径

**返回值**: 成功返回`True`

**示例**:
```pascal
begin
  if TSimpleHTTPSClient.Upload(
    'https://example.com/upload',
    'my_file.txt'
  ) then
    WriteLn('上传成功')
  else
    WriteLn('上传失败');
end;
```

---

## 数据类型

### THTTPSOptions

**定义**:
```pascal
THTTPSOptions = record
  Timeout: Integer;           // 超时（毫秒）
  FollowRedirects: Boolean;   // 跟随重定向
  MaxRedirects: Integer;      // 最大重定向次数
  VerifyPeer: Boolean;        // 验证服务器证书
  ClientCert: string;         // 客户端证书（PEM）
  ClientKey: string;          // 客户端私钥（PEM）
  CAFile: string;             // CA证书文件
  UserAgent: string;          // User-Agent
  Headers: TStringList;       // 自定义请求头
end;
```

**默认值**:
```pascal
LOptions := TSimpleHTTPSClient.DefaultOptions;
// Timeout = 30000 (30秒)
// FollowRedirects = True
// MaxRedirects = 5
// VerifyPeer = True
// UserAgent = 'fafafa.ssl-simple/1.0'
```

---

### THTTPResponse

**定义**:
```pascal
THTTPResponse = record
  StatusCode: Integer;        // HTTP状态码（200, 404等）
  StatusText: string;         // 状态文本（'OK', 'Not Found'）
  Headers: TStringList;       // 响应头
  Body: string;               // 响应体
  ContentLength: Int64;       // 内容长度
  Success: Boolean;           // 是否成功（200-299）
  ErrorMessage: string;       // 错误信息
end;
```

**示例**:
```pascal
var
  LResponse: THTTPResponse;
  i: Integer;
begin
  LResponse := TSimpleHTTPSClient.GetEx(LURL, LOptions);
  
  WriteLn('状态码: ', LResponse.StatusCode);
  WriteLn('状态文本: ', LResponse.StatusText);
  WriteLn('响应头:');
  for i := 0 to LResponse.Headers.Count - 1 do
    WriteLn('  ', LResponse.Headers[i]);
  WriteLn('响应体长度: ', Length(LResponse.Body));
  
  if LResponse.Success then
    WriteLn('请求成功')
  else
    WriteLn('请求失败: ', LResponse.ErrorMessage);
    
  // 记得清理
  LResponse.Headers.Free;
end;
```

---

## 异常处理

### EHTTPSClientException

**基类**: `Exception`

**属性**:
- `StatusCode: Integer` - HTTP状态码

**示例**:
```pascal
try
  LResponse := TSimpleHTTPSClient.Get('https://example.com/notfound');
except
  on E: EHTTPSClientException do
  begin
    WriteLn('HTTP错误: ', E.Message);
    WriteLn('状态码: ', E.StatusCode);
  end;
  on E: Exception do
    WriteLn('其他错误: ', E.Message);
end;
```

---

## 使用场景

### 场景1：API调用

```pascal
// 获取用户信息
function GetUserInfo(const AUserID: string): TJSONObject;
var
  LURL, LResponse: string;
begin
  LURL := Format('https://api.example.com/users/%s', [AUserID]);
  LResponse := TSimpleHTTPSClient.Get(LURL);
  Result := TJSONObject(GetJSON(LResponse));
end;
```

### 场景2：表单提交

```pascal
// 登录
function Login(const AUsername, APassword: string): Boolean;
var
  LData, LResponse: string;
begin
  LData := Format('username=%s&password=%s', [AUsername, APassword]);
  LResponse := TSimpleHTTPSClient.Post('https://example.com/login', LData);
  Result := Pos('success', LowerCase(LResponse)) > 0;
end;
```

### 场景3：文件下载

```pascal
// 下载图片
procedure DownloadImage(const AImageURL, ADestPath: string);
begin
  if not TSimpleHTTPSClient.Download(AImageURL, ADestPath) then
    raise Exception.Create('图片下载失败');
end;
```

### 场景4：认证请求

```pascal
// 带Token的API请求
function AuthenticatedRequest(const AURL, AToken: string): string;
var
  LOptions: THTTPSOptions;
  LResponse: THTTPResponse;
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    LOptions.Headers.Add(Format('Authorization: Bearer %s', [AToken]));
    LResponse := TSimpleHTTPSClient.GetEx(AURL, LOptions);
    
    if LResponse.Success then
      Result := LResponse.Body
    else
      raise Exception.Create(LResponse.ErrorMessage);
  finally
    LOptions.Headers.Free;
    LResponse.Headers.Free;
  end;
end;
```

---

## 性能提示

### 1. 复用Options

```pascal
// 好：复用Options对象
var
  LOptions: THTTPSOptions;
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  try
    // 多次请求
    LResponse1 := TSimpleHTTPSClient.GetEx(LURL1, LOptions);
    LResponse2 := TSimpleHTTPSClient.GetEx(LURL2, LOptions);
  finally
    LOptions.Headers.Free;
  end;
end;

// 差：每次都创建
for i := 1 to 100 do
begin
  LOptions := TSimpleHTTPSClient.DefaultOptions;
  // ... 使用
  LOptions.Headers.Free;  // 频繁创建/销毁
end;
```

### 2. 合适的超时值

```pascal
// 快速API：短超时
LOptions.Timeout := 5000;  // 5秒

// 大文件下载：长超时
LOptions.Timeout := 60000;  // 60秒

// 默认值
LOptions.Timeout := 30000;  // 30秒
```

---

## 注意事项

### 1. 内存管理

⚠️ **重要**: `THTTPResponse.Headers` 需要手动释放

```pascal
var
  LResponse: THTTPResponse;
begin
  LResponse := TSimpleHTTPSClient.GetEx(LURL, LOptions);
  try
    // 使用响应
  finally
    LResponse.Headers.Free;  // 必须释放
  end;
end;
```

### 2. 线程安全

✅ **线程安全**: 所有方法都是类方法，可以多线程调用

```pascal
// 可以在多个线程中调用
TThread.Execute(
  procedure
  begin
    LResponse := TSimpleHTTPSClient.Get('https://example.com');
  end
);
```

### 3. 证书验证

⚠️ **生产环境必须启用**:

```pascal
LOptions.VerifyPeer := True;  // 生产环境

LOptions.VerifyPeer := False;  // 仅开发测试
```

---

## 相关API

- [TCertificateManager](CertificateManager.md) - 证书管理
- [TLogger](Logger.md) - 日志记录
- [ISSLContext](ISSLContext.md) - 底层SSL上下文

---

## 示例程序

完整示例参考：
- `examples/simple_https_demo.pas` - 简化API演示
- `examples/production/https_client_simple.pas` - 生产级示例

---

**返回**: [API概述](概述.md) | **上一级**: [文档首页](../快速入门.md)


