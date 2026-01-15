# fafafa.ssl 返回类型约定

## 概述

本文档定义了 fafafa.ssl 库的返回类型约定，确保 API 的一致性和可预测性。

## 返回类型模式

### 1. Boolean 返回（接口方法）

用于核心接口方法，表示操作成功/失败：

```pascal
// ISSLCertificate
function LoadFromFile(const AFileName: string): Boolean;
function Verify(ACAStore: ISSLCertificateStore): Boolean;

// ISSLConnection
function Connect: Boolean;
function Shutdown: Boolean;
```

**约定**：
- `True` = 操作成功
- `False` = 操作失败（调用 `GetLastError` 获取详情）

### 2. Procedure（异常模式）

用于配置方法，失败时抛出异常：

```pascal
// ISSLContext
procedure LoadCertificate(const AFileName: string);
procedure LoadPrivateKey(const AFileName: string; const APassword: string = '');
procedure SetCipherList(const ACipherList: string);
```

**约定**：
- 成功时静默返回
- 失败时抛出 `ESSLException` 或其子类

### 3. Result 类型（Rust 风格）

用于需要详细错误信息的场景：

```pascal
// TSSLOperationResult - 无返回值操作
Result := TSSLOperationResult.Ok;
Result := TSSLOperationResult.Err(sslErrCertificate, 'Certificate expired');

// TSSLDataResult - 返回字节数据
Result := TSSLDataResult.Ok(LBytes);
Result := TSSLDataResult.Err(sslErrDecryptionFailed, 'Invalid key');

// TSSLStringResult - 返回字符串
Result := TSSLStringResult.Ok(LPEM);
Result := TSSLStringResult.Err(sslErrParseFailed, 'Invalid PEM format');
```

## 转换工具

由于核心接口已锁定（@locked 2025-12-24），使用 `fafafa.ssl.result.utils` 进行转换：

### Boolean -> Result

```pascal
uses fafafa.ssl.result.utils;

// 简单转换
LResult := ToResult(Cert.LoadFromFile('cert.pem'), 'LoadCertificate');

// 带异常捕获
LResult := TResultUtils.TryCatch(@LoadCertFunc, 'LoadCertificate');
```

### 链式操作

```pascal
LResult := ToResult(Cert.LoadFromFile('cert.pem'), 'Load')
  .AndThen(@ValidateCert, 'Validate')
  .AndThen(@StoreCert, 'Store')
  .WithContext('CertificateSetup');

if LResult.IsErr then
  WriteLn('Error: ', LResult.ErrorMessage);
```

### 组合多个操作

```pascal
// 全部成功才成功
LResult := TResultUtils.All([
  ToResult(LoadCert, 'LoadCert'),
  ToResult(LoadKey, 'LoadKey'),
  ToResult(Verify, 'Verify')
]);

// 任一成功即成功
LResult := TResultUtils.Any([
  ToResult(TryMethod1, 'Method1'),
  ToResult(TryMethod2, 'Method2')
]);
```

## 查询方法命名约定

| 前缀 | 返回类型 | 用途 |
|------|----------|------|
| `Is*` | Boolean | 状态查询 |
| `Has*` | Boolean | 存在性检查 |
| `Get*` | 具体类型 | 获取值（失败抛异常或返回默认值）|
| `Try*` | Boolean | 尝试操作（out 参数返回结果）|
| `Find*` | 接口/nil | 查找对象（未找到返回 nil）|

### 示例

```pascal
// Is* - 状态查询
function IsConnected: Boolean;
function IsExpired: Boolean;
function IsCA: Boolean;

// Has* - 存在性检查
function HasExtension(const AOID: string): Boolean;

// Get* - 获取值
function GetSubject: string;           // 失败抛异常
function GetFingerprint: string;       // 失败返回空字符串

// Try* - 尝试操作
function TryGetValue(out AValue: string): Boolean;

// Find* - 查找对象
function FindBySubject(const ASubject: string): ISSLCertificate;  // 未找到返回 nil
```

## 错误码定义

使用 `TSSLErrorCode` 枚举（定义在 `fafafa.ssl.base.pas`）：

| 错误码 | 含义 |
|--------|------|
| `sslErrNone` | 无错误 |
| `sslErrGeneral` | 一般错误 |
| `sslErrCertificate` | 证书错误 |
| `sslErrHandshake` | 握手错误 |
| `sslErrConnection` | 连接错误 |
| `sslErrProtocol` | 协议错误 |
| `sslErrConfiguration` | 配置错误 |
| `sslErrInvalidData` | 数据格式错误 |
| `sslErrDecryptionFailed` | 解密失败 |
| `sslErrEncryptionFailed` | 加密失败 |

## 新代码指南

### 推荐做法

1. **新的内部函数**：优先使用 Result 类型
2. **公共 API**：遵循现有接口约定（Boolean/Procedure）
3. **错误处理**：使用 `TResultUtils` 转换和组合

### 示例：新功能实现

```pascal
function ProcessCertificateChain(const ACerts: array of string): TSSLOperationResult;
var
  I: Integer;
  LCert: ISSLCertificate;
begin
  for I := Low(ACerts) to High(ACerts) do
  begin
    LCert := TSSLFactory.CreateCertificate;

    // 使用 Result 工具转换 Boolean 返回值
    Result := ToResult(LCert.LoadFromFile(ACerts[I]),
      Format('LoadCert[%d]', [I]));

    if Result.IsErr then
      Exit;

    Result := ToResult(LCert.Verify(FCAStore),
      Format('VerifyCert[%d]', [I]));

    if Result.IsErr then
      Exit;
  end;

  Result := Ok;
end;
```

## 版本历史

- 2025-12-27: 初始版本，定义返回类型约定
