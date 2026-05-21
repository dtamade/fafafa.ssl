# fafafa.ssl 错误处理最佳实践指南

**版本**: v1.5.0
**最后更新**: 2026-05-21
**作者**: fafafa.ssl 团队
**定位**: 当前 v1.5.0 错误处理模式与示例指南（具体 public API 以 API_REFERENCE 为准）

## 📋 目录

1. [概述](#概述)
2. [三种错误处理模式](#三种错误处理模式)
3. [何时使用哪种模式](#何时使用哪种模式)
4. [Result 类型详解](#result-类型详解)
5. [Try* 方法详解](#try-方法详解)
6. [异常处理详解](#异常处理详解)
7. [实战示例](#实战示例)
8. [性能考虑](#性能考虑)
9. [常见陷阱](#常见陷阱)

---

## 概述

fafafa.ssl 提供了三种错误处理模式，受 Rust 编程语言的启发，旨在提供更灵活、更安全的错误处理方式。

### 设计理念

- **显式优于隐式** - 错误应该明确处理，而不是被忽略
- **类型安全** - 编译时检查错误处理
- **性能可选** - 根据场景选择合适的性能特性
- **向后兼容** - 保留传统异常模式

---

## 三种错误处理模式

### 1. Result 模式 (推荐新代码使用)

**特点**:
- 🎯 函数式编程风格
- 🔍 明确的错误类型
- 🚀 零性能开销（无异常）
- 📦 可组合的错误处理

**示例**:
```pascal
var
  LResult: TSSLDataResult;
begin
  LResult := TCryptoUtils.SHA256(LData);
  if LResult.IsOk then
    ProcessData(LResult.Data)  // 使用数据
  else
    WriteLn('Error: ', LResult.ErrorMessage);  // 处理错误
end;
```

### 2. Try* 模式 (推荐库集成使用)

**特点**:
- ✅ 返回 Boolean 表示成功/失败
- 🚫 不抛异常
- ⚡ 高性能（适合循环/关键路径）
- 🧹 自动清理输出参数

**示例**:
```pascal
var
  LCert, LKey: string;
begin
  if TCertificateUtils.TryGenerateSelfSignedSimple(
    'example.com', 'Example Corp', 365, LCert, LKey
  ) then
    WriteLn('✓ Certificate generated')
  else
    WriteLn('✗ Failed to generate certificate');
end;
```

### 3. 异常模式 (传统模式)

**特点**:
- 📜 Pascal/Delphi 传统风格
- 💥 失败时抛出异常
- 🔧 详细的异常信息
- 🔙 向后兼容

**示例**:
```pascal
try
  LCert := TCertificateUtils.GenerateSelfSignedSimple(
    'example.com', 'Example Corp', 365, LCert, LKey
  );
  WriteLn('✓ Certificate generated');
except
  on E: ESSLCertError do
    WriteLn('Certificate error: ', E.Message);
  on E: ESSLException do
    WriteLn('SSL error: ', E.Message);
end;
```

---

## 何时使用哪种模式

### 使用 Result 模式的场景

✅ **适合**:
- 新编写的应用代码
- 需要函数式编程风格
- 希望编译时强制错误检查
- 需要链式操作（`IsOkAnd`, `Inspect`）
- 追求零异常开销

❌ **不适合**:
- 需要向后兼容的库代码
- 团队不熟悉函数式编程
- 需要与 Delphi/FPC 传统代码集成

**示例场景**:
```pascal
// 密码学操作链 - Result 模式很适合
function ProcessSecureData(const AInput: TBytes): TSSLDataResult;
var
  LHashResult: TSSLDataResult;
  LEncResult: TSSLDataResult;
begin
  // 1. 哈希
  LHashResult := TCryptoUtils.SHA256(AInput);
  if not LHashResult.IsOk then
    Exit(LHashResult);  // 直接返回错误

  // 2. 加密
  LEncResult := TCryptoUtils.AES_GCM_Encrypt(
    LHashResult.Data, LKey, LIV
  );
  Result := LEncResult;  // 返回最终结果
end;
```

### 使用 Try* 模式的场景

✅ **适合**:
- 性能关键代码（循环、实时处理）
- 库集成（不希望异常跨越边界）
- 批量操作（需要继续处理其他项）
- 可选操作（失败不致命）

❌ **不适合**:
- 需要详细错误信息的场景
- 复杂的错误恢复逻辑
- 嵌套深的调用链（难以传递错误）

**示例场景**:
```pascal
// 批量证书验证 - Try* 模式很适合
procedure ValidateCertificates(const ACerts: TStringList);
var
  I: Integer;
  LInfo: TCertInfo;
begin
  for I := 0 to ACerts.Count - 1 do
  begin
    // 失败不影响其他证书的处理
    if TCertificateUtils.TryGetInfo(ACerts[I], LInfo) then
      WriteLn('✓ Cert ', I, ': ', LInfo.Subject)
    else
      WriteLn('✗ Cert ', I, ': Invalid');

    LInfo.SubjectAltNames.Free;
  end;
end;
```

### 使用异常模式的场景

✅ **适合**:
- 传统 Pascal/Delphi 代码库
- 错误是例外情况（不是常态）
- 需要详细的调用栈信息
- 错误需要向上冒泡多层
- 与现有异常处理代码集成

❌ **不适合**:
- 性能关键路径
- 错误是常见情况（如验证）
- 库边界（C API 集成等）

**示例场景**:
```pascal
// 应用启动配置 - 异常模式很适合
procedure LoadApplicationConfig;
begin
  try
    // 配置加载失败应该终止应用
    LoadSSLCertificate('server.crt');
    LoadSSLPrivateKey('server.key');
    StartHTTPSServer;
  except
    on E: Exception do
    begin
      LogError('Failed to start application: ' + E.Message);
      Halt(1);  // 终止应用
    end;
  end;
end;
```

---

## Result 类型详解

### 可用的 Result 类型

fafafa.ssl 提供了三种 Result 类型：

```pascal
// 1. 通用操作结果（无数据返回）
TSSLOperationResult = record
  Success: Boolean;
  ErrorCode: TSSLErrorCode;
  ErrorMessage: string;
end;

// 2. 字节数据结果
TSSLDataResult = record
  Success: Boolean;
  Data: TBytes;
  ErrorCode: TSSLErrorCode;
  ErrorMessage: string;
end;

// 3. 字符串结果
TSSLStringResult = record
  Success: Boolean;
  Value: string;
  ErrorCode: TSSLErrorCode;
  ErrorMessage: string;
end;
```

### Result 类型方法

所有 Result 类型都支持以下方法：

#### 基础方法

```pascal
// 检查状态
function IsOk: Boolean;      // 成功？
function IsErr: Boolean;     // 失败？

// 获取值（失败时抛异常）
function Unwrap: T;          // 获取值或抛出异常

// 获取值（提供默认值）
function UnwrapOr(const ADefault: T): T;

// 获取值（自定义错误消息）
function Expect(const AMsg: string): T;

// 获取错误码（成功时抛异常）
function UnwrapErr: TSSLErrorCode;
```

#### 高级方法

```pascal
// 条件检查（需要回调对象）
function IsOkAnd(APredicate: TPredicateT): Boolean;

// 副作用操作（不消费 Result）
function Inspect(ACallback: TProcedureOfConstT): TSSLDataResult;
```

### 创建 Result 值

```pascal
// 成功结果
LResult := TSSLDataResult.Ok(LMyData);
LResult := TSSLOperationResult.Ok;

// 失败结果
LResult := TSSLDataResult.Err(sslErrInvalidParam, 'Invalid key size');
LResult := TSSLOperationResult.Err(sslErrConnection, 'Connection failed');
```

### Result 使用示例

#### 示例 1: 基本用法

```pascal
procedure ProcessData;
var
  LResult: TSSLDataResult;
begin
  LResult := TCryptoUtils.SHA256(LInput);

  // 方式 1: 显式检查
  if LResult.IsOk then
    WriteLn('Hash: ', BytesToHex(LResult.Data));

  // 方式 2: Unwrap（失败会抛异常）
  try
    LHash := LResult.Unwrap;  // 获取数据或抛异常
  except
    on E: ESSLException do
      WriteLn('Hash failed: ', E.Message);
  end;

  // 方式 3: UnwrapOr（提供默认值）
  LHash := LResult.UnwrapOr(EmptyByteArray);
end;
```

#### 示例 2: 使用 IsOkAnd

```pascal
type
  TDataValidator = class
    function IsValidLength(const AData: TBytes): Boolean;
  end;

procedure ValidateAndProcess;
var
  LResult: TSSLDataResult;
  LValidator: TDataValidator;
begin
  LValidator := TDataValidator.Create;
  try
    LResult := TCryptoUtils.AES_CBC_Encrypt(LData, LKey, LIV);

    // 检查是否成功且数据有效
    if LResult.IsOkAnd(@LValidator.IsValidLength) then
      WriteLn('✓ Encrypted and valid')
    else
      WriteLn('✗ Encryption failed or invalid length');
  finally
    LValidator.Free;
  end;
end;
```

#### 示例 3: 使用 Inspect

```pascal
type
  TDataLogger = class
    procedure LogData(const AData: TBytes);
  end;

procedure ProcessWithLogging;
var
  LResult: TSSLDataResult;
  LLogger: TDataLogger;
begin
  LLogger := TDataLogger.Create;
  try
    LResult := TCryptoUtils.SHA256(LData);

    // Inspect 不消费 Result，可以继续使用
    LResult := LResult.Inspect(@LLogger.LogData);

    if LResult.IsOk then
      SendToServer(LResult.Data);
  finally
    LLogger.Free;
  end;
end;
```

---

## Try* 方法详解

### Try* 方法的特点

1. **命名规范**: 以 `Try` 开头，如 `TryGenerateSelfSigned`
2. **返回值**: 总是返回 `Boolean` (成功 = True, 失败 = False)
3. **输出参数**: 使用 `out` 参数返回实际数据
4. **错误处理**: 捕获所有异常，不会向调用者抛出
5. **清理保证**: 失败时自动清理所有输出参数

### 可用的 Try* 方法

#### crypto.utils 模块

```pascal
// AES 加密
class function TryAES_CBC_Encrypt(const AData, AKey, AIV: TBytes; out AResult: TBytes): Boolean;
class function TryAES_CBC_Decrypt(const AData, AKey, AIV: TBytes; out AResult: TBytes): Boolean;
class function TryAES_GCM_Encrypt(const AData, AKey, AIV: TBytes; out AResult: TBytes): Boolean;
class function TryAES_GCM_Decrypt(const AData, AKey, AIV, ATag: TBytes; out AResult: TBytes): Boolean;

// 哈希函数
class function TrySHA256(const AData: TBytes; out AResult: TBytes): Boolean;
class function TrySHA384(const AData: TBytes; out AResult: TBytes): Boolean;
class function TrySHA512(const AData: TBytes; out AResult: TBytes): Boolean;
```

#### cert.utils 模块

```pascal
// 证书生成
class function TryGenerateSelfSigned(const AOptions: TCertGenOptions;
  out ACertPEM, AKeyPEM: string): Boolean;
class function TryGenerateSelfSignedSimple(const ACommonName, AOrganization: string;
  AValidDays: Integer; out ACertPEM, AKeyPEM: string): Boolean;
class function TryGenerateSigned(const AOptions: TCertGenOptions;
  const ACA_CertPEM, ACA_KeyPEM: string; out ACertPEM, AKeyPEM: string): Boolean;

// 证书信息
class function TryGetInfo(const ACertPEM: string; out AInfo: TCertInfo): Boolean;
class function TryGetFingerprint(const ACertPEM: string; out AFingerprint: string): Boolean;

// 格式转换
class function TryPEMToDER(const APEM: string; out ADER: TBytes): Boolean;
class function TryDERToPEM(const ADER: TBytes; out APEM: string): Boolean;

// 文件操作
class function TryLoadFromFile(const AFileName: string; out ACertPEM: string): Boolean;

// 验证
class function TryVerifyChain(const ACertPEM: string; const ACAPath: string;
  out AIsValid: Boolean): Boolean;
```

#### context.builder 模块

```pascal
// 上下文构建
function TryBuildClient(out AContext: ISSLContext): TSSLOperationResult;
function TryBuildServer(out AContext: ISSLContext): TSSLOperationResult;
```

### Try* 方法使用示例

#### 示例 1: 错误恢复

```pascal
procedure TryWithFallback;
var
  LResult: TBytes;
begin
  // 尝试 AES-256，失败则使用 AES-128
  if not TCryptoUtils.TryAES_CBC_Encrypt(LData, LKey256, LIV, LResult) then
  begin
    WriteLn('AES-256 failed, trying AES-128...');
    if not TCryptoUtils.TryAES_CBC_Encrypt(LData, LKey128, LIV, LResult) then
    begin
      WriteLn('All encryption methods failed!');
      Exit;
    end;
  end;

  WriteLn('Encrypted successfully');
end;
```

#### 示例 2: 批量操作

```pascal
procedure ProcessCertificateBatch(const AFiles: TStringList);
var
  I: Integer;
  LCert: string;
  LInfo: TCertInfo;
  LSuccessCount: Integer;
begin
  LSuccessCount := 0;

  for I := 0 to AFiles.Count - 1 do
  begin
    // 加载证书（失败不影响其他证书）
    if not TCertificateUtils.TryLoadFromFile(AFiles[I], LCert) then
    begin
      WriteLn('Failed to load: ', AFiles[I]);
      Continue;
    end;

    // 提取信息
    if TCertificateUtils.TryGetInfo(LCert, LInfo) then
    begin
      try
        WriteLn('File: ', AFiles[I]);
        WriteLn('  Subject: ', LInfo.Subject);
        WriteLn('  Valid: ', DateTimeToStr(LInfo.NotBefore),
                ' to ', DateTimeToStr(LInfo.NotAfter));
        Inc(LSuccessCount);
      finally
        LInfo.SubjectAltNames.Free;
      end;
    end
    else
      WriteLn('Failed to parse: ', AFiles[I]);
  end;

  WriteLn(Format('Processed %d/%d certificates successfully',
    [LSuccessCount, AFiles.Count]));
end;
```

---

## 异常处理详解

### 异常层次结构

```pascal
ESSLException                    // 基类 - 所有 SSL 异常
├── ESSLInitError                // 初始化错误
├── ESSLCertError                // 证书错误
├── ESSLConnectionError          // 连接错误
├── ESSLHandshakeError           // 握手错误
├── ESSLInvalidArgument          // 无效参数
└── ESSLConfigError              // 配置错误
```

### 捕获异常的最佳实践

#### 示例 1: 特定异常优先

```pascal
try
  // SSL 操作
  LContext := TSSLContextBuilder.Create
    .WithCertificatePEM(LCert)
    .WithPrivateKeyPEM(LKey)
    .BuildServer;
except
  // 特定异常优先处理
  on E: ESSLCertError do
    WriteLn('Certificate error: ', E.Message);
  on E: ESSLInitError do
    WriteLn('Initialization error: ', E.Message);
  on E: ESSLException do
    WriteLn('SSL error: ', E.Message);
  // 通用异常最后
  on E: Exception do
    WriteLn('Unexpected error: ', E.Message);
end;
```

如果这里只是普通单向 TLS server，请显式加 `.WithVerifyNone`；如果要做 mTLS，请改成 `.WithMutualTLS(...)` 或等价 direct-context 配置。

#### 示例 2: 异常重抛

```pascal
function CreateSecureServer: ISSLContext;
begin
  try
    Result := TSSLContextBuilder.CreateWithSafeDefaults
      .WithCertificateFile('server.crt')
      .WithPrivateKeyFile('server.key')
      .BuildServer;
  except
    on E: ESSLException do
    begin
      LogError('Failed to create SSL server: ' + E.Message);
      raise;  // 重新抛出异常让上层处理
    end;
  end;
end;
```

这里 `CreateWithSafeDefaults` 只负责协议/cipher/profile baseline，不会替 server 调用方决定是否校验客户端证书；verify 意图仍需要显式写出。

---

## 实战示例

### 示例 1: HTTPS 客户端（组合使用三种模式）

```pascal
procedure HTTPSClient(const AUrl: string);
var
  LContext: ISSLContext;
  LConnection: ISSLConnection;
  LResult: TSSLOperationResult;
  LHost: string;
begin
  // 1. 使用 Try* 构建上下文（不希望启动时崩溃）
  LResult := TSSLContextBuilder.Create
    .WithTLS13
    .WithVerifyPeer
    .WithSystemRoots
    .TryBuildClient(LContext);

  if not LResult.IsOk then
  begin
    WriteLn('Failed to create SSL context: ', LResult.ErrorMessage);
    Exit;
  end;

  // 2. 使用异常连接（连接失败是例外情况）
  try
    // 实际项目中应先从 AUrl 解析主机名，再据此建立或复用 LSocket。
    LHost := 'api.example.com';
    LConnection := LContext.CreateConnection(LSocket);
    (LConnection as ISSLClientConnection).SetServerName(LHost);
    if not LConnection.Connect then
      raise ESSLConnectionError.Create('Connection failed');

    // 3. 使用 Result 传输数据（可能需要重试）
    SendRequest(LConnection, AUrl);

  except
    on E: ESSLException do
      WriteLn('SSL Error: ', E.Message);
  end;
end;
```

这里使用 direct `CreateConnection(...)`，是因为示例正在讨论 URL 解析后的 socket ownership、连接异常，以及 Result/exception 的边界；如果你不需要这层低层控制，可以把握手入口收回到 `TSSLConnector`。

### 示例 2: 证书管理工具

```pascal
procedure CertificateTool;
var
  LCert, LKey: string;
  LInfo: TCertInfo;
  LFingerprint: string;
begin
  // 生成证书 - Try* 模式（可能失败，但不致命）
  if not TCertificateUtils.TryGenerateSelfSignedSimple(
    'localhost', 'Dev Team', 365, LCert, LKey
  ) then
  begin
    WriteLn('Failed to generate certificate');
    Exit;
  end;

  WriteLn('✓ Certificate generated');

  // 保存到文件 - 异常模式（写入失败应该通知）
  try
    TCertificateUtils.SaveToFile('cert.pem', LCert);
    TCertificateUtils.SaveToFile('key.pem', LKey);
    WriteLn('✓ Saved to files');
  except
    on E: Exception do
    begin
      WriteLn('✗ Failed to save: ', E.Message);
      Exit;
    end;
  end;

  // 提取信息 - Try* 模式（可选操作）
  if TCertificateUtils.TryGetInfo(LCert, LInfo) then
  begin
    try
      WriteLn('Subject: ', LInfo.Subject);
      WriteLn('Valid: ', DateTimeToStr(LInfo.NotBefore),
              ' to ', DateTimeToStr(LInfo.NotAfter));
    finally
      LInfo.SubjectAltNames.Free;
    end;
  end;

  // 计算指纹 - Try* 模式
  if TCertificateUtils.TryGetFingerprint(LCert, LFingerprint) then
    WriteLn('Fingerprint: ', LFingerprint);
end;
```

---

## 性能考虑

### 性能对比

| 模式 | 成功路径开销 | 失败路径开销 | 适合场景 |
|------|------------|------------|----------|
| Result | 极低 | 低 | 热路径、函数式代码 |
| Try* | 低 | 低 | 批量操作、可选操作 |
| 异常 | 低 | **高** | 例外情况、配置加载 |

### 性能建议

1. **热路径使用 Result 或 Try***
   ```pascal
   // 好：循环中使用 Try*
   for I := 0 to 1000000 do
     if TCryptoUtils.TrySHA256(LData, LHash) then
       Process(LHash);

   // 差：循环中使用异常
   for I := 0 to 1000000 do
   begin
     try
       LHash := TCryptoUtils.SHA256(LData);  // 可能抛异常
       Process(LHash);
     except
     end;
   end;
   ```

2. **避免不必要的 Unwrap**
   ```pascal
   // 好：直接检查
   if LResult.IsOk then
     Process(LResult.Data);

   // 差：Unwrap 后 try-except
   try
     Process(LResult.Unwrap);  // 多余的异常开销
   except
   end;
   ```

---

## 常见陷阱

### 陷阱 1: 忘记检查 Result

```pascal
// ❌ 错误：未检查就使用
var
  LResult: TSSLDataResult;
begin
  LResult := TCryptoUtils.SHA256(LData);
  Process(LResult.Data);  // 如果失败，Data 是空的！
end;

// ✅ 正确：先检查
var
  LResult: TSSLDataResult;
begin
  LResult := TCryptoUtils.SHA256(LData);
  if LResult.IsOk then
    Process(LResult.Data)
  else
    WriteLn('Hash failed: ', LResult.ErrorMessage);
end;
```

### 陷阱 2: 忘记释放 TCertInfo

```pascal
// ❌ 错误：内存泄漏
var
  LInfo: TCertInfo;
begin
  if TCertificateUtils.TryGetInfo(LCert, LInfo) then
    WriteLn(LInfo.Subject);
  // LInfo.SubjectAltNames 没有释放！
end;

// ✅ 正确：总是释放
var
  LInfo: TCertInfo;
begin
  if TCertificateUtils.TryGetInfo(LCert, LInfo) then
  begin
    try
      WriteLn(LInfo.Subject);
    finally
      LInfo.SubjectAltNames.Free;  // 总是释放
    end;
  end;
end;
```

### 陷阱 3: Try* 方法的输出参数未检查

```pascal
// ❌ 错误：未检查返回值就使用
var
  LHash: TBytes;
begin
  TCryptoUtils.TrySHA256(LData, LHash);
  SendToServer(LHash);  // 如果失败，LHash 是空的！
end;

// ✅ 正确：检查返回值
var
  LHash: TBytes;
begin
  if TCryptoUtils.TrySHA256(LData, LHash) then
    SendToServer(LHash)
  else
    WriteLn('Hash calculation failed');
end;
```

### 陷阱 4: 异常捕获顺序错误

```pascal
// ❌ 错误：通用异常在前
try
  // SSL 操作
except
  on E: Exception do          // 会捕获所有异常
    HandleGenericError(E);
  on E: ESSLException do      // 永远不会执行！
    HandleSSLError(E);
end;

// ✅ 正确：特定异常在前
try
  // SSL 操作
except
  on E: ESSLCertError do
    HandleCertError(E);
  on E: ESSLException do
    HandleSSLError(E);
  on E: Exception do           // 最后处理通用异常
    HandleGenericError(E);
end;
```

---

## 总结

### 快速决策指南

**需要函数式编程、链式操作？** → 使用 **Result 模式**

**性能关键、批量操作？** → 使用 **Try* 模式**

**传统代码、错误是例外？** → 使用 **异常模式**

### 记住这些原则

1. ✅ **显式优于隐式** - 总是检查错误
2. ✅ **选择合适的模式** - 根据场景选择
3. ✅ **释放资源** - 使用 try-finally
4. ✅ **提供上下文** - 错误消息要有意义
5. ✅ **一致性** - 同一模块使用相同模式

---

**文档版本**: v1.5.0
**最后更新**: 2026-05-21
**相关文档**:
- `PHASE_1.1_COMPLETION_REPORT.md` - Result 类型详细说明
- `PHASE_1.2_COMPLETION_REPORT.md` - 证书工具 Try* 方法
- `PHASE_1.3_COMPLETION_REPORT.md` - 核心构建器 Try* 方法
