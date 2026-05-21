# Phase 2.4 迁移指南 - 类型安全改进（历史阶段说明）

**版本**: 2.4.0
**日期**: 2025-12-15
**目标**: 帮助开发者将现有代码迁移到类型安全的 API

> **说明**:
> - 本页保留 `Phase 2.4` 类型安全主题的历史迁移背景。
> - 当前 active public API / migration truth 以 `docs/guides/MIGRATION_GUIDE.md`、`src/fafafa.ssl.safety.pas`、`src/fafafa.ssl.base.pas`、`docs/reference/API_REFERENCE.md` 为准。
> - 下面示例中的类型安全单元名已经按当前源码真相统一为 `fafafa.ssl.safety`。

---

## 📋 概述

Phase 2.4 引入了类型安全改进，包括：

1. **强类型枚举** - 替代魔法数字
2. **单位类型** - 防止单位混淆（bits/bytes, ms/seconds）
3. **泛型封装** - Rust 风格的 Option<T> 和 Result<T, E> 模式

本指南将帮助您逐步迁移现有代码。

---

## 1. 枚举类型迁移

### 1.1 TSSLVersion - SSL/TLS 版本

**之前（使用魔法数字）**：
```pascal
// ❌ 不安全：魔法数字，易出错
procedure SetupSSL(AVersion: Integer);
begin
  if AVersion = 12 then
    WriteLn('Using TLS 1.2')
  else if AVersion = 13 then
    WriteLn('Using TLS 1.3');
end;

// 调用
SetupSSL(12); // 12 代表什么？
```

**之后（使用类型安全枚举）**：
```pascal
// ✅ 安全：强类型，IDE 提示，编译时检查
uses
  fafafa.ssl.safety;

procedure SetupSSL(AVersion: TSSLVersion);
begin
  case AVersion of
    sslv_TLS12: WriteLn('Using TLS 1.2');
    sslv_TLS13: WriteLn('Using TLS 1.3');
  else
    raise Exception.Create('Unsupported version');
  end;
end;

// 调用
SetupSSL(sslv_TLS12); // 清晰明确
```

**迁移步骤**：
1. 添加 `uses fafafa.ssl.safety;`
2. 将参数类型从 `Integer` 改为 `TSSLVersion`
3. 将所有魔法数字替换为枚举常量：
   - `10` → `sslv_TLS10`
   - `11` → `sslv_TLS11`
   - `12` → `sslv_TLS12`
   - `13` → `sslv_TLS13`

**字符串转换**：
```pascal
// 从配置文件读取
LVersionStr := ReadConfig('ssl_version'); // "TLS 1.2"
LVersion := StringToSSLVersion(LVersionStr);

// 转换为字符串显示
WriteLn('SSL Version: ', SSLVersionToString(sslv_TLS13));
```

---

### 1.2 TKeyType - 密钥类型

**之前**：
```pascal
// ❌ 使用字符串，易拼写错误
function GenerateKey(AType: string; ASize: Integer): TBytes;
begin
  if AType = 'RSA' then
    // ...
  else if AType = 'EC' then
    // ...
  // 拼写错误：'ec', 'rsa', 'Rsa' 都会失败
end;

GenerateKey('RSA', 2048);
```

**之后**：
```pascal
// ✅ 类型安全
uses
  fafafa.ssl.safety;

function GenerateKey(AType: TKeyType; ASize: Integer): TBytes;
begin
  case AType of
    kt_RSA: // 生成 RSA 密钥
    kt_EC: // 生成 EC 密钥
    kt_Ed25519: // 生成 Ed25519 密钥
  else
    raise Exception.Create('Unsupported key type');
  end;
end;

GenerateKey(kt_RSA, 2048); // 编译时验证
```

---

### 1.3 TCertificateFormat - 证书格式

**之前**：
```pascal
// ❌ 魔法字符串
function LoadCertificate(APath: string; AFormat: string): TX509;
begin
  if AFormat = 'PEM' then
    // ...
  else if AFormat = 'DER' then
    // ...
end;

LoadCertificate('cert.pem', 'PEM');
```

**之后**：
```pascal
// ✅ 类型安全
uses
  fafafa.ssl.safety;

function LoadCertificate(APath: string; AFormat: TCertificateFormat): TX509;
begin
  case AFormat of
    cf_PEM: // 加载 PEM 格式
    cf_DER: // 加载 DER 格式
    cf_PKCS12: // 加载 PKCS#12 格式
  else
    raise Exception.Create('Unsupported format');
  end;
end;

LoadCertificate('cert.pem', cf_PEM);
```

---

### 1.4 TEllipticCurve - 椭圆曲线

**之前**：
```pascal
// ❌ 使用 OpenSSL NID 常量（魔法数字）
function CreateECKey(ANid: Integer): PEVP_PKEY;
begin
  // 415 是什么？需要查文档
  if ANid = 415 then
    WriteLn('Using P-256');
end;

CreateECKey(415);
```

**之后**：
```pascal
// ✅ 类型安全，自动转换 NID
uses
  fafafa.ssl.safety;

function CreateECKey(ACurve: TEllipticCurve): PEVP_PKEY;
var
  LNID: Integer;
begin
  LNID := EllipticCurveToNID(ACurve);
  WriteLn('Using curve: ', EllipticCurveToString(ACurve));
  // 使用 LNID 调用 OpenSSL
end;

CreateECKey(ec_P256); // 清晰明确
CreateECKey(ec_X25519); // 支持现代曲线
```

---

## 2. 单位类型迁移

### 2.1 TKeySize - 密钥大小

**问题**：bits 和 bytes 经常混淆。

**之前**：
```pascal
// ❌ 不明确：这是 bits 还是 bytes？
function GenerateAESKey(ASize: Integer): TBytes;
begin
  // 调用者可能传入 256（bits）或 32（bytes）
  SetLength(Result, ASize); // 错误！
end;

LKey := GenerateAESKey(256); // 期望 256 bits，实际创建 256 bytes
```

**之后**：
```pascal
// ✅ 类型安全：明确单位
uses
  fafafa.ssl.safety;

function GenerateAESKey(ASize: TKeySize): TBytes;
var
  LBytes: Integer;
begin
  LBytes := ASize.ToBytes;
  SetLength(Result, LBytes);
  // 生成密钥...
end;

// 调用时明确单位
LKey := GenerateAESKey(TKeySize.Bits(256)); // 256 bits = 32 bytes
LKey := GenerateAESKey(TKeySize.Bytes(32)); // 32 bytes = 256 bits

// 两者等价
Assert(TKeySize.Bits(256).IsEqual(TKeySize.Bytes(32)));
```

**迁移步骤**：
1. 将参数从 `Integer` 改为 `TKeySize`
2. 调用处使用 `TKeySize.Bits()` 或 `TKeySize.Bytes()`
3. 函数内使用 `.ToBits` 或 `.ToBytes` 获取值

**比较密钥大小**：
```pascal
var
  LSize1, LSize2: TKeySize;
begin
  LSize1 := TKeySize.Bits(256);
  LSize2 := TKeySize.Bits(128);

  if LSize1.Compare(LSize2) > 0 then
    WriteLn('256 bits is stronger than 128 bits');

  if LSize1.IsEqual(TKeySize.Bytes(32)) then
    WriteLn('256 bits = 32 bytes');
end;
```

---

### 2.2 TTimeoutDuration - 超时时长

**问题**：毫秒、秒、分钟经常混淆。

**之前**：
```pascal
// ❌ 不明确：这是毫秒还是秒？
procedure ConnectWithTimeout(ATimeout: Integer);
begin
  // ATimeout 是毫秒？秒？分钟？
  SetSocketTimeout(ATimeout);
end;

ConnectWithTimeout(5000); // 5000 什么？
ConnectWithTimeout(5);    // 5 什么？
```

**之后**：
```pascal
// ✅ 类型安全：明确单位
uses
  fafafa.ssl.safety;

procedure ConnectWithTimeout(ATimeout: TTimeoutDuration);
var
  LMS: Int64;
begin
  LMS := ATimeout.ToMilliseconds;
  SetSocketTimeout(LMS);
end;

// 调用时明确单位
ConnectWithTimeout(TTimeoutDuration.Milliseconds(5000)); // 5000 毫秒
ConnectWithTimeout(TTimeoutDuration.Seconds(5));        // 5 秒
ConnectWithTimeout(TTimeoutDuration.Minutes(2));        // 2 分钟
ConnectWithTimeout(TTimeoutDuration.Infinite);          // 无限等待

// 等价
Assert(TTimeoutDuration.Seconds(5).IsEqual(
  TTimeoutDuration.Milliseconds(5000)));
```

**配置文件示例**：
```pascal
// 从配置读取
LTimeoutStr := ReadConfig('timeout'); // "30s"

if Pos('ms', LTimeoutStr) > 0 then
  LTimeout := TTimeoutDuration.Milliseconds(StrToInt(Copy(LTimeoutStr, 1, Pos('ms', LTimeoutStr) - 1)))
else if Pos('s', LTimeoutStr) > 0 then
  LTimeout := TTimeoutDuration.Seconds(StrToInt(Copy(LTimeoutStr, 1, Pos('s', LTimeoutStr) - 1)))
else if Pos('m', LTimeoutStr) > 0 then
  LTimeout := TTimeoutDuration.Minutes(StrToInt(Copy(LTimeoutStr, 1, Pos('m', LTimeoutStr) - 1)));

ConnectWithTimeout(LTimeout);
```

---

### 2.3 TBufferSize - 缓冲区大小

**问题**：bytes, KB, MB 混淆。

**之前**：
```pascal
// ❌ 不明确
procedure AllocateBuffer(ASize: Integer);
begin
  SetLength(FBuffer, ASize); // 多大？
end;

AllocateBuffer(8192);  // 8192 bytes 还是 8192 KB？
AllocateBuffer(8);     // 8 bytes 还是 8 MB？
```

**之后**：
```pascal
// ✅ 类型安全
uses
  fafafa.ssl.safety;

procedure AllocateBuffer(ASize: TBufferSize);
var
  LBytes: NativeUInt;
begin
  LBytes := ASize.ToBytes;
  SetLength(FBuffer, LBytes);
end;

// 调用时明确单位
AllocateBuffer(TBufferSize.Bytes(8192));  // 8192 bytes = 8 KB
AllocateBuffer(TBufferSize.KB(8));        // 8 KB = 8192 bytes
AllocateBuffer(TBufferSize.MB(1));        // 1 MB = 1024 KB

// 等价
Assert(TBufferSize.KB(8).IsEqual(TBufferSize.Bytes(8192)));
```

**当前 `fafafa.ssl` 真相**：

- 当前 `fafafa.ssl` 的 TLS context / factory / direct-library 路径并没有单独的 `WithBufferSize(...)` / `SetBuffer(...)` public 入口。
- 当前 `TSSLConfig.BufferSize` 仍是 connection-scoped buffering hint；若在 factory / direct-library 创建路径写入自定义值，会被显式拒绝。
- 若你在当前库里需要调整缓冲策略，应放在外围 socket / stream / transport / app-level buffering layer。
- 因此 `TBufferSize` 在当前 repo 里更适合：
  - 你自己的 buffer pool / transport helper
  - app-level payload / chunk sizing policy
  - 而不是当前 TLS context builder / factory 配置入口

---

## 3. 泛型类型迁移

### 3.1 TSecureData<T> - Option<T> 模式

**用途**：表示可能不存在的值（替代 nil 或魔法值）。

**之前**：
```pascal
// ❌ 使用 nil 或特殊值表示"不存在"
function FindUser(AID: Integer): TUser;
begin
  // 返回 nil 表示未找到？
  Result := nil; // 调用者可能忘记检查
end;

// 或者
function GetConfigValue(AKey: string): string;
begin
  // 返回空字符串表示未找到？
  Result := ''; // 无法区分"未设置"和"设置为空字符串"
end;
```

**之后**：
```pascal
// ✅ 使用 TSecureData 明确表示
uses
  fafafa.ssl.safety;

type
  TUserSecureData = specialize TSecureData<TUser>;
  TStringSecureData = specialize TSecureData<string>;

function FindUser(AID: Integer): TUserSecureData;
begin
  if UserExists(AID) then
    Result := TUserSecureData.Some(LoadUser(AID))
  else
    Result := TUserSecureData.None('User not found');
end;

// 调用者明确检查
var
  LUserData: TUserSecureData;
  LUser: TUser;
begin
  LUserData := FindUser(123);

  if LUserData.IsSome then
  begin
    LUser := LUserData.Unwrap;
    WriteLn('Found user: ', LUser.Name);
  end
  else
    WriteLn('User not found: ', LUserData.ErrorMessage);

  // 或者提供默认值
  LUser := LUserData.UnwrapOr(DefaultUser);
end;
```

**配置值示例**：
```pascal
function GetConfigValue(AKey: string): TStringSecureData;
begin
  if ConfigExists(AKey) then
    Result := TStringSecureData.Some(ReadConfig(AKey))
  else
    Result := TStringSecureData.None('Config key not found');
end;

// 使用
var
  LServerData: TStringSecureData;
  LServer: string;
begin
  LServerData := GetConfigValue('server_address');

  // 方法 1: 检查后解包
  if LServerData.IsSome then
    LServer := LServerData.Unwrap
  else
    LServer := 'localhost'; // 默认值

  // 方法 2: 使用 UnwrapOr（更简洁）
  LServer := LServerData.UnwrapOr('localhost');
end;
```

---

### 3.2 TResult<T, E> - Result<T, E> 模式

**用途**：表示可能失败的操作（替代异常或 Boolean + out 参数）。

**之前**：
```pascal
// ❌ 方法 1: 使用异常（性能开销大）
function ParseInt(const AStr: string): Integer;
begin
  try
    Result := StrToInt(AStr);
  except
    on E: Exception do
      raise Exception.Create('Parse failed: ' + E.Message);
  end;
end;

// ❌ 方法 2: 使用 Boolean + out 参数（繁琐）
function TryParseInt(const AStr: string; out AValue: Integer): Boolean;
begin
  try
    AValue := StrToInt(AStr);
    Result := True;
  except
    Result := False;
  end;
end;
```

**之后**：
```pascal
// ✅ 使用 TResult 明确表示成功/失败
uses
  fafafa.ssl.safety;

type
  TIntResult = specialize TResult<Integer, string>;

function ParseInt(const AStr: string): TIntResult;
var
  LValue: Integer;
begin
  if TryStrToInt(AStr, LValue) then
    Result := TIntResult.Ok(LValue)
  else
    Result := TIntResult.Err('Invalid integer: ' + AStr);
end;

// 调用者明确处理
var
  LResult: TIntResult;
  LValue: Integer;
begin
  LResult := ParseInt('123');

  if LResult.IsOk then
  begin
    LValue := LResult.Unwrap;
    WriteLn('Parsed: ', LValue);
  end
  else
    WriteLn('Error: ', LResult.UnwrapErr);

  // 或者提供默认值
  LValue := LResult.UnwrapOr(0);
end;
```

**文件操作示例**：
```pascal
type
  TBytesResult = specialize TResult<TBytes, string>;

function ReadFileContents(const APath: string): TBytesResult;
var
  LFile: File of Byte;
  LData: TBytes;
  LSize: Integer;
begin
  if not FileExists(APath) then
    Exit(TBytesResult.Err('File not found: ' + APath));

  try
    AssignFile(LFile, APath);
    Reset(LFile);
    try
      LSize := FileSize(LFile);
      SetLength(LData, LSize);
      BlockRead(LFile, LData[0], LSize);
      Result := TBytesResult.Ok(LData);
    finally
      CloseFile(LFile);
    end;
  except
    on E: Exception do
      Result := TBytesResult.Err('Read error: ' + E.Message);
  end;
end;

// 使用
var
  LResult: TBytesResult;
  LData: TBytes;
begin
  LResult := ReadFileContents('config.bin');

  if LResult.IsOk then
  begin
    LData := LResult.Unwrap;
    ProcessData(LData);
  end
  else
  begin
    WriteLn('Failed to read file: ', LResult.UnwrapErr);
    // 使用默认配置
    LData := GetDefaultConfig;
  end;
end;
```

---

## 4. 实际迁移示例

### 示例 1: SSL 连接配置

**之前**：
```pascal
procedure ConfigureLegacyClientPolicy(
  AVersion: Integer;           // ❌ 魔法数字
  ATimeout: Integer;           // ❌ 单位不明
  ABufferSize: Integer;        // ❌ 单位不明
  AVerifyMode: Integer         // ❌ 魔法数字
);
begin
  // 代码难以理解
  if AVersion = 13 then
    SetTLS13;
  SetTimeout(ATimeout);
  SetBuffer(ABufferSize);
  if AVerifyMode = 1 then
    EnableVerification;
end;

// 调用
ConfigureLegacyClientPolicy(13, 30000, 8192, 1);
// 参数含义需要查文档
```

**之后**：
```pascal
uses
  fafafa.ssl.safety;

procedure ConfigureClientPolicy(
  AVersion: TSSLVersion;               // ✅ 类型安全
  ATimeout: TTimeoutDuration;          // ✅ 明确单位
  ABufferSize: TBufferSize;            // ✅ 明确单位
  AVerifyMode: TVerificationMode       // ✅ 类型安全
);
var
  LBufferBytes: NativeUInt;
begin
  // 代码自解释
  case AVersion of
    sslv_TLS13: SetTLS13;
    sslv_TLS12: SetTLS12;
  end;

  // timeout 在当前库里应映射到连接级 API
  ApplyConnectionTimeout(ATimeout.ToMilliseconds);

  // buffer sizing 保持在 TLS 外围的 transport / IO 层
  LBufferBytes := ABufferSize.ToBytes;
  ConfigureTransportBuffering(LBufferBytes);

  if AVerifyMode = vm_Peer then
    EnableVerification;
end;

// 调用
ConfigureClientPolicy(
  sslv_TLS13,                          // TLS 1.3
  TTimeoutDuration.Seconds(30),        // 30 秒
  TBufferSize.KB(8),                   // 8 KB
  vm_Peer                              // 验证对等方
);
// 参数含义一目了然
```

下面这个组合示意应理解为“你自己的 typed wrapper / policy boundary”，不是当前 `fafafa.ssl` 直接提供的单一 SSL 配置入口。

---

### 示例 2: 密钥生成

**之前**：
```pascal
function GenerateKey(AType: string; ASize: Integer): TBytes;
begin
  if AType = 'RSA' then
  begin
    // ASize 是 bits 还是 bytes？
    if ASize < 2048 then
      raise Exception.Create('Key too small');
    // ...
  end;
end;

// 调用
LKey := GenerateKey('RSA', 2048); // 2048 bits 还是 bytes？
```

**之后**：
```pascal
uses
  fafafa.ssl.safety;

function GenerateKey(AType: TKeyType; ASize: TKeySize): TBytes;
begin
  case AType of
    kt_RSA:
    begin
      if ASize.Compare(TKeySize.Bits(2048)) < 0 then
        raise Exception.Create('RSA key must be at least 2048 bits');
      // 使用 ASize.ToBits 调用 OpenSSL
    end;
    kt_EC:
      // EC 密钥生成
    kt_Ed25519:
      // Ed25519 固定大小
  end;
end;

// 调用
LKey := GenerateKey(kt_RSA, TKeySize.Bits(2048)); // 明确 2048 bits
LKey := GenerateKey(kt_EC, TKeySize.Bits(256));   // P-256 曲线
```

---

## 5. 迁移检查清单

### 第 1 步：识别需要迁移的代码

- [ ] 查找魔法数字（SSL 版本、密钥类型等）
- [ ] 查找单位混淆的参数（timeout, key size, buffer size）
- [ ] 查找使用 nil 或特殊值表示"不存在"的代码
- [ ] 查找使用异常或 Boolean + out 表示失败的代码

### 第 2 步：添加 uses 子句

```pascal
uses
  fafafa.ssl.safety;
```

### 第 3 步：迁移枚举

- [ ] 替换 SSL 版本魔法数字 → `TSSLVersion`
- [ ] 替换密钥类型字符串 → `TKeyType`
- [ ] 替换证书格式字符串 → `TCertificateFormat`
- [ ] 替换加密模式 → `TCipherMode`

### 第 4 步：迁移单位类型

- [ ] 密钥大小参数 → `TKeySize.Bits()` 或 `TKeySize.Bytes()`
- [ ] 超时参数 → `TTimeoutDuration.Seconds()` 等
- [ ] 缓冲区大小（你自己的 transport / buffer policy helper）→ `TBufferSize.KB()` 等

### 第 5 步：迁移泛型类型

- [ ] 可选值 → `TSecureData<T>`
- [ ] 可能失败的操作 → `TResult<T, E>`

### 第 6 步：测试

- [ ] 单元测试通过
- [ ] 集成测试通过
- [ ] 代码审查

---

## 6. 常见问题

### Q1: 我必须一次性迁移所有代码吗？

**A**: 不需要。类型安全 API 与旧 API 可以共存。您可以：

1. 先迁移新代码
2. 逐步迁移旧代码
3. 在旧代码和新代码之间添加适配层

```pascal
// 适配层示例
function OldSetupSSL(AVersion: Integer): Boolean;
var
  LVersion: TSSLVersion;
begin
  // 转换旧参数到新类型
  case AVersion of
    10: LVersion := sslv_TLS10;
    11: LVersion := sslv_TLS11;
    12: LVersion := sslv_TLS12;
    13: LVersion := sslv_TLS13;
  else
    raise Exception.Create('Invalid version');
  end;

  // 调用新 API
  Result := NewSetupSSL(LVersion);
end;
```

---

### Q2: FreePascal 的泛型和 Delphi 的泛型有什么区别？

**A**: FreePascal 使用 `specialize` 关键字实例化泛型：

```pascal
// FreePascal
type
  TIntSecureData = specialize TSecureData<Integer>;

// Delphi (不需要 specialize)
type
  TIntSecureData = TSecureData<Integer>;
```

---

### Q3: 单位类型的性能开销如何？

**A**: 几乎没有开销。单位类型是简单的 record，编译器会内联方法调用。

---

### Q4: 我可以扩展枚举类型吗？

**A**: 不能直接扩展，但可以创建新枚举并提供转换函数：

```pascal
type
  TMySSLVersion = (
    mysslv_TLS12,
    mysslv_TLS13,
    mysslv_TLS14  // 自定义扩展
  );

function ToStandardSSLVersion(AVersion: TMySSLVersion): TSSLVersion;
begin
  case AVersion of
    mysslv_TLS12: Result := sslv_TLS12;
    mysslv_TLS13: Result := sslv_TLS13;
    mysslv_TLS14: raise Exception.Create('Not yet supported');
  end;
end;
```

---

## 7. 参考资料

- **Phase 2.4 完成报告**: `docs/PHASE_2.4_COMPLETION_REPORT.md`
- **类型安全测试**: `tests/test_type_safety.pas`
- **类型定义**: `src/fafafa.ssl.safety.pas`

---

## 8. 总结

Phase 2.4 的类型安全改进为 fafafa.ssl 带来：

- ✅ **编译时验证** - 错误在编译时发现，而非运行时
- ✅ **代码自解释** - 参数含义清晰，减少注释需求
- ✅ **IDE 支持** - 自动补全、类型检查、重构工具
- ✅ **防止单位混淆** - bits/bytes, ms/seconds 不再混淆
- ✅ **Rust 风格模式** - Option<T> 和 Result<T, E> 提升代码质量

**建议的迁移顺序**：

1. **新代码优先** - 所有新代码使用类型安全 API
2. **关键路径** - 迁移安全关键代码（密钥生成、证书验证）
3. **逐步迁移** - 重构时顺便迁移旧代码
4. **完全迁移** - 最终移除所有魔法数字和单位混淆

**立即行动**：

- 阅读测试文件 `tests/test_type_safety.pas` 了解用法
- 在新项目中使用类型安全 API
- 分享反馈，帮助改进 API 设计

---

**迁移愉快！** 🚀

如有问题，请查看测试代码或提交 issue。
