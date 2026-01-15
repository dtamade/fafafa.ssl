# fafafa.ssl 接口设计指南

**版本**: 1.0.0  
**目标**: 建立清晰、一致、易用的API接口

---

## 1. API设计原则

### 1.1 最小惊讶原则
API行为应该符合用户的直觉和预期

```pascal
// ✅ 符合预期
LHash := TCryptoUtils.SHA256('Hello');  // 显然返回哈希值
LEncrypted := AES_Encrypt(LData, LKey); // 显然返回密文

// ❌ 令人困惑
LHash := TCryptoUtils.Process('Hello');  // Process什么？
LResult := DoIt(LData, LKey);            // DoIt做什么？
```

### 1.2 一致性原则
相似的功能使用相似的接口

```pascal
// ✅ 一致的API
class function SHA256(const AData: TBytes): TBytes;
class function SHA512(const AData: TBytes): TBytes;
class function SHA3_256(const AData: TBytes): TBytes;

// ❌ 不一致
class function SHA256(const AData: TBytes): TBytes;
class procedure SHA512(const AData: TBytes; out AHash: TBytes);
class function SHA3_256Hash(AData: TBytes): string;  // 不同的返回类型
```

### 1.3 完整性原则
提供完整的功能覆盖

```pascal
type
  TCryptoUtils = class
    // ✅ 完整的加密/解密对
    class function AES_GCM_Encrypt(...): TBytes;
    class function AES_GCM_Decrypt(...): TBytes;
    
    // ✅ 多种输入类型重载
    class function SHA256(const AData: TBytes): TBytes; overload;
    class function SHA256(const AData: string): TBytes; overload;
    class function SHA256(AStream: TStream): TBytes; overload;
    
    // ✅ 便利方法
    class function SHA256Hex(const AData: TBytes): string;
    class function SHA256Base64(const AData: TBytes): string;
  end;
```

---

## 2. 类设计模式

### 2.1 静态工具类
用于无状态的工具函数

```pascal
type
  {**
   * 加密工具类（静态）
   * 所有方法都是类方法，无需创建实例
   *}
  TCryptoUtils = class
  private
    class procedure EnsureInitialized; static;
  public
    class function SHA256(const AData: TBytes): TBytes; static;
    class function AES_Encrypt(...): TBytes; static;
  end;

// 使用
LHash := TCryptoUtils.SHA256(LData);  // 无需创建实例
```

### 2.2 有状态类
需要保持状态的功能

```pascal
type
  {**
   * 加密上下文（有状态）
   * 用于流式加密操作
   *}
  TEncryptionContext = class
  private
    FAlgorithm: TEncryptionAlgorithm;
    FKey: TBytes;
    FContext: PEVP_CIPHER_CTX;
  public
    constructor Create(
      AAlgorithm: TEncryptionAlgorithm;
      const AKey: TBytes
    );
    destructor Destroy; override;
    
    procedure Update(const AData: TBytes);
    function Finalize: TBytes;
    procedure Reset;
  end;

// 使用
LEncCtx := TEncryptionContext.Create(ENCRYPT_AES_GCM, LKey);
try
  LEncCtx.Update(LData1);
  LEncCtx.Update(LData2);
  LResult := LEncCtx.Finalize;
finally
  LEncCtx.Free;
end;
```

### 2.3 构建器模式
复杂对象的创建

```pascal
type
  {**
   * SSL连接构建器
   * 用于配置复杂的SSL连接选项
   *}
  TSSLConnectionBuilder = class
  private
    FHost: string;
    FPort: Word;
    FTimeout: Integer;
    FVerifyPeer: Boolean;
    FCertFile: string;
  public
    function SetHost(const AHost: string): TSSLConnectionBuilder;
    function SetPort(APort: Word): TSSLConnectionBuilder;
    function SetTimeout(ATimeout: Integer): TSSLConnectionBuilder;
    function EnablePeerVerification: TSSLConnectionBuilder;
    function UseCertificate(const ACertFile: string): TSSLConnectionBuilder;
    
    function Build: TSSLConnection;
  end;

// 使用（流式API）
LConn := TSSLConnectionBuilder.Create
  .SetHost('example.com')
  .SetPort(443)
  .SetTimeout(30000)
  .EnablePeerVerification
  .Build;
```

---

## 3. 方法设计模式

### 3.1 基础方法
简单直接，专注单一功能

```pascal
{**
 * 计算SHA-256哈希
 * @param AData 要计算哈希的数据
 * @return 32字节哈希值
 *}
class function SHA256(const AData: TBytes): TBytes;
```

### 3.2 扩展方法（重载）
提供便利的输入方式

```pascal
// 字节数组输入
class function SHA256(const AData: TBytes): TBytes; overload;

// 字符串输入
class function SHA256(const AData: string): TBytes; overload;

// 流输入
class function SHA256(AStream: TStream): TBytes; overload;

// 文件输入
class function SHA256File(const AFileName: string): TBytes;
```

### 3.3 Try系列方法
不抛异常的安全版本

```pascal
// 标准方法 - 失败时抛异常
class function Encrypt(const AData, AKey: TBytes): TBytes;

// Try版本 - 失败时返回False
class function TryEncrypt(
  const AData, AKey: TBytes;
  out AResult: TBytes
): Boolean;

// 使用
if TCryptoUtils.TryEncrypt(LData, LKey, LResult) then
  WriteLn('Success')
else
  WriteLn('Failed');
```

### 3.4 扩展结果方法
返回详细信息

```pascal
type
  TEncryptionResult = record
    Success: Boolean;
    Ciphertext: TBytes;
    Tag: TBytes;
    Algorithm: TEncryptionAlgorithm;
    Duration: Cardinal;  // 毫秒
    ErrorMessage: string;
  end;

// 扩展版本 - 返回详细结果
class function EncryptEx(
  const AData, AKey: TBytes;
  AOptions: TEncryptOptions
): TEncryptionResult;
```

---

## 4. 参数设计

### 4.1 参数顺序

标准顺序: **主要输入** → **次要输入** → **输出参数** → **选项/可选参数**

```pascal
function Encrypt(
  const APlaintext: TBytes;        // 主要输入
  const AKey: TBytes;              // 次要输入
  const AIV: TBytes;               // 次要输入
  out ACiphertext: TBytes;         // 输出
  const AOptions: TEncryptOptions = []  // 可选
): Boolean;
```

### 4.2 选项参数

使用记录或集合类型封装选项：

```pascal
type
  TEncryptOption = (
    EO_VERIFY_KEY,      // 验证密钥
    EO_PAD_PKCS7,       // 使用PKCS7填充
    EO_INCLUDE_TAG      // 包含认证标签
  );
  TEncryptOptions = set of TEncryptOption;

// 使用
LResult := Encrypt(
  AData,
  AKey,
  [EO_VERIFY_KEY, EO_PAD_PKCS7]
);
```

或使用配置记录：

```pascal
type
  TEncryptConfig = record
    Algorithm: TEncryptionAlgorithm;
    Mode: TEncryptionMode;
    Padding: TPaddingMode;
    VerifyKey: Boolean;
    
    class function Default: TEncryptConfig; static;
  end;

// 使用
LConfig := TEncryptConfig.Default;
LConfig.VerifyKey := True;
LResult := Encrypt(AData, AKey, LConfig);
```

### 4.3 默认参数

合理使用默认参数：

```pascal
// ✅ 好 - 合理的默认值
class function GenerateKey(
  ABits: Integer = 256  // 默认256位，安全的选择
): TBytes;

class function Connect(
  const AHost: string;
  APort: Word = 443;    // HTTPS标准端口
  ATimeout: Integer = 30000  // 30秒超时
): Boolean;

// ❌ 差 - 不明确的默认值
function DoSomething(
  AValue: Integer = 0;  // 0 有特殊含义吗？
  AFlag: Boolean = False  // False 是安全默认吗？
): Boolean;
```

---

## 5. 返回值设计

### 5.1 简单返回
大多数情况使用简单类型：

```pascal
function CalculateHash(const AData: TBytes): TBytes;
function IsValid: Boolean;
function GetCount: Integer;
```

### 5.2 结果记录
复杂场景返回结构化结果：

```pascal
type
  TOperationResult = record
    Success: Boolean;
    Data: TBytes;
    ErrorCode: Integer;
    ErrorMessage: string;
    Timestamp: TDateTime;
    
    class function CreateSuccess(const AData: TBytes): TOperationResult; static;
    class function CreateError(ACode: Integer; const AMsg: string): TOperationResult; static;
  end;
```

### 5.3 智能指针模式
自动管理资源：

```pascal
type
  IAutoContext = interface
    function GetHandle: PEVP_MD_CTX;
    property Handle: PEVP_MD_CTX read GetHandle;
  end;

function CreateAutoContext: IAutoContext;
// 实现会在接口释放时自动清理资源
```

---

## 6. 错误处理策略

### 6.1 三层错误处理

```pascal
// 层1: 标准方法 - 抛异常
class function Encrypt(const AData, AKey: TBytes): TBytes;
// 失败抛 ESSLCryptoError

// 层2: Try方法 - 返回布尔
class function TryEncrypt(
  const AData, AKey: TBytes;
  out AResult: TBytes
): Boolean;
// 失败返回 False

// 层3: Ex方法 - 返回详细结果
class function EncryptEx(
  const AData, AKey: TBytes
): TEncryptionResult;
// 失败在 Result.Success = False，包含错误信息
```

### 6.2 验证和前置条件

```pascal
class function ProcessData(const AData: TBytes): TBytes;
begin
  // 参数验证
  if Length(AData) = 0 then
    raise ESSLInvalidArgument.Create('Data cannot be empty');
    
  if Length(AData) > MAX_DATA_SIZE then
    raise ESSLInvalidArgument.CreateFmt(
      'Data size %d exceeds maximum %d',
      [Length(AData), MAX_DATA_SIZE]
    );
    
  // 状态验证
  EnsureInitialized;
  
  // 执行操作
  Result := InternalProcess(AData);
end;
```

---

## 7. 线程安全设计

### 7.1 无状态类
类方法自然线程安全：

```pascal
type
  TCryptoUtils = class
    // 所有方法都是静态的，线程安全
    class function SHA256(const AData: TBytes): TBytes; static;
  end;
```

### 7.2 有状态类
使用临界区保护：

```pascal
type
  TCryptoContext = class
  private
    FLock: TCriticalSection;
    FState: TBytes;
  public
    constructor Create;
    destructor Destroy; override;
    
    procedure Update(const AData: TBytes);
    function Finalize: TBytes;
  end;

procedure TCryptoContext.Update(const AData: TBytes);
begin
  FLock.Enter;
  try
    // 线程安全操作
  finally
    FLock.Leave;
  end;
end;
```

---

## 8. 版本兼容性

### 8.1 添加新功能
使用重载或新方法，保持向后兼容：

```pascal
// v1.0
class function Encrypt(const AData, AKey: TBytes): TBytes;

// v1.1 - 添加重载，不破坏兼容性
class function Encrypt(
  const AData, AKey: TBytes;
  AOptions: TEncryptOptions
): TBytes; overload;
```

### 8.2 废弃功能
使用编译器提示：

```pascal
class function OldMethod: Integer;
  deprecated 'Use NewMethod instead';
  
class function NewMethod: Integer;
```

---

## 9. API使用示例

良好的API应该易于使用：

```pascal
// ✅ 简单任务简单做
LHash := TCryptoUtils.SHA256('Hello');

// ✅ 复杂任务也清晰
LConfig := TEncryptConfig.Create;
LConfig.Algorithm := ENCRYPT_AES_GCM;
LConfig.KeySize := 256;
LConfig.Padding := PAD_PKCS7;

LResult := TCryptoUtils.EncryptEx(
  APlaintext := LData,
  AKey := LKey,
  AConfig := LConfig
);

if LResult.Success then
  ProcessCiphertext(LResult.Ciphertext)
else
  LogError(LResult.ErrorMessage);
```

---

## 10. 检查清单

API设计完成后，检查：

- [ ] 方法名清晰表达意图
- [ ] 参数顺序合理（输入→输出→选项）
- [ ] 所有输入参数使用const
- [ ] 提供Try版本（对于可能失败的操作）
- [ ] 提供重载以支持常见输入类型
- [ ] 文档注释完整（参数、返回值、异常）
- [ ] 参数验证完善
- [ ] 错误消息详细有用
- [ ] 示例代码可运行
- [ ] 向后兼容（或明确标记breaking change）

---

**遵循本指南，创建专业、易用、健壮的API接口。**
