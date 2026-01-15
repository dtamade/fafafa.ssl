# fafafa.ssl 企业级代码规范

**版本**: 1.0.0  
**日期**: 2025-11-26  
**适用范围**: fafafa.ssl 及所有子模块

---

## 1. 总则

### 1.1 目标
本规范旨在建立统一、专业、可维护的代码标准，确保 fafafa.ssl 达到企业级质量要求。

### 1.2 原则
- **一致性**: 所有代码遵循相同规范
- **可读性**: 代码自文档化，易于理解
- **可维护性**: 易于修改和扩展
- **健壮性**: 完善的错误处理和验证
- **专业性**: 符合行业最佳实践

---

## 2. 命名约定

### 2.1 标识符前缀

#### 强制前缀规则

```pascal
// 类型名
T     - 类、记录、枚举类型      (TCryptoUtils, TEncryptionMode)
E     - 异常类型                (ESSLException, ESSLCryptoError)
I     - 接口类型                (ICryptoProvider, IHashAlgorithm)
P     - 指针类型                (PEVP_MD_CTX, PCertificate)

// 变量名
A     - 方法参数                (AData, AFileName, AOptions)
L     - 局部变量                (LBuffer, LContext, LResult)
F     - 类字段（private）       (FHandle, FInitialized)
G     - 全局变量（避免使用）    (GInitialized, GConfiguration)

// 常量
CONST - 全大写+下划线           (AES_KEY_SIZE, DEFAULT_TIMEOUT)
```

### 2.2 命名风格

#### 帕斯卡命名法（PascalCase）
用于：类名、方法名、属性名、类型名

```pascal
type
  TCryptoUtils = class
  public
    class function CalculateHash(const AData: TBytes): TBytes;
    property IsInitialized: Boolean;
  end;
```

#### 全大写+下划线
用于：常量、枚举值

```pascal
const
  AES_256_KEY_SIZE = 32;
  DEFAULT_BUFFER_SIZE = 8192;
  MAX_RETRY_COUNT = 3;

type
  THashAlgorithm = (
    HASH_SHA256,
    HASH_SHA512,
    HASH_SHA3_256
  );
```

### 2.3 命名语义

#### 动词开头
方法名应以动词开头，清晰表达动作：

```pascal
// ✅ 好
function CalculateHash(...): TBytes;
procedure InitializeContext(...);
function ValidateKey(...): Boolean;

// ❌ 差
function Hash(...): TBytes;         // 不够明确
procedure Context(...);             // 含糊
function Key(...): Boolean;         // 不清楚
```

#### 布尔属性/方法
使用 `Is/Has/Can/Should` 前缀：

```pascal
property IsInitialized: Boolean;
function HasValidSignature: Boolean;
function CanEncrypt: Boolean;
function ShouldRetry: Boolean;
```

#### 避免缩写
除非是行业标准缩写：

```pascal
// ✅ 允许的缩写
AES, RSA, SSL, TLS, HTTP, HTTPS, IV, MAC, HMAC

// ❌ 避免的缩写
Ctx (应该用 Context)
Buf (应该用 Buffer) 
Tmp (应该用 Temporary)
Num (应该用 Number/Count)
```

---

## 3. 代码布局

### 3.1 缩进和空格

```pascal
// 使用 2 空格缩进（不使用Tab）
type
  TCryptoUtils = class
  private
    FInitialized: Boolean;
    
    procedure EnsureInitialized;
    
  public
    constructor Create;
    destructor Destroy; override;
  end;

// 运算符前后保留空格
LResult := LValue1 + LValue2;
if (LCount > 0) and (LFlag = True) then

// 逗号后保留空格
MyFunction(LParam1, LParam2, LParam3);

// 赋值运算符对齐（可选，同一组时）
LShortVar    := 10;
LLongerVar   := 20;
LVeryLongVar := 30;
```

### 3.2 行长度限制

```pascal
// 每行不超过 100 字符
// 超长行应该换行并适当缩进

// ✅ 好
LResult := SomeVeryLongFunction(
  LParameter1,
  LParameter2,
  LParameter3
);

// ✅ 也可以
if (LCondition1 and LCondition2) or
   (LCondition3 and LCondition4) then
begin
  DoSomething;
end;
```

### 3.3 空行使用

```pascal
type
  TMyClass = class
  private
    // 字段组之间空一行
    FHandle: THandle;
    FInitialized: Boolean;
    
    FBuffer: TBytes;
    FBufferSize: Integer;
    
    // 私有方法前空一行
    procedure InternalProcess;
    
  public
    // 公开方法组之间空一行
    { 初始化方法 }
    constructor Create;
    destructor Destroy; override;
    
    { 核心方法 }
    function Process(const AData: TBytes): TBytes;
    procedure Reset;
  end;

implementation

// 方法实现之间空两行
constructor TMyClass.Create;
begin
  FInitialized := False;
end;


destructor TMyClass.Destroy;
begin
  Reset;
  inherited;
end;
```

---

## 4. 接口设计

### 4.1 类设计原则

#### 单一职责
每个类只负责一个明确的功能域：

```pascal
// ✅ 好 - 单一职责
type
  TCryptoUtils = class      // 只负责加密操作
  THashUtils = class        // 只负责哈希计算
  TCertificateManager = class  // 只负责证书管理

// ❌ 差 - 职责混乱
type
  TSSLHelper = class  // 混合了加密、证书、网络等
```

#### 接口隔离
提供细粒度的接口：

```pascal
type
  // ✅ 好 - 细粒度接口
  IHashAlgorithm = interface
    function Calculate(const AData: TBytes): TBytes;
  end;
  
  IEncryptionAlgorithm = interface
    function Encrypt(const AData, AKey: TBytes): TBytes;
    function Decrypt(const AData, AKey: TBytes): TBytes;
  end;
  
  // ❌ 差 - 臃肿接口
  ICryptoOperations = interface
    function Hash(...): TBytes;
    function Encrypt(...): TBytes;
    function Sign(...): TBytes;
    function Verify(...): Boolean;
    // 太多不相关方法
  end;
```

### 4.2 方法签名设计

#### 参数顺序
遵循：输入参数 → 输出参数 → 可选参数

```pascal
function EncryptData(
  const APlaintext: TBytes;      // 输入
  const AKey: TBytes;            // 输入
  out ACiphertext: TBytes;       // 输出
  const AOptions: TEncryptOptions = []  // 可选
): Boolean;
```

#### 使用 const 修饰符
所有不修改的引用类型参数使用 `const`：

```pascal
// ✅ 好
function ProcessData(const AData: TBytes): Boolean;
procedure LogMessage(const AMessage: string);

// ❌ 差
function ProcessData(AData: TBytes): Boolean;  // 可能暗示会修改
```

#### 使用命名参数
对于多参数方法，调用时使用命名参数：

```pascal
// 定义
function Connect(
  const AHost: string;
  APort: Word;
  ATimeout: Integer;
  AUseSSL: Boolean
): Boolean;

// ✅ 调用 - 清晰
LResult := Connect(
  AHost := 'example.com',
  APort := 443,
  ATimeout := 30000,
  AUseSSL := True
);
```

### 4.3 返回值设计

#### 使用专门的结果类型
避免使用通用类型作为返回值：

```pascal
// ✅ 好 - 专门的结果类型
type
  THashResult = record
    Success: Boolean;
    Hash: TBytes;
    Algorithm: THashAlgorithm;
    ErrorMessage: string;
  end;

function CalculateHash(const AData: TBytes): THashResult;

// ❌ 差 - 通用类型，错误时返回nil
function CalculateHash(const AData: TBytes): TBytes;  // 错误时返回什么？
```

#### 错误处理策略
明确且一致的错误处理：

```pascal
// 策略1: 异常（推荐用于库代码）
function Encrypt(const AData: TBytes): TBytes;  
// 抛出 ESSLCryptoError 异常

// 策略2: 布尔返回值（用于可选操作）
function TryEncrypt(const AData: TBytes; out AResult: TBytes): Boolean;
// 成功返回True，失败返回False

// 策略3: 结果类型（用于复杂场景）
function EncryptEx(const AData: TBytes): TEncryptResult;
// 返回详细结果信息
```

---

## 5. 文档注释

### 5.1 XML文档注释格式

#### 单元头注释

```pascal
{**
 * Unit: fafafa.ssl.crypto.utils
 * Purpose: 加密工具类，提供常用加密操作
 * 
 * Dependencies:
 *   - OpenSSL 1.1.1+ or 3.0+
 *   - fafafa.ssl.exceptions
 * 
 * @author fafafa.ssl team
 * @version 1.0.0
 * @since 2025-11-26
 *}
unit fafafa.ssl.crypto.utils;
```

#### 类注释

```pascal
{**
 * 加密工具类
 * 
 * 提供常用的加密、解密和哈希功能，包括：
 * - AES-256-GCM/CBC 加密
 * - SHA-256/SHA-512 哈希
 * - 安全随机数生成
 * 
 * 线程安全性: 所有方法都是线程安全的（类方法）
 * 
 * @example
 * <code>
 *   var LHash: TBytes;
 *   LHash := TCryptoUtils.SHA256('Hello World');
 * </code>
 *}
type
  TCryptoUtils = class
```

#### 方法注释

```pascal
{**
 * 使用AES-256-GCM算法加密数据
 * 
 * GCM模式提供认证加密，确保数据的机密性和完整性。
 * 输出包含密文和16字节认证标签。
 * 
 * @param AData 要加密的明文数据
 * @param AKey 32字节(256位)加密密钥
 * @param AIV 12字节初始化向量（推荐长度）
 * @param AAAD 附加认证数据（可选），不会被加密但会被认证
 * 
 * @return 密文+16字节认证标签
 * 
 * @raises ESSLInvalidArgument 密钥长度不是32字节
 * @raises ESSLInvalidArgument IV长度不是12字节
 * @raises ESSLCryptoError OpenSSL加密操作失败
 * 
 * @see AES_GCM_Decrypt
 * 
 * @example
 * <code>
 *   var
 *     LKey: TBytes;
 *     LIV: TBytes;
 *     LCiphertext: TBytes;
 *   begin
 *     LKey := TCryptoUtils.GenerateKey(256);
 *     LIV := TCryptoUtils.GenerateIV(12);
 *     LCiphertext := TCryptoUtils.AES_GCM_Encrypt(
 *       TEncoding.UTF8.GetBytes('Secret'),
 *       LKey,
 *       LIV
 *     );
 *   end;
 * </code>
 *}
class function AES_GCM_Encrypt(
  const AData, AKey, AIV: TBytes;
  const AAAD: TBytes = nil
): TBytes;
```

### 5.2 内联注释

```pascal
procedure ProcessData;
var
  LBuffer: TBytes;
  LContext: PEVP_MD_CTX;
begin
  // 分配缓冲区 - 包含数据和填充空间
  SetLength(LBuffer, DATA_SIZE + PADDING_SIZE);
  
  // 创建OpenSSL上下文
  // 注意：必须在try-finally中释放
  LContext := EVP_MD_CTX_new();
  try
    // 初始化哈希计算
    if EVP_DigestInit_ex(LContext, EVP_sha256(), nil) <> 1 then
      raise ESSLCryptoError.Create('Failed to initialize digest');
      
    // TODO: 添加流式处理支持
    // FIXME: 处理大数据时可能内存不足
    
  finally
    EVP_MD_CTX_free(LContext);
  end;
end;
```

---

## 6. 异常处理

### 6.1 异常层次结构

```pascal
ESSLException (base)
  ├─ ESSLInitError          // 初始化错误
  ├─ ESSLCryptoError        // 加密操作错误
  │   ├─ ESSLEncryptError   // 加密错误
  │   └─ ESSLDecryptError   // 解密错误
  ├─ ESSLCertError          // 证书错误
  ├─ ESSLNetworkError       // 网络错误
  ├─ ESSLInvalidArgument    // 参数无效
  └─ ESSLSystemError        // 系统错误
```

### 6.2 异常使用原则

```pascal
// 1. 使用具体的异常类型
if Length(AKey) <> 32 then
  raise ESSLInvalidArgument.Create('AES-256 requires 32-byte key');

// 2. 提供详细的错误消息
if LResult <> 1 then
  raise ESSLCryptoError.CreateFmt(
    'EVP_EncryptInit_ex failed for algorithm %s',
    [LAlgorithmName]
  );

// 3. 包含错误码（如果有）
if LResult <> 1 then
  raise ESSLCryptoError.CreateWithCode(
    'OpenSSL operation failed',
    ERR_get_error()
  );

// 4. 捕获并重新抛出时添加上下文
try
  InternalOperation();
except
  on E: Exception do
    raise ESSLCryptoError.CreateFmt(
      'Encryption failed in %s: %s',
      [Self.ClassName, E.Message]
    );
end;
```

### 6.3 资源管理

```pascal
// 所有资源都必须使用 try-finally
procedure ProcessData;
var
  LContext: PEVP_MD_CTX;
  LFile: TFileStream;
begin
  LContext := EVP_MD_CTX_new();
  if LContext = nil then
    raise ESSLCryptoError.Create('Failed to allocate context');
    
  try
    LFile := TFileStream.Create(AFileName, fmOpenRead);
    try
      // 使用资源
      ProcessWithContextAndFile(LContext, LFile);
    finally
      LFile.Free;
    end;
  finally
    EVP_MD_CTX_free(LContext);
  end;
end;
```

---

## 7. 类型定义

### 7.1 枚举类型

```pascal
// 使用完整名称，避免冲突
type
  THashAlgorithm = (
    HASH_SHA256,
    HASH_SHA512,
    HASH_SHA3_256,
    HASH_SHA3_512
  );
  
  TEncryptionMode = (
    ENCRYPT_AES_GCM,
    ENCRYPT_AES_CBC,
    ENCRYPT_CHACHA20
  );

// 提供转换函数
function HashAlgorithmToString(AAlgorithm: THashAlgorithm): string;
function StringToHashAlgorithm(const AName: string): THashAlgorithm;
```

### 7.2 记录类型

```pascal
// 使用记录封装相关数据
type
  TEncryptionResult = record
    Success: Boolean;
    Ciphertext: TBytes;
    Tag: TBytes;
    ErrorMessage: string;
    
    class function CreateSuccess(
      const ACiphertext, ATag: TBytes
    ): TEncryptionResult; static;
    
    class function CreateError(
      const AMessage: string
    ): TEncryptionResult; static;
  end;
```

### 7.3 类型别名

```pascal
// 提供有意义的类型别名
type
  TEncryptionKey = TBytes;
  TInitializationVector = TBytes;
  THashValue = TBytes;
  TCiphertext = TBytes;
  
// 但避免过度使用，保持清晰
```

---

## 8. 测试要求

### 8.1 单元测试

```pascal
// 每个公开方法必须有测试
procedure TestAES_GCM_Encrypt_Normal;
procedure TestAES_GCM_Encrypt_EmptyData;
procedure TestAES_GCM_Encrypt_InvalidKey;
procedure TestAES_GCM_Encrypt_InvalidIV;

// 测试命名: Test[方法名]_[场景]
```

### 8.2 测试覆盖

- 正常路径测试
- 边界条件测试
- 错误条件测试
- 性能测试（关键方法）

---

## 9. 版本管理

### 9.1 版本号

遵循语义化版本：`MAJOR.MINOR.PATCH`

```
1.0.0 - 初始发布
1.1.0 - 添加新功能
1.1.1 - Bug修复
2.0.0 - 破坏性变更
```

### 9.2 变更日志

每个版本维护 CHANGELOG.md

---

## 10. 代码审查清单

### 提交前检查

- [ ] 所有标识符遵循命名约定
- [ ] 所有公开方法有XML文档注释
- [ ] 所有资源使用try-finally
- [ ] 所有参数经过验证
- [ ] 错误使用具体异常类型
- [ ] 没有编译警告
- [ ] 单元测试通过
- [ ] 代码格式化一致

---

## 附录A: 快速参考

### 标识符前缀速查表

| 前缀 | 用途 | 示例 |
|------|------|------|
| T | 类/类型 | TCryptoUtils |
| E | 异常 | ESSLException |
| I | 接口 | ICryptoProvider |
| P | 指针 | PEVP_MD_CTX |
| A | 参数 | AData |
| L | 局部变量 | LBuffer |
| F | 字段 | FInitialized |
| G | 全局 | GConfiguration |

### 常见命名模式

| 模式 | 示例 |
|------|------|
| 动词+名词 | CalculateHash, ValidateKey |
| Is/Has/Can | IsValid, HasPermission, CanEncrypt |
| Get/Set | GetValue, SetOptions |
| Create/Destroy | CreateContext, DestroyHandle |
| Load/Save | LoadCertificate, SaveKey |
| Try前缀 | TryParse, TryConnect |

---

**本规范为强制执行标准，所有代码必须符合要求。**
