# Design Document: SSH Key Support

## Overview

本设计文档描述 fafafa.ssl 库的 SSH 密钥支持功能实现。该功能将提供完整的 SSH 密钥管理能力，包括读取、生成、转换和验证 SSH 密钥对。

设计遵循 fafafa.ssl 现有的架构模式：
- 使用接口（Interface）定义 API 契约
- 提供 Result 类型用于错误处理
- 支持 Try 方法（不抛异常版本）
- 与现有 PEM 和加密模块集成

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                        Application Layer                         │
├─────────────────────────────────────────────────────────────────┤
│                     fafafa.ssl.ssh.pas                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ TSSHKeyManager  │  │ TSSHPublicKey   │  │ TSSHPrivateKey  │  │
│  │ (Factory/Utils) │  │ (Public Key)    │  │ (Private Key)   │  │
│  └────────┬────────┘  └────────┬────────┘  └────────┬────────┘  │
│           │                    │                    │           │
│  ┌────────┴────────────────────┴────────────────────┴────────┐  │
│  │                    ISSHKeyPair Interface                   │  │
│  └────────────────────────────────────────────────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                     Internal Components                          │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ SSH Format      │  │ Key Generation  │  │ Fingerprint     │  │
│  │ Parser/Writer   │  │ (via OpenSSL)   │  │ Calculator      │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                     Existing Modules                             │
│  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  │
│  │ fafafa.ssl.pem  │  │ fafafa.ssl.     │  │ fafafa.ssl.     │  │
│  │ (PEM Parser)    │  │ crypto.utils    │  │ encoding        │  │
│  └─────────────────┘  └─────────────────┘  └─────────────────┘  │
├─────────────────────────────────────────────────────────────────┤
│                     OpenSSL Backend                              │
│  ┌─────────────────────────────────────────────────────────────┐│
│  │ fafafa.ssl.openssl.api.* (RSA, EC, EVP, PEM)                ││
│  └─────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────┘
```

## Components and Interfaces

### 1. SSH 密钥类型枚举

```pascal
type
  TSSHKeyType = (
    sshKeyUnknown,
    sshKeyRSA,           // ssh-rsa
    sshKeyEd25519,       // ssh-ed25519
    sshKeyECDSA_P256,    // ecdsa-sha2-nistp256
    sshKeyECDSA_P384,    // ecdsa-sha2-nistp384
    sshKeyECDSA_P521     // ecdsa-sha2-nistp521
  );

  TSSHKeyFormat = (
    sshFormatOpenSSH,    // OpenSSH native format
    sshFormatPEM,        // PEM (PKCS#1/PKCS#8)
    sshFormatPKCS8       // PKCS#8 specifically
  );
```

### 2. SSH 公钥接口

```pascal
type
  ISSHPublicKey = interface
    ['{SSH-PUB-KEY-GUID}']
    function GetKeyType: TSSHKeyType;
    function GetKeyData: TBytes;
    function GetComment: string;
    procedure SetComment(const AComment: string);
    
    // 导出
    function ToOpenSSHFormat: string;
    function ToPEMFormat: string;
    function ToBytes: TBytes;  // Raw key blob
    
    // 指纹
    function GetFingerprintSHA256: string;  // SHA256:base64...
    function GetFingerprintMD5: string;     // MD5:xx:xx:xx...
    
    // 验证
    function IsValid: Boolean;
    function GetKeySize: Integer;  // bits
  end;
```

### 3. SSH 私钥接口

```pascal
type
  ISSHPrivateKey = interface
    ['{SSH-PRIV-KEY-GUID}']
    function GetKeyType: TSSHKeyType;
    function GetPublicKey: ISSHPublicKey;
    function IsEncrypted: Boolean;
    
    // 导出
    function ToOpenSSHFormat(const APassphrase: string = ''): string;
    function ToPEMFormat(const APassphrase: string = ''): string;
    
    // 验证
    function IsValid: Boolean;
    function MatchesPublicKey(APubKey: ISSHPublicKey): Boolean;
  end;
```

### 4. SSH 密钥对接口

```pascal
type
  ISSHKeyPair = interface
    ['{SSH-KEY-PAIR-GUID}']
    function GetPublicKey: ISSHPublicKey;
    function GetPrivateKey: ISSHPrivateKey;
    
    // 保存到文件
    procedure SaveToFiles(const APrivateKeyPath, APublicKeyPath: string;
      const APassphrase: string = '');
    
    // 验证配对
    function IsValidPair: Boolean;
  end;
```

### 5. SSH 密钥管理器

```pascal
type
  TSSHKeyManager = class
  public
    // 解析公钥
    class function ParsePublicKey(const AKeyString: string): ISSHPublicKey;
    class function ParsePublicKeyFile(const AFileName: string): ISSHPublicKey;
    class function TryParsePublicKey(const AKeyString: string; 
      out AKey: ISSHPublicKey): Boolean;
    
    // 解析私钥
    class function ParsePrivateKey(const AKeyData: string; 
      const APassphrase: string = ''): ISSHPrivateKey;
    class function ParsePrivateKeyFile(const AFileName: string;
      const APassphrase: string = ''): ISSHPrivateKey;
    class function TryParsePrivateKey(const AKeyData: string;
      const APassphrase: string; out AKey: ISSHPrivateKey): Boolean;
    
    // 生成密钥对
    class function GenerateRSAKeyPair(ABits: Integer = 4096;
      const AComment: string = ''): ISSHKeyPair;
    class function GenerateEd25519KeyPair(
      const AComment: string = ''): ISSHKeyPair;
    class function GenerateECDSAKeyPair(AKeyType: TSSHKeyType;
      const AComment: string = ''): ISSHKeyPair;
    
    // 格式转换
    class function ConvertToOpenSSH(AKey: ISSHPrivateKey): string;
    class function ConvertToPEM(AKey: ISSHPrivateKey): string;
    class function ConvertPublicKeyToOpenSSH(AKey: ISSHPublicKey): string;
    class function ConvertPublicKeyToPEM(AKey: ISSHPublicKey): string;
    
    // 指纹计算
    class function CalculateFingerprint(AKey: ISSHPublicKey;
      ASHA256: Boolean = True): string;
    
    // 密钥验证
    class function ValidateKeyPair(APubKey: ISSHPublicKey;
      APrivKey: ISSHPrivateKey): Boolean;
    class function IsWeakKey(AKey: ISSHPublicKey): Boolean;
    
    // authorized_keys 处理
    class function ParseAuthorizedKeys(const AContent: string): TArray<ISSHPublicKey>;
    class function WriteAuthorizedKeys(const AKeys: array of ISSHPublicKey): string;
  end;
```

### 6. authorized_keys 条目

```pascal
type
  TSSHAuthorizedKeyEntry = record
    Options: string;           // command=, from=, no-pty, etc.
    Key: ISSHPublicKey;
    
    function ToString: string;
    class function Parse(const ALine: string): TSSHAuthorizedKeyEntry; static;
  end;
```

## Data Models

### OpenSSH 公钥格式

```
<key-type> <base64-encoded-key-blob> [comment]

Example:
ssh-rsa AAAAB3NzaC1yc2EAAA... user@host
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAA... user@host
```

### OpenSSH 私钥格式 (openssh-key-v1)

```
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAA...
-----END OPENSSH PRIVATE KEY-----
```

内部结构：
```
"openssh-key-v1\0"           # Auth magic
uint32  ciphername length
string  ciphername           # "none" or "aes256-ctr"
uint32  kdfname length
string  kdfname              # "none" or "bcrypt"
uint32  kdfoptions length
string  kdfoptions           # KDF parameters (salt, rounds)
uint32  number of keys       # Usually 1
uint32  public key length
string  public key blob
uint32  private section length
string  private section      # Encrypted if passphrase set
```

### SSH 公钥 Blob 格式

```
RSA:
  uint32  "ssh-rsa" length
  string  "ssh-rsa"
  mpint   e (public exponent)
  mpint   n (modulus)

Ed25519:
  uint32  "ssh-ed25519" length
  string  "ssh-ed25519"
  uint32  public key length (32)
  bytes   public key (32 bytes)

ECDSA:
  uint32  key type length
  string  key type (e.g., "ecdsa-sha2-nistp256")
  uint32  curve name length
  string  curve name (e.g., "nistp256")
  uint32  public key length
  bytes   public key (EC point)
```

## Correctness Properties

*A property is a characteristic or behavior that should hold true across all valid executions of a system—essentially, a formal statement about what the system should do. Properties serve as the bridge between human-readable specifications and machine-verifiable correctness guarantees.*

### Property 1: Public Key Parse-Export Round Trip

*For any* valid SSH public key string, parsing it and then exporting it back to OpenSSH format SHALL produce an equivalent string (same key type, key data, and comment).

**Validates: Requirements 1.1, 4.1**

### Property 2: Private Key Parse-Export Round Trip

*For any* valid SSH private key (encrypted or unencrypted), parsing it with the correct passphrase and then exporting it SHALL produce a key that can be parsed again to yield equivalent key data.

**Validates: Requirements 2.1, 2.2, 2.3, 2.5, 4.2, 4.3**

### Property 3: Invalid Input Error Handling

*For any* malformed or invalid SSH key string, the parser SHALL return an error result (not crash or return invalid data).

**Validates: Requirements 1.3, 2.4**

### Property 4: Key Generation Validity

*For any* generated SSH key pair (RSA, Ed25519, or ECDSA), the public and private keys SHALL form a valid pair, and the key parameters SHALL match the requested configuration (bit size, curve).

**Validates: Requirements 3.1, 3.2, 3.3, 3.4, 3.5, 7.1, 7.2**

### Property 5: Format Conversion Round Trip

*For any* SSH key, converting from OpenSSH format to PEM format and back SHALL preserve the key data (the resulting key SHALL be functionally equivalent).

**Validates: Requirements 6.1, 6.2, 6.3, 6.4, 6.5**

### Property 6: Fingerprint Consistency

*For any* SSH public key, the calculated fingerprint SHALL be deterministic (same key always produces same fingerprint) and SHALL follow the correct format (SHA256:base64 or MD5:hex:colon:separated).

**Validates: Requirements 5.1, 5.2, 5.3, 5.4**

### Property 7: Key Pair Validation Correctness

*For any* public key and private key, the validation function SHALL return true if and only if they form a mathematically valid pair (the private key can sign data that the public key can verify).

**Validates: Requirements 7.1, 7.3**

### Property 8: authorized_keys Round Trip

*For any* valid authorized_keys file content, parsing it and writing it back SHALL preserve all valid entries (key data and options), while invalid entries are skipped with warnings.

**Validates: Requirements 8.1, 8.2, 8.3, 8.4**

## Error Handling

### 错误类型

```pascal
type
  TSSHKeyError = (
    sshKeyErrNone,
    sshKeyErrInvalidFormat,      // 格式无效
    sshKeyErrUnsupportedKeyType, // 不支持的密钥类型
    sshKeyErrDecryptionFailed,   // 解密失败（密码错误）
    sshKeyErrWeakKey,            // 弱密钥
    sshKeyErrKeyMismatch,        // 密钥不匹配
    sshKeyErrFileNotFound,       // 文件未找到
    sshKeyErrPermissionDenied,   // 权限拒绝
    sshKeyErrInvalidPassphrase,  // 密码无效
    sshKeyErrGenerationFailed    // 生成失败
  );
```

### Result 类型

```pascal
type
  TSSHKeyResult = record
    Success: Boolean;
    ErrorCode: TSSHKeyError;
    ErrorMessage: string;
    
    class function Ok: TSSHKeyResult; static;
    class function Err(ACode: TSSHKeyError; const AMsg: string): TSSHKeyResult; static;
  end;

  TSSHPublicKeyResult = record
    Success: Boolean;
    Key: ISSHPublicKey;
    ErrorCode: TSSHKeyError;
    ErrorMessage: string;
    
    class function Ok(AKey: ISSHPublicKey): TSSHPublicKeyResult; static;
    class function Err(ACode: TSSHKeyError; const AMsg: string): TSSHPublicKeyResult; static;
  end;
```

## Testing Strategy

### 单元测试

单元测试用于验证特定示例和边界情况：

1. **已知密钥测试** - 使用预生成的测试密钥验证解析正确性
2. **格式验证测试** - 验证输出格式符合 OpenSSH 规范
3. **错误处理测试** - 验证各种错误情况的处理
4. **边界情况测试** - 空输入、超长输入、特殊字符等

### 属性测试

使用 FPCUnit 的属性测试扩展，每个属性测试运行至少 100 次迭代：

1. **Property 1**: 生成随机有效公钥 → 解析 → 导出 → 比较
2. **Property 2**: 生成随机私钥 → 解析 → 导出 → 重新解析 → 比较
3. **Property 3**: 生成随机无效输入 → 验证返回错误
4. **Property 4**: 生成密钥对 → 验证参数 → 验证配对
5. **Property 5**: 生成密钥 → OpenSSH→PEM→OpenSSH → 比较
6. **Property 6**: 生成密钥 → 计算指纹两次 → 比较
7. **Property 7**: 生成配对/不配对密钥 → 验证结果正确
8. **Property 8**: 生成 authorized_keys → 解析 → 写入 → 比较

### 测试框架配置

```pascal
// 属性测试标签格式
// **Feature: ssh-key-support, Property N: <property_text>**
```
