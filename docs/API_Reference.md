# fafafa.ssl API 参考文档

## TCertificateUtils

企业级证书工具类。所有方法为静态方法，线程安全。

---

### 证书生成

#### GenerateSelfSigned

生成自签名证书。

```pascal
class function GenerateSelfSigned(
  const AOptions: TCertGenOptions;
  out ACertPEM, AKeyPEM: string
): Boolean;
```

**参数**:
- `AOptions`: 证书生成选项
- `ACertPEM`: [out] 生成的证书PEM
- `AKeyPEM`: [out] 生成的私钥PEM

**返回**: 成功返回True

**异常**: `ESSLCertError` 生成失败

**示例**:
```pascal
var
  LOptions: TCertGenOptions;
  LCert, LKey: string;
begin
  LOptions := TCertificateUtils.DefaultGenOptions;
  LOptions.CommonName := 'example.com';
  LOptions.ValidDays := 365;
  
  if TCertificateUtils.GenerateSelfSigned(LOptions, LCert, LKey) then
    WriteLn('Success!');
end;
```

---

#### GenerateSigned

生成CA签名的证书。

```pascal
class function GenerateSigned(
  const AOptions: TCertGenOptions;
  const ACA_CertPEM, ACA_KeyPEM: string;
  out ACertPEM, AKeyPEM: string
): Boolean;
```

**参数**:
- `AOptions`: 证书生成选项
- `ACA_CertPEM`: CA证书PEM
- `ACA_KeyPEM`: CA私钥PEM
- `ACertPEM`: [out] 生成的证书PEM
- `AKeyPEM`: [out] 生成的私钥PEM

**返回**: 成功返回True

**异常**: `ESSLCertError` 生成失败或CA证书无效

---

#### DefaultGenOptions

获取默认生成选项。

```pascal
class function DefaultGenOptions: TCertGenOptions;
```

**返回**: 预配置的选项（RSA 2048位，365天有效期）

---

### 证书加载/保存

#### LoadFromFile

从文件加载证书PEM。

```pascal
class function LoadFromFile(const AFileName: string): string;
```

**参数**:
- `AFileName`: 证书文件路径

**返回**: 证书PEM字符串

**异常**: `EStreamError` 文件读取失败

---

#### SaveToFile

保存证书PEM到文件。

```pascal
class function SaveToFile(const AFileName, ACertPEM: string): Boolean;
```

**参数**:
- `AFileName`: 目标文件路径
- `ACertPEM`: 证书PEM内容

**返回**: 成功返回True

---

### 证书信息

#### GetInfo

提取证书详细信息。

```pascal
class function GetInfo(const ACertPEM: string): TCertInfo;
```

**参数**:
- `ACertPEM`: 证书PEM

**返回**: `TCertInfo`记录，包含：
- `Subject`: 主题DN
- `Issuer`: 颁发者DN
- `SerialNumber`: 序列号
- `NotBefore`, `NotAfter`: 有效期
- `PublicKeyType`, `PublicKeyBits`: 公钥信息
- `SignatureAlgorithm`: 签名算法
- `SubjectAltNames`: SAN列表（需手动释放）
- `KeyUsage`: 密钥用途
- `IsCA`: 是否CA证书
- `Version`: X.509版本

**注意**: 返回的`SubjectAltNames`为TStringList，使用后需手动释放。

**示例**:
```pascal
var
  LInfo: TCertInfo;
begin
  LInfo := TCertificateUtils.GetInfo(LCertPEM);
  WriteLn(LInfo.Subject);
  
  if Assigned(LInfo.SubjectAltNames) then
    LInfo.SubjectAltNames.Free;  // 重要！
end;
```

---

#### GetFingerprint

计算证书SHA256指纹。

```pascal
class function GetFingerprint(const ACertPEM: string): string;
```

**参数**:
- `ACertPEM`: 证书PEM

**返回**: 64字符十六进制指纹（小写）

**示例**: `'a1b2c3d4...'`

---

#### IsValid

检查证书当前是否有效。

```pascal
class function IsValid(const ACertPEM: string): Boolean;
```

**参数**:
- `ACertPEM`: 证书PEM

**返回**: 当前时间在有效期内返回True

---

### 格式转换

#### PEMToDER

PEM转DER格式。

```pascal
class function PEMToDER(const APEM: string): TBytes;
```

**参数**:
- `APEM`: 证书PEM字符串

**返回**: DER格式字节数组

**异常**: 解析失败返回空数组

---

#### DERToPEM

DER转PEM格式。

```pascal
class function DERToPEM(const ADER: TBytes): string;
```

**参数**:
- `ADER`: DER格式字节数组

**返回**: PEM字符串

**异常**: 解析失败返回空字符串

---

#### ConvertFormat

通用格式转换。

```pascal
class function ConvertFormat(
  const AInput: TBytes;
  AFromFormat, AToFormat: TCertFormat
): TBytes;
```

**参数**:
- `AInput`: 输入数据
- `AFromFormat`: 源格式（`cfPEM` 或 `cfDER`）
- `AToFormat`: 目标格式

**返回**: 转换后的数据

---

### DN比较

#### CompareX509Names

比较两个X.509 DN。

```pascal
class function CompareX509Names(
  const AName1, AName2: string;
  ACaseInsensitive: Boolean = True
): Boolean;
```

**参数**:
- `AName1`, `AName2`: DN字符串
- `ACaseInsensitive`: 大小写不敏感（默认True）

**返回**: DN等价返回True

**功能**:
- 自动规范化空格
- 组件顺序无关
- 可选大小写敏感

**示例**:
```pascal
// 顺序不同但等价
CompareX509Names('CN=Test,O=Org', 'O=Org,CN=Test')  // True

// 大小写不敏感
CompareX509Names('CN=test', 'CN=TEST')  // True (默认)
CompareX509Names('CN=test', 'CN=TEST', False)  // False
```

---

### 证书链验证

#### VerifyChain

验证证书链。

```pascal
class function VerifyChain(
  const ACertPEM: string;
  const ACAPath: string = ''
): Boolean;
```

**参数**:
- `ACertPEM`: 证书PEM（可包含中间证书）
- `ACAPath`: 可信CA路径（文件或目录）

**返回**: 验证通过返回True

**功能**:
- 支持PEM bundle（leaf + intermediates）
- 自动提取中间证书
- 验证签名链
- 检查有效期

**示例**:
```pascal
// 单个证书
VerifyChain(LeafCert, '/etc/ssl/certs')

// 证书链bundle
VerifyChain(LeafCert + IntermediateCert, '/path/to/root.pem')
```

---

## TCertGenOptions

证书生成选项记录。

```pascal
type
  TCertGenOptions = record
    CommonName: string;           // CN
    Organization: string;         // O
    OrganizationalUnit: string;   // OU
    Country: string;              // C (2字母代码)
    State: string;                // ST
    Locality: string;             // L
    ValidDays: Integer;           // 有效天数（默认365）
    KeyType: TKeyType;            // ktRSA, ktECDSA, ktEd25519
    KeyBits: Integer;             // RSA位数（2048/4096）
    ECCurve: string;              // EC曲线（'prime256v1'等）
    SubjectAltNames: TStringList; // SAN列表
    IsCA: Boolean;                // 是否CA证书
    SerialNumber: Int64;          // 序列号（0=自动）
  end;
```

---

## TCertInfo

证书信息记录。

```pascal
type
  TCertInfo = record
    Subject: string;
    Issuer: string;
    SerialNumber: string;
    NotBefore: TDateTime;
    NotAfter: TDateTime;
    PublicKeyType: string;
    PublicKeyBits: Integer;
    SignatureAlgorithm: string;
    SubjectAltNames: TStringList;  // 需手动释放！
    KeyUsage: string;
    IsCA: Boolean;
    Version: Integer;
  end;
```

**重要**: `SubjectAltNames`字段为对象引用，使用后必须手动释放。

---

## 异常类型

### ESSLCertError

证书操作错误。

```pascal
type
  ESSLCertError = class(Exception);
```

**触发场景**:
- 证书生成失败
- PEM解析失败
- 签名验证失败
- OpenSSL操作错误

**处理示例**:
```pascal
try
  TCertificateUtils.GenerateSelfSigned(Options, Cert, Key);
except
  on E: ESSLCertError do
    WriteLn('Certificate error: ', E.Message);
end;
```

---

## 最佳实践

### 1. 内存管理

```pascal
// ✅ 正确
var LInfo: TCertInfo;
begin
  LInfo := TCertificateUtils.GetInfo(CertPEM);
  // 使用LInfo...
  if Assigned(LInfo.SubjectAltNames) then
    LInfo.SubjectAltNames.Free;  // 释放
end;

// ❌ 错误：忘记释放
LInfo := TCertificateUtils.GetInfo(CertPEM);
// 内存泄漏！
```

### 2. 错误处理

```pascal
// ✅ 正确：捕获异常
try
  if not TCertificateUtils.GenerateSelfSigned(...) then
    WriteLn('Generation failed');
except
  on E: ESSLCertError do
    WriteLn('Error: ', E.Message);
end;
```

### 3. 线程安全

所有TCertificateUtils方法线程安全，可在多线程环境使用：

```pascal
// ✅ 安全：多线程调用
TThread.CreateAnonymousThread(procedure
begin
  LInfo := TCertificateUtils.GetInfo(CertPEM);
  // ...
end).Start;
```

---

## 相关文档

- [快速入门](QuickStart.md)
- [FAQ](FAQ.md)
- [示例程序](../examples/)
