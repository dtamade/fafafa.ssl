# PKCS12 模块使用指南

## 概述

PKCS#12 是一种用于存储和传输私钥、证书和其他敏感信息的标准格式。通常使用 `.p12` 或 `.pfx` 文件扩展名。fafafa.ssl 项目通过 OpenSSL 后端提供了完整的 PKCS#12 支持。

本指南讨论的是 OpenSSL backend 暴露的 PKCS#12 helper/API surface，不代表所有 backend 都发布同等能力。

- `OpenSSL`: 提供完整 PKCS#12 helper/API surface
- `WinSSL`: 仅发布 PFX/P12 import path，不提供本指南中的 helper/API surface
- `FreePascal` / `MbedTLS` / `WolfSSL`: 当前不发布 PKCS#12 bundle create / parse / import surface

## 推荐入口

推荐入口分两层：

- 高入口 helper：`fafafa.ssl` / `TPKCS12Manager` / `DefaultPKCS12Options`
  - 适合当前项目代码直接做 PKCS#12 导入/导出
- OpenSSL raw API：`fafafa.ssl.openssl.api.pkcs12` + `fafafa.ssl.openssl.api.pem`
  - 适合需要直接操作 `PPKCS12` / `PX509` / `PEVP_PKEY` 的低层场景

## 功能特性

### ✅ 已实现功能

- **证书和私钥打包**：将证书、私钥和证书链打包到单个文件
- **密码保护**：使用密码加密保护 PKCS#12 文件
- **证书链管理**：支持完整的证书链导入导出
- **解析和提取**：从 PKCS#12 文件中提取证书和私钥
- **多种加密算法**：支持不同的加密算法和密钥派生函数
- **I/O 操作**：文件读写和内存操作

### 📊 测试覆盖

- **综合测试通过率**：100% (34/34 测试)
- **创建解析测试**：100% 通过
- **工作流测试**：100% 通过
- **OpenSSL 兼容性**：支持 OpenSSL 1.1.1+ 和 3.x
- **测试文件**：
  - `tests/certificate/test_p2_pkcs12_comprehensive.pas` - 综合功能测试
  - `tests/certificate/test_p2_pkcs12_create_parse.pas` - 创建解析测试
  - `tests/certificate/test_pkcs12_workflow.pas` - 工作流测试
  - `tests/unit/test_pkcs12*.pas` - 单元测试

## API 参考

### 核心函数

#### 创建和释放

```pascal
function PKCS12_new: PPKCS12;
procedure PKCS12_free(p12: PPKCS12);
```

#### 创建 PKCS12 文件

```pascal
function PKCS12_create(
  pass: PAnsiChar;          // 密码
  name: PAnsiChar;          // 友好名称
  pkey: PEVP_PKEY;         // 私钥
  cert: PX509;             // 证书
  ca: PSTACK_OF_X509;      // 证书链（可选）
  nid_key: cint;           // 密钥加密算法
  nid_cert: cint;          // 证书加密算法
  iter: cint;              // 迭代次数
  mac_iter: cint;          // MAC 迭代次数
  keytype: cint            // 密钥类型
): PPKCS12;
```

#### 解析 PKCS12 文件

```pascal
function PKCS12_parse(
  p12: PPKCS12;            // PKCS12 结构
  pass: PAnsiChar;         // 密码
  pkey: PPEVP_PKEY;        // 输出：私钥
  cert: PPX509;            // 输出：证书
  ca: PPSTACK_OF_X509      // 输出：证书链
): cint;
```

#### I/O 操作

```pascal
function i2d_PKCS12_bio(bp: PBIO; p12: PPKCS12): cint;
function d2i_PKCS12_bio(bp: PBIO; p12: PPPKCS12): PPKCS12;
function i2d_PKCS12_fp(fp: PFile; p12: PPKCS12): cint;
function d2i_PKCS12_fp(fp: PFile; p12: PPPKCS12): PPKCS12;
```

### 常量定义

#### 加密算法 NID

```pascal
const
  NID_pbe_WithSHA1And3_Key_TripleDES_CBC = 146;  // 3DES-CBC
  NID_pbe_WithSHA1And40BitRC2_CBC = 148;         // RC2-40-CBC
  NID_pbe_WithSHA1And128BitRC4 = 144;            // RC4-128
  NID_pbe_WithSHA1And40BitRC4 = 145;             // RC4-40
```

#### 密钥类型

```pascal
const
  KEY_SIG = 1;    // 签名密钥
  KEY_EX = 2;     // 密钥交换
```

## 使用示例

### 示例 1：通过高入口 helper 创建 PKCS12 文件

参考文件：

- `tests/unit/test_pkcs12_full.pas`
- `tests/test_cert_advanced_pkcs12_bio_contract.pas`

```pascal
uses
  SysUtils,
  fafafa.ssl,
  fafafa.ssl.cert;

var
  KeyPair: IKeyPairWithCertificate;
  Options: TPKCS12Options;
begin
  KeyPair := TCertificate.CreateSelfSigned('pkcs12-demo.local');

  Options := DefaultPKCS12Options;
  Options.FriendlyName := 'Demo Certificate';
  Options.Password := 'MyStr0ng!P@ssw0rd';
  Options.Iterations := 2048;

  TPKCS12Manager.CreatePKCS12ToFile(
    KeyPair.Certificate,
    KeyPair.PrivateKey,
    'output.p12',
    Options
  );
end;
```

### 示例 2：通过高入口 helper 解析 PKCS12 文件

参考文件：`tests/unit/test_pkcs12_full.pas`

```pascal
uses
  SysUtils,
  fafafa.ssl;

var
  LoadedCert: ICertificate;
  LoadedKey: IPrivateKey;
begin
  if TPKCS12Manager.LoadFromPKCS12File(
    'input.p12',
    'MyStr0ng!P@ssw0rd',
    LoadedCert,
    LoadedKey
  ) then
  begin
    WriteLn('解析成功！');
    WriteLn('证书主题: ', LoadedCert.Subject);
    WriteLn('证书颁发者: ', LoadedCert.Issuer);
  end;
end;
```

### 示例 3：直接使用 OpenSSL raw API

参考文件：

- `tests/certificate/test_p2_pkcs12_create_parse.pas`
- `tests/certificate/test_pkcs12_workflow.pas`

```pascal
uses
  fafafa.ssl.openssl.api.bio,
  fafafa.ssl.openssl.api.evp,
  fafafa.ssl.openssl.api.pem,
  fafafa.ssl.openssl.api.pkcs12,
  fafafa.ssl.openssl.api.x509;

var
  p12: PPKCS12;
  cert: PX509;
  pkey: PEVP_PKEY;
begin
  cert := LoadCertificateFromPEM('cert.pem');
  pkey := LoadPrivateKeyFromPEM('key.pem', '');

  p12 := PKCS12_create(
    'password',
    'My Certificate',
    pkey,
    cert,
    nil,
    0, 0, 0, 0, 0
  );

  if p12 <> nil then
    PKCS12_free(p12);

  if pkey <> nil then
    EVP_PKEY_free(pkey);
  if cert <> nil then
    X509_free(cert);
end;
```

### 示例 4：运行综合测试

```bash
# 编译综合测试
cd /home/dtamade/projects/fafafa.ssl
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib/test \
    tests/certificate/test_p2_pkcs12_comprehensive.pas \
    -o./bin/test_p2_pkcs12_comprehensive

# 运行测试
./bin/test_p2_pkcs12_comprehensive
```

预期输出：
```
=============================================================
PKCS#12 模块综合测试
=============================================================

✅ OpenSSL 库加载成功
✅ PKCS12 模块加载成功

=== 测试 1: PKCS12 基本操作 ===
PKCS12_new 函数加载: PASS
PKCS12_free 函数加载: PASS
...

=============================================================
测试结果总结
=============================================================
总测试数: 34
通过: 34
失败: 0
通过率: 100.0%

🎉 所有测试通过！PKCS#12 模块工作正常
```

## 常见用例

### 用例 1：导出证书和私钥

**场景**：将证书和私钥打包到 PKCS#12 文件中，便于传输和备份。

**步骤**：
1. 加载证书和私钥
2. 使用 `PKCS12_create` 创建 PKCS#12 结构
3. 使用 `i2d_PKCS12_bio` 或 `i2d_PKCS12_fp` 写入文件
4. 清理资源

**参考**：`tests/certificate/test_p2_pkcs12_create_parse.pas`

### 用例 2：导入证书和私钥

**场景**：从 PKCS#12 文件中提取证书和私钥。

**步骤**：
1. 使用 `d2i_PKCS12_bio` 或 `d2i_PKCS12_fp` 读取文件
2. 使用 `PKCS12_parse` 解析并提取证书和私钥
3. 使用提取的证书和私钥
4. 清理资源

**参考**：`tests/certificate/test_pkcs12_workflow.pas`

### 用例 3：证书链管理

**场景**：将完整的证书链（包括中间证书和根证书）打包到 PKCS#12 文件中。

**步骤**：
1. 加载证书、私钥和证书链
2. 创建 `STACK_OF(X509)` 存储证书链
3. 使用 `PKCS12_create` 创建包含证书链的 PKCS#12 结构
4. 写入文件

**参考**：`tests/certificate/test_p2_pkcs12_comprehensive.pas`

## 最佳实践

### 1. 密码保护

始终使用强密码保护 PKCS#12 文件：

```pascal
// 不推荐：空密码或弱密码
p12 := PKCS12_create('', 'name', pkey, cert, nil, 0, 0, 0, 0, 0);

// 推荐：使用强密码
p12 := PKCS12_create('MyStr0ng!P@ssw0rd', 'name', pkey, cert, nil, 0, 0, 0, 0, 0);
```

### 2. 加密算法选择

使用现代加密算法：

```pascal
// 推荐：使用 3DES-CBC（默认）
p12 := PKCS12_create(
  'password', 'name', pkey, cert, nil,
  NID_pbe_WithSHA1And3_Key_TripleDES_CBC,  // 密钥加密
  NID_pbe_WithSHA1And40BitRC2_CBC,         // 证书加密
  2048,                                     // 迭代次数
  1,                                        // MAC 迭代次数
  0
);
```

### 3. 错误处理

始终检查函数返回值：

```pascal
p12 := PKCS12_create('password', 'name', pkey, cert, nil, 0, 0, 0, 0, 0);
if p12 = nil then
begin
  err := ERR_get_error();
  WriteLn('创建失败: ', ERR_error_string(err, nil));
  Exit;
end;
```

### 4. 资源管理

及时释放资源：

```pascal
try
  p12 := PKCS12_create(...);
  // 使用 p12
finally
  PKCS12_free(p12);
end;
```

### 5. 证书链验证

解析后验证证书链：

```pascal
if PKCS12_parse(p12, 'password', @pkey, @cert, @ca) = 1 then
begin
  // 验证证书链
  if ca <> nil then
  begin
    count := sk_X509_num(ca);
    WriteLn('证书链包含 ', count, ' 个证书');
  end;
end;
```

## 故障排除

### 问题 1：解析失败

**可能原因**：
- 密码错误
- 文件损坏
- 不支持的加密算法

**解决方案**：
1. 验证密码正确性
2. 检查文件完整性
3. 使用 OpenSSL 命令行工具验证：`openssl pkcs12 -in file.p12 -info`

### 问题 2：创建失败

**可能原因**：
- 证书或私钥无效
- 内存不足
- 加密算法不支持

**解决方案**：
1. 验证证书和私钥有效性
2. 检查系统资源
3. 使用默认加密算法（传递 0）

### 问题 3：OpenSSL 3.x 兼容性

**说明**：某些加密算法在 OpenSSL 3.x 中已弃用。

**解决方案**：
- 使用默认加密算法（传递 0）
- 参考 `test_p2_pkcs12_comprehensive.pas` 中的兼容性处理

## PKCS#12 vs PEM

| 特性 | PKCS#12 (.p12/.pfx) | PEM (.pem) |
|------|---------------------|------------|
| 格式 | 二进制 | Base64 文本 |
| 内容 | 证书 + 私钥 + 证书链 | 单个证书或私钥 |
| 密码保护 | 内置支持 | 需要额外加密 |
| 跨平台 | 广泛支持 | 广泛支持 |
| 用途 | 传输和备份 | 配置和存储 |

**推荐**：
- 传输和备份：使用 PKCS#12
- 服务器配置：使用 PEM
- 浏览器导入：使用 PKCS#12

## 相关资源

### 测试文件

- `tests/certificate/test_p2_pkcs12_comprehensive.pas` - 综合功能测试
- `tests/certificate/test_p2_pkcs12_create_parse.pas` - 创建解析测试
- `tests/certificate/test_pkcs12_workflow.pas` - 工作流测试
- `tests/unit/test_pkcs12_full.pas` - 高入口 helper 导入导出测试
- `tests/test_cert_advanced_pkcs12_bio_contract.pas` - helper BIO guard / fail-safe 合同
- `tests/unit/test_pkcs12*.pas` - 单元测试

### API 绑定

- `src/fafafa.ssl.openssl.api.pkcs12.pas` - PKCS#12 API 绑定

### 文档

- [OpenSSL PKCS12 文档](https://www.openssl.org/docs/man3.0/man3/PKCS12_create.html)
- [RFC 7292 - PKCS #12: Personal Information Exchange Syntax](https://tools.ietf.org/html/rfc7292)

## 更新日志

### 2026-05-19
- ✅ `PKCS12_USER_GUIDE` 重新锚到当前 helper/raw API truth
- ✅ 高入口 helper 示例改为 `TPKCS12Manager` / `DefaultPKCS12Options`
- ✅ raw API 示例改为 `LoadCertificateFromPEM` / `LoadPrivateKeyFromPEM`

### 2026-01-24
- ✅ PKCS12 综合测试 100% 通过（34/34）
- ✅ PKCS12 创建解析测试 100% 通过
- ✅ PKCS12 工作流测试 100% 通过
- ✅ 验证 OpenSSL 1.x 和 3.x 兼容性
- ✅ 创建 PKCS12 使用指南文档

### 2025-10-24
- ✅ 完成 PKCS12 API 绑定
- ✅ 实现基础测试套件
- ✅ 支持创建、解析、I/O 功能

## 贡献

如果您发现问题或有改进建议，请：
1. 查看现有测试文件了解用法
2. 运行综合测试验证功能
3. 提交 Issue 或 Pull Request

---

**文档版本**: 1.1
**最后更新**: 2026-05-19
**维护者**: fafafa.ssl 项目组
