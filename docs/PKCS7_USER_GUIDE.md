# PKCS#7 模块使用指南

## 概述

PKCS#7（Public-Key Cryptography Standards #7）是一种加密消息语法标准，用于数字签名、数据加密和数据封装。fafafa.ssl 项目通过 OpenSSL 后端提供了完整的 PKCS#7 支持。

## 功能特性

### ✅ 已实现功能

- **数字签名**：对数据进行签名和验证
- **数据加密**：使用接收者公钥加密数据
- **数据解密**：使用私钥解密数据
- **数据封装**：标准的数据容器格式
- **证书管理**：添加证书和 CRL
- **签名者信息**：管理签名者属性
- **I/O 操作**：PEM/DER 格式的序列化和反序列化

### 📊 测试覆盖

- **综合测试通过率**：100% (41/41 测试)
- **OpenSSL 兼容性**：支持 OpenSSL 1.1.1+ 和 3.x
- **测试文件**：
  - `tests/certificate/test_p2_pkcs7_comprehensive.pas` - 综合功能测试
  - `tests/certificate/test_pkcs7_sign_verify_workflow.pas` - 签名验证工作流
  - `tests/certificate/test_p2_pkcs7_*.pas` - 各功能模块测试

## API 参考

### 核心函数

#### 创建和释放

```pascal
function PKCS7_new: PPKCS7;
procedure PKCS7_free(p7: PPKCS7);
```

#### 签名操作

```pascal
function PKCS7_sign(
  signcert: PX509;           // 签名证书
  pkey: PEVP_PKEY;          // 私钥
  certs: PSTACK_OF_X509;    // 额外证书链（可选）
  data: PBIO;               // 要签名的数据
  flags: cint               // 标志位
): PPKCS7;

function PKCS7_verify(
  p7: PPKCS7;               // PKCS7 结构
  certs: PSTACK_OF_X509;    // 验证证书链（可选）
  store: PX509_STORE;       // 证书存储（可选）
  indata: PBIO;             // 原始数据
  out_: PBIO;               // 输出 BIO（可选）
  flags: cint               // 标志位
): cint;
```

#### 加密操作

```pascal
function PKCS7_encrypt(
  certs: PSTACK_OF_X509;    // 接收者证书
  in_: PBIO;                // 要加密的数据
  cipher: PEVP_CIPHER;      // 加密算法
  flags: cint               // 标志位
): PPKCS7;

function PKCS7_decrypt(
  p7: PPKCS7;               // PKCS7 结构
  pkey: PEVP_PKEY;          // 私钥
  cert: PX509;              // 证书
  data: PBIO;               // 输出 BIO
  flags: cint               // 标志位
): cint;
```

#### I/O 操作

```pascal
function i2d_PKCS7_bio(bp: PBIO; p7: PPKCS7): cint;
function d2i_PKCS7_bio(bp: PBIO; p7: PPPKCS7): PPKCS7;
function PEM_write_bio_PKCS7(bp: PBIO; p7: PPKCS7): cint;
function PEM_read_bio_PKCS7(bp: PBIO; x: PPPKCS7; cb: pem_password_cb; u: Pointer): PPKCS7;
```

### 常量定义

#### PKCS7 类型

```pascal
const
  NID_pkcs7_data = 21;                    // 数据
  NID_pkcs7_signed = 22;                  // 签名数据
  NID_pkcs7_enveloped = 23;               // 加密数据
  NID_pkcs7_signedAndEnveloped = 24;      // 签名并加密
  NID_pkcs7_digest = 25;                  // 摘要
  NID_pkcs7_encrypted = 26;               // 加密
```

#### 标志位

```pascal
const
  PKCS7_TEXT = $1;          // 添加文本 MIME 头
  PKCS7_NOCERTS = $2;       // 不包含签名者证书
  PKCS7_NOSIGS = $4;        // 不包含签名
  PKCS7_NOCHAIN = $8;       // 不包含证书链
  PKCS7_NOINTERN = $10;     // 不搜索内部证书
  PKCS7_NOVERIFY = $20;     // 不验证签名者证书
  PKCS7_DETACHED = $40;     // 分离式签名
  PKCS7_BINARY = $80;       // 二进制模式
  PKCS7_NOATTR = $100;      // 不包含认证属性
  PKCS7_NOSMIMECAP = $200;  // 不包含 S/MIME 能力
```

## 使用示例

### 示例 1：基础功能检查

参考文件：`examples/pkcs7_basic_example.pas`

这个示例演示如何：
- 初始化 OpenSSL 库
- 检查 PKCS#7 函数可用性
- 显示 PKCS#7 常量定义

### 示例 2：签名和验证工作流

参考文件：`tests/certificate/test_pkcs7_sign_verify_workflow.pas`

这个完整的测试文件演示了：
- 生成测试证书和密钥
- 对数据进行 PKCS#7 签名
- 验证 PKCS#7 签名
- 处理分离式签名和嵌入式签名

### 示例 3：运行综合测试

```bash
# 编译综合测试
cd /home/dtamade/projects/fafafa.ssl
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib/test \
    tests/certificate/test_p2_pkcs7_comprehensive.pas \
    -o./bin/test_p2_pkcs7_comprehensive

# 运行测试
./bin/test_p2_pkcs7_comprehensive
```

预期输出：
```
=============================================================
PKCS#7 模块综合测试
=============================================================

✅ OpenSSL 库加载成功
✅ PKCS7 模块加载成功

=== 测试 1: PKCS7 基本操作 ===
PKCS7_new 函数加载: PASS
PKCS7_free 函数加载: PASS
...

=============================================================
测试结果总结
=============================================================
总测试数: 41
通过: 41
失败: 0
通过率: 100.0%

🎉 所有测试通过！PKCS#7 模块工作正常
```

## 常见用例

### 用例 1：数字签名文档

**场景**：对重要文档进行数字签名，确保文档完整性和来源真实性。

**步骤**：
1. 加载签名证书和私钥
2. 读取要签名的文档
3. 使用 `PKCS7_sign` 创建签名
4. 保存签名数据
5. 使用 `PKCS7_verify` 验证签名

**参考**：`tests/certificate/test_pkcs7_sign_verify_workflow.pas`

### 用例 2：加密敏感数据

**场景**：加密敏感数据，只有持有私钥的接收者才能解密。

**步骤**：
1. 加载接收者证书
2. 使用 `PKCS7_encrypt` 加密数据
3. 保存加密数据
4. 接收者使用私钥和 `PKCS7_decrypt` 解密

**参考**：`tests/certificate/test_p2_pkcs7_encrypt_decrypt.pas`

### 用例 3：数据封装

**场景**：使用标准格式封装数据，便于传输和存储。

**步骤**：
1. 创建 PKCS7 结构
2. 设置类型为 `NID_pkcs7_data`
3. 使用 `PKCS7_set_content` 设置内容
4. 使用 `i2d_PKCS7_bio` 导出

**参考**：`tests/certificate/test_p2_pkcs7_data.pas`

## 最佳实践

### 1. 证书验证

生产环境应使用完整的证书链验证：

```pascal
// 不推荐（仅用于测试）
PKCS7_verify(p7, nil, nil, data, nil, PKCS7_NOVERIFY);

// 推荐（生产环境）
store := X509_STORE_new();
// 加载受信任的根证书
X509_STORE_add_cert(store, root_cert);
PKCS7_verify(p7, nil, store, data, nil, 0);
```

### 2. 错误处理

始终检查函数返回值：

```pascal
p7 := PKCS7_sign(cert, key, nil, data, PKCS7_DETACHED);
if p7 = nil then
begin
  // 获取 OpenSSL 错误信息
  err := ERR_get_error();
  WriteLn('签名失败: ', ERR_error_string(err, nil));
  Exit;
end;
```

### 3. 资源管理

及时释放资源：

```pascal
try
  p7 := PKCS7_sign(...);
  // 使用 p7
finally
  PKCS7_free(p7);
end;
```

### 4. 标志位组合

合理使用标志位：

```pascal
// 分离式签名 + 二进制模式
flags := PKCS7_DETACHED or PKCS7_BINARY;

// 不包含证书链（减小签名大小）
flags := PKCS7_DETACHED or PKCS7_NOCERTS;
```

## 故障排除

### 问题 1：签名验证失败

**可能原因**：
- 证书链不完整
- 证书已过期
- 数据被篡改

**解决方案**：
1. 检查证书有效期
2. 确保证书链完整
3. 使用 `PKCS7_NOVERIFY` 跳过证书验证（仅测试）

### 问题 2：加密失败

**可能原因**：
- 接收者证书无效
- 加密算法不支持

**解决方案**：
1. 验证证书有效性
2. 使用标准加密算法（如 AES-256-CBC）

### 问题 3：OpenSSL 3.x 兼容性

**说明**：某些函数在 OpenSSL 3.x 中已弃用或不可用。

**解决方案**：
- 使用替代函数
- 检查函数指针是否为 nil
- 参考 `test_p2_pkcs7_comprehensive.pas` 中的兼容性处理

## 相关资源

### 测试文件

- `tests/certificate/test_p2_pkcs7_comprehensive.pas` - 综合功能测试
- `tests/certificate/test_pkcs7_sign_verify_workflow.pas` - 签名验证工作流
- `tests/certificate/test_p2_pkcs7_sign_verify.pas` - 签名验证测试
- `tests/certificate/test_p2_pkcs7_encrypt_decrypt.pas` - 加密解密测试
- `tests/certificate/test_p2_pkcs7_data.pas` - 数据封装测试
- `tests/certificate/test_p2_pkcs7_boundary.pas` - 边界测试

### API 绑定

- `src/fafafa.ssl.openssl.api.pkcs7.pas` - PKCS#7 API 绑定

### 文档

- [OpenSSL PKCS7 文档](https://www.openssl.org/docs/man3.0/man3/PKCS7_sign.html)
- [RFC 2315 - PKCS #7: Cryptographic Message Syntax](https://tools.ietf.org/html/rfc2315)

## 更新日志

### 2026-01-23
- ✅ PKCS7 综合测试 100% 通过（41/41）
- ✅ 验证 OpenSSL 1.x 和 3.x 兼容性
- ✅ 创建 PKCS7 使用指南文档

### 2025-10-24
- ✅ 完成 PKCS7 API 绑定
- ✅ 实现基础测试套件
- ✅ 支持签名、验证、加密、解密功能

## 贡献

如果您发现问题或有改进建议，请：
1. 查看现有测试文件了解用法
2. 运行综合测试验证功能
3. 提交 Issue 或 Pull Request

---

**文档版本**: 1.0  
**最后更新**: 2026-01-23  
**维护者**: fafafa.ssl 项目组
