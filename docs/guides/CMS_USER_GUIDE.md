# CMS 模块使用指南

## 概述

CMS (Cryptographic Message Syntax) 是 PKCS#7 的继任者，是一种用于数字签名、数据加密和数据封装的标准格式。fafafa.ssl 项目通过 OpenSSL 后端提供了完整的 CMS 支持。

## 功能特性

### ✅ 已实现功能

- **数字签名**：对数据进行签名和验证
- **数据加密**：使用接收者公钥加密数据
- **数据解密**：使用私钥解密数据
- **数据封装**：标准的数据容器格式
- **证书管理**：添加证书和 CRL
- **签名者信息**：管理签名者属性
- **I/O 操作**：DER 格式的序列化和反序列化

### 📊 测试覆盖

- **OpenSSL 兼容性**：支持 OpenSSL 1.1.1+ 和 3.x
- **测试文件**：
  - `tests/certificate/test_p2_cms_comprehensive.pas` - 综合功能测试
  - `tests/certificate/test_p2_cms.pas` - 基础功能测试
  - `tests/certificate/test_p2_cms_boundary.pas` - 边界测试

这些测试命令是当前可执行验证入口，但不要把固定的总测试数、通过率或历史输出文本当成当前接口 truth。

## CMS vs PKCS#7

| 特性     | PKCS#7     | CMS             |
| -------- | ---------- | --------------- |
| 标准     | RFC 2315   | RFC 5652        |
| 发布时间 | 1998       | 2009            |
| 加密算法 | 较旧的算法 | 现代算法支持    |
| 扩展性   | 有限       | 更好的扩展性    |
| 互操作性 | 广泛支持   | 向后兼容 PKCS#7 |

**推荐**：新项目使用 CMS，现有 PKCS#7 项目可以逐步迁移。

## API 参考

### 核心函数

#### 创建和释放

```pascal
function CMS_ContentInfo_new: PCMS_ContentInfo;
procedure CMS_ContentInfo_free(cms: PCMS_ContentInfo);
```

#### 签名操作

```pascal
function CMS_sign(
  signcert: PX509;           // 签名证书
  pkey: PEVP_PKEY;          // 私钥
  certs: PSTACK_OF_X509;    // 额外证书链（可选）
  data: PBIO;               // 要签名的数据
  flags: cuint              // 标志位
): PCMS_ContentInfo;

function CMS_verify(
  cms: PCMS_ContentInfo;    // CMS 结构
  certs: PSTACK_OF_X509;    // 验证证书链（可选）
  store: PX509_STORE;       // 证书存储（可选）
  detached_data: PBIO;      // 分离式签名的原始数据
  out_: PBIO;               // 输出 BIO（可选）
  flags: cuint              // 标志位
): cint;
```

#### 加密操作

```pascal
function CMS_encrypt(
  certs: PSTACK_OF_X509;    // 接收者证书
  in_: PBIO;                // 要加密的数据
  cipher: PEVP_CIPHER;      // 加密算法
  flags: cuint              // 标志位
): PCMS_ContentInfo;

function CMS_decrypt(
  cms: PCMS_ContentInfo;    // CMS 结构
  pkey: PEVP_PKEY;          // 私钥
  cert: PX509;              // 证书
  dcont: PBIO;              // 分离式内容（可选）
  out_: PBIO;               // 输出 BIO
  flags: cuint              // 标志位
): cint;
```

#### I/O 操作

```pascal
function i2d_CMS_bio(bp: PBIO; cms: PCMS_ContentInfo): cint;
function d2i_CMS_bio(bp: PBIO; cms: PPCMS_ContentInfo): PCMS_ContentInfo;
```

### 常量定义

#### CMS 类型

```pascal
const
  NID_id_smime_ct_contentInfo = 786;      // 内容信息
  NID_id_smime_ct_authData = 787;         // 认证数据
  NID_id_smime_ct_publishCert = 788;      // 发布证书
  NID_id_smime_ct_TSTInfo = 789;          // 时间戳信息
  NID_id_smime_ct_TDTInfo = 790;          // TDT 信息
  NID_id_smime_ct_contentCollection = 791; // 内容集合
  NID_id_smime_ct_contentWithAttrs = 792;  // 带属性的内容
```

#### 标志位

```pascal
const
  CMS_TEXT = $1;              // 添加文本 MIME 头
  CMS_NOCERTS = $2;           // 不包含签名者证书
  CMS_NO_CONTENT_VERIFY = $4; // 不验证内容
  CMS_NO_ATTR_VERIFY = $8;    // 不验证属性
  CMS_NOSIGS = $10;           // 不包含签名
  CMS_NOINTERN = $20;         // 不搜索内部证书
  CMS_NO_SIGNER_CERT_VERIFY = $40; // 不验证签名者证书
  CMS_NOVERIFY = $80;         // 不验证签名
  CMS_DETACHED = $100;        // 分离式签名
  CMS_BINARY = $200;          // 二进制模式
  CMS_NOATTR = $400;          // 不包含认证属性
  CMS_NOSMIMECAP = $800;      // 不包含 S/MIME 能力
  CMS_NOOLDMIMETYPE = $1000;  // 不使用旧的 MIME 类型
  CMS_CRLFEOL = $2000;        // 使用 CRLF 换行
```

## 使用示例

### 示例 1：运行综合测试

```bash
# 编译综合测试
cd /home/dtamade/projects/fafafa.ssl
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib/test \
    tests/certificate/test_p2_cms_comprehensive.pas \
    -o./bin/test_p2_cms_comprehensive

# 运行测试
./bin/test_p2_cms_comprehensive
```

成功标准：

- 测试程序成功编译并运行
- CMS 相关 API availability / workflow assertions 全部通过
- 如果测试输出摘要数字与历史截图不同，以当前运行结果为准

### 示例 2：运行基础测试

```bash
# 编译基础测试
fpc -B -Mobjfpc -Sh -Fu./src -Fi./src -FU./lib/test \
    tests/certificate/test_p2_cms.pas \
    -o./bin/test_p2_cms

# 运行测试
./bin/test_p2_cms
```

成功标准：

- 基础测试程序成功编译并运行
- CMS 基础函数可用性断言全部通过
- 不要把某次历史运行的固定计数当成当前文档 truth

## 常见用例

### 用例 1：数字签名文档

**场景**：对重要文档进行数字签名，确保文档完整性和来源真实性。

**步骤**：

1. 加载签名证书和私钥
2. 读取要签名的文档
3. 使用 `CMS_sign` 创建签名
4. 保存签名数据
5. 使用 `CMS_verify` 验证签名

**参考**：`tests/certificate/test_p2_cms_comprehensive.pas` 中的签名测试

### 用例 2：加密敏感数据

**场景**：加密敏感数据，只有持有私钥的接收者才能解密。

**步骤**：

1. 加载接收者证书
2. 使用 `CMS_encrypt` 加密数据
3. 保存加密数据
4. 接收者使用私钥和 `CMS_decrypt` 解密

**参考**：`tests/certificate/test_p2_cms_comprehensive.pas` 中的加密测试

### 用例 3：数据封装

**场景**：使用标准格式封装数据，便于传输和存储。

**步骤**：

1. 创建 CMS 结构
2. 设置内容类型
3. 使用 `i2d_CMS_bio` 导出

**参考**：`tests/certificate/test_p2_cms_comprehensive.pas` 中的 I/O 测试

## 最佳实践

### 1. 证书验证

生产环境应使用完整的证书链验证：

```pascal
// 不推荐（仅用于测试）
CMS_verify(cms, nil, nil, data, nil, CMS_NOVERIFY);

// 推荐（生产环境）
store := X509_STORE_new();
// 加载受信任的根证书
X509_STORE_add_cert(store, root_cert);
CMS_verify(cms, nil, store, data, nil, 0);
```

### 2. 错误处理

始终检查函数返回值：

```pascal
cms := CMS_sign(cert, key, nil, data, CMS_DETACHED);
if cms = nil then
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
  cms := CMS_sign(...);
  // 使用 cms
finally
  CMS_ContentInfo_free(cms);
end;
```

### 4. 标志位组合

合理使用标志位：

```pascal
// 分离式签名 + 二进制模式
flags := CMS_DETACHED or CMS_BINARY;

// 不包含证书链（减小签名大小）
flags := CMS_DETACHED or CMS_NOCERTS;
```

## CMS 与 PKCS#7 互操作性

CMS 向后兼容 PKCS#7，可以：

- 使用 CMS API 读取 PKCS#7 数据
- 使用 PKCS#7 API 读取 CMS 数据
- 在同一应用中混合使用两种格式

**注意**：某些 CMS 特有功能（如新的加密算法）可能无法被旧的 PKCS#7 实现识别。

## 故障排除

### 问题 1：签名验证失败

**可能原因**：

- 证书链不完整
- 证书已过期
- 数据被篡改

**解决方案**：

1. 检查证书有效期
2. 确保证书链完整
3. 使用 `CMS_NOVERIFY` 跳过证书验证（仅测试）

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
- 参考 `test_p2_cms_comprehensive.pas` 中的兼容性处理

## 相关资源

### 测试文件

- `tests/certificate/test_p2_cms_comprehensive.pas` - 综合功能测试
- `tests/certificate/test_p2_cms.pas` - 基础功能测试
- `tests/certificate/test_p2_cms_boundary.pas` - 边界测试

### API 绑定

- `src/fafafa.ssl.openssl.api.cms.pas` - CMS API 绑定

### 文档

- [OpenSSL CMS 文档](https://www.openssl.org/docs/man3.0/man3/CMS_sign.html)
- [RFC 5652 - Cryptographic Message Syntax (CMS)](https://tools.ietf.org/html/rfc5652)
- [PKCS#7 用户指南](PKCS7_USER_GUIDE.md) - 了解 PKCS#7 与 CMS 的区别

## 维护说明

- 当前指南重点记录的是 CMS surface、入口测试与用法示例
- 历史测试数量、通过率、阶段完成度不再作为本页正文 truth 维护
- 如果你需要 live status，请直接运行上面的测试命令并以当前结果为准

## 贡献

如果您发现问题或有改进建议，请：

1. 查看现有测试文件了解用法
2. 运行综合测试验证功能
3. 提交 Issue 或 Pull Request

---

**文档版本**: 1.1
**最后更新**: 2026-05-19
**维护者**: fafafa.ssl 项目组
