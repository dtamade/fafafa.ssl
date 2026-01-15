# 数字签名工具

RSA 数字签名示例程序，演示如何使用 fafafa.ssl 库进行文件签名和验证。

## 功能特性

- ✅ **RSA 密钥对生成** - 支持 2048/4096 位密钥
- ✅ **文件签名** - 使用 RSA + SHA-256 进行数字签名
- ✅ **签名验证** - 验证文件完整性和真实性
- ✅ **PEM 格式** - 标准的密钥存储格式

## 编译

```bash
# 使用 Free Pascal Compiler
fpc -FuD:\projects\Pascal\lazarus\My\libs\fafafa.ssl\src ^
    -FED:\projects\Pascal\lazarus\My\libs\fafafa.ssl\examples\digital_signature\lib ^
    digital_signature.pas
```

## 使用方法

### 1. 生成 RSA 密钥对

```bash
# 生成 2048 位密钥（默认）
digital_signature -g private.pem public.pem

# 生成 4096 位密钥（更安全）
digital_signature -g private.pem public.pem 4096
```

**输出文件：**
- `private.pem` - 私钥文件（**请妥善保管，不要泄露！**）
- `public.pem` - 公钥文件（可以公开分发）

### 2. 对文件进行签名

```bash
digital_signature -s document.txt document.sig private.pem
```

**参数说明：**
- `document.txt` - 要签名的文件
- `document.sig` - 输出的签名文件
- `private.pem` - 私钥文件

### 3. 验证文件签名

```bash
digital_signature -v document.txt document.sig public.pem
```

**参数说明：**
- `document.txt` - 待验证的文件
- `document.sig` - 签名文件
- `public.pem` - 公钥文件

## 工作原理

### 数字签名过程

```
原始文件 → SHA-256 哈希 → RSA 私钥签名 → 签名文件
```

1. **计算文件哈希**：使用 SHA-256 算法计算文件的哈希值
2. **私钥签名**：使用 RSA 私钥对哈希值进行加密
3. **保存签名**：将加密后的签名保存到文件

### 签名验证过程

```
原始文件 → SHA-256 哈希 ┐
签名文件 → RSA 公钥解密 ┘ → 比较 → ✅/❌
```

1. **计算文件哈希**：对待验证文件计算 SHA-256 哈希
2. **解密签名**：使用 RSA 公钥解密签名文件
3. **比较验证**：比较两个哈希值是否相同

## 使用场景

### 软件发布

```bash
# 开发者签名软件包
digital_signature -s myapp-v1.0.zip myapp-v1.0.sig developer_private.pem

# 用户验证下载的软件
digital_signature -v myapp-v1.0.zip myapp-v1.0.sig developer_public.pem
```

### 文档认证

```bash
# 签署合同或证书
digital_signature -s contract.pdf contract.sig my_private.pem

# 验证文档真实性
digital_signature -v contract.pdf contract.sig signer_public.pem
```

### 代码审计

```bash
# 签名代码提交
digital_signature -s commit.patch commit.sig auditor_private.pem

# 验证代码来源
digital_signature -v commit.patch commit.sig auditor_public.pem
```

## 技术细节

### 算法选择

- **非对称算法**：RSA（2048 或 4096 位）
- **哈希算法**：SHA-256
- **签名方案**：PKCS#1 v2.1 (RSA-PSS 或 PKCS#1 v1.5)

### 密钥长度建议

| 密钥长度 | 安全级别 | 签名时间 | 推荐使用 |
|---------|---------|---------|---------|
| 2048 位 | 中等 | 快速 | 一般应用 |
| 3072 位 | 高 | 较慢 | 敏感数据 |
| 4096 位 | 很高 | 慢 | 长期存储 |

### API 使用

程序使用 OpenSSL EVP 接口：

```pascal
// 签名流程
EVP_MD_CTX_new         // 创建上下文
EVP_DigestSignInit     // 初始化签名（指定 SHA-256）
EVP_DigestSignUpdate   // 更新数据（分块读取文件）
EVP_DigestSignFinal    // 完成签名

// 验证流程
EVP_MD_CTX_new         // 创建上下文
EVP_DigestVerifyInit   // 初始化验证（指定 SHA-256）
EVP_DigestVerifyUpdate // 更新数据（分块读取文件）
EVP_DigestVerifyFinal  // 完成验证
```

## 安全注意事项

### 私钥保护

⚠️ **私钥文件必须严格保密！**

- 不要将私钥提交到版本控制系统
- 设置严格的文件权限（Unix: `chmod 600 private.pem`）
- 考虑使用密码保护（生产环境推荐）
- 定期轮换密钥

### 最佳实践

1. **分离环境**：开发、测试、生产使用不同的密钥对
2. **密钥备份**：安全备份私钥到离线存储
3. **公钥分发**：通过安全渠道发布公钥（HTTPS、PGP 签名等）
4. **签名验证**：始终验证签名，不要盲目信任文件

## 扩展功能

### 添加密码保护（未实现）

修改密钥生成代码，添加密码保护：

```pascal
// 使用密码保护私钥
PEM_write_bio_PrivateKey(bio_priv, pkey, 
  EVP_aes_256_cbc(),     // 加密算法
  PAnsiChar(password),   // 密码
  Length(password),      // 密码长度
  nil, nil);
```

### 时间戳签名（未实现）

添加时间戳服务器支持，防止签名重放攻击。

### 批量签名（未实现）

支持对目录中的所有文件进行批量签名。

## 故障排除

### 错误：无法加载 OpenSSL

```
❌ 错误: 无法加载 OpenSSL
```

**解决方法：**
- 确保 `libcrypto-3-x64.dll` 在系统 PATH 中
- 或将 DLL 复制到程序同目录

### 错误：密钥格式不正确

```
❌ 错误: 无法读取私钥/公钥
```

**解决方法：**
- 确认密钥文件格式为 PEM
- 检查文件是否损坏
- 如果私钥有密码保护，需要修改代码添加密码回调

### 错误：签名验证失败

```
❌ ❌ ❌ 签名验证失败！
```

**可能原因：**
- 文件已被修改
- 签名文件损坏
- 使用了错误的公钥
- 签名和文件不匹配

## 依赖项

- Free Pascal Compiler (FPC) 3.2.0+
- fafafa.ssl 库
- OpenSSL 3.0+ (libcrypto-3-x64.dll)

## 许可证

本示例程序遵循 fafafa.ssl 项目的许可证。

## 参考资料

- [OpenSSL EVP API](https://www.openssl.org/docs/man3.0/man7/evp.html)
- [RSA 数字签名标准](https://en.wikipedia.org/wiki/RSA_(cryptosystem))
- [PKCS #1: RSA Cryptography Standard](https://datatracker.ietf.org/doc/html/rfc8017)
