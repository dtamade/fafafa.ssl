# fafafa.ssl 示例代码

本目录包含 fafafa.ssl 库的各种使用示例，从基础到高级，帮助您快速上手。

---

## 📑 示例索引

### 🎯 推荐学习路径

**初学者**:
1. [基础初始化](#基础示例) → `test_openssl_basic.pas`
2. [简单哈希](#哈希算法) → `test_openssl.pas`
3. [AES加密](#对称加密) → `test_openssl_aes.pas`

**进阶用户**:
1. [AEAD加密](#aead加密) → `example_aes_gcm_aead.pas` ⭐
2. [公钥加密](#公钥算法) → `test_ecdsa.lpr`
3. [SSL/TLS](#ssltls) → `example_tls_client.pas`

---

## 📚 按类别分类

### 基础示例

| 文件 | 说明 | 难度 |
|------|------|------|
| `test_openssl_basic.pas` | OpenSSL 基本初始化和版本检查 | ⭐ |
| `test_basic.pas` | 基础功能测试 | ⭐ |
| `test_core_modules.pas` | 核心模块加载示例 | ⭐ |
| `test_init_order.pas` | 模块初始化顺序 | ⭐ |

**学习要点**:
- 如何加载 OpenSSL 库
- 检查版本和功能
- 基本错误处理

**示例代码**:
```pascal
uses
  fafafa.ssl.openssl.core;

begin
  LoadOpenSSLCore;
  if IsOpenSSLCoreLoaded then
    WriteLn('OpenSSL: ', GetOpenSSLVersionString);
end.
```

---

### 哈希算法

| 文件 | 说明 | 难度 |
|------|------|------|
| `test_openssl.pas` | SHA 系列哈希算法 | ⭐ |
| `test_openssl_blake2.lpr` | BLAKE2 哈希 | ⭐⭐ |

**支持的算法**:
- MD5, SHA-1 (不推荐用于安全场景)
- SHA-256, SHA-384, SHA-512 (推荐)
- SHA3-256, SHA3-512
- BLAKE2b, BLAKE2s
- SM3 (中国标准)
- RIPEMD160

**示例代码**:
```pascal
uses
  fafafa.ssl.openssl.evp;

var
  Ctx: PEVP_MD_CTX;
  MD: PEVP_MD;
  Hash: array[0..31] of Byte;
begin
  MD := EVP_MD_fetch(nil, 'SHA256', nil);
  Ctx := EVP_MD_CTX_new;
  
  EVP_DigestInit_ex(Ctx, MD, nil);
  EVP_DigestUpdate(Ctx, 'data', 4);
  EVP_DigestFinal_ex(Ctx, @Hash, nil);
  
  EVP_MD_CTX_free(Ctx);
  EVP_MD_free(MD);
end.
```

---

### 对称加密

| 文件 | 说明 | 难度 |
|------|------|------|
| `test_openssl_aes.pas` | AES 加密基础 | ⭐⭐ |
| `test_des.lpr` | DES/3DES 加密 | ⭐⭐ |
| `test_camellia.lpr` | Camellia 加密 | ⭐⭐ |
| `test_aria.lpr` | ARIA 加密 (韩国标准) | ⭐⭐ |

**支持的算法**:
- AES (128/192/256-bit)
- ChaCha20
- Camellia
- DES, 3DES (不推荐)
- ARIA (韩国标准)
- SEED (韩国标准)
- SM4 (中国标准)

**支持的模式**:
- ECB (不推荐)
- CBC
- CTR
- OFB
- CFB
- GCM (AEAD)
- CCM (AEAD)
- XTS (磁盘加密)

**示例代码**:
```pascal
uses
  fafafa.ssl.openssl.evp;

var
  Ctx: PEVP_CIPHER_CTX;
  Cipher: PEVP_CIPHER;
begin
  Cipher := EVP_CIPHER_fetch(nil, 'AES-256-CBC', nil);
  Ctx := EVP_CIPHER_CTX_new;
  
  EVP_EncryptInit_ex(Ctx, Cipher, nil, @Key, @IV);
  EVP_EncryptUpdate(Ctx, @Ciphertext, @Len, @Plaintext, PlainLen);
  EVP_EncryptFinal_ex(Ctx, @Ciphertext[Len], @FinalLen);
  
  EVP_CIPHER_CTX_free(Ctx);
  EVP_CIPHER_free(Cipher);
end.
```

---

### AEAD 加密

**⭐ 推荐示例**

| 文件 | 说明 | 难度 |
|------|------|------|
| `example_aes_gcm_aead.pas` | AES-GCM 完整示例 | ⭐⭐⭐ |

**什么是 AEAD?**

AEAD (Authenticated Encryption with Associated Data) 认证加密提供:
- **机密性**: 数据加密
- **完整性**: 防止篡改
- **真实性**: 验证数据来源

**支持的 AEAD 模式**:
- AES-GCM (推荐)
- ChaCha20-Poly1305 (高性能)
- AES-CCM
- AES-OCB

**使用场景**:
- 网络通信加密
- 文件加密
- 数据库字段加密
- API 请求/响应加密

**关键特性**:
```pascal
// 1. 加密数据
EVP_EncryptUpdate(Ctx, @Ciphertext, @Len, @Plaintext, PlainLen);

// 2. 添加 AAD (额外认证数据 - 不加密但验证)
EVP_EncryptUpdate(Ctx, nil, @Len, @AAD, AADLen);

// 3. 获取认证标签
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_GET_TAG, 16, @Tag);

// 4. 解密时验证标签
EVP_CIPHER_CTX_ctrl(Ctx, EVP_CTRL_GCM_SET_TAG, 16, @Tag);
if EVP_DecryptFinal_ex(Ctx, ...) <> 1 then
  WriteLn('数据被篡改！');
```

---

### 公钥算法

| 文件 | 说明 | 难度 |
|------|------|------|
| `test_ecdsa.lpr` | ECDSA 数字签名 | ⭐⭐⭐ |
| `test_ecdh.lpr` | ECDH 密钥交换 | ⭐⭐⭐ |

**支持的算法**:
- RSA (加密/签名)
- EC (椭圆曲线)
- DSA (数字签名)
- DH (密钥交换)
- Ed25519 (现代签名算法)
- X25519 (现代密钥交换)

**常见操作**:
- 密钥生成
- 数字签名和验证
- 密钥交换
- 公钥加密

---

### MAC 和 KDF

| 文件 | 说明 | 难度 |
|------|------|------|
| `test_cmac.lpr` | CMAC 消息认证码 | ⭐⭐ |
| `test_kdf.lpr` | 密钥派生函数 | ⭐⭐⭐ |

**MAC 算法**:
- HMAC (哈希MAC)
- CMAC (分组密码MAC)
- Poly1305

**KDF 算法**:
- PBKDF2 (密码派生)
- HKDF (HMAC-based KDF)
- SCrypt (抗ASIC)
- Argon2 (现代推荐)

**使用场景**:
- 密码存储
- 密钥派生
- 消息认证

---

### PKI 和证书

| 文件 | 说明 | 难度 |
|------|------|------|
| `test_certchain.pas` | 证书链验证 | ⭐⭐⭐ |

**功能**:
- X.509 证书解析
- 证书链验证
- CRL 检查
- OCSP 验证
- PKCS#7 / PKCS#12
- CMS (加密消息语法)

---

### SSL/TLS

| 文件 | 说明 | 难度 |
|------|------|------|
| `example_tls_client.pas` | TLS 客户端 | ⭐⭐⭐⭐ |
| `https_client.pas` | HTTPS 客户端 | ⭐⭐⭐⭐ |

**功能**:
- TLS 1.2 / TLS 1.3
- 证书验证
- SNI 支持
- 会话复用
- ALPN 协商

**示例场景**:
- HTTPS 客户端
- 安全 API 调用
- VPN 隧道
- 安全文件传输

---

### 工具和实用示例

| 文件 | 说明 | 难度 |
|------|------|------|
| `practical_examples.pas` | 实用功能集合 | ⭐⭐ |
| `test_factory_simple.pas` | 工厂模式使用 | ⭐⭐ |
| `test_openssl_bio.pas` | BIO I/O 抽象 | ⭐⭐⭐ |

**实用功能**:
- Base64 编码/解码
- PEM 文件读写
- 随机数生成
- 密钥导入/导出

---

## 🚀 快速开始

### 运行示例

**方法 1: 命令行编译**
```bash
# 编译单个示例
fpc example_aes_gcm_aead.pas

# 运行
./example_aes_gcm_aead
```

**方法 2: 使用 Lazarus IDE**
1. 打开 .lpr 或 .pas 文件
2. 按 F9 编译并运行
3. 查看输出

**方法 3: 批量测试**
```powershell
# Windows PowerShell
Get-ChildItem *.pas | ForEach-Object { fpc $_.Name }
```

---

## 📖 学习建议

### 第一周：基础
1. ✅ 学习 OpenSSL 初始化 (`test_openssl_basic.pas`)
2. ✅ 实践哈希算法 (`test_openssl.pas`)
3. ✅ 理解对称加密 (`test_openssl_aes.pas`)

### 第二周：进阶
1. ✅ 掌握 AEAD 加密 (`example_aes_gcm_aead.pas`)
2. ✅ 学习公钥算法 (`test_ecdsa.lpr`)
3. ✅ 理解 MAC 和 KDF (`test_cmac.lpr`, `test_kdf.lpr`)

### 第三周：实战
1. ✅ 证书处理 (`test_certchain.pas`)
2. ✅ 构建 TLS 客户端 (`example_tls_client.pas`)
3. ✅ 实现完整应用

---

## ⚠️ 安全建议

### ✅ 推荐

- **使用 AEAD 模式** (GCM, Poly1305) 而不是 CBC+HMAC
- **使用 SHA-256+** 而不是 MD5/SHA-1
- **使用 AES-256** 而不是 DES/3DES
- **使用 Ed25519** 而不是 RSA-1024
- **使用 EVP 高级接口** 而不是低级 API

### ❌ 避免

- ❌ ECB 模式 (不安全)
- ❌ MD5, SHA-1 用于安全场景
- ❌ DES, 3DES (已过时)
- ❌ RSA < 2048 bits
- ❌ 硬编码密钥和 IV
- ❌ 重复使用 IV (GCM, CTR 模式)

### 🔒 最佳实践

1. **密钥管理**
   ```pascal
   // ✅ 好：使用随机生成
   RAND_bytes(@Key, SizeOf(Key));
   
   // ❌ 差：硬编码
   Key := 'my-secret-key';
   ```

2. **IV 使用**
   ```pascal
   // ✅ 好：每次加密生成新的 IV
   RAND_bytes(@IV, SizeOf(IV));
   
   // ❌ 差：重复使用相同 IV
   FillChar(IV, SizeOf(IV), 0);
   ```

3. **错误处理**
   ```pascal
   // ✅ 好：检查返回值
   if EVP_EncryptInit_ex(...) <> 1 then
     raise Exception.Create('加密失败');
   
   // ❌ 差：忽略错误
   EVP_EncryptInit_ex(...);
   ```

4. **资源清理**
   ```pascal
   // ✅ 好：总是释放资源
   try
     Ctx := EVP_CIPHER_CTX_new;
     // 使用...
   finally
     EVP_CIPHER_CTX_free(Ctx);
   end;
   ```

---

## 🐛 常见问题

### Q: 示例无法编译？

**A**: 确保：
1. 已安装 OpenSSL 3.x
2. Free Pascal 3.3.1+
3. 添加了正确的库路径

### Q: 运行时找不到 OpenSSL 库？

**A**: 
- **Windows**: 确保 OpenSSL DLL 在 PATH 中
- **Linux**: 安装 `libssl-dev`
- **macOS**: 使用 `brew install openssl@3`

### Q: 哪个加密算法最好？

**A**: 
- **对称**: AES-256-GCM (推荐)
- **哈希**: SHA-256 或 SHA-512
- **公钥**: Ed25519 (签名), X25519 (密钥交换)
- **密码**: SCrypt 或 Argon2

### Q: GCM 和 CBC 有什么区别？

**A**:
- **GCM**: AEAD 模式，自动验证完整性，推荐
- **CBC**: 需要额外 HMAC 验证，容易出错，不推荐

---

## 📚 参考资源

### 官方文档
- [OpenSSL Documentation](https://www.openssl.org/docs/)
- [EVP API](https://www.openssl.org/docs/man3.0/man7/evp.html)
- [fafafa.ssl 项目文档](../DOCUMENTATION_INDEX.md)

### 推荐阅读
- **[PROJECT_STATUS_2025-10-02.md](../PROJECT_STATUS_2025-10-02.md)** - 项目状态
- **[TESTING_README.md](../TESTING_README.md)** - 测试指南
- **[OPENSSL3_COMPATIBILITY_STRATEGY.md](../OPENSSL3_COMPATIBILITY_STRATEGY.md)** - 兼容性策略

### 学习材料
- [Cryptography I - Coursera](https://www.coursera.org/learn/crypto)
- [The Cryptopals Crypto Challenges](https://cryptopals.com/)
- [Practical Cryptography for Developers](https://cryptobook.nakov.com/)

---

## 🤝 贡献示例

欢迎贡献新示例！请确保：

1. **代码清晰** - 添加注释解释关键步骤
2. **错误处理** - 包含适当的错误检查
3. **完整性** - 可以独立编译运行
4. **文档** - 在本 README 中添加条目

**示例模板**:
```pascal
program ExampleTemplate;
{$mode objfpc}{$H+}

uses
  SysUtils,
  fafafa.ssl.openssl.core;

begin
  WriteLn('=== 示例标题 ===');
  
  // 1. 初始化
  LoadOpenSSLCore;
  if not IsOpenSSLCoreLoaded then
    Halt(1);
  
  // 2. 您的代码...
  
  // 3. 清理
  
  WriteLn('完成！');
end.
```

---

## 📞 获取帮助

- **查看文档**: [DOCUMENTATION_INDEX.md](../DOCUMENTATION_INDEX.md)
- **运行测试**: [TESTING_README.md](../TESTING_README.md)
- **报告问题**: 提供完整的错误信息和代码示例

---

**最后更新**: 2025-10-02  
**维护者**: fafafa.ssl 项目组  
**示例总数**: 20+
