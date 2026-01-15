# 文件加密工具

基于 **AES-256-GCM** 认证加密的命令行文件加密/解密工具。

## 特性

- 🔒 **AES-256-GCM 认证加密**
  - 256-bit 密钥长度
  - GCM 模式提供认证和加密
  - 防篡改保护

- 🔑 **安全的密钥派生**
  - 基于密码的密钥派生
  - 随机盐（16 bytes）
  - 每次加密使用唯一 IV（12 bytes）

- ✅ **认证标签验证**
  - 16-byte Poly1305 认证标签
  - 文件名作为 AAD（额外认证数据）
  - 解密时自动验证完整性

- 📦 **自定义文件格式**
  - 文件头包含魔数、版本、算法信息
  - Salt 和 IV 存储在文件中
  - 认证标签附加在文件末尾

## 编译

```bash
fpc -Mdelphi -Fu"..\..\src" -o"file_encrypt.exe" file_encrypt.pas
```

或使用提供的编译脚本。

## 使用方法

### 加密文件

```bash
file_encrypt -e <输入文件> <输出文件> <密码>
```

示例：
```bash
file_encrypt -e document.pdf document.pdf.enc MySecretPassword123
```

### 解密文件

```bash
file_encrypt -d <输入文件> <输出文件> <密码>
```

示例：
```bash
file_encrypt -d document.pdf.enc document.pdf MySecretPassword123
```

## 文件格式

```
+-------------------+
| Header (64 bytes) |
|  - Magic (8)      |
|  - Version (2)    |
|  - Algorithm (2)  |
|  - Salt (16)      |
|  - IV (12)        |
|  - Reserved (16)  |
+-------------------+
| Encrypted Data    |
| (variable size)   |
+-------------------+
| Tag (16 bytes)    |
+-------------------+
```

### Header 详情

- **Magic**: `FAFAFASL` (8 bytes) - 文件标识
- **Version**: 文件格式版本号 (2 bytes)
- **Algorithm**: 加密算法标识 (2 bytes)
  - `1` = AES-256-GCM
- **Salt**: 随机盐，用于密钥派生 (16 bytes)
- **IV**: GCM 模式的 nonce (12 bytes)
- **Reserved**: 保留字段，供future use (16 bytes)

### 加密数据

- 使用 AES-256-GCM 加密的原始文件数据
- 以 64KB 块进行流式处理
- 支持任意大小的文件

### 认证标签

- 16-byte Poly1305 MAC
- 验证数据完整性和真实性
- 包含 AAD（文件名）的认证

## 安全特性

### ✅ 已实现

1. **机密性**
   - AES-256 提供强加密
   - 密文无法在没有密码的情况下解密

2. **完整性**
   - GCM 模式提供内置认证
   - 任何篡改都会被检测

3. **认证性**
   - 认证标签确保数据来源
   - AAD 保护文件名

4. **随机性**
   - 每次加密生成新的 Salt 和 IV
   - 相同文件多次加密产生不同密文

### ⚠️ 注意事项

**当前实现的限制**（仅用于演示）：

1. **密钥派生**: 使用简化的 XOR 派生，**不安全**
   - 生产环境应使用 PBKDF2、Argon2 等
   
2. **随机数生成**: 使用 Pascal 的 Random()
   - 生产环境应使用 OpenSSL 的 RAND_bytes()

3. **密码强度**: 未检查密码强度
   - 建议使用至少 12 字符的强密码

## 示例输出

### 加密

```
========================================
文件加密工具 v1.0.0
========================================

✅ OpenSSL 加载成功

🔒 正在加密文件...
输入: document.pdf
输出: document.pdf.enc
.....
✅ 加密成功！
输入大小: 2458624 字节
输出大小: 2458704 字节
```

### 解密

```
========================================
文件加密工具 v1.0.0
========================================

✅ OpenSSL 加载成功

🔓 正在解密文件...
输入: document.pdf.enc
输出: document.pdf
.....
✅ 解密成功！
输出大小: 2458624 字节
```

### 错误情况

```
❌ 认证失败！文件可能已被篡改或密码错误
```

## 技术细节

### 加密流程

1. 读取输入文件
2. 生成随机 Salt 和 IV
3. 从密码派生加密密钥
4. 创建文件头并写入
5. 使用 AES-256-GCM 流式加密数据
6. 获取认证标签并附加到文件末尾

### 解密流程

1. 读取并验证文件头
2. 提取 Salt 和 IV
3. 从密码派生解密密钥
4. 读取文件末尾的认证标签
5. 使用 AES-256-GCM 流式解密数据
6. 验证认证标签（失败则拒绝）

### 性能

- **64KB 缓冲区**: 平衡内存使用和性能
- **流式处理**: 支持任意大小文件
- **进度指示**: 每处理 1MB 显示一个点

### 内存安全

- 使用后清零密钥：`FillChar(LKey, SizeOf(LKey), 0)`
- 使用 try-finally 确保资源释放
- 异常安全处理

## 改进建议

生产环境使用时应考虑：

1. **使用 PBKDF2/Argon2** 进行密钥派生
   ```pascal
   // 示例（需要实现）
   PKCS5_PBKDF2_HMAC(password, salt, iterations, hashFunc, keyLen, key);
   ```

2. **使用 OpenSSL RAND_bytes** 生成随机数
   ```pascal
   RAND_bytes(@salt[0], SALT_SIZE);
   RAND_bytes(@iv[0], IV_SIZE);
   ```

3. **添加密码强度检查**
   - 最小长度要求
   - 复杂度要求
   - 常见密码检查

4. **文件完整性**
   - 原子写入操作
   - 临时文件处理
   - 失败时清理

5. **用户体验**
   - 进度条而不是点
   - 预估剩余时间
   - 人类可读的文件大小

## 依赖

- Free Pascal 3.2.0+
- OpenSSL 3.x (libcrypto)
- fafafa.ssl 库

## 许可

与 fafafa.ssl 项目相同许可。

## 相关文档

- [EVP_MODULE_TEST_REPORT.md](../../EVP_MODULE_TEST_REPORT.md) - EVP 模块测试报告
- [PROJECT_STATUS_UPDATE_2025-10-01.md](../../PROJECT_STATUS_UPDATE_2025-10-01.md) - 项目状态
