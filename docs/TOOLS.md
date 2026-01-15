# fafafa.ssl 实用工具集

本项目包含多个即用型命令行工具，展示 fafafa.ssl 库的实际应用。

---

## 🔧 可用工具

### 1. 哈希计算器 (`hash_calculator`)

计算文件的加密哈希值（SHA-256, SHA-512, MD5等）。

**编译**:
```bash
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/hash_calculator.pas -o./bin/hash_calculator
```

**用法**:
```bash
# SHA-256 (默认)
./bin/hash_calculator file.txt

# SHA-512
./bin/hash_calculator sha512 document.pdf

# 多个文件
./bin/hash_calculator *.txt

# MD5
./bin/hash_calculator md5 archive.zip
```

**示例输出**:
```
SHA256 (QUICKSTART.md) = 2a5303e96240abd399d4166538cd18af...
  文件大小: 10230 字节
```

---

### 2. 密码哈希工具 (`password_hash`)

安全地哈希和验证密码。

**编译**:
```bash
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/password_hash.pas -o./bin/password_hash
```

**用法**:
```bash
# 生成密码哈希
./bin/password_hash hash mypassword123

# 验证密码
./bin/password_hash verify mypassword123 <哈希值>
```

**示例输出**:
```
密码: mypassword123
SHA-256: ef92b778bafe771e89245b89ecbc08a44a4e166c06659911881...
✓ 请保存此哈希值用于验证
```

---

### 3. 加密示例 (`example_crypto_working`)

演示AES-GCM加密和SHA-256哈希。

**编译**:
```bash
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/example_crypto_working.pas -o./bin/example
```

**运行**:
```bash
./bin/example
```

**输出**:
```
✓ OpenSSL加载成功: 3.x (libcrypto.so.3)

=== AES-256-GCM 加密/解密示例 ===
  明文: Hello, fafafa.ssl!
  密文长度: 18 字节
  ✓ 加密成功
  ✓ 解密成功，认证通过

=== SHA-256 哈希示例 ===
  SHA-256: 22052DC71024F61595A40918D6D2986C...
  ✓ 哈希计算成功
```

---

## 🚀 快速开始

### 一次性编译所有工具

```bash
cd ~/projects/fafafa.ssl

# 创建bin目录
mkdir -p bin

# 编译所有工具
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/hash_calculator.pas -o./bin/hash_calculator
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/password_hash.pas -o./bin/password_hash
fpc -Mobjfpc -Sh -Fu./src -Fi./src examples/example_crypto_working.pas -o./bin/example
```

### 实际应用场景

#### 场景1: 验证文件完整性
```bash
# 下载文件后验证
./bin/hash_calculator sha256 downloaded_file.zip

# 与提供的哈希值比对
```

#### 场景2: 密码存储
```bash
# 生成用户密码哈希（存入数据库）
./bin/password_hash hash user_password_123

# 登录时验证
./bin/password_hash verify user_password_123 <stored_hash>
```

#### 场景3: 加密敏感数据
```pascal
// 在您的程序中使用
LoadOpenSSLCore();
LoadEVP(GetCryptoLibHandle);

// 加密数据...
```

---

## 📚 更多示例

查看 `examples/` 目录获取更多示例：

- `example_crypto_simple.pas` - 简化的加密演示
- `example_https_api.pas` - HTTPS API调用
- `file_encrypt_tool.pas` - 文件加密工具（开发中）

---

## 🔍 故障排除

### 问题: "Can't find OpenSSL library"

**解决**: 安装OpenSSL
```bash
sudo apt-get install libssl-dev  # Ubuntu/Debian
```

### 问题: 编译错误

**确保使用正确的编译选项**:
```bash
fpc -Mobjfpc -Sh -Fu./src -Fi./src your_program.pas
```

---

## 💡 贡献

欢迎贡献更多实用工具！

可能的扩展：
- 文件加密/解密工具
- HTTPS下载器
- 证书查看器
- 签名验证工具

---

## 📖 文档

- [快速开始指南](QUICKSTART.md)
- [API文档](docs/API.md) (即将推出)
- [示例集合](examples/)

---

**开始使用 fafafa.ssl 构建安全应用！** 🔐
