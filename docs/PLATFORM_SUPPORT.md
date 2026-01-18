# 平台支持文档

**最后更新**: 2026-01-18
**版本**: 1.0

---

## 📊 支持的平台概览

fafafa.ssl 是一个跨平台的 SSL/TLS 抽象框架,支持多个操作系统和后端实现。

| 平台 | 状态 | 后端支持 | 测试覆盖率 | CI/CD |
|------|------|----------|-----------|-------|
| **Windows** | ✅ 完全支持 | OpenSSL, WinSSL | 97.5% | ✅ |
| **Linux** | ✅ 完全支持 | OpenSSL | 97.5% | ✅ |
| **macOS** | 🔄 验证中 | OpenSSL | 待测试 | 🔄 配置中 |

---

## 🪟 Windows 平台

### 支持状态
- **状态**: ✅ 完全支持
- **测试覆盖率**: 97.5% (39/40 核心测试通过)
- **生产就绪度**: 99%+

### 支持的后端
1. **OpenSSL** (推荐)
   - 版本: 1.1.x, 3.x
   - 动态库: `libssl-3-x64.dll`, `libcrypto-3-x64.dll`
   - 安装方式: 从 OpenSSL 官网下载或使用包管理器

2. **WinSSL (Schannel)** (零依赖，100% 完成)
   - 版本: Windows Vista+
   - TLS 1.3 支持: Windows 10 20348+ 或 Windows 11
   - 状态: ✅ 生产就绪（所有 6 个阶段完成）
   - 优势: 零外部依赖,使用系统原生 SSL/TLS,自动安全更新

### 安装指南

#### 方式 1: 使用 OpenSSL
```powershell
# 下载 OpenSSL for Windows
# https://slproweb.com/products/Win32OpenSSL.html

# 或使用 Chocolatey
choco install openssl

# 验证安装
openssl version
```

#### 方式 2: 使用 WinSSL (无需安装)
```pascal
// WinSSL 使用系统原生 Schannel,无需额外安装
uses fafafa.ssl.factory;

var
  Lib: ISSLLibrary;
begin
  // 自动选择 WinSSL (如果可用)
  Lib := CreateSSLLibrary();
  WriteLn('Backend: ', Lib.GetLibraryType);
end;
```

### 编译和测试
```powershell
# 编译核心测试
cd tests
.\run_core_tests.ps1

# 运行 WinSSL 测试
.\run_winssl_tests.ps1
```

### 已知问题
- 无重大问题

---

## 🐧 Linux 平台

### 支持状态
- **状态**: ✅ 完全支持
- **测试覆盖率**: 97.5% (39/40 核心测试通过)
- **生产就绪度**: 99%+

### 支持的后端
1. **OpenSSL** (唯一后端)
   - 版本: 1.1.x, 3.x
   - 动态库: `libssl.so.3`, `libcrypto.so.3`
   - 安装方式: 系统包管理器

### 安装指南

#### Ubuntu/Debian
```bash
# 安装 Free Pascal
sudo apt-get update
sudo apt-get install fpc

# 安装 OpenSSL (通常已预装)
sudo apt-get install libssl-dev

# 验证安装
fpc -version
openssl version
```

#### Fedora/RHEL/CentOS
```bash
# 安装 Free Pascal
sudo dnf install fpc

# 安装 OpenSSL
sudo dnf install openssl-devel

# 验证安装
fpc -version
openssl version
```

#### Arch Linux
```bash
# 安装 Free Pascal
sudo pacman -S fpc

# 安装 OpenSSL
sudo pacman -S openssl

# 验证安装
fpc -version
openssl version
```

### 编译和测试
```bash
# 使用构建脚本
chmod +x build_linux.sh
./build_linux.sh

# 或手动编译
cd tests
chmod +x run_core_tests.sh
./run_core_tests.sh
```

### 已知问题
- 无重大问题

---

## 🍎 macOS 平台

### 支持状态
- **状态**: 🔄 验证中
- **测试覆盖率**: 待测试
- **生产就绪度**: 待验证

### 支持的后端
1. **OpenSSL** (唯一后端)
   - 版本: 1.1.x, 3.x (推荐 3.x)
   - 动态库: `libssl.3.dylib`, `libcrypto.3.dylib`
   - 安装方式: Homebrew

### 安装指南

#### 使用 Homebrew (推荐)
```bash
# 安装 Free Pascal
brew install fpc

# 安装 OpenSSL 3.x
brew install openssl@3

# 链接 OpenSSL (可选)
brew link openssl@3 --force

# 验证安装
fpc -version
openssl version
```

#### 设置库路径
```bash
# 对于 Apple Silicon (M1/M2)
export DYLD_LIBRARY_PATH=/opt/homebrew/opt/openssl@3/lib:$DYLD_LIBRARY_PATH

# 对于 Intel Mac
export DYLD_LIBRARY_PATH=/usr/local/opt/openssl@3/lib:$DYLD_LIBRARY_PATH

# 添加到 ~/.zshrc 或 ~/.bash_profile 以永久生效
echo 'export DYLD_LIBRARY_PATH=/opt/homebrew/opt/openssl@3/lib:$DYLD_LIBRARY_PATH' >> ~/.zshrc
```

### 编译和测试
```bash
# 使用 macOS 构建脚本
chmod +x build_macos.sh
./build_macos.sh

# 或手动编译
cd tests
fpc -Mobjfpc -Sh -Fu../src -Fi../src -FUlib -FEbin test_aes.pas
./bin/test_aes
```

### 平台特定注意事项

#### 1. OpenSSL 库路径
macOS 上 Homebrew 安装的 OpenSSL 不在系统默认路径:
- Apple Silicon: `/opt/homebrew/opt/openssl@3/`
- Intel Mac: `/usr/local/opt/openssl@3/`

需要设置 `DYLD_LIBRARY_PATH` 环境变量。

#### 2. 架构差异
- **Apple Silicon (M1/M2)**: ARM64 架构
- **Intel Mac**: x86_64 架构

确保 Free Pascal 和 OpenSSL 架构匹配。

#### 3. 代码签名
某些测试可能需要代码签名才能运行。如果遇到权限问题:
```bash
# 临时允许运行
xattr -d com.apple.quarantine ./bin/test_aes
```

#### 4. 大小写敏感性
macOS 默认文件系统不区分大小写 (APFS 可配置)。确保文件名大小写一致。

### 已知问题
- 🔄 macOS 平台验证正在进行中
- 待完成完整测试套件验证
- CI/CD 配置待完成

---

## 🔧 平台选择指南

### 自动后端选择
工厂方法会自动选择最佳可用后端:

```pascal
uses fafafa.ssl.factory;

var
  Lib: ISSLLibrary;
begin
  // 自动选择:
  // - Windows: WinSSL (如果可用) 或 OpenSSL
  // - Linux/macOS: OpenSSL
  Lib := CreateSSLLibrary();

  WriteLn('使用后端: ', Lib.GetLibraryType);
end;
```

### 显式后端选择
```pascal
// 强制使用 OpenSSL
Lib := CreateOpenSSLLibrary();

// 强制使用 WinSSL (仅 Windows)
Lib := CreateWinSSLLibrary();
```

### 后端对比

| 特性 | OpenSSL | WinSSL |
|------|---------|--------|
| **平台** | Windows/Linux/macOS | 仅 Windows |
| **依赖** | 需要 OpenSSL 库 | 零依赖 |
| **TLS 版本** | 1.0-1.3 | 1.0-1.3 |
| **性能** | 优秀 | 优秀 |
| **证书管理** | 文件/内存 | 系统证书存储 |
| **FIPS 模式** | 支持 | 支持 |

---

## 🧪 测试覆盖率

### 核心测试套件
- **总测试**: 40 个核心测试
- **Windows**: 39/40 通过 (97.5%)
- **Linux**: 39/40 通过 (97.5%)
- **macOS**: 待测试

### 测试类别
1. **对称加密**: AES, DES, ChaCha20, Blowfish, Camellia
2. **哈希函数**: SHA, SHA3, BLAKE2, SM3
3. **AEAD 模式**: GCM, CCM
4. **HMAC/MAC**: HMAC, CMAC
5. **KDF**: PBKDF2, HKDF
6. **签名验证**: RSA, ECDSA, DSA
7. **算法可用性**: 动态检测

---

## 🚀 CI/CD 支持

### GitHub Actions
项目使用 GitHub Actions 进行跨平台测试:

```yaml
# .github/workflows/test-all-platforms.yml
jobs:
  test:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        fpc-version: ['3.2.2', '3.3.1']
    runs-on: ${{ matrix.os }}
```

### 当前状态
- ✅ Windows CI: 已配置并运行
- ✅ Linux CI: 已配置并运行
- 🔄 macOS CI: 配置中

---

## 📚 相关文档

- [快速入门](QUICKSTART.md)
- [入门指南](GETTING_STARTED.md)
- [API 参考](API_REFERENCE.md)
- [故障排除](TROUBLESHOOTING.md)
- [WinSSL 用户指南](WINSSL_USER_GUIDE.md)

---

## 🤝 贡献

如果您在特定平台上遇到问题或有改进建议,请:
1. 查看 [故障排除文档](TROUBLESHOOTING.md)
2. 搜索现有 [Issues](https://github.com/your-repo/fafafa.ssl/issues)
3. 创建新 Issue 并提供详细信息:
   - 操作系统和版本
   - Free Pascal 版本
   - OpenSSL 版本
   - 错误信息和日志

---

**维护者**: fafafa.ssl 团队
**许可证**: [LICENSE](../LICENSE)
