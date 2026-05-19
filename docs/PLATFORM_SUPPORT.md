# 平台支持文档

**最后更新**: 2026-03-25
**版本**: 1.0

---

## 📊 支持的平台概览

fafafa.ssl 是一个跨平台的 SSL/TLS 抽象框架,支持多个操作系统和后端实现。

## 当前发布与平台验证入口

如果你是在继续当前工程的验证或收口，先看这三页，再按当前平台口径推进：

- `docs/ROADMAP.md`
- `docs/plans/2026-05-12-release-v1.5.0-formalization.md`
- `docs/test_reports/RELEASE_READINESS_V1.5.0.md`

当前默认本地 release baseline 是 Linux release-control 链：

- `python3 scripts/compile_all_modules.py`
- `bash scripts/run_minimal_ci_gate.sh --fast-local`
- `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local`
- `python3 scripts/check_code_style.py src`
- `bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local`

Wave C 页面现在只保留 closeout / approval / historical reference 角色，不再是默认工程入口。Windows 保留现有 PowerShell 测试入口；macOS 当前更适合先做 focused smoke，再按 release-control 状态决定是否需要继续扩展验证。全量多平台 workflow 仍以 template/manual 为主。

| 平台        | 状态        | 后端支持        | 测试覆盖率 | CI/CD     |
| ----------- | ----------- | --------------- | ---------- | --------- |
| **Windows** | ✅ 完全支持 | OpenSSL, WinSSL | 97.5%      | ✅        |
| **Linux**   | ✅ 完全支持 | OpenSSL         | 97.5%      | ✅        |
| **macOS**   | 🔄 验证中   | OpenSSL         | 待测试     | 🔄 配置中 |

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
  // 自动选择当前优先级最高的可用后端
  // Windows 常见结果是 WinSSL，但仍取决于注册状态与可用性
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

1. **OpenSSL** (默认/最常见入口)
   - 版本: 1.1.x, 3.x
   - 动态库: `libssl.so.3`, `libcrypto.so.3`
   - 安装方式: 系统包管理器
   - 说明: 如果工程编译并注册了 MbedTLS / WolfSSL，`CreateSSLLibrary()` 会按优先级选择可用后端，而不是把 Linux 固定到 OpenSSL

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
# 当前默认编译门禁
python3 scripts/compile_all_modules.py

# 当前本地最小门禁
bash scripts/run_minimal_ci_gate.sh --fast-local

# 可选：只检查 Phase 2 基准入口
bash scripts/run_phase2_performance_baseline.sh --dry-run --fast-local
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

1. **OpenSSL** (默认/最常见入口)
   - 版本: 1.1.x, 3.x (推荐 3.x)
   - 动态库: `libssl.3.dylib`, `libcrypto.3.dylib`
   - 安装方式: Homebrew
   - 说明: 如果工程编译并注册了 MbedTLS / WolfSSL，`CreateSSLLibrary()` 会按优先级选择可用后端，而不是把 macOS 固定到 OpenSSL

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
# 当前建议先做 focused compile smoke
mkdir -p tmp/platform_support_macos
fpc -B -Fu./src \
    -FUtmp/platform_support_macos \
    -FEtmp/platform_support_macos \
    -otmp/platform_support_macos/test_openssl_simple \
    tests/openssl/test_openssl_simple.pas
./tmp/platform_support_macos/test_openssl_simple
```

如需继续对齐当前 release-control 状态，先回到 `ROADMAP.md`、`plans/2026-05-12-release-v1.5.0-formalization.md` 与 `test_reports/RELEASE_READINESS_V1.5.0.md`；只有在需要核对 Wave C closeout / approval 材料时，再回看 `WAVE_C_CLOSEOUT_STATUS_2026-03-18.md` 与 `WAVE_C_LOCAL_FIRST_AND_PRE_CI_CHAIN_STATUS_2026-03-16.md`。

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

工厂方法会在已注册且当前可用的实现里，选择 highest-priority available backend，而不是按平台硬编码单一路径。

当前注册优先级为：`WinSSL=200, MbedTLS=175, WolfSSL=150, OpenSSL=100`。

```pascal
uses fafafa.ssl.factory;

var
  Lib: ISSLLibrary;
begin
  // 自动选择当前优先级最高、且真正可用的后端
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

| 特性          | OpenSSL             | WinSSL       |
| ------------- | ------------------- | ------------ |
| **平台**      | Windows/Linux/macOS | 仅 Windows   |
| **依赖**      | 需要 OpenSSL 库     | 零依赖       |
| **TLS 版本**  | 1.0-1.3             | 1.0-1.3      |
| **性能**      | 优秀                | 优秀         |
| **证书管理**  | 文件/内存           | 系统证书存储 |
| **FIPS 模式** | 支持                | 支持         |

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

### GitHub Actions（当前口径）

默认启用的是 Linux minimal gate（`ci.yml`），其执行入口与本地 smoke 对齐：

```yaml
# .github/workflows/ci.yml（节选）
jobs:
  minimal-gate-linux:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: bash scripts/run_minimal_ci_gate.sh --fast-local
```

### 当前状态

- ✅ Linux minimal gate：启用（push/PR 自动触发）
- ✅ TLS13 signer gate：启用（按路径触发 + 手动）
- ✅ Wave B/B2 跨平台手动门禁：启用（workflow_dispatch）
- ⏸ 全量多平台 workflow：默认禁用（模板保留：`.github/workflows/test-all-platforms.yml.disabled`）

> 备注：历史多平台矩阵草案保留为 `.github/workflows/ci-matrix-draft.yml.disabled`，需要时可按需启用。

---

## 📚 相关文档

- [快速入门](guides/QUICKSTART.md)
- [入门指南](guides/GETTING_STARTED.md)
- [API 参考](reference/API_REFERENCE.md)
- [故障排除](guides/TROUBLESHOOTING.md)
- [WinSSL 用户指南](guides/WINSSL_USER_GUIDE.md)

---

## 🤝 贡献

如果您在特定平台上遇到问题或有改进建议,请:

1. 查看 [故障排除文档](guides/TROUBLESHOOTING.md)
2. 搜索现有 [Issues](https://github.com/your-repo/fafafa.ssl/issues)
3. 创建新 Issue 并提供详细信息:
   - 操作系统和版本
   - Free Pascal 版本
   - OpenSSL 版本
   - 错误信息和日志

---

**维护者**: fafafa.ssl 团队
**许可证**: [LICENSE](../LICENSE)
