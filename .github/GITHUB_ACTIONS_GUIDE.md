# GitHub Actions CI/CD 流水线使用指南

## 🎉 已自动配置的流水线

我们已为 fafafa.ssl 项目创建了完整的 GitHub Actions CI/CD 流水线，包含：

### 📦 5 个主要工作流

1. **`test-all-platforms.yml`** - 多平台测试 (Windows/Linux/macOS)
2. **`code-quality.yml`** - 代码质量检查
3. **`winssl-tests.yml`** - WinSSL 详细测试 (仅 Windows)
4. **`performance.yml`** - 性能基准测试
5. **`pr-checks.yml`** - PR 自动检查

---

## 🚀 如何使用

### 方式 1：推送代码自动触发

```bash
git add .
git commit -m "feat: 新功能"
git push origin master
```

**自动触发**：
- ✅ 所有平台的测试
- ✅ 代码质量检查
- ✅ 性能基准测试
- ✅ WinSSL 测试（仅 Windows）

### 方式 2：手动触发特定测试

在 GitHub 仓库页面：
1. 进入 **Actions** 标签
2. 选择工作流
3. 点击 **Run workflow**
4. 选择参数并运行

**可用参数**：
- `test_suite`: all, winssl, performance
- `benchmark`: all, crypto, ssl, memory

### 方式 3：PR 自动检查

创建 PR 时自动运行：
- ✅ 代码风格检查
- ✅ 编译验证
- ✅ 模块完整性检查
- ✅ 安全扫描

---

## 📊 测试覆盖范围

### 🪟 Windows 测试 (包含 WinSSL!)
- **OpenSSL 测试**:
  - Core 模块 (6/6)
  - P1 高优先级 (14/14)
  - P2 中优先级 (11/11) - PKCS#7, PKCS#12, CMS, OCSP, CT, TS, Engine
  - P3 低优先级 (15/15)

- **WinSSL 测试**:
  - ✅ 库初始化 (TWinSSLLibrary)
  - ✅ 上下文创建 (TWinSSLContext)
  - ✅ 连接管理 (TWinSSLConnection)
  - ✅ 客户端握手 (ClientHandshake)
  - ✅ 服务器握手 (ServerHandshake)
  - ✅ 会话管理 (TWinSSLSession/TWinSSLSessionManager)
  - ✅ 证书处理 (TWinSSLCertificate)

### 🐧 Linux 测试
- 完整的 OpenSSL 测试套件
- 跨平台兼容性验证
- 代码风格检查

### 🍎 macOS 测试
- OpenSSL 功能测试
- 跨平台兼容性验证

---

## 💰 费用说明

### ✅ **完全免费** (公共仓库)

- **免费额度**: 2000 分钟/月
- **典型使用**: 10-15 分钟/次
- **可运行**: 130+ 次/月
- **完全够用**: 日常开发和测试

### 💡 优化措施

- **并发控制**: 避免重复运行
- **缓存机制**: 加速构建 (FPC packages, Lazarus)
- **智能触发**: 只在必要时运行

---

## 🎯 立即开始使用

### 1. 推送到 GitHub

```bash
git init
git add .
git commit -m "Initial commit with CI/CD"
git branch -M master
git remote add origin https://github.com/YOUR_USERNAME/fafafa.ssl.git
git push -u origin master
```

### 2. 等待 CI/CD 运行

推送后 2-3 分钟内，GitHub Actions 会自动运行：
1. 🏗️ 构建所有测试程序
2. 🧪 运行测试套件
3. 📊 生成测试报告
4. ✅ 标记结果 (成功/失败)

### 3. 查看结果

在仓库的 **Actions** 标签查看：
- 实时构建日志
- 详细测试报告
- 性能基准数据
- 代码质量评分

---

## 📋 工作流详情

### 1. test-all-platforms.yml

**触发时机**: 每次 push 或 PR
**运行平台**: Windows, Linux, macOS
**FPC 版本**: 3.2.2, 3.3.1
**包含**:
- 编译所有测试程序
- 运行 OpenSSL 测试 (所有平台)
- 运行 WinSSL 测试 (仅 Windows)
- 生成汇总报告

**运行时长**: ~20 分钟

### 2. code-quality.yml

**触发时机**: 每次 push 或 PR
**检查项目**:
- 代码风格 (使用 `scripts/check_code_style.py`)
- 文档完整性 (README.md, CLAUDE.md 等)
- 编译验证 (跨 FPC 版本)
- 模块完整性 (接口一致性)
- 安全检查 (硬编码密钥检测)

**运行时长**: ~5 分钟

### 3. winssl-tests.yml

**触发时机**: 手动触发或 Windows 推送
**运行平台**: Windows only
**专门测试**:
- WinSSL 库初始化
- 客户端/服务器模式
- 会话管理
- 证书处理
- API 绑定验证

**运行时长**: ~10 分钟

### 4. performance.yml

**触发时机**: 推送到 master 或手动触发
**运行平台**: Windows, Linux, macOS
**基准测试**:
- 加密操作性能 (AES, SHA, RSA)
- TLS 握手性能
- 内存使用分析
- 跨平台性能对比

**运行时长**: ~15 分钟

---

## 🔧 自定义配置

### 修改触发条件

在 `.github/workflows/*.yml` 中编辑:

```yaml
on:
  push:
    branches: [ master, develop ]  # 改为你想要的分支
  pull_request:
    branches: [ master ]           # 改为你想要的分支
```

### 修改 FPC 版本

在矩阵中修改:

```yaml
strategy:
  matrix:
    fpc-version: [ '3.2.2', '3.3.1' ]  # 添加或删除版本
```

### 跳过某些测试

添加条件:

```yaml
- name: Run test
  if: runner.os != 'macOS'  # 在 macOS 上跳过
  run: ./run-test.sh
```

---

## 📊 查看测试结果

### 在 GitHub Actions 页面

1. 进入 **Actions** 标签
2. 点击具体的工作流运行
3. 查看每个作业的状态
4. 下载 artifacts (测试报告)

### 生成的报告

- **Test-Results-*.zip**: 编译后的测试程序
- **WinSSL-Test-Report.md**: WinSSL 详细测试报告
- **Performance-Report-*.md**: 性能基准报告
- **Code-Style-Report**: 代码风格检查结果

---

## 🚨 故障排除

### 测试失败

1. **查看日志**: Actions → 具体任务 → 展开日志
2. **常见问题**:
   - 编译错误 → 检查 Pascal 代码语法
   - 测试失败 → 检查测试逻辑
   - 超时 → 检查是否有死循环

### 权限问题

确保在 GitHub 仓库设置中：
1. 进入 **Settings** → **Actions**
2. 选择 **Allow all actions and workflows**

### 缓存问题

缓存可能过时，清理方法：
1. Actions → 右侧 **Caches** → Delete
2. 或修改缓存键来强制刷新

---

## 🎉 下一步

### 自动发布 (可选)

创建 `release.yml` 工作流：

```yaml
name: 🚀 Release
on:
  push:
    tags: [ 'v*' ]

jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Create Release
        uses: softprops/action-gh-release@v1
```

### 分支保护规则 (推荐)

在仓库设置中：
1. **Settings** → **Branches**
2. 添加规则保护 `master` 分支
3. 要求 PR 通过 CI/CD 检查

---

## 📞 获取帮助

- **GitHub Actions 文档**: https://docs.github.com/en/actions
- **Lazarus CI 插件**: https://github.com/lazarus-ide/setup-lazarus
- **FPC CI 插件**: https://github.com/pascalfpc/setup-fpascal

---

## 🎊 总结

**您现在拥有了**:
- ✅ 完整的 CI/CD 流水线
- ✅ 跨平台测试 (Windows/Linux/macOS)
- ✅ WinSSL 专用测试
- ✅ 代码质量保证
- ✅ 性能基准监控
- ✅ **完全免费** (公共仓库)

**无需额外配置，推送代码即可自动运行！** 🚀
