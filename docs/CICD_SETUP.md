# CI/CD配置指南

本文档说明`fafafa.ssl`项目的持续集成/持续部署(CI/CD)配置。

## 概述

项目使用GitHub Actions进行自动化构建、测试和发布。所有workflow配置位于`.github/workflows/`目录。

## 可用的Workflows

### 1. 🐧 Linux CI - Build & Test

**文件**: `.github/workflows/linux-ci.yml`

**触发条件**:
- Push到`master`/`main`/`develop`分支
- Pull Request到上述分支
- 手动触发

**功能**:
- ✅ 安装Free Pascal和依赖（OpenSSL, FCL）
- ✅ 编译所有核心模块（75个.pas文件）
- ✅ 运行测试套件
- ✅ 生成测试报告
- ✅ 上传测试artifacts

**运行环境**: Ubuntu Latest

**预计时长**: 5-10分钟

### 2. 🚀 Release Automation

**文件**: `.github/workflows/release.yml`

**触发条件**:
- Push版本标签（如`v1.0.0-rc.1`）
- 手动触发（需指定版本）

**功能**:
- ✅ 验证版本号（检测pre-release）
- ✅ 构建和测试
- ✅ 生成Release Notes
- ✅ 创建源码归档
- ✅ 发布GitHub Release

**运行环境**: Ubuntu Latest

**预计时长**: 10-15分钟

### 3. 🔍 Basic Code Checks

**文件**: `.github/workflows/basic-checks.yml`

**触发条件**:
- Push到`master`/`develop`分支
- Pull Request到上述分支

**功能**:
- ✅ 检查项目结构
- ✅ 验证必需文件
- ✅ 基础Pascal语法检查

**运行环境**: Ubuntu Latest

**预计时长**: 1-2分钟

## 使用指南

### 本地测试CI流程

在提交前，可以本地运行与CI相同的命令：

```bash
# 1. 编译所有模块
./build_linux.sh

# 2. 运行测试
./run_tests_linux.sh

# 3. 检查编译详情
python3 scripts/compile_all_modules.py
```

### 发布新版本

#### 方法1: 推送版本标签

```bash
# 1. 确保代码已提交
git add .
git commit -m "feat: prepare release v1.0.0-rc.1"

# 2. 创建并推送标签
git tag v1.0.0-rc.1
git push origin v1.0.0-rc.1

# 3. GitHub Actions自动创建Release
```

#### 方法2: 手动触发

1. 访问 GitHub仓库 → **Actions**
2. 选择 **🚀 Release Automation**
3. 点击 **Run workflow**
4. 输入版本号（如`v1.0.0-rc.1`）
5. 点击 **Run workflow**

### 查看CI结果

#### 在GitHub界面

1. 访问 **Actions** 标签
2. 选择对应的workflow run
3. 查看每个job的日志和artifacts

#### 在PR中

每个Pull Request会自动运行CI，结果显示在PR页面底部。

#### Job摘要

每个workflow运行完成后会生成摘要，包括：
- 环境信息
- 编译统计
- 测试结果
- 关键指标

## 配置和自定义

### 修改FPC版本

编辑`.github/workflows/linux-ci.yml`:

```yaml
env:
  FPC_VERSION: "3.2.2"  # 修改此处
```

**注意**: Ubuntu apt仓库可能不包含最新版本，需确认可用性。

### 添加更多测试

编辑`run_tests_linux.sh`，在`CORE_TESTS`数组中添加测试文件：

```bash
CORE_TESTS=(
    "tests/test_hash_utils.pas"
    "tests/test_openssl_simple.pas"
    "tests/your_new_test.pas"  # 添加新测试
)
```

### 调整超时时间

如果workflow经常超时，可增加`timeout-minutes`:

```yaml
jobs:
  build-and-test:
    timeout-minutes: 20  # 从15分钟增加到20分钟
```

### 添加Windows CI

创建`.github/workflows/windows-ci.yml`:

```yaml
name: 🪟 Windows CI

on:
  push:
    branches: [ master, main ]

jobs:
  build-windows:
    runs-on: windows-latest
    
    steps:
      - uses: actions/checkout@v4
      
      - name: Install FPC
        run: |
          choco install fpc
          
      - name: Build
        run: |
          fpc -Fusrc examples/01_basic_ssl_client.pas
```

**限制**: Windows runner需要额外配置FPC和OpenSSL路径。

## Artifacts说明

### test-results-linux

**内容**: 编译后的测试二进制文件

**位置**: `tests/bin/`

**保留期**: 7天

**下载方式**:
1. 进入workflow run页面
2. 滚动到底部 **Artifacts** 区域
3. 点击下载链接

**用途**:
- 调试测试失败问题
- 离线运行测试
- 对比不同commit的测试结果

## Release Notes模板

Release workflow使用内嵌的Release Notes模板，位于`.github/workflows/release.yml`的`Generate Release Notes`步骤。

### 自定义Release Notes

修改`release.yml`中的模板：

```yaml
- name: 📝 Generate Release Notes
  run: |
    cat > release_notes.md << 'EOF'
    # 你的自定义模板
    ...
    EOF
```

### 包含Changelog

如果项目有`CHANGELOG.md`，可以提取相关版本的changelog：

```yaml
- name: Extract changelog
  run: |
    sed -n '/^## \['$VERSION'\]/,/^## \[/p' CHANGELOG.md > section.md
```

## 常见问题

### Q: CI编译失败，本地编译成功？

**A**: 检查FCL路径差异

本地可能使用自定义路径：
```bash
$HOME/freePascal/fpc/units/x86_64-linux/
```

CI使用系统路径：
```bash
/usr/lib/fpc/3.2.2/
```

确保代码不依赖特定路径。

### Q: 测试在CI中失败？

**A**: 检查环境差异

- **网络**: CI可能无法访问外部HTTPS服务器
- **OpenSSL版本**: Ubuntu可能使用不同版本
- **文件权限**: CI环境权限可能受限

解决：
1. Mock外部依赖
2. 适配多个OpenSSL版本
3. 使用相对路径

### Q: Release workflow没有触发？

**A**: 检查tag格式

必须匹配`v*.*.*`格式：
- ✅ `v1.0.0`
- ✅ `v1.0.0-rc.1`
- ❌ `1.0.0` (缺少v前缀)
- ❌ `release-1.0` (格式不匹配)

### Q: 如何禁用某个workflow？

**A**: 两种方法

**方法1**: 重命名文件（推荐）
```bash
mv .github/workflows/linux-ci.yml .github/workflows/linux-ci.yml.disabled
```

**方法2**: 注释触发条件
```yaml
# on:
#   push:
#     branches: [ master ]
```

### Q: Artifacts上传失败？

**A**: 检查磁盘空间和文件大小

GitHub Actions限制：
- 单个artifact ≤ 2GB
- 总存储 ≤ 500MB（免费账户）

解决：压缩大文件或排除不必要的内容。

## 成本和配额

### GitHub Actions免费额度

**公共仓库**: 无限制

**私有仓库**（免费账户）:
- 2,000分钟/月（Linux runner）
- 500MB存储

**超出额度**: 需升级到付费计划

### 优化策略

1. **条件执行**: 仅在特定分支运行完整测试
```yaml
if: github.ref == 'refs/heads/master'
```

2. **缓存依赖**: 使用`actions/cache`缓存FPC单元
```yaml
- uses: actions/cache@v4
  with:
    path: ~/.fpc
    key: fpc-${{ runner.os }}-${{ hashFiles('fafafa_ssl.lpk') }}
```

3. **并发限制**: 取消旧的运行
```yaml
concurrency:
  group: ${{ github.workflow }}-${{ github.ref }}
  cancel-in-progress: true
```

## 安全考虑

### Secrets管理

如果需要访问私有资源（如私有registry），使用GitHub Secrets：

1. 仓库 → **Settings** → **Secrets and variables** → **Actions**
2. 添加secret（如`DEPLOY_TOKEN`）
3. 在workflow中引用：
```yaml
env:
  TOKEN: ${{ secrets.DEPLOY_TOKEN }}
```

### 权限控制

Release workflow需要写权限：

```yaml
permissions:
  contents: write  # 创建Release
```

限制其他workflow权限：

```yaml
permissions:
  contents: read  # 只读
```

## 进一步参考

- [GitHub Actions文档](https://docs.github.com/en/actions)
- [Free Pascal CI示例](https://wiki.freepascal.org/Continuous_Integration)
- [项目CI指南](.github/BASIC_CI_GUIDE.md)

---

## 测试质量审计集成

### 概述

CI管道集成了测试质量审计工具，用于自动评估测试覆盖率和质量指标。

### 审计命令

```bash
# 单独运行审计
./ci_pipeline.sh audit

# 作为完整管道的一部分运行
./ci_pipeline.sh all
```

### 质量门禁

审计工具会检查以下质量指标：

| 指标 | 目标阈值 | 说明 |
|------|----------|------|
| Overall Score | ≥70% | 综合质量评分 |
| Error Handling | ≥60% | 错误处理测试覆盖 |
| Thread Safety | ≥60% | 线程安全测试覆盖 |
| Backend Consistency | ≥60% | 后端一致性测试 |
| Crypto Testing | ≥60% | 加密功能测试 |
| Coverage | ≥60% | 代码覆盖率 |
| Boundary Testing | ≥80% | 边界条件测试 |

### 配置质量阈值

通过环境变量配置质量门禁阈值：

```bash
# 设置质量阈值为75%
QUALITY_THRESHOLD=75 ./ci_pipeline.sh audit
```

### 审计报告

审计工具生成以下报告：

- `reports/audit/audit_YYYYMMDD_HHMMSS.md` - Markdown格式报告
- `reports/audit/audit_YYYYMMDD_HHMMSS.json` - JSON格式报告
- `reports/audit/quality_trend.csv` - 质量趋势数据

### 质量趋势追踪

每次审计运行后，质量分数会追加到趋势文件中：

```csv
Date,Overall,Coverage,Boundary,Error,Crypto,Thread,Resource,Backend
20260106_120000,75,65,82,68,70,62,58,72
```

### GitHub Actions集成

在GitHub Actions中添加审计步骤：

```yaml
- name: 🔍 Run Test Quality Audit
  run: |
    ./ci_pipeline.sh audit
  env:
    QUALITY_THRESHOLD: 70

- name: 📊 Upload Audit Reports
  uses: actions/upload-artifact@v4
  with:
    name: audit-reports
    path: reports/audit/
    retention-days: 30
```

### 质量徽章

在README.md中添加质量徽章（需要配置徽章服务）：

```markdown
![Test Quality](https://img.shields.io/badge/test%20quality-75%25-green)
```

### 本地审计

开发者可以在提交前本地运行审计：

```bash
# 编译审计工具
fpc -Fusrc -Futools/test_audit -otools/test_audit/bin/test_audit tools/test_audit/test_audit_main.pas

# 运行审计
./tools/test_audit/bin/test_audit -s src -t tests -o reports/audit -v
```

---

**更新日期**: 2025-10-28  
**适用版本**: fafafa.ssl v1.0.0-rc  
**维护者**: fafafa.ssl团队

