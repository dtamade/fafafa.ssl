# 私有仓库 GitHub Actions 使用指南

## 💰 私有仓库费用说明

### 免费额度 (每月)
- **GitHub Free**: 500 分钟
- **GitHub Pro**: 1000 分钟
- **GitHub Team**: 2000 分钟
- **GitHub Enterprise**: 3000 分钟

### 我们的 CI/CD 使用量
- **完整测试**: ~15-20 分钟/次
- **PR 检查**: ~5 分钟/次
- **WinSSL 测试**: ~10 分钟/次

### 💡 优化建议 (节省 50-70% 时间)

#### 1. 限制触发条件
修改工作流，只在必要时运行：

```yaml
on:
  push:
    branches: [ master ]
    paths:
      - 'src/**'
      - 'tests/**'
      - '.github/workflows/**'
  pull_request:
    branches: [ master ]
    paths:
      - 'src/**'
      - 'tests/**'
```

#### 2. 使用条件运行

```yaml
- name: Run WinSSL Tests
  if: runner.os == 'Windows' && contains(github.event.head_commit.message, 'winssl')
  run: echo "Only run on WinSSL changes"
```

#### 3. 减少矩阵组合

```yaml
strategy:
  matrix:
    fpc-version: [ '3.2.2' ]  # 只测试一个版本
    # os: [ubuntu-latest]     # 只测试 Linux
```

#### 4. 并行作业优化

```yaml
# 代码质量检查 - 快速
code-quality:
  runs-on: ubuntu-latest
  timeout-minutes: 10

# 编译检查 - 中等
build:
  runs-on: ubuntu-latest
  timeout-minutes: 30

# 完整测试 - 慢
full-tests:
  runs-on: ubuntu-latest
  timeout-minutes: 60
  needs: [code-quality, build]
```

---

## 🆓 切换到公共仓库 (推荐)

### 优势
- ✅ **2000 分钟/月** 免费
- ✅ **完全够用** (130+ 次运行/月)
- ✅ **开放协作** (社区贡献)
- ✅ **透明开发** (公开进度)

### 如何切换
1. GitHub 仓库 → Settings
2. 滚动到底部 → **Danger Zone**
3. 点击 **Change visibility** → **Make public**

⚠️ **注意**: 这会使代码公开可见

---

## 💸 付费方案 (如果必须用私有仓库)

### GitHub Pro - $4/月
- ✅ 1000 分钟/月
- ✅ 足够日常开发 (50+ 次完整测试)
- ✅ 私有仓库
- ✅ Issue 管理

### GitHub Team - $4/用户/月
- ✅ 2000 分钟/月
- ✅ 团队协作
- ✅ 高级功能
- ✅ 适合 5-10 人团队

---

## 🎯 针对私有仓库的优化配置

让我为您创建一个**轻量级**的工作流配置：
