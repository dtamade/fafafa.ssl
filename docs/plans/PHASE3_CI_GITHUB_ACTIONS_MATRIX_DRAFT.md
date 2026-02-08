# Phase 3 CI：GitHub Actions 多平台矩阵测试工作流草案

> **Batch**: B63
> **Status**: draft
> **Created**: 2026-02-07

## 目标

建立支持 Linux/macOS/Windows 多平台的 CI 矩阵测试工作流。

## 工作流文件

- **路径**: `.github/workflows/ci-matrix-draft.yml`
- **状态**: 草案，需要根据实际环境调整

## 工作流结构

```
┌─────────────────┐
│  compile-gate   │  快速编译门禁
└────────┬────────┘
         │
    ┌────┴────┐
    │         │
    v         v
┌───────┐ ┌───────┐ ┌─────────┐
│ Linux │ │ macOS │ │ Windows │
│ Matrix│ │ Test  │ │  Test   │
└───┬───┘ └───┬───┘ └────┬────┘
    │         │          │
    └────┬────┴──────────┘
         │
         v
┌─────────────────┐
│  test-summary   │  汇总报告
└─────────────────┘
```

## Jobs 说明

### 1. compile-gate

- **运行环境**: ubuntu-latest
- **功能**: 快速编译门禁，失败则阻止后续测试
- **脚本**: `python3 scripts/compile_all_modules.py`

### 2. linux-matrix

- **运行环境**: ubuntu-latest
- **矩阵**: OpenSSL 3.0/3.1/3.2
- **功能**: Linux 平台多 OpenSSL 版本测试

### 3. macos-test

- **运行环境**: macos-latest
- **依赖**: Homebrew FPC + OpenSSL@3
- **注意**: 需要设置 `DYLD_LIBRARY_PATH`

### 4. windows-test

- **运行环境**: windows-latest
- **依赖**: Chocolatey FPC
- **特点**: 可使用 WinSSL 后端，无需 OpenSSL DLL

### 5. test-summary

- **功能**: 汇总所有平台测试结果
- **输出**: GitHub Step Summary

## 触发条件

```yaml
on:
  push:
    branches: [ master, main, develop ]
  pull_request:
    branches: [ master, main ]
  workflow_dispatch:
    inputs:
      skip_windows: 'false'
      skip_macos: 'false'
```

## 手动触发

```bash
# 使用 GitHub CLI
gh workflow run ci-matrix-draft.yml

# 跳过 Windows 测试
gh workflow run ci-matrix-draft.yml -f skip_windows=true
```

## 产物保留

| 产物 | 保留天数 |
|------|----------|
| compile-gate-results | 7 |
| linux-openssl-*-reports | 14 |
| macos-reports | 14 |
| windows-reports | 14 |
| all-platform-reports | 30 |

## 已知限制

1. **macOS**: Homebrew OpenSSL 路径可能因 runner 版本变化
2. **Windows**: FPC Chocolatey 包版本可能滞后
3. **OpenSSL 矩阵**: Ubuntu apt 仓库可能不提供所有版本

## 后续优化

- [ ] 添加 OpenSSL 1.1.1 兼容性测试
- [ ] 添加性能基准测试 job
- [ ] 添加文档构建 job
- [ ] 添加发布自动化 job

## 相关文档

- `docs/plans/PHASE3_MINIMAL_CI_GATE_DRAFT.md`
- `docs/plans/PHASE3_OPENSSL_MATRIX_COMMAND_DRAFT.md`
- `docs/plans/PHASE3_CI_ARTIFACT_ARCHIVE_STRATEGY_DRAFT.md`
