# Phase 3 CI：示例编译验证脚本草案

> **Batch**: B70
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本草案定义了示例编译验证脚本的设计与实现，用于 CI 门禁和本地开发验证。

## 脚本位置

```
scripts/verify_examples_compile.sh
```

## 功能特性

### 核心功能

1. **自动扫描**: 递归扫描 `examples/` 目录下所有 `.pas` 文件
2. **编译验证**: 使用 FPC 编译每个示例文件
3. **平台感知**: 自动跳过平台不兼容的示例（如 Linux 下跳过 WinSSL 示例）
4. **多格式输出**: 支持 text、json、markdown 三种输出格式

### 命令行选项

| 选项 | 说明 |
|------|------|
| `-v, --verbose` | 显示详细编译输出 |
| `-s, --stop-on-error` | 遇到第一个错误时停止 |
| `-f, --format FORMAT` | 输出格式: text, json, markdown |
| `-o, --output FILE` | 输出报告到文件 |
| `-h, --help` | 显示帮助信息 |

## 使用示例

```bash
# 基本用法
./scripts/verify_examples_compile.sh

# 详细模式
./scripts/verify_examples_compile.sh -v

# 生成 Markdown 报告
./scripts/verify_examples_compile.sh -f markdown -o examples_report.md

# CI 模式（遇错停止）
./scripts/verify_examples_compile.sh -s
```

## 首次执行结果

### 执行环境

- **日期**: 2026-02-07
- **FPC 版本**: 3.3.1
- **平台**: Linux x86_64

### 统计摘要

| 指标 | 数值 |
|------|------|
| 总计 | 75 |
| 通过 | 37 |
| 失败 | 34 |
| 跳过 | 4 |
| 通过率 | 52.1% |

### 失败分析

失败的示例主要分为以下几类：

1. **缺少依赖单元**: 部分示例引用了尚未实现的单元
2. **API 变更**: 部分示例使用了已变更的 API
3. **平台特定**: 部分示例依赖特定平台功能

### 后续行动

1. **P1 - 高优先级**: 修复核心示例（01-10 系列）
2. **P2 - 中优先级**: 更新 API 变更相关示例
3. **P3 - 低优先级**: 清理或归档过时示例

## CI 集成

### GitHub Actions 集成

```yaml
- name: Verify Examples Compile
  run: ./scripts/verify_examples_compile.sh -f json -o examples_report.json

- name: Upload Examples Report
  uses: actions/upload-artifact@v4
  with:
    name: examples-compile-report
    path: examples_report.json
```

### 门禁策略

| 阶段 | 通过率要求 | 说明 |
|------|-----------|------|
| 开发 | 无要求 | 仅报告 |
| PR | ≥ 80% | 警告 |
| 发布 | ≥ 95% | 阻断 |

## 相关文档

- `docs/guides/QUICKSTART_30SEC.md` - 30 秒示例索引
- `docs/guides/COMMON_PITFALLS.md` - 常见陷阱指南
- `.github/workflows/ci-matrix-draft.yml.disabled` - CI 工作流草案（默认禁用）

## 变更历史

| 日期 | 版本 | 变更 |
|------|------|------|
| 2026-02-07 | draft | 初始版本 |
