# WinSSL 后端实现状态报告

> **Batch**: B76
> **Status**: draft
> **Created**: 2026-02-07

## 概述

本报告记录 WinSSL (Windows Schannel) 后端的当前实现状态。

## 实现状态

### 已实现模块

| 模块     | 文件                                                                   | 状态                   |
| -------- | ---------------------------------------------------------------------- | ---------------------- |
| API 绑定 | `fafafa.ssl.winssl.api.pas`                                            | ✅ 完成                |
| 库加载   | `fafafa.ssl.winssl.lib.pas`                                            | ✅ 完成                |
| 基础类型 | `fafafa.ssl.winssl.base.pas`                                           | ✅ 完成                |
| 上下文   | `fafafa.ssl.winssl.context.pas`                                        | ✅ 完成                |
| 连接     | `fafafa.ssl.winssl.connection.pas`                                     | ✅ 完成                |
| 证书     | `fafafa.ssl.winssl.certificate.pas`                                    | ✅ 完成                |
| 证书存储 | `fafafa.ssl.winssl.certstore.pas`                                      | ✅ 完成                |
| 会话     | `fafafa.ssl.winssl.connection.pas`（`winssl.session.pas` 为兼容 shim） | ✅ source truth 已收敛 |
| 原生句柄 | `fafafa.ssl.winssl.native_handle.pas`                                  | ✅ 完成                |
| 错误处理 | `fafafa.ssl.winssl.errors.pas`                                         | ✅ 完成                |
| 工具函数 | `fafafa.ssl.winssl.utils.pas`                                          | ✅ 完成                |
| 企业功能 | `fafafa.ssl.winssl.enterprise.pas`                                     | ✅ 完成                |

### 测试状态

| 测试     | 文件                                | 平台    |
| -------- | ----------------------------------- | ------- |
| 快速验证 | `tests/quick_winssl_validation.ps1` | Windows |
| 完整测试 | `tests/run_winssl_tests.ps1`        | Windows |

Linux 侧现在还能补两类静态证据：

- 选定的 WinSSL / backend comparison 用例已经可以继续做 Win64 交叉编译验证。
- `tests/integration/test_backend_comparison.pas` 现在也能成功交叉编译到 Win64，说明共享 replay-store 单元上的 target-conditioned compile drift 已被清掉。
- 这能证明共享源码和目标平台编译面仍然闭合，但它不等于 runtime proof。

**注意**: WinSSL 运行时测试仍然需要可用的 Windows 环境。本机 Linux 上的 `wine` 当前直接退出 `159`，且没有 `pwsh`，所以不能把本地 `wine` 执行结果当成 WinSSL runtime 证据。

## 功能覆盖

### TLS 协议

| 功能     | 状态    | 说明              |
| -------- | ------- | ----------------- |
| TLS 1.2  | ✅ 支持 | 所有 Windows 版本 |
| TLS 1.3  | ✅ 支持 | Windows 10 1903+  |
| DTLS 1.2 | ✅ 支持 | Windows 10+       |

### 证书功能

| 功能          | 状态    | 说明          |
| ------------- | ------- | ------------- |
| 系统证书存储  | ✅ 支持 | 原生集成      |
| 证书链验证    | ✅ 支持 | CryptoAPI     |
| OCSP          | ✅ 支持 | 自动检查      |
| OCSP Stapling | ✅ 支持 | 服务器端      |
| 证书固定      | ✅ 支持 | 通过回调      |
| SNI           | ✅ 支持 | 客户端/服务器 |

### 会话管理

| 功能           | 状态    | 说明     |
| -------------- | ------- | -------- |
| Session 复用   | ✅ 支持 | 完整支持 |
| Session Ticket | ✅ 支持 | TLS 1.2+ |
| Session Cache  | ✅ 支持 | 系统管理 |

### 高级功能

| 功能       | 状态    | 说明         |
| ---------- | ------- | ------------ |
| ALPN       | ✅ 支持 | Windows 8.1+ |
| 客户端证书 | ✅ 支持 | 双向 TLS     |
| 智能卡     | ✅ 支持 | 原生支持     |
| TPM        | ✅ 支持 | 硬件密钥     |

## 与其他后端对比

| 特性      | WinSSL     | OpenSSL   | MbedTLS   |
| --------- | ---------- | --------- | --------- |
| 安装依赖  | 无         | 需要 DLL  | 需要 DLL  |
| 系统集成  | 原生       | 独立      | 独立      |
| 证书存储  | 系统存储   | 文件/内存 | 文件/内存 |
| FIPS 模式 | 系统级     | 库级      | 无        |
| 跨平台    | 仅 Windows | 全平台    | 全平台    |
| PSK       | ❌         | ✅        | ✅        |
| Ed25519   | ❌         | ✅        | ⚠️        |

## 平台支持

| Windows 版本        | 支持状态 | TLS 1.3 |
| ------------------- | -------- | ------- |
| Windows 11          | ✅ 完整  | ✅      |
| Windows 10 1903+    | ✅ 完整  | ✅      |
| Windows 10 1809-    | ✅ 支持  | ❌      |
| Windows 8.1         | ✅ 支持  | ❌      |
| Windows 7 SP1       | ⚠️ 部分  | ❌      |
| Windows Server 2022 | ✅ 完整  | ✅      |
| Windows Server 2019 | ✅ 支持  | ⚠️      |
| Windows Server 2016 | ✅ 支持  | ❌      |

## 优势

1. **零依赖**: 无需安装额外 DLL
2. **系统集成**: 自动使用 Windows 证书存储
3. **自动更新**: 通过 Windows Update 获取安全更新
4. **硬件支持**: 原生支持智能卡和 TPM
5. **企业功能**: 支持组策略配置

## 限制

1. **仅 Windows**: 不支持其他操作系统
2. **PSK 不支持**: Schannel 不支持预共享密钥
3. **Ed25519 不支持**: 不支持 Edwards 曲线
4. **调试困难**: 错误信息不如 OpenSSL 详细

## 后续工作

1. **Windows 平台测试**: 在 Windows CI 环境中运行完整测试
2. **性能基准**: 与 OpenSSL 对比
3. **文档完善**: 使用指南和示例

## 相关文档

- `docs/reference/WINSSL_BACKEND_CAPABILITY_MATRIX.md` - 能力矩阵
- `docs/guides/WINSSL_QUICKSTART.md` - 快速开始
- `examples/winssl_*.pas` - WinSSL 示例
