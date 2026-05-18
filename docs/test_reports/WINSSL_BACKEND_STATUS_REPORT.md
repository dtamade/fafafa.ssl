# WinSSL 后端实现状态报告

> **Status**: draft
> **Updated**: 2026-05-18

## 概述

本报告只记录 **当前已经拿到证据的 WinSSL 真相**。它区分三层边界：

1. public surface 和源码结构
2. Linux 主机上可重复的 source/compile proof
3. 仍然需要 Windows 主机的 runtime proof，以及足够强的 artifact evidence

这意味着报告不会再把“代码存在”直接写成“Windows runtime 已验证”。

## 当前已证实

### 源码与结构

| 区域     | 当前真相                                                                                                                              |
| -------- | ------------------------------------------------------------------------------------------------------------------------------------- |
| 上下文   | `src/fafafa.ssl.winssl.context.pas` 是当前 canonical context 实现，公开暴露 `ISSLNativeHandleAccess`                                  |
| 连接     | `src/fafafa.ssl.winssl.connection.pas` 是当前 canonical connection 实现                                                               |
| session  | `src/fafafa.ssl.winssl.connection.pas` 内的 `TWinSSLSession` 是当前 truth source；`src/fafafa.ssl.winssl.session.pas` 只保留兼容 shim |
| 内部协作 | connection 不再把 `ISSLContext` / `ISSLLibrary` 硬转成 `TWinSSLContext` / `TWinSSLLibrary`，而是走 internal access interface          |

### Linux 上已经复现的证据

| 证据                                                                       | 结果                                                                |
| -------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| `tests/scripts/test_winssl_session_truth_source_contract.sh`               | session truth-source / fake native-handle surface 已锁住            |
| `tests/scripts/test_winssl_connection_context_access_contract.sh`          | connection/context/library access seam 已锁住                       |
| Win64 focused cross-target compile                                         | 通过，且 `ISSLContext`/`ISSLLibrary` 到具体类的不安全类型告警已消失 |
| `tests/integration/test_backend_comparison.pas` Win64 cross-target compile | 通过，说明共享 replay-store compile drift 已清掉                    |
| `python3 scripts/compile_all_modules.py`                                   | `185/185`                                                           |
| `bash scripts/run_minimal_ci_gate.sh --fast-local`                         | `[PASS]`                                                            |

## 当前 capability truth

以下表述以 `src/fafafa.ssl.winssl.lib.pas` 的 `GetCapabilities` 和 `docs/BACKEND_CAPABILITY_MATRIX.md` 为准。

| 能力                                 | 当前 truth                | 备注                                                                    |
| ------------------------------------ | ------------------------- | ----------------------------------------------------------------------- |
| TLS 1.2                              | 支持                      | Windows capability surface                                              |
| TLS 1.3                              | 条件支持                  | 仅在 Windows 10 build `18362+` / 对应较新 Server build 发布为 supported |
| DTLS                                 | 不支持                    | `GetCapabilities.SupportsDTLS=False`                                    |
| Early Data (0-RTT)                   | 不支持                    | 当前不暴露 `ISSLEarlyDataContext` public surface                        |
| caller-provided server OCSP stapling | 不支持                    | 当前不暴露 `ISSLServerOCSPStaplingContext` public surface               |
| SNI                                  | 支持                      | public capability 已对齐                                                |
| ALPN                                 | 条件支持                  | 受 Windows 版本影响                                                     |
| Session resumption / tickets         | public capability 存在    | 真实 Windows runtime 行为仍需独立证明                                   |
| Native handle access                 | context / connection 暴露 | session 不暴露 `ISSLNativeHandleAccess`                                 |

## GitHub Windows runner 当前真相

- `wave-b-b2-manual.yml` 的 live run `26030261335` 已证明 GitHub Actions `windows-latest` 能真实执行：
  - quick smoke
  - WinSSL minimal gate
  - broader `tests/run_winssl_tests.ps1`
- 但下载下来的 `winssl_runtime_suite_wave_b_b2_20260518_191939.log` 只有 transcript 壳
- 同一次 run 的 job console log 则明确显示 broader suite 编译并运行了 6 个 lane

这说明当时的问题不是“Windows runtime 根本没跑”，而是“artifact 证据强度不够”。

- 修复后的 live rerun `26031191987`（head `fa7f5af`）已经把这个缺口补上：
  - `winssl_runtime_suite_wave_b_b2_20260518_193941_evidence_fix.log` 直接保存了 broader suite 的编译、逐项执行、汇总输出
  - 日志中明确包含 `[WINSSL-RUNTIME] suite_start / suite_summary / suite_end status=PASS`
  - `wave_b_b2_evidence_consistency_wave_b_b2_20260518_193941_evidence_fix.md` 也已把 `windows_runtime_transcript` 记成 `substantive runtime evidence; suite_end_status=PASS`

## 当前还没有证实的部分

- Windows 主机上的真实握手路径
- 真实系统证书存储加载与企业策略交互
- 真实 session resumption / session tickets 行为
- 真实 server/client runtime 的 OCSP、证书验证、错误映射细节
- 新 workflow 产出的 runtime artifact 是否已经从 transcript 壳升级成 substantive evidence

**原因**:

- 本机 Linux 上的 `wine` 当前直接退出 `159`
- 因此本地 Linux 仍不能独立承担 WinSSL runtime proof
- 但 GitHub Actions Windows runner 已经是当前可用的 live proof surface

## 结论

现在对 WinSSL 更准确的说法是：

- **代码结构和 compile surface 持续收口中，且当前已通过选定的 source contract 与 Win64 交叉编译验证**
- **仓库级 Linux gate 继续全绿**
- **GitHub Windows runner 现在已经同时给出“实际执行 + substantive artifact evidence”**
- **真正剩余的高风险未证实区域，已经前移到高风险 runtime lane 的逐项结论，而不再是 workflow capture 本身**

## 下一步

1. 以 run `26031191987` 的 artifact 作为当前 Wave B/B2 manual lane 的 Windows runtime baseline
2. 若继续深挖 WinSSL，优先扩展 session resumption / certstore / OCSP / enterprise / error-mapping 等高风险 lane 的 live proof
3. 继续保持 Linux 侧 source contract 和 Win64 compile 作为前置守门，不把它们误写成 runtime 证明

## 相关文档

- [后端能力矩阵](../BACKEND_CAPABILITY_MATRIX.md)
- [WinSSL 设计文档](../reference/WINSSL_DESIGN.md)
- [Windows 运行时验证清单](../../tests/windows/WINDOWS_VALIDATION_CHECKLIST.md)
- [Windows bundle inventory](../../tests/windows/VALIDATION_BUNDLE.md)
