# WinSSL Windows Validation Bundle Inventory

这份 bundle 不是一套独立打包物，而是当前仓库里用于 Windows runtime proof 的真实入口映射。

如果你在旧笔记里还看到 `Run-WindowsValidation.ps1`、`Run-QuickValidation.ps1`、`test_cert_load`、`test_factory_mode` 这一类名称，按旧模板处理就会偏离当前仓库真相。现在以这份 inventory 为准。

## 先看当前 bundle 由哪些文件组成

| 文件                                            | 角色                        | 什么时候用                              |
| ----------------------------------------------- | --------------------------- | --------------------------------------- |
| `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` | 当前执行清单                | 在 Windows 主机按步骤跑实机验证时       |
| `tests/windows/VALIDATION_BUNDLE.md`            | 当前 inventory              | 想确认入口、产物和 targeted lane 映射时 |
| `tests/quick_winssl_validation.ps1`             | quick smoke                 | 先验证证书加载和基础编译路径            |
| `run_winssl_tests.ps1`                          | minimal WinSSL-only runner  | 先隔离 WinSSL 最小 gate 是否通过        |
| `run_openssl_tests.ps1`                         | minimal OpenSSL runner      | 作为 Wave B Windows gate 的相邻步骤     |
| `scripts/run_wave_b_windows_gate.ps1`           | Windows gate orchestration  | 需要统一 summary 和分步骤 log 时        |
| `scripts/validate_all_modules.ps1`              | OpenSSL 单元批量编译校验    | 需要保住 Windows gate 的 modules 证据时 |
| `tests/run_winssl_tests.ps1`                    | broader manual WinSSL suite | 需要更宽的 WinSSL runtime 证据时        |
| `tests/winssl/*.lpi` / `tests/winssl/*.pas`     | 实际 WinSSL 测试项目        | 需要单独追某个高风险 lane 时            |
| `tests/integration/test_backend_comparison.lpi` | cross-backend comparison    | 需要比较 WinSSL 和其他后端的行为时      |

## 推荐执行顺序

从仓库根目录执行:

```powershell
powershell -ExecutionPolicy Bypass -File .\tests\quick_winssl_validation.ps1
powershell -ExecutionPolicy Bypass -File .\run_winssl_tests.ps1 -RunId winssl_min_20260505 -OutputDir test-reports
powershell -ExecutionPolicy Bypass -File .\scripts\run_wave_b_windows_gate.ps1 -RunId wave_b_windows_20260505 -OutputDir test-reports
powershell -ExecutionPolicy Bypass -File .\tests\run_winssl_tests.ps1
```

这四步对应四个层次:

1. quick smoke
2. WinSSL minimal gate
3. full Wave B Windows gate
4. broader manual WinSSL suite

## 这套 bundle 会产出什么

真正应该保存的，是这些可审查的产物:

| 产物                                                   | 来源                                  |
| ------------------------------------------------------ | ------------------------------------- |
| `test-reports/wave_b_windows_gate_summary_<run_id>.md` | `scripts/run_wave_b_windows_gate.ps1` |
| `test-reports/wave_b_windows_winssl_<run_id>.log`      | `scripts/run_wave_b_windows_gate.ps1` |
| `test-reports/wave_b_windows_openssl_<run_id>.log`     | `scripts/run_wave_b_windows_gate.ps1` |
| `test-reports/wave_b_windows_modules_<run_id>.log`     | `scripts/run_wave_b_windows_gate.ps1` |
| `test-reports/validate_all_modules_report_<run_id>.md` | `scripts/validate_all_modules.ps1`    |
| wider suite transcript / console capture               | `tests/run_winssl_tests.ps1`          |

如果只留下“我跑过了”，没有这些 summary / log / transcript，后续审查仍然会回到猜。

## 高风险区域和对应入口

Windows runtime proof 目前最值得盯的不是“全部跑一遍”，而是下面这些 lane:

| 风险区域                     | 入口                                                                                                         |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------ |
| 基础 API / 库可用性          | `tests/winssl/test_winssl_api_basic.pas`, `tests/unit/test_winssl_comprehensive.pas`                         |
| 证书加载 / 证书存储          | `tests/winssl/test_winssl_certificate_loading.lpi`, `tests/winssl/test_winssl_certstore.lpi`                 |
| 握手 / HTTPS client          | `tests/winssl/test_winssl_handshake_debug.lpi`, `tests/winssl/test_winssl_https_client.lpi`                  |
| Session resumption / tickets | `tests/winssl/test_winssl_session_resumption.lpi`, `tests/winssl/test_winssl_session_management.lpi`         |
| 错误映射 / online flow       | `tests/winssl/test_winssl_error_mapping_online.lpi`, `tests/winssl/test_winssl_hostname_mismatch_online.lpi` |
| mTLS / enterprise path       | `tests/winssl/test_winssl_mtls_e2e_local.lpi`, `tests/winssl/test_winssl_enterprise.lpi`                     |
| Cross-backend comparison     | `tests/integration/test_backend_comparison.lpi`                                                              |

## 这批收口后，仓库内还剩什么

这套 bundle truth alignment 做完之后，repo-side 还剩下的重点不再是“文档写错了哪个入口”，而是:

- 需要真实 Windows 主机执行
- 需要把 high-risk lanes 的 pass / fail 结果带回仓库
- 只有 fresh runtime RED 才值得重开 `src/fafafa.ssl.winssl.*`

也就是说，这批的目标不是宣称“WinSSL 已完成”，而是让剩余 blocker 变成纯外部环境和实跑证据。

## 不属于当前 bundle 的旧模板名称

这些名称不是当前仓库的有效入口，不要再当作 runtime validation bundle 的一部分:

- `Run-WindowsValidation.ps1`
- `Run-QuickValidation.ps1`
- `test_cert_load`
- `test_factory_mode`
- `ROLLBACK_PLAN.md`
- `WINDOWS_VALIDATION_REPORT.md`

## 配套文档

- [Windows 运行时验证清单](./WINDOWS_VALIDATION_CHECKLIST.md)
- [WinSSL 当前状态报告](../../docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md)
- [WinSSL 设计文档](../../docs/reference/WINSSL_DESIGN.md)
