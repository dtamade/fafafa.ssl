# WinSSL Windows 运行时验证清单

这份清单只服务于一件事: 在真实 Windows 主机上补齐 WinSSL 的 runtime proof。

它不替代 Linux 上已经拿到的 source contract、Win64 cross-target compile，或 `python3 scripts/compile_all_modules.py` / `bash scripts/run_minimal_ci_gate.sh --fast-local` 这些仓库级证据。当前还缺的，是 Windows 主机上的真实握手、证书存储、session resumption，以及 server/client runtime 行为。

## 先明确当前边界

在开始 Windows 实跑前，先接受这几个前提:

- Linux 侧已经证明:
  - source contract 持续收口
  - 选定 WinSSL / backend comparison 路径可以继续做 Win64 交叉编译
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
- 当前还没证明:
  - Windows 主机上的真实握手路径
  - Windows 系统证书存储和企业策略交互
  - session resumption / tickets 的真实行为
  - server/client runtime 的 OCSP、证书验证、错误映射细节

如果你只是想确认“仓库当前还有没有 repo-side 阻塞”，这份清单的意义就是把阻塞压缩到 Windows 主机实跑，而不是继续在 Linux 上猜。

GitHub Actions 上可以用 `.github/workflows/wave-b-b2-manual.yml` 走同一条顺序：先 quick smoke，再 Wave B Windows gate，最后 broader WinSSL suite transcript。

## 先准备 Windows 主机

从仓库根目录执行下面这些步骤。命令默认使用 `powershell`; 如果机器已经装了 `pwsh`，可以直接替换。

确认环境:

- Windows 10/11 x64，或支持所需 TLS 能力的 Windows Server
- `fpc` / Lazarus / `lazbuild` 可用
- PowerShell 5.1+ 可用
- 仓库完整 checkout 到本机
- 如果要跑 online / HTTPS / revocation 相关测试，机器允许出网

先看工具是否齐:

```powershell
fpc -iV
lazbuild --version
$PSVersionTable.PSVersion
```

## 按这个顺序跑

### 1. 先跑 quick smoke

Run:

```powershell
powershell -ExecutionPolicy Bypass -File .\tests\quick_winssl_validation.ps1
```

这一步的目标很小:

- 创建或复用测试证书
- 编译 `tests/winssl/test_winssl_certificate_loading.lpi`
- 运行证书加载 smoke

如果这一步失败，先别继续。先修:

- `lazbuild` / FPC 不可用
- 证书创建权限问题
- WinSSL 证书加载基础路径问题

### 2. 再跑 WinSSL minimal gate

Run:

```powershell
powershell -ExecutionPolicy Bypass -File .\run_winssl_tests.ps1 -RunId winssl_min_20260505 -OutputDir test-reports
```

这一步是最小 WinSSL-only runner。它会编译并运行:

- `tests\winssl\test_winssl_api_basic.pas`
- `tests\unit\test_winssl_comprehensive.pas`

期望结果:

- 控制台最后出现 `[WAVE-B-WINSSL] PASS`

这一步适合先定位“是不是 WinSSL 自己先挂了”，不用把 OpenSSL 模块校验混进来。

### 3. 再跑完整 Wave B Windows gate

Run:

```powershell
powershell -ExecutionPolicy Bypass -File .\scripts\run_wave_b_windows_gate.ps1 -RunId wave_b_windows_20260505 -OutputDir test-reports
```

这一步会统一串起:

- `run_winssl_tests.ps1`
- `run_openssl_tests.ps1`
- `scripts/validate_all_modules.ps1`

必须保留的产物:

- `test-reports/wave_b_windows_gate_summary_<run_id>.md`
- `test-reports/wave_b_windows_winssl_<run_id>.log`
- `test-reports/wave_b_windows_openssl_<run_id>.log`
- `test-reports/wave_b_windows_modules_<run_id>.log`
- `test-reports/validate_all_modules_report_<run_id>.md`

你需要的不是一句“跑过了”，而是这几份产物能明确告诉你哪一步过、哪一步挂。

### 4. 最后跑 broader WinSSL suite

Run:

```powershell
powershell -ExecutionPolicy Bypass -File .\tests\run_winssl_tests.ps1
```

这一步是更宽的手动 Windows suite。脚本会自动切到 `tests/winssl`，不要求你先 `cd tests\winssl`。

当前 wider suite 会覆盖这些项目:

- `test_winssl_unit_comprehensive.lpi`
- `test_winssl_integration_multi.lpi`
- `test_backend_comparison.lpi`
- `test_winssl_performance.lpi`
- `test_winssl_handshake_debug.lpi`
- `test_winssl_https_client.lpi`

如果需要把 broader suite 控制台输出落成可审查 artifact，建议在 PowerShell 里用 UTF-8 console capture，而不是只依赖 transcript 壳:

```powershell
$runtimeOutput = @()
pwsh -NoProfile -ExecutionPolicy Bypass -File .\tests\run_winssl_tests.ps1 *>&1 | Tee-Object -Variable runtimeOutput
@($runtimeOutput) | Out-File -FilePath .\test-reports\winssl_runtime_suite_20260505.log -Encoding utf8
```

日志里至少应能看到这些稳定 marker，后续 CI / handoff 才能把它当成 substantive runtime evidence:

```text
[WINSSL-RUNTIME] suite_start total=...
[WINSSL-RUNTIME] suite_summary passed=... failed=... total=... success_rate=...
[WINSSL-RUNTIME] suite_end status=PASS|FAIL
```

## 高风险区域要单独盯

这些区域是 broad objective 还不能直接标记完成的原因。即使 gate 绿了，也要单独记结论:

| 区域                         | 优先观察的用例                                                                                               |
| ---------------------------- | ------------------------------------------------------------------------------------------------------------ |
| 握手 / HTTPS client          | `tests/winssl/test_winssl_handshake_debug.lpi`, `tests/winssl/test_winssl_https_client.lpi`                  |
| 系统证书存储                 | `tests/winssl/test_winssl_certstore.lpi`, `tests/winssl/test_winssl_certificate_loading.lpi`                 |
| Session resumption / tickets | `tests/winssl/test_winssl_session_resumption.lpi`, `tests/winssl/test_winssl_session_management.lpi`         |
| 错误映射 / online flow       | `tests/winssl/test_winssl_error_mapping_online.lpi`, `tests/winssl/test_winssl_hostname_mismatch_online.lpi` |
| mTLS / enterprise behavior   | `tests/winssl/test_winssl_mtls_e2e_local.lpi`, `tests/winssl/test_winssl_enterprise.lpi`                     |

## 什么时候算“这台 Windows 主机拿到有效证据”

至少满足这些条件:

- quick smoke 退出码为 `0`
- `scripts/run_wave_b_windows_gate.ps1` 产出 summary 和三份 step log
- wider suite 的通过/失败项被明确记录，且日志里带有 `[WINSSL-RUNTIME]` markers
- 对高风险区域有逐项说明，而不是只写一句“整体通过”

不要把下面这些情况误写成“WinSSL 已完整 runtime proof”:

- 只有 Linux 交叉编译是绿的
- 只有 `python3 scripts/compile_all_modules.py` 是绿的
- 只跑了 quick smoke，没有 wider suite 证据
- 有失败但没有明确说明是平台限制、环境问题，还是实现缺口

## 常见阻塞

### `lazbuild` 不存在

先修 Lazarus / FPC 安装，再继续。不要把这个记成 WinSSL 实现失败。

### 脚本提示缺文件或找不到 `.lpi`

当前 `tests/quick_winssl_validation.ps1` 和 `tests/run_winssl_tests.ps1` 已经会自己切到 `tests/winssl`。如果这里再次出现路径问题，优先按“仓库入口漂移”排查，而不是先怀疑 WinSSL 运行时。

### 只有某个 online 用例失败

先记清楚:

- 主机网络是否受限
- 失败发生在 DNS、证书链、握手，还是业务响应
- 是否只影响 online lane，而离线 lane 仍然通过

### `pwsh` 不存在

这不阻塞 checklist 本身。外层 gate 会优先选 `pwsh`，没有就回退到 `powershell`。

## 相关文档

- [WinSSL 当前状态报告](../../docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md)
- [WinSSL bundle inventory](./VALIDATION_BUNDLE.md)
- [WinSSL 设计文档](../../docs/reference/WINSSL_DESIGN.md)
