# WinSSL Windows Validation Bundle Truth Alignment Plan

**Goal:** 把 WinSSL 的 Windows runtime validation bundle 收口到当前仓库真相：`tests/windows` 文档只引用真实存在的入口，手动 PowerShell 验证脚本不再依赖启动 cwd，并把剩余 blocker 明确压缩到“缺 Windows 主机实跑证据”。

**Architecture:** 这批不改任何 `src/fafafa.ssl.winssl.*` 生产实现，也不重开 capability 设计。先用 shell contract 证明当前 `tests/windows` 文档仍指向旧模板入口、`tests/*.ps1` 手动脚本仍受 cwd 影响；再只做最小修复：让文档回到真实入口链路，让手动脚本自解析到 `tests/winssl`，并把 WinSSL 状态报告补上实际执行口径。

**Files:**

- Add: `tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
- Modify: `tests/quick_winssl_validation.ps1`
- Modify: `tests/run_winssl_tests.ps1`
- Modify: `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- Modify: `tests/windows/VALIDATION_BUNDLE.md`
- Modify: `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove validation bundle drift

Run:

```bash
bash -n tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
```

Expected findings:

- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` / `tests/windows/VALIDATION_BUNDLE.md` 仍引用 `Run-WindowsValidation.ps1`、`Run-QuickValidation.ps1`、`test_cert_load`、`test_factory_mode` 等旧模板名称
- `tests/quick_winssl_validation.ps1` / `tests/run_winssl_tests.ps1` 仍依赖调用者先切到正确目录，不能作为稳定的 Windows 运行时验证入口
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md` 还缺少指向当前 validation bundle 的明确执行口径

## Task 2: GREEN - align docs and script entrypoints

Change:

- `tests/quick_winssl_validation.ps1`
  - 自解析脚本目录
  - 自动切到 `tests/winssl`
  - 保持 quick smoke 目标不变
- `tests/run_winssl_tests.ps1`
  - 自解析脚本目录
  - 自动切到 `tests/winssl`
  - 保持 wider manual suite 目标不变
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - 改成当前真实入口顺序：quick smoke -> WinSSL minimal gate -> Wave B Windows gate -> broader manual suite
  - 明确 Windows runtime proof 的边界和验收物
- `tests/windows/VALIDATION_BUNDLE.md`
  - 列出当前真实 bundle 文件、职责、产物路径和 targeted follow-up mapping
- `docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md`
  - 补上指向 checklist / bundle 的执行口径链接

Constraints:

- 不改 `src/fafafa.ssl.winssl.*`
- 不把 Linux compile / contract 证据写成 Windows runtime 已完成
- 不扩大成新的 WinSSL 行为修复批次；只有 Windows host 实跑暴露 fresh RED 时才允许重开实现线

## Task 3: Verification

Run:

```bash
bash -n tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh
git diff --check -- docs/plans/2026-05-05-winssl-windows-validation-bundle-truth-alignment.md tests/scripts/test_winssl_windows_validation_bundle_contract.sh tests/quick_winssl_validation.ps1 tests/run_winssl_tests.ps1 tests/windows/WINDOWS_VALIDATION_CHECKLIST.md tests/windows/VALIDATION_BUNDLE.md docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md task_plan.md findings.md progress.md
```

Formatting:

```bash
/home/dtamade/node_modules/.bin/prettier --write /home/dtamade/projects/fafafa.ssl/docs/plans/2026-05-05-winssl-windows-validation-bundle-truth-alignment.md /home/dtamade/projects/fafafa.ssl/tests/windows/WINDOWS_VALIDATION_CHECKLIST.md /home/dtamade/projects/fafafa.ssl/tests/windows/VALIDATION_BUNDLE.md /home/dtamade/projects/fafafa.ssl/docs/test_reports/WINSSL_BACKEND_STATUS_REPORT.md /home/dtamade/projects/fafafa.ssl/task_plan.md /home/dtamade/projects/fafafa.ssl/findings.md /home/dtamade/projects/fafafa.ssl/progress.md
```

## Definition Of Done

- `tests/windows` 文档只指向当前真实入口
- 手动 Windows PowerShell 验证脚本不再要求调用者先切 cwd
- WinSSL 状态报告给出当前 runtime validation 执行口径
- 新增 contract test 通过，现有 Wave B Windows gate contract 继续通过
- 当前 broad objective 的剩余 blocker 被进一步收紧到“等待 Windows 主机实跑”
