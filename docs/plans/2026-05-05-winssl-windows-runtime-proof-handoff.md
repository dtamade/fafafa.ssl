# WinSSL Windows Runtime Proof Handoff Plan

**Goal:** 把 broad objective 的唯一剩余 requirement 落成一个可直接执行的 Windows 主机批次，不再让 `task_plan.md` 停在 Linux 侧 `-Twin64` 交叉编译结论；明确当前 Linux 主机已经完成全部可验证的 repo-side 收口，下一步只接受真实 Windows runtime 证据。

**Architecture:** 这批不改任何 `src/` 生产代码，也不重开新的 Linux-only 审计。只做三件事：
- 把当前 broad blocker 明确收敛到 `WinSSL` 的真实 Windows runtime proof。
- 以 `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 为执行入口，把 Windows 主机上的命令顺序、必留产物、验收标准和失败分流写成正式计划。
- 把 `task_plan.md` / `findings.md` / `progress.md` 切到这个真实下一步，避免后续“继续”又回到已经闭合的 Linux 侧 compile/source-contract 线。

**Files:**

- Add: `docs/plans/2026-05-05-winssl-windows-runtime-proof-handoff.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Reconfirm the actual blocker before handoff

Run on the current Linux host:

```bash
git status --short
git log --oneline -1
sed -n '1,220p' task_plan.md
sed -n '1,240p' findings.md
sed -n '1,260p' progress.md
sed -n '1,220p' docs/plans/2026-05-05-backend-broad-completion-audit.md
sed -n '1,220p' docs/plans/2026-05-05-win64-cross-target-fresh-revalidation.md
sed -n '1,240p' tests/windows/WINDOWS_VALIDATION_CHECKLIST.md
sed -n '1,240p' docs/reference/WINSSL_DESIGN.md
```

Expected interpretation:

- 工作树应保持干净，说明上一批已收口完成。
- broad completion audit 和 fresh Win64 cross-target revalidation 都应继续指向同一个结论：
  - Linux 侧 public surface / capability truth / repo gates / source contracts / Win64 compile proof 已闭合
  - 唯一剩余 requirement 是 `WinSSL` 的真实 Windows runtime proof
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 必须已经能直接充当 Windows 主机执行入口。

## Task 2: Define the Windows-host execution order

Run on a real Windows host from repo root:

```powershell
fpc -iV
lazbuild --version
$PSVersionTable.PSVersion
powershell -ExecutionPolicy Bypass -File .\tests\quick_winssl_validation.ps1
powershell -ExecutionPolicy Bypass -File .\run_winssl_tests.ps1 -RunId winssl_min_20260505 -OutputDir test-reports
powershell -ExecutionPolicy Bypass -File .\scripts\run_wave_b_windows_gate.ps1 -RunId wave_b_windows_20260505 -OutputDir test-reports
Start-Transcript -Path .\test-reports\winssl_runtime_suite_20260505.log
powershell -ExecutionPolicy Bypass -File .\tests\run_winssl_tests.ps1
Stop-Transcript
```

Execution order rules:

1. quick smoke 没过就停止，不继续跑 gate。
2. WinSSL minimal gate 没过，就把问题先局限在 WinSSL 自身，不混入 OpenSSL / modules。
3. Wave B Windows gate 必须留 summary 和 step logs，不能只给一句“跑过了”。
4. broader suite 用来补握手、证书存储、session resumption、online/error mapping、mTLS 这些高风险区域的运行时证据。

## Task 3: Define acceptance artifacts and failure routing

Required artifacts:

- `test-reports/wave_b_windows_gate_summary_<run_id>.md`
- `test-reports/wave_b_windows_winssl_<run_id>.log`
- `test-reports/wave_b_windows_openssl_<run_id>.log`
- `test-reports/wave_b_windows_modules_<run_id>.log`
- `test-reports/validate_all_modules_report_<run_id>.md`
- `test-reports/winssl_runtime_suite_<run_id>.log` or equivalent transcript

Decision rules:

- 只有当 quick smoke、minimal gate、Wave B gate、broader suite 产物都齐全时，才允许把 `WinSSL` 写成“runtime proof complete”。
- 如果失败来自 Windows 环境本身，例如 `lazbuild` / PowerShell / 出网限制 / 证书权限问题，要明确记成环境 blocker，不能直接记成实现缺陷。
- 只有当 Windows 主机上的 fresh failure 指向真实 WinSSL 运行时行为缺口时，才允许重新打开 `src/fafafa.ssl.winssl.*` 生产修复批次。

### Definition Of Done

- 当前 broad blocker 被正式切换成 Windows 主机运行时验证批次
- `task_plan.md` / `findings.md` / `progress.md` 顶部不再停留在旧的 Linux 侧 compile 批次
- 后续执行者可以直接按本计划或 `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 上机实跑

## Execution Result

- 当前 Linux 主机复核后，工作树干净，最新提交仍为 `c10bf22 docs: record fresh win64 cross-target proof`
- broad completion audit 结论未变化：
  - Linux 侧 public contract / capability truth / compile gate / minimal gate / WinSSL source+bundle contracts 已闭合
  - fresh Win64 cross-target compile proof 也已补齐
  - 唯一未闭合 requirement 仍然是 `WinSSL` 的真实 Windows runtime proof
- 因此本批只做 handoff/ledger 收口，不再在 Linux 主机上虚构新的 repo-side “继续”项
