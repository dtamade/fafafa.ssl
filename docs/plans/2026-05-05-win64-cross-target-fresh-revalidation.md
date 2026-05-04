# Win64 Cross-Target Fresh Revalidation Plan

**Goal:** 在当前 Linux 主机上补一轮 fresh 的 Win64 cross-target compile evidence，确认 `WinSSL` 相关选定用例和 `backend_comparison` 路径今天仍然可以交叉编译，而不是继续依赖旧批次记录或文档表述。

**Architecture:** 这批只做 compile-surface revalidation，不做任何生产代码改动，也不把交叉编译结果误写成 Windows runtime proof。沿用历史 closeout 已收口过的两条代表性路径：
- `tests/winssl/test_winssl_session_management.pas`
- `tests/integration/test_backend_comparison.pas`

如果二者都能 fresh 交叉编译成功，就把 Linux 侧 Win64 compile proof 写回台账；如果任一命中新的 compile drift，则按最小共享修复继续收口。

**Files:**

- Add: `docs/plans/2026-05-05-win64-cross-target-fresh-revalidation.md`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: Re-run selected Win64 cross-target compiles

Run:

```bash
mkdir -p tmp/winssl_session_mgmt_win64
fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas
mkdir -p tmp/backend_comparison_win64
fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/backend_comparison_win64 -FEtmp/backend_comparison_win64 -otmp/backend_comparison_win64/test_backend_comparison.exe tests/integration/test_backend_comparison.pas
```

Interpretation:

- 如果两条路径都成功，说明当前 Linux 主机上的 WinSSL source/compile surface 继续闭合。
- 如果命中 `Windows` 单元、Win64 RTL、或共享单元 drift，则需要先区分“环境缺交叉编译能力”和“仓库源码回归”。

## Task 2: Feed the result back into the broad objective audit

Decision rules:

- 交叉编译成功只能补强 Linux 侧 compile proof，不能替代真实 Windows runtime proof。
- 如果交叉编译失败但失败原因是环境缺少 Win64 交叉编译能力，那么 broad objective 的 blocker 应改写为“缺 Windows runtime proof + 缺 fresh cross-target compile capability”。
- 如果交叉编译失败且是仓库源码回归，那么 broad objective 还存在 repo-side drift，需要继续修。

### Definition Of Done

- 选定 WinSSL / backend comparison 路径获得 fresh Win64 compile evidence，或明确证明当前主机缺这项能力
- broad objective 的剩余 requirement 被进一步收紧，而不是继续依赖旧记录

## Execution Result

- fresh Win64 cross-target compile 结果：
  - `tests/winssl/test_winssl_session_management.pas`
    - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas`
    - 结果：通过，成功链接 `tmp/winssl_session_mgmt_win64/test_winssl_session_management.exe`
  - `tests/integration/test_backend_comparison.pas`
    - `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/backend_comparison_win64 -FEtmp/backend_comparison_win64 -otmp/backend_comparison_win64/test_backend_comparison.exe tests/integration/test_backend_comparison.pas`
    - 结果：通过，成功链接 `tmp/backend_comparison_win64/test_backend_comparison.exe`
- 解释：
  - 当前 Linux 主机没有暴露新的 Win64 cross-target compile drift
  - broad objective 的 Linux 侧 compile proof 继续闭合
  - 这批不会把交叉编译结果误写成 Windows runtime proof；唯一剩余 requirement 仍然是 Windows 主机上的真实 WinSSL runtime evidence
