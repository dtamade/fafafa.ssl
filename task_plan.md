# Task Plan - Win64 Cross-Target Fresh Revalidation

## Goal
补一轮当前 Linux 主机上的 fresh Win64 cross-target compile evidence，确认 `WinSSL` 相关选定用例和 `backend_comparison` 路径今天仍然可以交叉编译；这样 broad objective 的剩余 requirement 才能更硬地收敛到真实 Windows runtime proof，而不是继续依赖旧 compile 记录。

## Current Batch
1. 重新跑两条代表性的 Win64 cross-target compile：
   - `tests/winssl/test_winssl_session_management.pas`
   - `tests/integration/test_backend_comparison.pas`
2. 判断 fresh 结果属于哪一种：
   - compile surface 继续闭合
   - 当前主机缺 Win64 交叉编译能力
   - 仓库源码重新出现 compile drift
3. 把结果回写到 broad objective 审计结论里。

## Status
- [completed] Win64 cross-target fresh evidence
- [completed] Interpret compile result versus environment capability
- [completed] Feed result back into the broad blocker statement

## Current Evidence
- 上一批 `Backend Broad Completion Audit` 已经 fresh 证明：
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `tests/test_capability_cache.pas`：`FreePascal` / `WolfSSL` / `MbedTLS` wording truth 全绿
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：PASS
  - `WinSSL` source/bundle contract：全部 PASS
- 这一批只补最后一块 Linux 侧 compile proof：fresh 的 `-Twin64` 交叉编译结果。
- fresh Win64 cross-target compile result：
  - `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas`
    - 结果：通过，成功链接 `tmp/winssl_session_mgmt_win64/test_winssl_session_management.exe`
  - `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/backend_comparison_win64 -FEtmp/backend_comparison_win64 -otmp/backend_comparison_win64/test_backend_comparison.exe tests/integration/test_backend_comparison.pas`
    - 结果：通过，成功链接 `tmp/backend_comparison_win64/test_backend_comparison.exe`
- 解释：
  - 当前 Linux 主机不仅保留了 WinSSL source contract / bundle contract，还保留了 fresh Win64 cross-target compile capability
  - 因此 broad objective 的 Linux 侧剩余不确定性已经进一步清空
  - 唯一未闭合 requirement 继续收敛为真实 Windows 主机上的 `WinSSL` runtime proof

## Verification Plan
- `mkdir -p tmp/winssl_session_mgmt_win64`
- `fpc -Twin64 -B -Fu./src -Fu./tests -FUtmp/winssl_session_mgmt_win64 -FEtmp/winssl_session_mgmt_win64 -otmp/winssl_session_mgmt_win64/test_winssl_session_management.exe tests/winssl/test_winssl_session_management.pas`
- `mkdir -p tmp/backend_comparison_win64`
- `fpc -Twin64 -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/backend_comparison_win64 -FEtmp/backend_comparison_win64 -otmp/backend_comparison_win64/test_backend_comparison.exe tests/integration/test_backend_comparison.pas`

## Risks
- 交叉编译成功也不能替代真实 Windows runtime proof。
- `compile_all_modules.py` 会跳过 WinSSL，所以这批必须用显式 `-Twin64` 交叉编译补 WinSSL compile evidence。
- 如果当前主机缺 Win64 交叉编译能力，不能把环境缺口误写成仓库源码回归。

## Follow-up Queue
1. 当前 broad objective 的唯一剩余 requirement：真实 Windows 主机上的 `WinSSL` runtime proof。
2. 需要按 `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 在 Windows 主机上依次拿到 quick smoke、minimal gate、Wave B Windows gate、broader suite 的产物。
3. 在拿到 Windows 环境之前，当前 Linux 主机上已经没有新的高价值 repo-side 收口项。
