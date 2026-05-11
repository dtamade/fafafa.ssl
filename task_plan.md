# Task Plan - Working-Memory, Artifact Hygiene, And WinSSL Workflow Closeout

## Goal
把当前工作树从“历史批次 + 本地产物残留”收口回 `HEAD` `e80100a` 的真实状态，清掉 3 个测试二进制残留，并把 `wave-b-b2-manual.yml` 的 Windows lane 对齐到 WinSSL runtime checklist。

## Current Batch
1. 清理 `tests/contract/` 与 `tests/wolfssl/` 下的无扩展名 ELF 测试产物。
2. 新增本批 plan 文档，作为可恢复的工作记忆。
3. 把 `task_plan.md` / `findings.md` / `progress.md` 顶部对齐到当前真相。
4. 修复 `test_wave_b_b2_windows_runtime_workflow_contract.sh` 打出的 workflow RED。
5. 复跑 focused contracts、diff hygiene，并提交。

## Status
- [completed] Freeze current state and remove generated test binaries
- [completed] Resync working-memory files to current HEAD and next queue
- [completed] Align Wave B/B2 Windows workflow to the runtime checklist
- [completed] Verify diff hygiene and record results
- [pending] Commit the batch

## Current Evidence
- `git log --oneline -1` shows current `HEAD` as `e80100a fix: batch 6 - compiler warning reduction and capabilities contract test`
- `git status --short` initially listed only three untracked ELF test binaries:
  - `tests/contract/test_capabilities_contract`
  - `tests/wolfssl/test_wolfssl_connection_contract`
  - `tests/wolfssl/test_wolfssl_context_contract`
- `file` confirmed those artifacts are Linux ELF executables, not source files
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh` produced a real RED: the workflow did not install or verify Lazarus / `lazbuild`
- current product-side blocker remains real Windows runtime evidence, but the GitHub Actions lane can now be made capable of collecting it

## Risks
- Do not reopen `src/fafafa.ssl.winssl.*` in this batch.
- Do not treat workflow readiness as a substitute for Windows runtime proof.
- Keep the batch narrow so the commit stays reviewable.

## Follow-up Queue
1. Commit the closeout batch.
2. Trigger the updated `wave-b-b2-manual.yml` on GitHub Actions when a real Windows runtime proof run is needed.

# Task Plan - Wave B/B2 WinSSL Runtime Workflow Alignment

## Goal
把当前手动 Windows CI workflow 对齐到最新 `WinSSL` runtime checklist，让仓库在没有本地 Windows 主机时，仍能通过 `wave-b-b2-manual.yml` 去推进 quick smoke、Wave B gate、broader suite 这条真实运行时证据链。

## Current Batch
1. 用 focused contract 证明当前 `wave-b-b2-manual.yml` 的 Windows job 仍低于最新 runtime checklist。
2. 最小修改 workflow / docs，把 quick smoke、Wave B gate、broader suite transcript 接进现有 Windows lane。
3. 复跑 focused contracts，并更新计划/台账。

## Status
- [pending] RED workflow contract for Windows runtime checklist alignment
- [pending] Align wave-b-b2-manual Windows lane
- [pending] Re-run workflow + bundle contracts
- [pending] Record evidence and commit the batch

## Current Evidence
- fresh broad completion audit 已证明：
  - `tests/contract/test_backend_contract.pas`：`135 total / 111 passed / 0 failed / 24 skipped`
  - `tests/test_capability_cache.pas`：`FreePascal` / `WolfSSL` / `MbedTLS` wording truth 全绿
  - `python3 scripts/compile_all_modules.py`：`185/185`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id broad_completion_audit_20260505`：`17 passed / 0 failed`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`：PASS
  - `WinSSL` source contract / bundle contract：全部 PASS
- fresh Win64 cross-target compile 已补齐：
  - `tests/winssl/test_winssl_session_management.pas` 可成功交叉编译到 Win64
  - `tests/integration/test_backend_comparison.pas` 可成功交叉编译到 Win64
- 当前 Linux 主机环境边界已确认：
  - `command -v pwsh`：空 / exit `1`
  - `wine --version`：exit `159`
- 结论：
  - Linux 侧 public surface、capability truth、repo gates、source contract、Win64 compile proof 都已闭合
  - 唯一未闭合 requirement 是真实 Windows 主机上的 `WinSSL` runtime proof
- 当前仓库已有的 Windows CI 入口是 `.github/workflows/wave-b-b2-manual.yml`，但从源码可见：
  - 只跑 `scripts/run_wave_b_windows_gate.ps1`
  - 还没显式安装/验证 `lazbuild`
  - 还没把 quick smoke 和 broader suite transcript 纳入 artifact
- 因此“有 Windows workflow”还不等于“这条 workflow 已覆盖当前 runtime checklist”。

## Risks
- 这批只能把 CI lane 对齐到 checklist，不能替代真实 Windows runtime 结果本身。
- 如果 workflow 只补命令不补 artifact，后续仍然没法做可审查闭环。
- 如果只改 `.yml` 不同步 `.disabled` 模板和文档，后续容易再次漂移。

## Follow-up Queue
1. 触发对齐后的 `wave-b-b2-manual.yml` Windows lane。
2. 审查 quick smoke / Wave B / broader suite artifacts。
3. 只有当这些 Windows runtime 结果真实返回后，才继续判断 broad objective 是否可标记完成。
