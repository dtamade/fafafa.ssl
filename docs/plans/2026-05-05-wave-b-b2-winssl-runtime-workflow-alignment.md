# Wave B/B2 WinSSL Runtime Workflow Alignment Plan

**Goal:** 把 `.github/workflows/wave-b-b2-manual.yml` 的 Windows job 从旧的 Wave B 级别入口，收口到当前 `WinSSL` runtime checklist 所要求的最小证据链：quick smoke、Wave B Windows gate、broader WinSSL suite transcript，以及对应 artifact 上传。这样在没有本地 Windows 主机时，仓库仍然有一条可触发的 Windows CI lane 去推进剩余 blocker。

**Architecture:** 这批不改任何 `src/` 生产代码，也不宣称已经拿到 Windows runtime proof。范围只在 workflow / docs / focused contract：
- 先用 shell contract 证明当前 workflow 还没覆盖 `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md` 的完整顺序。
- 然后最小修改 `.github/workflows/wave-b-b2-manual.yml`（及 `.disabled` 模板）：
  - Windows runner 安装并验证 `lazbuild`
  - 先跑 `tests/quick_winssl_validation.ps1`
  - 再跑 `scripts/run_wave_b_windows_gate.ps1`
  - 最后跑 `tests/run_winssl_tests.ps1` 并留下 transcript
  - 上传新增 runtime artifacts
- 最后把 `.github/README.md` 与 Windows checklist/bundle 文档补上这条 CI 入口。

**Files:**

- Add: `docs/plans/2026-05-05-wave-b-b2-winssl-runtime-workflow-alignment.md`
- Add: `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- Modify: `.github/workflows/wave-b-b2-manual.yml`
- Modify: `.github/workflows/wave-b-b2-manual.yml.disabled`
- Modify: `.github/README.md`
- Modify: `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- Modify: `tests/windows/VALIDATION_BUNDLE.md`
- Add: `tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
- Update: `task_plan.md`
- Update: `findings.md`
- Update: `progress.md`

## Task 1: RED - prove the current Windows workflow is below the runtime checklist

Run:

```bash
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
```

Expected RED on current head:

- workflow 还没有显式安装/验证 `lazbuild`
- workflow 只跑 `scripts/run_wave_b_windows_gate.ps1`
- workflow 还没有 quick smoke 步骤
- workflow 还没有 broader WinSSL suite transcript artifact

## Task 2: GREEN - align the workflow to the current runtime checklist

Changes:

- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
  - Windows job 安装 `freepascal` / `lazarus` / `openssl`
  - 补 `fpc -iV` / `lazbuild --version` / `openssl version`
  - 先跑 quick smoke 并保存日志
  - 继续跑现有 Wave B Windows gate
  - 运行 broader WinSSL suite 并保存 transcript
  - artifact 上传补齐 quick smoke log / broader suite transcript
- `.github/README.md`
  - 明确 `wave-b-b2-manual.yml` 的 Windows lane 现在可以承接 WinSSL runtime checklist
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
  - 增补“可在 GitHub Actions 的 `wave-b-b2-manual.yml` 上执行同一顺序”
- `tests/windows/VALIDATION_BUNDLE.md`
  - 把 workflow 入口也列入当前 bundle inventory

Constraints:

- 不改 `src/fafafa.ssl.winssl.*`
- 不把 workflow 存在本身写成 runtime proof complete
- 不把 quick smoke / Wave B gate 任一单项通过误写成全部完成

## Task 3: Verify and close out

Run:

```bash
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_wave_b_windows_gate_pwsh_and_verbose_contract.sh
bash tests/scripts/test_winssl_windows_runtime_project_target_contract.sh
git diff --check -- .github/workflows/wave-b-b2-manual.yml .github/workflows/wave-b-b2-manual.yml.disabled .github/README.md tests/windows/WINDOWS_VALIDATION_CHECKLIST.md tests/windows/VALIDATION_BUNDLE.md tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh docs/plans/2026-05-05-wave-b-b2-winssl-runtime-workflow-alignment.md task_plan.md findings.md progress.md
```

## Task 4: Runtime truth follow-up after the first real Windows runs

As of `2026-05-17`, this plan is no longer hypothetical:

- real manual run `25985103443` proved the workflow could reach the Windows lane and exposed a shell/encoding blocker
- commit `d32ab3a` fixed that by switching the workflow WinSSL script entrypoints to `pwsh`
- real manual run `25985356670` then moved the failure boundary into `tests/quick_winssl_validation.ps1`
- the new first hard blocker was not shell/runtime policy anymore, but the checked-in Lazarus project files forcing `TargetOS=linux`

Current follow-up scope:

- keep using GitHub manual dispatch as the source of truth for Windows runtime proof
- guard the quick smoke + broader suite project set with `tests/scripts/test_winssl_windows_runtime_project_target_contract.sh`
- remove the hardcoded Linux target from the runtime-entry `.lpi` files so Windows runners build with host target truth
- after push, re-dispatch `wave-b-b2-manual.yml` and record the next real Windows failure boundary

## Task 5: Fix the next startup-time Windows import truth exposed by run `25985680381`

Observed runtime update:

- quick smoke compile now succeeds on Windows
- the new failure is process startup exit `-1073741511` (`0xC0000139`) before any test-body output
- source audit points to a likely import-table mismatch: live code binds SSPI `AcceptSecurityContext` as `AcceptSecurityContextW`

Changes for this batch:

- add `tests/scripts/test_winssl_acceptsecuritycontext_import_contract.sh`
- change `src/fafafa.ssl.winssl.api.pas` to bind the unsuffixed `AcceptSecurityContext`
- change `src/fafafa.ssl.winssl.connection.pas` live callsites to use `AcceptSecurityContext`
- re-run `python3 scripts/compile_all_modules.py`
- push and dispatch another `wave-b-b2-manual.yml` run to verify whether quick smoke now gets past process startup

## Task 6: Fix the next Wave B Windows gate truth exposed by run `25985958467`

Observed runtime update:

- quick smoke now fully passes on Windows
- `Run Windows Wave B gate` becomes the next first hard blocker
- substep evidence shows two separate issues:
  - WinSSL minimal runner does not surface failing test output clearly enough
  - Windows workflow path precedence lets later steps resolve `fpc` to `i386-win32` / `ppc386`

Changes for this batch:

- add `tests/scripts/test_workflow_windows_fpc_preference_contract.sh`
- update `run_winssl_tests.ps1` so failing child-process stdout/stderr is captured into the Wave B log
- update the active/dormant Windows workflows to choose one preferred FPC path and log the resolved `fpc`
- push and dispatch another `wave-b-b2-manual.yml` run so the next Windows failure boundary is no longer blurred by toolchain/path ambiguity

### Definition Of Done

- 当前手动 Windows workflow 被锁定为覆盖 quick smoke + Wave B gate + broader suite transcript
- Windows checklist / bundle / GitHub workflow docs 不再互相漂移
- 仓库在没有本地 Windows 主机时，仍有一条明确的 CI lane 去推进 `WinSSL` runtime proof
