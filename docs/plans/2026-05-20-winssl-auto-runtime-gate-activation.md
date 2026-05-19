# WinSSL Auto Runtime Gate Activation

## Goal

把当前主要靠 `workflow_dispatch`
才能运行的 WinSSL Windows runtime 证据链，
提升成一条会在 WinSSL 相关改动上自动触发的 GitHub Actions workflow。

避免后续继续把
“Linux CI 绿色”
误读成
“WinSSL 已自动验证”。

## Scope

- 新增活跃 workflow：
  - `.github/workflows/winssl-tests.yml`
- 同步 dormant template：
  - `.github/workflows/winssl-tests.yml.disabled`
- 更新 focused workflow contract：
  - `tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
- 更新 workflow 说明：
  - `.github/README.md`
- 更新 `task_plan.md` / `findings.md` / `progress.md`

不做：

- 不改 WinSSL 生产代码
- 不重开 `wave-b-b2-manual.yml` 的结构整改
- 不让所有 push 都无条件跑 Windows

## Architecture Truth

- 当前活跃 push CI：
  - `.github/workflows/ci.yml`
  - 只有 Linux / FreePascal 主线 gates
- 当前真正的 Windows runtime 证据链
  已存在于：
  - `tests/quick_winssl_validation.ps1`
  - `scripts/run_wave_b_windows_gate.ps1`
  - `tests/run_winssl_tests.ps1`
  - `.github/workflows/wave-b-b2-manual.yml`
- 缺口不是“没有 Windows 证据链”，
  而是“没有自动 Windows 证据链”

## Steps

1. 新增自动 WinSSL workflow，按 WinSSL 相关路径触发
2. 复用 manual lane 已经验证过的 Windows 证据链：
   - quick smoke
   - Windows Wave B gate
   - broader WinSSL suite
3. 同步 dormant template 与 workflow contract
4. 更新 `.github/README.md`
5. 跑 focused workflow/static verification
6. push 后观察新的 GitHub run

## Verification

```bash
bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh
bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
bash tests/scripts/test_workflow_checkout_node24_contract.sh
bash tests/scripts/test_workflow_upload_artifact_node24_contract.sh
git diff --check
```

## Expected Outcome

- WinSSL 相关改动会自动拉起 Windows runner
- 自动 lane 复用真实 runtime evidence chain，
  不再只是文件存在性或弱摘要
- `.github/README.md`
  不再让人误以为
  `ci.yml`
  已覆盖 WinSSL runtime

