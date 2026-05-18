# 2026-05-18 WinSSL Runtime Evidence Strengthening

## Goal

修复 GitHub Actions Windows broader WinSSL suite 的 evidence gap，确保：

1. broader suite artifact 本身就携带可审查的 runtime 输出
2. consistency / handoff 脚本不再把“文件存在但内容空壳”的日志判成合格
3. Windows checklist / bundle docs 与新的 runtime evidence truth 对齐

## Architecture / Scope

- 不改 WinSSL backend 实现语义
- 只修 evidence capture / evidence verification / docs truth
- active workflow 与 dormant templates 保持同一 truth

## Files

- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `.github/workflows/winssl-tests.yml.disabled`
- `tests/run_winssl_tests.ps1`
- `scripts/check_wave_b_b2_evidence_consistency.sh`
- `scripts/prepare_wave_b_b2_handoff_bundle.sh`
- `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- `tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh`
- `tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh`
- `tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `tests/scripts/test_workflow_winssl_tests_truth_contract.sh`
- `tests/scripts/test_winssl_runtime_suite_markers_contract.sh`
- `tests/scripts/test_winssl_windows_validation_bundle_contract.sh`
- `tests/windows/WINDOWS_VALIDATION_CHECKLIST.md`
- `tests/windows/VALIDATION_BUNDLE.md`

## Steps

1. 先用 GitHub job log 与 downloaded artifacts 交叉确认 root cause。
2. 让 workflow 改成 UTF-8 console capture，而不是 transcript-only shell capture。
3. 在 `tests/run_winssl_tests.ps1` 增加稳定的 `[WINSSL-RUNTIME]` markers。
4. 收紧 consistency / handoff 规则，要求 runtime log 具备 substantive markers。
5. 更新 focused contracts 与 Windows docs。
6. 本地跑 focused shell contracts。
7. push 后重跑 `wave-b-b2-manual.yml` 做 live verification。

## Commands

```bash
gh run view 26030261335 --json status,conclusion,workflowName,createdAt,updatedAt,jobs,url
gh run download 26030261335 -D tmp/gh-run-26030261335
gh run view 26030261335 --job 76514096222 --log

bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_runtime_substantive_contract.sh
bash tests/scripts/test_wave_b_b2_consistency_windows_companion_path_contract.sh
bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh
bash tests/scripts/test_workflow_winssl_tests_truth_contract.sh
bash tests/scripts/test_winssl_windows_validation_bundle_contract.sh
bash tests/scripts/test_winssl_runtime_suite_markers_contract.sh
bash tests/scripts/test_wave_b_b2_evidence_consistency_windows_runtime_artifacts_contract.sh
git diff --check
```

## Expected Outputs

- workflow contract 通过，锁住 UTF-8 runtime log capture
- consistency contract 会拒绝 marker-less Windows runtime log
- `tests/run_winssl_tests.ps1` 源码内存在 `[WINSSL-RUNTIME]` markers
- Windows checklist / bundle 明确要求 runtime log 包含 markers
- push 后新的 Actions artifact 应能直接包含 substantive runtime evidence
