# Wave B/B2 Optional Runner Artifact Download Tolerance

## Goal
让 `summary` job 对缺失的 macOS/Windows artifact 保持可继续执行，把缺证据语义交还给 `prepare_wave_b_b2_handoff_bundle.sh`，同时保持 Linux artifact download 继续严格。

## Why This Batch
- handoff 链已经有完善的缺证据表示
- 但 summary 还可能在 `download-artifact` 上更早失败
- 这会绕开 repo 里刚刚收好的 `prepare -> cross/closure/consistency/handoff bundle` 真相链

## Files
- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused workflow contract，要求：
   - Linux evidence download 继续严格
   - macOS/Windows evidence download 对缺失容错
2. 让合同先在当前模板上 RED。
3. 最小同步 live 与 `.disabled` 模板：
   - `Download macOS evidence` 加 `continue-on-error: true`
   - `Download Windows evidence` 加 `continue-on-error: true`
4. 跑 focused workflow contracts 与 `git diff --check`。
5. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- `git diff --check`
