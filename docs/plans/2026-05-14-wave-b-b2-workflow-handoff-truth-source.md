# Wave B/B2 Workflow Handoff Truth Source

## Goal
把 `.github/workflows/wave-b-b2-manual.yml` 的 summary job 收口到 `scripts/prepare_wave_b_b2_handoff_bundle.sh`，让 workflow summary 与当前 handoff bundle 语义保持单一真相。

## Why This Batch
- workflow 仍在复制 `MACOS_*ARGS` / `WINDOWS_*ARGS` 与三个下游脚本调用
- repo 里最新的交接语义已经集中到 `prepare_wave_b_b2_handoff_bundle.sh`
- workflow 还没有生成/上传 `wave_b_b2_handoff_bundle_<run_id>.md`

## Files
- `.github/workflows/wave-b-b2-manual.yml`
- `tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- `tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 workflow RED contract，要求 summary job 调用 `prepare_wave_b_b2_handoff_bundle.sh`，并上传 handoff bundle。
2. 更新旧的 macOS/Windows workflow 合同，移除对 summary 内部重复逻辑的旧断言，改为验证平台 lane 契约仍经由统一入口落地。
3. 最小修改 `.github/workflows/wave-b-b2-manual.yml`：
   - 保留 artifact download/copy
   - 用单个 `PREPARE_ARGS` 调用 `prepare_wave_b_b2_handoff_bundle.sh`
   - `strict_closure=true` 时附加 `--strict`
   - 最终 upload 增加 `wave_b_b2_handoff_bundle_<run_id>.md`
4. 跑 focused workflow contracts 与 handoff 邻近回归。
5. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_macos_probe_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_windows_runtime_workflow_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_replay_command_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_windows_companion_path_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_explicit_missing_evidence_contract.sh`
- `bash tests/scripts/test_prepare_wave_b_b2_handoff_bundle_next_actions_contract.sh`
- `git diff --check`
