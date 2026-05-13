# Wave B/B2 Strict Input Description Truth

## Goal
让 workflow 输入 `strict_closure` 的描述文字与当前真实 strict 行为一致，避免把完整 handoff strict gate 继续误写成仅检查 closure 的开关。

## Why This Batch
- `strict_closure=true` 现在已经映射到 `prepare_wave_b_b2_handoff_bundle.sh --strict`
- 这条 strict 路径不仅会因为 closure 未闭环失败，还会因为 consistency/evidence 缺失失败
- 旧描述会误导操作者对失败边界的预期

## Files
- `.github/workflows/wave-b-b2-manual.yml`
- `.github/workflows/wave-b-b2-manual.yml.disabled`
- `tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps
1. 写 focused workflow contract，要求 `strict_closure` 描述不再是 closure-only 语义。
2. 让合同先在当前模板上 RED。
3. 最小同步 live 与 `.disabled` 模板的输入描述。
4. 跑 focused workflow contracts 与 `git diff --check`。
5. 更新 working-memory，给出简短 review 结论并提交。

## Expected Verification
- `bash -n tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
- `bash tests/scripts/test_wave_b_b2_strict_input_description_contract.sh`
- `bash tests/scripts/test_wave_b_b2_handoff_bundle_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_linux_baseline_required_workflow_contract.sh`
- `bash tests/scripts/test_wave_b_b2_optional_runner_artifact_download_workflow_contract.sh`
- `git diff --check`
