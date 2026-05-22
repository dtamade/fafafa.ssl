# VerifyMode Contract Guard And C-Library Runtime Proof Sync

## Goal

把两条已经被实现证明为“语义正确、守卫过旧”的线收回：

- `tests/scripts/test_factory_config_verifymode_empty_set_contract.sh` 还在盯旧的 `LConfig.VerifyMode` 文本
- `tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh` 还在盯旧的 `TSSLConnector.WithTimeout` 文本

当前源码 truth 已经明确：

- factory / direct-library path 统一通过 `LVerifyMode` 落地 verify-mode 语义
- C-library 连接作用域运行证明当前是通过 `ISSLConnectionControl.SetTimeout`、`transport/IO` 和 safe-default 结论来表达替换指导

## Architecture

这批保持 contract-only：

- 只刷新 shell 守卫文本，不改 runtime 语义
- 重新跑两个 focused contract
- 如果还有新的真实 RED，再单独拆更小的 batch

## Files

- `tests/scripts/test_factory_config_verifymode_empty_set_contract.sh`
- `tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 把 factory verify-mode 守卫改成当前 `LVerifyMode` truth。
2. 把 C-library runtime proof 守卫改成当前 `ISSLConnectionControl.SetTimeout` truth。
3. 重新跑两个 focused contract。
4. 更新台账和计划状态。

## Verification

```bash
bash -n tests/scripts/test_factory_config_verifymode_empty_set_contract.sh
bash tests/scripts/test_factory_config_verifymode_empty_set_contract.sh
bash -n tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh
bash tests/scripts/test_clibrary_direct_library_runtime_parity_contract.sh
git diff --check
```

## Expected Outcome

- 两个守卫回到绿色
- 现有 runtime 语义不变
- 后续再继续扫 interface/backend completeness 时，不会被这两条旧 guard 重复拉起
