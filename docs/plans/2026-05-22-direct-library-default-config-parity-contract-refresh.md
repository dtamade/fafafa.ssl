# Direct-Library Default-Config Parity Contract Refresh

## Goal

把 `tests/scripts/test_direct_library_default_config_parity_contract.sh` 从“盯死 `LConfig.VerifyMode` 变量名”修成“盯住 direct-library 实际 verify-mode truth”。

当前源码里四个 backend 的 direct-library `CreateContext(AType)` 都已经先计算 `LVerifyMode`，再把它应用到 context；合同应当验证这个语义，而不是把实现细节变量名当成真相。

## Architecture

这批先保持 contract-only：

- 先刷新 focused shell contract
- 重新跑 direct-library parity contract
- 如果还有新的真实 RED，再按最小范围补源码
- 如果没有 RED，就把这条线收口成“验证守卫更新”

## Files

- `tests/scripts/test_direct_library_default_config_parity_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 把 VerifyMode 守卫改成接受当前 `LVerifyMode` 实现。
2. 重新跑 direct-library parity contract。
3. 如果合同继续报红，再看是否真有 backend implementation gap。
4. 更新台账和计划状态。

## Verification

```bash
bash tests/scripts/test_direct_library_default_config_parity_contract.sh
git diff --check
```

## Expected Outcome

- 如果只是合同太死，直接回到绿色
- 如果真的还有 backend parity gap，会在重新验证时暴露出来
