# Interface Audit Current Truth And Backend Capability Quick Reference Fix

## Goal

修复两个当前 contract 红点：

- `docs/ARCHITECTURE.md` 需要更明确地写出当前 public surface 只声明了 `ISSLClientConnection`
- `tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh` 需要在不依赖 markdown 列宽的前提下验证 WinSSL PSK 仍然是 unsupported

## Architecture

这批保持 docs-first 和 contract-only：

- 一处架构文档措辞收紧
- 一处 shell contract 稳定性修复
- 不改生产代码

## Files

- `docs/ARCHITECTURE.md`
- `tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Steps

1. 调整 `docs/ARCHITECTURE.md` 的 current-surface 句子，让它显式使用 `surface` 说法。
2. 把 WinSSL PSK 守卫改成宽容 markdown spacing 的正则匹配，但继续要求 `PSK` + `❌ 不支持` + `Schannel 限制`。
3. 重新跑两个 focused contract 和 `git diff --check`。
4. 把这次发现写回工作台账。

## Verification

```bash
bash tests/scripts/test_interface_audit_current_truth_contract.sh
bash tests/scripts/test_backend_capability_matrix_quick_reference_truth_contract.sh
git diff --check
```

## Expected Outcome

- `docs/ARCHITECTURE.md` 明确写出当前只声明了 `ISSLClientConnection`
- WinSSL PSK 继续被分类为 unsupported
- contract 不再因为表格列宽差异误报
