# ISSLConnection Contract Truth Refresh

## Goal

收掉一组新的 false-red：

- `ISSLConnectionInfo` migration contract 仍漏掉了已经落地的 `ISSLConnectionTextIO`
- `ISSLSessionResumption` compiler-deprecation contract 仍盯着旧的 residual wording

本批不改生产实现，也不改 public API，只把 focused contracts 与当前 source/doc truth 对齐。

## Scope

- Update:
  - `tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh`
  - `tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`

## Current Evidence

- `src/fafafa.ssl.connection.base.pas` 当前真实实现：
  - `TBaseSSLConnection` 仍显式实现 `ISSLConnectionTextIO`
  - 这是 `ReadString` / `WriteString` owner-path 批次已经收掉后的真相
- `docs/reference/INTERFACE_DESIGN_V2.md` 当前也同步写成：
  - `TBaseSSLConnection = class(... ISSLConnectionTextIO, ...)`
- `src/fafafa.ssl.connection.base.pas` 的 session-resumption residual note 当前真实措辞已经收紧为：
  - `backend-specific semantic truth proofs`
- `tests/scripts/test_isslsessionresumption_runtime_residual_classification_contract.sh`
  也已经冻结了这套新措辞

## Minimal Fix

1. 把 `test_isslconnectioninfo_migration_targets_contract.sh` 的 source/doc multiline truth
   更新为包含 `ISSLConnectionTextIO`。
2. 把 `test_isslsessionresumption_compiler_deprecated_contract.sh` 的 residual note 期望
   更新为 `backend-specific semantic truth proofs`。
3. 重新跑 focused contracts，确认这次失败只是 stale contract，不是新的 source drift。

## Verification

```bash
bash tests/scripts/test_isslconnection_surface_truth_contract.sh
bash tests/scripts/test_isslconnectioninfo_migration_targets_contract.sh
bash tests/scripts/test_issldiagnostics_compiler_deprecated_contract.sh
bash tests/scripts/test_isslsessionresumption_compiler_deprecated_contract.sh
bash tests/scripts/test_getverifyresult_compiler_deprecated_contract.sh
bash tests/scripts/test_isslocspstapling_compiler_deprecated_contract.sh
bash tests/scripts/test_tsslconfig_scope_bucket_truth_contract.sh
bash tests/scripts/test_facade_main_entry_truth_contract.sh
git diff --check
```

## Expected Outcome

- focused contracts 回到当前真实的 `ISSLConnection` shared owner/mirror truth
- session-resumption residual wording 不再回退成旧分类
- 下一次继续接口审查时，不会再把这两个 stale contract 误判成新的源码缺口
