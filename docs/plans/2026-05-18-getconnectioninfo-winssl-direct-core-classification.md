# `GetConnectionInfo` WinSSL Direct-Core Classification

## Goal

确认当前 residual direct-core `GetConnectionInfo` WinSSL 测试面是否属于 intentional core-surface proof，并把这条边界显式记录下来，避免后续再次在“该不该迁到 `ISSLConnectionInfo`”上反复摇摆。

## Scope

- `tests/winssl/test_winssl_connection_info.pas`
- `tests/winssl/test_winssl_connection_edge_cases.pas`
- `tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh`
- `tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh`
- `docs/plans/2026-05-18-post-sni-interface-debt-roadmap.md`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改生产实现
- 不改 WinSSL runtime 行为
- 不直接进入 `GetConnectionInfo` compiler deprecation

## Planned Changes

1. 给剩余 WinSSL direct-core `GetConnectionInfo` 测试显式补上 `INTENTIONAL_CORE_SURFACE` 分类。
2. 新增 focused shell contract，守住：
   - WinSSL residual file set
   - direct core `GetConnectionInfo` 命中
   - intentional core-surface marker
3. 把路线图默认下一步推进到更强 wording / slimming route。

## Verification

```bash
bash tests/scripts/test_getconnectioninfo_winssl_direct_core_classification_contract.sh
bash tests/scripts/test_isslconnectioninfo_getconnectioninfo_residual_classification_contract.sh
git diff --check
```

## Expected Outcome

- 剩余 WinSSL direct-core `GetConnectionInfo` 测试被正式认定为 intentional core-surface proof
- `GetConnectionInfo` route 不再卡在 residual 分类不清
- 默认下一步可直接转入更强 owner / deprecation wording route
