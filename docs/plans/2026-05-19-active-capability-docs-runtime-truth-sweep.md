# Active Capability Docs Runtime Truth Sweep

## Goal

收掉当前仍会误导后续开发的 active docs capability drift，重点对齐：

- `OpenSSL PKCS#11` 已改为 runtime-aware truth
- `WinSSL PKCS11/TPM` 当前不发布 capability
- `OpenSSL SupportsFIPSMode` 当前默认构建仍为 `False`

## Scope

只处理 3 份活跃入口文档：

- `docs/MIGRATION_GUIDE_V1.1.md`
- `docs/BACKEND_SELECTION_GUIDE.md`
- `docs/CAPABILITY_MATRIX_GUIDE.md`

不改生产代码，不重开 backend 实现。

## Why This Batch

静态复审已经确认：

- `MIGRATION_GUIDE_V1.1.md` 仍把：
  - `WinSSL PKCS#11 = ✅`
  - `WinSSL TPM = ✅`
  - `OpenSSL FIPS = ✅`
  写成当前 truth
- `BACKEND_SELECTION_GUIDE.md` 的 OpenSSL 评分示例仍把：
  - `SupportsPKCS11: Yes`
  写成 unconditional truth
- `CAPABILITY_MATRIX_GUIDE.md` 仍用：
  - `if Caps.SupportsSystemCertStore and Caps.SupportsTPM then`
  当作 Windows 推荐示例

这些都已经和当前 shipped capability truth 不一致。

## Files

- Add: `tests/scripts/test_active_capability_docs_runtime_truth_contract.sh`
- Modify: `docs/MIGRATION_GUIDE_V1.1.md`
- Modify: `docs/BACKEND_SELECTION_GUIDE.md`
- Modify: `docs/CAPABILITY_MATRIX_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Verification

```bash
bash -n tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
bash tests/scripts/test_active_capability_docs_runtime_truth_contract.sh
git diff --check
```

## Expected Outcome

- active docs 不再把 WinSSL `PKCS11/TPM` 写成已发布 capability
- active docs 不再把 OpenSSL `PKCS#11` 写成 unconditional truth
- active docs 不再把 OpenSSL 默认构建写成 `FIPS = ✅`
