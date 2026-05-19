# API Inventory And PKCS11 High-Entry Doc Truth

## Goal

把高入口参考文档重新锚回当前源码 truth，避免 `API_INVENTORY.md` 和 `PKCS11` 专题页继续把接口设计 / 后端实现审查带回旧世界。

## Architecture

这批保持 docs-only，不动生产实现：

- 把 `docs/reference/API_INVENTORY.md` 从历史 phase snapshot 收回成当前 public-surface 索引
- 给 `PKCS11` 用户指南和架构页补上：
  - 当前 published path = `OpenSSL` backend
  - capability truth = runtime-aware readiness
  - 其它 backend 当前不发布 `PKCS11` capability
- 新增 focused shell contract，冻结上述高入口 truth

## Files

- Add: `docs/plans/2026-05-19-api-inventory-pkcs11-high-entry-doc-truth.md`
- Add: `tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh`
- Modify: `docs/reference/API_INVENTORY.md`
- Modify: `docs/guides/PKCS11_USER_GUIDE.md`
- Modify: `docs/reference/PKCS11_ARCHITECTURE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

## Why This Batch

当前已经收口了多轮 capability/public-surface truth，但还有两个高入口文档源明显滞后：

- `docs/reference/API_INVENTORY.md`
  - 仍停在 2026-01-31 的旧快照
  - 仍只列 `OpenSSL` / `WinSSL` context/connection 实现
  - 仍把 `GetOCSPStaplingEnabled` / `GetOCSPResponse` / `IsOCSPResponseVerified` / `GetOCSPResponseStatus` 写成“待实现”
  - 仍把 `PKCS#11` 和 `OCSP Stapling` 写成“未完成”
- `docs/guides/PKCS11_USER_GUIDE.md` / `docs/reference/PKCS11_ARCHITECTURE.md`
  - 虽然 builder 示例已经较新
  - 但高层叙事仍没有明确 current published path 是 `OpenSSL` backend
  - 也还没有把 `SupportsPKCS11` 的 runtime-aware truth 讲成主叙事

如果这批不收，后面继续审查各 backend 接口实现时，讨论入口会一直被旧文档污染。

## Verification

```bash
bash -n tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh
bash tests/scripts/test_api_inventory_pkcs11_high_entry_truth_contract.sh
bash tests/scripts/test_pkcs11_docs_builder_guidance_contract.sh
npx prettier --write docs/reference/API_INVENTORY.md docs/guides/PKCS11_USER_GUIDE.md docs/reference/PKCS11_ARCHITECTURE.md
git diff --check
```

## Expected Outcome

- `API_INVENTORY.md` 不再冒充历史阶段报告，而是当前 public-surface 索引
- `PKCS11` 专题页不再暗示“全 backend 通用支持”，而是明确：
  - `OpenSSL` published path
  - Provider / ENGINE runtime readiness gate
  - 其它 backend 当前 `SupportsPKCS11=False`
- 高入口参考页与当前源码 truth 再次闭环
