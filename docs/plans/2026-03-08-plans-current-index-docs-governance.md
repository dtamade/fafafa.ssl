# Plans Current Index Docs Governance (2026-03-08)

## Goal
- 在 `docs/` 根目录提供一个轻量“活跃索引”，减少 `docs/plans/**` 历史记录过多导致的定位噪音。
- 不做大规模归档迁移；只补一个当前入口并接到现有文档入口。

## Scope
- `docs/PLANS_CURRENT_INDEX.md`
- `docs/README.md`
- `docs/DOCUMENTATION_INDEX.md`
- `docs/plans/README.md`
- `tests/scripts/test_plans_current_index_contract.sh`
- `tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

## Design
- 新增 `docs/PLANS_CURRENT_INDEX.md` 作为当前执行/治理入口。
- 只链接“当前真相”与最近高信号计划，不把旧计划重新组织到新目录结构里。
- 在 `docs/README.md`、`docs/DOCUMENTATION_INDEX.md`、`docs/plans/README.md` 增加可发现性链接。
- 新增轻量合同锁定入口存在与关键链接不回退。

## RED
1. Add:
   - `tests/scripts/test_plans_current_index_contract.sh`
2. Run:
   - `bash tests/scripts/test_plans_current_index_contract.sh`
   - Expected: FAIL，因为 `docs/PLANS_CURRENT_INDEX.md` 尚不存在。

## GREEN
1. Add / update:
   - `docs/PLANS_CURRENT_INDEX.md`
   - `docs/README.md`
   - `docs/DOCUMENTATION_INDEX.md`
   - `docs/plans/README.md`
2. Add the new docs-governance contract to:
   - `tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`

## Regression
- `bash tests/scripts/test_plans_current_index_contract.sh`
- `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
- `bash tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh`
- `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh`

## Execution Log (2026-03-08)

### RED
- Added:
  - `tests/scripts/test_plans_current_index_contract.sh`
- RED runs:
  - `bash tests/scripts/test_plans_current_index_contract.sh` => FAIL
  - Key failure: `docs/PLANS_CURRENT_INDEX.md should exist`

### GREEN
- Added / updated:
  - `docs/PLANS_CURRENT_INDEX.md`
  - `docs/README.md`
  - `docs/DOCUMENTATION_INDEX.md`
  - `docs/plans/README.md`
  - `tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh`
- Added a lightweight active index at docs-root level instead of reorganizing historical plan files.
- Wired the new contract into the docs strict batch so index discoverability regresses with docs-governance checks.

### Regression
- `bash tests/scripts/test_plans_current_index_contract.sh` => PASS
- `bash tests/scripts/test_docs_active_noise_and_index_dedup_strict_batch.sh` => PASS
- `bash tests/scripts/test_scan_active_docs_noise_path_and_strict_contract.sh` => PASS
- `bash tests/scripts/test_minimal_ci_gate_docs_governance_batch_option.sh` => PASS
- `bash scripts/scan_active_docs_noise_draft.sh --strict --output tmp/active_docs_noise_scan_post_index.md` => PASS
- `bash scripts/check_docs_index_dedup_draft.sh --scope all --strict --output tmp/docs_index_dedup_post_index.md` => PASS
