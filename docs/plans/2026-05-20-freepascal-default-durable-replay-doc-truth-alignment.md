# 2026-05-20 FreePascal Default Durable Replay Doc Truth Alignment

## Goal

把 FreePascal server-side early-data 默认 durable replay-store 这条当前 live truth，
在 active docs 与 focused contract 中重新对齐，避免：

- 源码 / capability `KnownIssues` 已经是 durable-by-default
- 但 active docs 还在说 default path 仍是 `in-memory single-process`
- 或同一份 active doc 内部前后互相矛盾

## Scope

- `docs/reference/API_REFERENCE.md`
- `docs/INTEGRATION_GUIDE.md`
- `docs/guides/security-best-practices.md`
- `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
- `task_plan.md`
- `findings.md`
- `progress.md`

不做：

- 不改生产实现
- 不重开 replay-store durability 功能线
- 不回写历史 `docs/plans/2026-04-*` / `2026-05-04-*` 档案

## Why This Batch

当前 source/runtime truth 已经明确：

- `TFreePascalContext` server path 默认创建
  `TFreePascalDefaultPersistentEarlyDataReplayLedger`
- `TFreePascalSSLLibrary.GetCapabilities.KnownIssues`
  也已经改成
  `local persistent anti-replay replay-store path ... fail-closed`

但 active docs / focused contract 仍残留旧真相：

- `docs/INTEGRATION_GUIDE.md`
  仍写 default path 是 `in-memory single-process anti-replay ledger`
- `docs/guides/security-best-practices.md`
  仍引用旧的 `KnownIssues` 句子
- `docs/reference/API_REFERENCE.md`
  前面说 default shipped path 已改为 persistent，
  后面又说 “不代表默认路径已经改成持久化”
- 现有 `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  还要求 README 保留旧 wording，已经和 live truth 冲突

## Steps

1. 用现有 focused docs contract 做 RED
2. 最小修 active docs 的 durable-default wording
3. 升级 focused docs contract，冻结：
   - source truth
   - capability wording truth
   - active docs no-longer-in-memory truth
4. 跑 focused contract、必要的 capability runtime proof、diff hygiene

## Verification

```bash
bash -n tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh
bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh
mkdir -p tmp/capability_cache_units && fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_units/test_capability_cache
git diff --check
```

## Expected Outcome

- active docs stop teaching the old in-memory default truth
- API reference no longer contradicts itself on whether the default path is persistent
- focused docs contract now freezes the durable-default truth instead of the retired wording
