# 2026-05-22 FreePascal Early-Data Final Caveat Closeout

## Goal

围绕 `fafafa.ssl` 当前 post-release 唯一剩余主线，
对 FreePascal early-data / replay-store caveat
做一次最终收口验证：

- 以 `docs/ROADMAP.md` 为当前真相源
- 核对 `KnownIssues`、README、API/集成文档、capability 文档、
  focused contracts 与相关实现是否一致
- 如果没有 fresh drift，就把
  “这是当前有意保留的最终 experimental boundary”
  这层 workflow truth
  明确写清并加守卫，
  避免后续反复拉起旧 release /
  directory-store family /
  历史计划

## Scope

- Update:
  - `docs/ROADMAP.md`
  - `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Reference:
  - `README.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/INTEGRATION_GUIDE.md`
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/EARLY_DATA_GUIDE.md`
  - `docs/guides/security-best-practices.md`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `tests/test_capability_cache.pas`

不做：

- 不重开 production early-data /
  replay-store 实现线
- 不无 fresh RED
  回头重开 historical directory-store family
- 不重开 release-control /
  `v1.5.0`
  路线

## Why This Batch

当前 active docs 与 runtime truth
实际上已经基本对齐：

- FreePascal server context
  默认创建
  `TFreePascalDefaultPersistentEarlyDataReplayLedger`
- `KnownIssues`
  当前只保留
  `0-RTT / early data is experimental ... local persistent anti-replay replay-store ... fail-closed`
  这条边界
- README / API /
  Integration /
  Early-Data Guide /
  Backend Capability Matrix
  当前都已经写回
  persistent-by-default +
  fail-closed +
  experimental
  口径

这次真正发现的缺口
不是实现漂移，
而是 workflow guard
还不够完整：

- focused contract
  之前没有冻结
  `docs/ROADMAP.md`
  的 final-boundary 口径
- 同一个 contract
  也没有冻结
  `docs/BACKEND_CAPABILITY_MATRIX.md`
  /
  `docs/guides/EARLY_DATA_GUIDE.md`
  里的 FreePascal caveat truth
- `docs/ROADMAP.md`
  虽然已经暗示
  不建议重开 directory-store family，
  但还值得再明确一句：
  当前剩余 caveat
  应视为
  post-release 阶段
  有意保留的最终 experimental boundary

## Verification

```bash
bash -n tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh
bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh
bash tests/scripts/test_early_data_docs_truth_contract.sh
mkdir -p tmp/capability_cache_units && fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_units/test_capability_cache
git diff --check
```

## Current Execution Receipt

- `bash -n tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - PASS
- `bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - PASS
- `bash tests/scripts/test_early_data_docs_truth_contract.sh`
  - PASS
- `mkdir -p tmp/capability_cache_units && fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_units/test_capability_cache`
  - PASS
  - focused runtime summary:
    - FreePascal `KnownIssues`
      当前输出：
      `0-RTT / early data is experimental and currently relies on a local persistent anti-replay replay-store path; if the path is unavailable or unwritable, resumed early data is rejected fail-closed.`
    - `ZeroRTTSupport = sslSupportExperimental`
    - `EarlyDataSupport = sslSupportExperimental`
- `git diff --check`
  - PASS

## Outcome

- 没有发现新的 production implementation drift
- 当前剩余 caveat
  确认为
  有意保留的最终 experimental boundary
- 这批收口的是
  roadmap / capability-doc /
  focused-contract
  的防回退覆盖，
  不是功能扩线

## Follow-up

- 同日的后续 completion audit
  发现
  `README.md`
  仍残留一处
  “默认 shipped path 已持久化”
  /
  “opt-in 不代表默认路径已经持久化”
  的自相矛盾
- 对应修复与补充 guard
  已单独记录在：
  - `docs/plans/2026-05-22-freepascal-early-data-readme-contradiction-guard-closeout.md`
