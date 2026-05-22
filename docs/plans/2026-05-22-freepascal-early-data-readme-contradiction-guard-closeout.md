# 2026-05-22 FreePascal Early-Data README Contradiction Guard Closeout

## Goal

对同日
`FreePascal early-data final caveat closeout`
做 follow-up completion audit，
确认当前 active docs
是否真的已经与
`docs/ROADMAP.md`
/
`KnownIssues`
/
focused contracts
完全一致。

如果发现仍有
active self-contradiction，
就按最小范围修掉，
并把 focused contract
补成会拦截这类回退。

## Scope

- Update:
  - `README.md`
  - `tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
  - `docs/plans/2026-05-22-freepascal-early-data-final-caveat-closeout.md`
  - `task_plan.md`
  - `findings.md`
  - `progress.md`
- Reference:
  - `docs/ROADMAP.md`
  - `docs/reference/API_REFERENCE.md`
  - `docs/INTEGRATION_GUIDE.md`
  - `docs/BACKEND_CAPABILITY_MATRIX.md`
  - `docs/guides/EARLY_DATA_GUIDE.md`
  - `docs/guides/security-best-practices.md`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `tests/test_capability_cache.pas`

不做：

- 不重开 production early-data /
  replay-store 实现线
- 不重开 historical
  directory-store family
- 不改写历史 archive plans；
  只把当前 active truth
  和 follow-up evidence
  写清

## Why This Batch

当前 source/runtime truth
没有再漂：

- `src/fafafa.ssl.freepascal.context.pas`
  server path
  仍默认创建
  `TFreePascalDefaultPersistentEarlyDataReplayLedger`
- `src/fafafa.ssl.freepascal.lib.pas`
  `KnownIssues`
  仍继续发布
  `local persistent anti-replay replay-store ... fail-closed`
- `docs/ROADMAP.md`
  也已经把这条 caveat
  定位成
  post-release 阶段
  有意保留的最终
  `experimental` boundary

真正遗漏的是
completion audit 的 guard：

- `README.md`
  段首已经说
  默认 shipped path
  会把 replay truth
  落到本地持久化 replay-store
- 但段尾
  还残留一句
  “这条 opt-in
  不代表默认路径已经持久化”
- 现有 focused contract
  又只锁了
  正向关键词存在，
  没有拦截
  这种反向自我否定

## Verification

```bash
bash -n tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh
bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh
bash tests/scripts/test_early_data_docs_truth_contract.sh
mkdir -p tmp/capability_cache_units && fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_units/test_capability_cache
git diff --check
```

## Current Execution Receipt

- pre-fix audit:
  - `bash tests/scripts/test_freepascal_early_data_public_optin_docs_contract.sh`
    - PASS
  - `bash tests/scripts/test_early_data_docs_truth_contract.sh`
    - PASS
  - manual audit result:
    - focused contract
      仍为绿色，
      但 `README.md`
      仍有
      default persistent truth
      自相矛盾
- post-fix verification:
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
        当前继续输出：
        `0-RTT / early data is experimental and currently relies on a local persistent anti-replay replay-store path; if the path is unavailable or unwritable, resumed early data is rejected fail-closed.`
      - `ZeroRTTSupport = sslSupportExperimental`
      - `EarlyDataSupport = sslSupportExperimental`
  - `git diff --check`
    - PASS

## Outcome

- 没有发现新的 production implementation drift
- 真实缺口是：
  - `README.md`
    一处 active self-contradiction
  - focused docs contract
    没有对这类反向表述设 guard
- 现在 active docs /
  runtime wording /
  focused contracts
  对
  `persistent-by-default + fail-closed + experimental + not distributed-ready`
  这组边界
  已再次收平
