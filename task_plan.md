# Task Plan - FreePascal Early-Data Default Durable Shipped Path

## Goal
在不扩 public API 的前提下，把 FreePascal server-side early-data 默认 shipped path 从单进程内存 replay ledger 收口到默认 durable replay-store 路径，并同步修正 capability / roadmap / API docs 对这条默认路径的真相表述。

## Current Batch
1. 先补 focused RED：
   - `tests/test_freepascal_tls13_early_data.pas` 锁住默认 server context 的 same-process / cross-process durable replay truth
   - `tests/test_capability_cache.pas` 锁住 `KnownIssues` 不再声称默认路径是 `in-memory single-process anti-replay ledger`
2. 最小 GREEN：
   - `src/fafafa.ssl.freepascal.earlydatareplay.pas` 增加默认 replay-store 路径解析与 managed persistent provider
   - `src/fafafa.ssl.freepascal.context.pas` 默认 server ledger 改走 durable path
   - `src/fafafa.ssl.freepascal.lib.pas` 更新 capability wording
3. 文档真相同步：
   - `docs/ROADMAP.md`
   - `docs/BACKEND_CAPABILITY_MATRIX.md`
   - `README.md`
   - `docs/reference/API_REFERENCE.md`
4. 跑 focused tests / completeness gate / compile gate / diff hygiene，回写台账并提交。

## Status
- [completed] RED: default durable-path contracts and capability wording assertions
- [completed] GREEN: managed persistent default replay provider
- [completed] Docs truth alignment
- [completed] Verification, review, and commit

## Verification Plan
- focused default early-data runtime:
  - `mkdir -p tmp/freepascal_tls13_early_data`
  - `fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas`
  - `./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- focused capability truth:
  - `mkdir -p tmp/capability_cache_units`
  - `fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas`
  - `./tmp/capability_cache_units/test_capability_cache`
- repo verification:
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_durable_shipped_path_20260504`
  - `python3 scripts/compile_all_modules.py`
- hygiene:
  - `git diff --check -- docs/plans/2026-05-04-freepascal-early-data-default-durable-shipped-path.md src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_tls13_early_data.pas tests/test_capability_cache.pas docs/ROADMAP.md docs/BACKEND_CAPABILITY_MATRIX.md README.md docs/reference/API_REFERENCE.md task_plan.md findings.md progress.md`

## Batch Result
- default server-side FreePascal early-data shipped path 已切到本地持久化 replay-store 路径，并保住 `SetSessionCacheMode` / `SetSessionCacheSize` 的 managed clear 语义。
- `KnownIssues`、`ROADMAP`、capability matrix、README、API reference 都已收口到“local persistent + fail-closed + experimental”的同一真相。
- focused tests、`run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_durable_shipped_path_20260504`、`python3 scripts/compile_all_modules.py`、以及 `git diff --check` 都已通过。
- 本批已提交，后续继续以这份批次结果作为 `WinSSL` runtime proof / FreePascal residual wording 复盘的起点。

## Risks
- 默认 durable path 不能通过“上下文创建时就强制落盘”来制造新初始化失败面；必须保持按需 materialize，并在不可写时 fail closed。
- 默认路径改成持久化后，现有 default-ledger cache-sync 合同仍必须保持 green，不能把 disable / zero-capacity 语义退化成“只关本进程开关、不清 replay truth”。
- 文档和 capability string 只能收口到本批 fresh evidence，不能顺手把 `experimental` 擅自提成 `stable`。

## Follow-up Queue
1. 这批已经去掉默认 `in-memory single-process` caveat；下一步应重新判断 `FreePascal early-data` 是否只剩 `experimental` / local-persistent / non-distributed wording，或 Linux 侧最高价值缺口是否已经转到 `WinSSL` runtime proof。
2. 若 default durable path 落地后仍有 fresh RED，再决定是否要补 default-path filesystem blocker 专属合同。
3. `WinSSL` 的 Windows runtime proof 仍需独立环境，不能在当前 Linux 主机上伪造完成。
