# FreePascal Early-Data Internal Replay-Store Shape Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data anti-replay 增加一个 backend-private internal replay-store seam，并验证它能复用现有 context / resumed accept wiring，同时保持默认内存 ledger 与现有 file-backed/provider 行为不退化。

**Architecture:** 继续复用现有 `IFreePascalEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`IFreePascalContextEarlyDataReplayProviderInstaller` 与 resumed early-data accept path，不重开 public/context surface。最小修法是在 provider 之下新增一个更窄的 internal replay-store contract：store 只负责 update-guard + load/save entry truth，generic store-backed provider 负责 `load -> prune -> replay check -> append -> save` 语义并统一 fail closed；现有 file-backed provider 退化成这个内部 store seam 的一个特化实现。这样后续如果继续扩 callback/provider/file/local persistence 形态，不需要复制 replay acquire 语义。

**Tech Stack:** FreePascal (ObjFPC), backend-private replay-store interface, store-backed replay provider helper, `TFileStream`, Unix advisory lock, TLS 1.3 early-data focused runtime tests, completeness gate, file-based working memory.

---

## Summary

- 当前 live truth：
  - default in-memory ledger、provider-backed ledger、callback/provider helper、file-backed provider、context installer seam、builder/factory replay-store opt-in 都已存在
  - resumed early-data accept path 已经通过 active ledger seam 工作
  - 当前真正还绑死在具体实现上的，是 file-backed provider 自己同时持有 `lock + load + prune + replay-check + save` 全套逻辑
- 当前最高 ROI 的下一步不是再改 context/connection/public API，而是把“replay acquire 语义”和“持久化 store 形态”拆开
- 本批只做：
  - internal replay-store contract
  - generic store-backed replay provider / install helper
  - file-backed provider 退化成 store-backed provider 的特化实现
  - focused RED / GREEN / verification / ledger closeout
- 本批明确不做：
  - capability wording 升级
  - distributed / cross-host anti-replay
  - 新的 public builder/factory/config 字段
  - public `ISSLContext` / `ISSLEarlyDataContext` 扩面

## Delivery Order

1. 写本轮 plan，并在 working-memory 中登记这批目标与边界。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，锁住 store-backed provider/helper/runtime 语义与 fail-closed 合同。
3. 最小 GREEN：只改 `src/fafafa.ssl.freepascal.session.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
4. 跑 focused regression、focused gate、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock Internal Replay-Store Contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.session.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a shared in-memory replay-store test helper**
- 在 focused test 文件里新增最小 helper：
  - 一个空 guard object
  - 一个 shared replay-store object，内部持有 entry 数组
  - store 只暴露：
    - acquire update guard
    - load current entries
    - save updated entries
- helper 自己不做 replay-check；让 replay 语义留给待实现的 generic store-backed provider

**Step 2: Add direct store-backed provider contracts**
- 新增 direct provider contracts，覆盖：
  - 两个 provider 实例共享同一个 replay-store object
  - 第一个 provider-backed ledger 对 fresh valid session 返回 `True`
  - 第二个 provider-backed ledger 对同一 session 返回 `False`
  - expired session 仍在进入 store 之前被拒绝
  - fresh session 仍可继续通过

**Step 3: Add fail-closed store error contracts**
- 新增 focused direct contracts，覆盖：
  - store 在 acquire-guard / load / save 任一阶段抛异常或返回失败
  - store-backed provider-backed ledger 必须 fail closed，而不是向外冒异常或 silent accept

**Step 4: Add install-helper / runtime contract**
- 新增 focused runtime contract，覆盖：
  - 两个 FreePascal server context 通过一个新的 `InstallStoreBackedReplayLedger(...)` helper 安装共享 replay-store
  - 第一次 resumed early-data 仍 accepted
  - 第二次跨 context resumed early-data 被 reject
  - reject 后握手仍成功、session 仍 reused、early bytes 不可读

**Step 5: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - RED，优先失败在缺少 internal replay-store symbols / helper，或 file-backed provider 还未退化到新的 store seam

### Task 2: GREEN - Add The Smallest Internal Replay-Store Seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.session.pas`
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add narrow backend-private store interfaces**
- 在 `src/fafafa.ssl.freepascal.session.pas`：
  - 新增 internal replay-store entry record / array type
  - 新增 replay-store guard interface
  - 新增 replay-store interface，contract 至少表达：
    - acquire update guard
    - load entries
    - save entries
- 不改 `IFreePascalEarlyDataReplayLedger`、`IFreePascalEarlyDataReplayProvider`、public `ISSLContext`

**Step 2: Add a generic store-backed provider**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.pas`：
  - 新增 `TFreePascalStoreBackedEarlyDataReplayProvider`
  - replay acquire 语义继续保持：
    - `AKey=''` => reject
    - expired-at-before-now => reject
    - acquire guard
    - load
    - prune expired
    - reject live replay
    - append fresh entry
    - save
  - store exception / load failure / save failure / guard failure => fail closed

**Step 3: Add a thin install helper**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.pas`：
  - 新增 `InstallStoreBackedReplayLedger(AContext, AStore)` helper
  - helper 继续通过现有 `InstallReplayProviderBackedLedger(...)` / backend-private installer seam 接入 context
  - 对 `nil` context / `nil` store / missing seam 继续 fail closed

**Step 4: Refactor file-backed provider into a store specialization**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`：
  - 新增 file-backed replay-store 实现，保留现有：
    - sidecar advisory lock
    - orphan `.tmp` recovery
    - corruption fail-closed
    - canonical main-file temp replace write path
  - 让现有 `TFreePascalFileEarlyDataReplayProvider` 退化成创建 file-backed store 并委托 generic store-backed provider
  - `InstallFileBackedReplayLedger(...)` helper 保持接口与行为不变

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify No Adjacent Drift

**Files:**
- Reference: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Run focused verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_internal_replay_store_shape_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-internal-replay-store-shape-validation.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- provider 之下有一个可复用的 internal replay-store seam
- generic store-backed provider 统一承接 replay acquire 语义，并对 store failures fail close
- file-backed provider 退化成 store-backed provider 的特化实现，但现有 file-backed focused contracts 不退化
- `InstallStoreBackedReplayLedger(...)` helper 能通过现有 context wiring 接到 resumed early-data accept path
- default in-memory ledger、public API、builder/factory/config surface 与 capability wording 都不变
- focused tests、focused gate、compile gate、diff hygiene 都有 fresh evidence
