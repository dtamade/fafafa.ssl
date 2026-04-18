# FreePascal Early-Data Mixed Public-Path Durability Closeout Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改默认 shipped behavior、capability wording 或现有 `src/` wiring 的前提下，为 FreePascal early-data file-backed anti-replay 的 mixed public path 再补一层低返工高收益的 durable truth：builder 与 one-shot factory 互相消费同一个 replay store 时，跨 context replay 仍正确 reject，且 expired persisted entries 通过 public-installed ledger 仍会 prune。

**Architecture:** 继续复用现有 focused scripted early-data runtime harness、`CaptureServerIssuedSession(...)`、`BuildReplayProviderStoreFilePath(...)`、`CleanupReplayProviderStoreFiles(...)`、以及 file-backed replay-store binary helper。新的 contracts 只落在 `tests/test_freepascal_tls13_early_data.pas`：两条 mixed builder/factory runtime contracts 锁住 public path 之间的 shared replay truth，一条 public-installed ledger prune contract 锁住 expired persisted entry 语义。只有 fresh RED 明确指向实现 drift，才最小查看 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas` 或 `src/fafafa.ssl.freepascal.earlydatareplay.pas`。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 early-data focused runtime tests, `TSSLContextBuilder`, `TSSLFactory.CreateContext(const AConfig)`, file-backed replay-store opt-in, file-based working memory, roadmap closeout notes.

---

## Summary

- 刚完成的 public builder / one-shot factory runtime restart durability 证明了两条 public path 都能各自写出 persisted replay truth，并在新进程里继续 reject replay。
- 当前最高 ROI 的下一步不再是重复同类型 restart smoke，而是锁住 mixed public path 是否共享同一个 persisted truth：
  - builder first -> factory second
  - factory first -> builder second
- 另一个低返工高收益的 adjacent truth 是 public-installed ledger 经过 file-backed store 仍会 prune expired persisted entries，而不是只有 direct provider rebuild 才有这条合同。
- 如果这些 fresh contracts 直接 GREEN，本批应保持 tests/docs/working-memory only；不要伪造 production 修复。

## Delivery Order

1. 写本轮 plan，并把 `task_plan.md` / `findings.md` / `progress.md` 顶部切到 mixed public-path durability closeout 批次。
2. 在 `tests/test_freepascal_tls13_early_data.pas` 先补 mixed builder/factory runtime RED。
3. 再补一条 public-installed ledger expired-entry prune RED。
4. 跑 focused suite；只有 fresh RED 明确落在实现 drift 时，才最小查看 `src/`。
5. 跑 adjacent regressions、completeness gate、compile gate、diff hygiene。
6. 用 fresh evidence 更新 `docs/ROADMAP.md`、`task_plan.md`、`findings.md`、`progress.md`。

### Task 1: RED - Lock Mixed Builder/Factory Runtime Durability

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.context.builder.pas`
- Reference: `src/fafafa.ssl.factory.pas`

**Step 1: Add builder-first / factory-second runtime contract**
- 复用现有：
  - `BuildReplayProviderStoreFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
  - `CaptureServerIssuedSession(...)`
  - scripted resumed early-data runtime helper
- 新增 focused contract，覆盖：
  - builder-built `ctx1` 与 one-shot factory-built `ctx2` 指向同一个 replay-store file
  - `ctx1` first resumed early-data accept
  - `ctx2` second resumed early-data reject
  - reject 后 resumed handshake 仍成功、session 仍 reused、discarded early bytes 不可读

**Step 2: Verify RED**

Run:

```bash
mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data
```

Expected:
- 若 mixed public path 存在 drift：focused FAIL，且失败点明确落在 builder/factory shared replay truth
- 若现有实现已满足：focused PASS，可继续补下一条合同

**Step 3: Add factory-first / builder-second runtime contract**
- 新增镜像合同，覆盖：
  - one-shot factory-built `ctx1` 与 builder-built `ctx2` 指向同一个 replay-store file
  - `ctx1` first resumed early-data accept
  - `ctx2` second resumed early-data reject
  - 继续断言 resumed handshake / reuse / no early bytes drift

### Task 2: RED - Lock Public Installed-Ledger Expiry Semantics

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a mixed public-path prune contract**
- 复用现有：
  - `BuildManualSession(...)`
  - `ResolveSessionReplayKey(...)`
  - `WriteReplayProviderStoreSingleEntry(...)`
  - `BuildReplayProviderStoreFilePath(...)`
  - `CleanupReplayProviderStoreFiles(...)`
- 新增 focused contract，覆盖：
  - 先往 replay-store file 写一个 matching-but-expired persisted entry
  - builder-built context 上通过 installed replay ledger 对该 session first acquire 成功
  - 新的 factory-built context 指向同一路径时，对同一 session acquire 失败
  - 证明 public-installed ledger 仍然继承 prune-then-persist 语义

**Step 2: Verify RED**
- Re-run Task 1 command
- Expected:
  - FAIL only if public-installed ledger path 丢了 expiry/prune 语义
  - 否则 focused PASS

### Task 3: GREEN - Only If Fresh RED Proves Public Path Drift

**Files:**
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify if needed: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Keep production scope minimal**
- 只有 fresh RED 明确落在 file-backed store / provider / ledger expiry semantics 时，才最小查看：
  - file-backed store load/save/prune path
  - provider-backed ledger acquire path
- 不先碰：
  - `src/fafafa.ssl.factory.pas`
  - `src/fafafa.ssl.freepascal.context.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`

**Step 2: Re-run focused verification**
- Re-run Task 1 command
- Expected: PASS

### Task 4: Closeout, Verification, and Roadmap Truth

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run adjacent verification**

Run:

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_mixed_public_path_durability_20260414
```

```bash
python3 scripts/compile_all_modules.py
```

Expected:
- focused early-data PASS
- backend basic PASS
- capability cache PASS with unchanged KnownIssues wording
- completeness gate PASS
- compile gate PASS

**Step 2: Update roadmap + working memory**
- `docs/ROADMAP.md`：
  - 补一句 mixed builder/factory public path 也已获得 shared persisted replay truth evidence
  - 下一条 queue 保持为“更重 provider / durability 形态验证”，而不是重开 seam/parity/wiring
- `task_plan.md` / `findings.md` / `progress.md`：
  - 记录本批 closeout、fresh evidence、以及是否避免了 `src/` 变更

**Step 3: Run diff hygiene**

Run:

```bash
git diff --check -- docs/plans/2026-04-14-freepascal-early-data-mixed-public-path-durability-closeout.md docs/ROADMAP.md tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md
```

Expected:
- no output

### Definition Of Done

- mixed builder/factory public path 共享同一个 file-backed replay truth 的 runtime 合同已有 fresh evidence
- public-installed ledger 对 expired persisted entry 继续保持 prune-then-persist 语义
- 若 fresh tests 直接 GREEN，则本批保持 tests/docs/working-memory only
- capability wording 继续保持 `0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`
- focused suite、adjacent regressions、completeness gate、compile gate、diff hygiene 都有 fresh evidence
