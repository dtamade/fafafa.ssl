# FreePascal Early-Data Anti-Replay Provider / Persistent Prototype Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩大 public early-data surface、也不提前上调 capability wording 的前提下，为 FreePascal TLS 1.3 0-RTT / early-data 的 anti-replay 再补一层最小 provider-backed prototype，让多个 context 可以通过共享 provider 协同拒绝 replay，并为后续本地文件型持久化实现固定接口边界。

**Architecture:** 继续保持现有 `IFreePascalEarlyDataReplayLedger` 与 context access seam 不变，本批只在内部增加一个更窄的 provider contract：ledger 仍负责从 session 解析 `ticket key + expires at` 真值，provider 只负责“按 key 原子 acquire / ignore expired / reject replay”。默认 in-memory ledger 不改成 provider 驱动，先以最小 callback/provider-backed ledger 原型证明跨 context replay coordination seam 是对的。

**Tech Stack:** FreePascal (ObjFPC), pure Pascal TLS 1.3 session/resumption/early-data units, internal provider-backed replay ledger prototype, offline scripted early-data tests, focused completeness gate, file-based working memory.

---

## Summary

- 上一批已经完成：
  - default in-memory anti-replay ledger 抽离
  - context active ledger seam
  - resumed early-data accept path 走 active ledger
- 当前最高 ROI 的剩余缺口是：
  - replay state 仍默认停在单 context / 单进程内存对象
  - 还没有一个“最小 persistent/provider seam”来证明多个 context 可以共享 replay truth
- 本批只做三件事：
  1. 先补 RED，锁定 provider-backed ledger 和 cross-context replay coordination contract
  2. 新增内部 provider contract + callback/provider-backed ledger prototype
  3. 保持 default in-memory ledger、focused gate 与 capability wording 不变
- 本批明确不做：
  - public API / builder 扩面
  - 分布式 anti-replay
  - 本地文件型实现
  - `KnownIssues` wording 升级

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，覆盖 provider-backed ledger 与 cross-context replay coordination。
3. 最小 GREEN：在 internal replay 单元增加 provider contract 与 callback/provider-backed ledger prototype。
4. 跑 focused regressions、focused gate、compile gate、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: RED - Lock provider-backed replay contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.session.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`

**Step 1: Add direct provider-backed ledger contract**
- 在测试里增加一个共享 provider store / callback object：
  - 内部按 `ticket key -> expires at` 记账
  - acquire 时先忽略/清理已过期 entry
  - 已存在且未过期则返回 `False`
  - 不存在则写入并返回 `True`
- 用两个独立的 provider-backed ledger 实例覆盖：
  - 同一个 valid resumable session 在 ledger A 第一次 acquire 成功
  - 同 session 在 ledger B 再次 acquire 失败
  - expired session acquire 失败且不会污染 provider state
  - fresh valid session 仍可成功 acquire

**Step 2: Add cross-context replay coordination runtime contract**
- 用两个 server context + 同一个 shared provider 覆盖：
  - `ctx1` / `ctx2` 都注入各自的 provider-backed ledger
  - 初始握手在 `ctx1` 上完成并拿到 resumable session
  - 把 session 存入 `ctx2` 的 resumption cache
  - first-use resumed early-data 在 `ctx1` 上成功 accept
  - 对同一 session 的第二次 resumed early-data 在 `ctx2` 上必须被 reject
  - reject 后握手仍成功、session 仍 reused、early-data bytes 不可读

**Step 3: Run RED**

```bash
mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data
```

**Expected:**
- FAIL，优先暴露 provider-backed replay symbols 还不存在，而不是相邻旧行为回归。

### Task 2: GREEN - Add the minimal provider-backed prototype

**Files:**
- Modify: `src/fafafa.ssl.freepascal.session.pas`
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`

**Step 1: Add a narrow internal provider contract**
- 在 `src/fafafa.ssl.freepascal.session.pas`：
  - 保持 `IFreePascalEarlyDataReplayLedger.TryAcquireEarlyDataSession(...)` 不变
  - 新增 internal provider interface，contract 只表达：
    - input: `ticket key + expires at + now`
    - output: acquire success / replay reject
    - provider 自己负责 ignore/prune expired state
  - 如有必要，再补一个 managed provider interface，允许 provider 响应 clear / enable / capacity

**Step 2: Add provider-backed ledger prototype**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.pas`：
  - 保留现有 default in-memory ledger
  - 新增 callback/provider-backed ledger 类
  - replay 真值继续由 ledger 从 session 解析：
    - ticket key
    - expiry based on `IsValid` / timeout / ticket lifetime
  - provider-backed ledger 只把 acquire 最终委托给 provider
  - 若 provider 支持 managed contract，则继续接 enable / capacity

**Step 3: Reuse existing context assembly path**
- 不扩 public/context surface
- 继续用已有：
  - `IFreePascalEarlyDataReplayLedgerAccess.SetEarlyDataReplayLedger(...)`
  - `ResetEarlyDataReplayLedger`
  - `SetSessionCacheMode(...)` / `SetSessionCacheSize(...)` 对 managed ledger 的同步行为

**Step 4: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify adjacent truth stays locked

**Files:**
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Keep capability wording stable**
- 本批不改 `KnownIssues`
- 继续要求：
  - `ZeroRTTSupport = sslSupportExperimental`
  - `EarlyDataSupport = sslSupportExperimental`
  - `KnownIssues` 仍保留 single-process / in-memory anti-replay caveat

**Step 2: Run focused wording checks**

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
```

```bash
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

**Expected:**
- PASS

### Task 4: Focused gate, compile gate, diff hygiene

**Commands:**

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_provider_proto_20260412
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-provider-persistent-prototype.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md
```

**Expected:**
- focused gate => PASS
- compile gate => PASS
- diff hygiene => exit `0`

## Definition Of Done

- provider-backed replay ledger prototype 已经存在，且不改 public early-data surface
- 两个独立 context 可以通过 shared provider 协同拒绝 replay
- default in-memory ledger 与 current capability wording 均未退化
- 本轮计划、findings、progress 都有 fresh RED/GREEN evidence
