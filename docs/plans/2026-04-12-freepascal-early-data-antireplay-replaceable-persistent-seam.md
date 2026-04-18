# FreePascal Early-Data Anti-Replay Replaceable / Persistent Seam Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩大 public early-data surface、也不提前上调 capability wording 的前提下，把 FreePascal TLS 1.3 0-RTT / early-data 的 anti-replay 从“context 内嵌内存 ledger”拆成“默认内存实现 + 可替换实现”的抽象层，并把这条 seam 接到 resumed early-data accept path。

**Architecture:** 继续保持现有 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` 与 focused gate 不变。内部只做一层 narrow seam：`IFreePascalEarlyDataReplayLedger` 继续承担 acquire contract；新增 context-host access seam 暴露 get/set/reset；新建默认内存 ledger 实现承接当前 ticket-key + expiry + bounded behavior；`TFreePascalContext` 负责保存 active ledger，`TFreePascalConnection.DoAccept` 在 resumed early-data path 上解析 active ledger 并决定 accept/reject。这样本批先把“默认内存实现”和“可替换实现”解耦，后续再安全加本地文件型或 callback/provider 型持久化实现。

**Tech Stack:** FreePascal (ObjFPC), TLS 1.3 resumption / early-data runtime contracts, backend-private optional interfaces, focused completeness gate, file-based working memory.

---

## Summary

- 当前树上的真实剩余缺口只在 anti-replay persistence / replaceability：
  - `TFreePascalContext` 仍把 replay ledger 内嵌成 in-memory array
  - resumed early-data accept path 直接依赖 context 自己的 ledger 实现
  - 这会让“默认内存行为”和“后续持久化实现”耦合在一起
- 本批只做三件事：
  1. 先补 RED，固定 default in-memory ledger、replaceable seam、生存期/重放语义
  2. 把内存实现搬到独立 internal unit，并给 context 增加 get/set/reset seam
  3. 让 resumed accept path 走 active ledger，而不是假设只有 context 自己的内存数组
- 本批明确不做：
  - 分布式 anti-replay
  - public docs / API wording 扩写
  - capability level 上调
  - 更重的持久化实现原型

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，覆盖 default ledger、replaceable seam、resumed accept-path wiring。
3. 最小 GREEN：新增 internal in-memory ledger unit，扩 internal access seam，并让 context / connection 接线。
4. 跑 focused regressions、focused gate、compile gate、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: RED - Lock the replay-ledger seam contract

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.session.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Extend the direct replay-ledger test**
- 在 `TestReplayLedgerSessionValidity` 附近新增/收紧 contract：
  - server context 必须同时暴露：
    - `IFreePascalEarlyDataReplayLedger`
    - 新的 context access seam（get/set/reset）
  - default getter 返回的 ledger 非空
  - default in-memory ledger 继续满足：
    - valid session 第一次 acquire 成功
    - 同 session 第二次 acquire 失败
    - expired session acquire 失败
    - expired attempt 不污染 fresh valid session

**Step 2: Add a replaceable-ledger runtime contract**
- 在测试里定义一个最小 custom ledger（例如 rejecting ledger）：
  - 记录 acquire 调用次数
  - 记录收到的 session
  - 返回 `False`
- 用 server context + resumed early-data scripted client 覆盖：
  - policy 仍为 `sslEarlyDataServerAccept`
  - default path 本应 accept first-use early data
  - 注入 custom ledger 后，first-use resumed early-data 也必须被拒绝
  - 握手仍成功、session 仍 reused、early-data bytes 不可读
  - custom ledger 至少被调用一次，且收到 cached resumable session

**Step 3: Run RED**

```bash
mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data
```

**Expected:**
- FAIL，优先暴露新的 internal seam 还不存在（例如缺少 access interface / setter / getter），而不是相邻旧行为回归。

### Task 2: GREEN - Introduce the replaceable in-memory seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.session.pas`
- Create: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Extend internal replay-ledger interfaces**
- 在 `src/fafafa.ssl.freepascal.session.pas`：
  - 保持 `IFreePascalEarlyDataReplayLedger.TryAcquireEarlyDataSession(ASession: ISSLSession)` 不变
  - 新增 internal access seam，用于 context get/set/reset active ledger
  - 如有必要，再加一个 managed/configurable internal interface，供默认内存实现响应 enable/size 变化

**Step 2: Move the default in-memory behavior into a dedicated internal unit**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.pas`：
  - 新增默认 in-memory ledger 类
  - 复用当前真实语义：
    - ticket key + expires at
    - acquire 前 prune expired entry
    - `ASession.IsValid` / timeout / ticket lifetime 继续作为 expiry 真值
    - bounded eviction 继续跟随 context session-cache size

**Step 3: Rewire `TFreePascalContext`**
- 在 `src/fafafa.ssl.freepascal.context.pas`：
  - context 改为持有 default ledger + active ledger reference
  - constructor 默认装配 in-memory ledger
  - `TryAcquireEarlyDataSession(...)` 改为 delegate 到 active ledger
  - `SetSessionCacheMode(False)` / `SetSessionCacheSize(...)` 继续同步影响 default in-memory ledger；custom managed ledger 若支持配置接口，也同步接线
  - 暴露 get/set/reset seam，允许测试与后续持久化实现替换 active ledger

**Step 4: Rewire resumed accept path**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - resumed early-data accept 先从 context seam 解析 active ledger
  - 如 seam 不可用，再回退到已有 `IFreePascalEarlyDataReplayLedger` contract
  - accept 条件保持不变：
    - `max_early_data_size > 0`
    - `sslEarlyDataServerAccept`
    - replay ledger acquire success
  - reject 后握手继续成功，状态仍为 `sslEarlyDataRejected`

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify capability wording and adjacent contracts stay locked

**Files:**
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Keep wording stable**
- 本批不提前改 `KnownIssues`
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
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_replaceable_20260412
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-replaceable-persistent-seam.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md
```

**Expected:**
- focused gate => PASS
- compile gate => PASS
- diff hygiene => exit `0`

## Definition Of Done

- default in-memory replay ledger 继续通过 direct ledger + end-to-end replay tests
- context 暴露 active replay-ledger seam，允许替换实现
- resumed early-data accept path 真实走 active ledger，而不是硬绑 context 内存数组
- capability wording 继续保持 experimental / single-process / in-memory truth
- 本轮计划、findings、progress 都有 fresh RED/GREEN evidence
