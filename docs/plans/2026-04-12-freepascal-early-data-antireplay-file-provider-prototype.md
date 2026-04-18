# FreePascal Early-Data Anti-Replay File Provider Prototype Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 FreePascal TLS 1.3 0-RTT / early-data 增加最小本地文件型 anti-replay provider prototype，证明 replay truth 在 provider / ledger / context 重建后仍可保持，并继续保持 public surface 与 capability wording 不变。

**Architecture:** 继续复用现有 `IFreePascalEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger` 与 context active-ledger seam，不改 `ISSLEarlyDataContext`、builder 或 capability wording。新增一个内部 file-backed provider 单元，provider 只负责 `key + expiresAt + now` 的持久化 acquire：每次 acquire 读取文件、剪掉过期 entry、检查 replay、追加新 entry，并用进程内锁 + temp-file replace 做最小稳定写回。

**Tech Stack:** FreePascal (ObjFPC), `TFileStream`, `TRTLCriticalSection`, TLS 1.3 early-data runtime contracts, focused completeness gate, file-based working memory.

---

## Summary

- 上一批已经完成：
  - replaceable replay-ledger seam
  - provider-backed ledger prototype
  - cross-context replay coordination via shared provider object
- 这批最高 ROI 的下一步是：
  - 增加最小 file-backed provider prototype
  - 证明 replay truth 可跨 provider / ledger / context 重建持续存在
- 本批只做三件事：
  1. 先补 RED，锁定 file-backed persistence contract
  2. 新增内部 file-backed provider
  3. 跑 focused tests / gate / compile / diff hygiene 并回填 ledgers
- 本批明确不做：
  - public API / builder 扩面
  - `KnownIssues` wording 升级
  - 分布式或跨进程强一致锁
  - 文档对外宣称 persistent anti-replay 已默认可用

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，覆盖 file-backed persistence / cross-context replay contract。
3. 最小 GREEN：新增内部 file-backed provider 单元。
4. 跑 focused regressions、focused gate、compile gate、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: RED - Lock the file-backed provider contract

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.session.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add a temp-path helper**
- 在测试里增加一个最小 helper：
  - 生成 `tmp/freepascal_tls13_early_data_replay_provider/...` 下的独立文件路径
  - 测试前清理旧文件
  - 测试后删除生成文件

**Step 2: Add a direct file-provider persistence contract**
- 新增 focused contract：
  - provider1 + ledger1 用 fresh valid session 第一次 acquire 成功
  - provider2 + ledger2 指向同一路径时，对同一 session acquire 失败
  - expired session acquire 失败，且不会污染后续 fresh valid session
- provider1 / provider2 必须是不同实例，证明 persistence 不依赖单对象内存状态

**Step 3: Add a cross-context runtime persistence contract**
- 新增 focused runtime contract：
  - `ctx1` / `ctx2` 分别注入指向同一路径的两个 file-backed provider-backed ledger
  - 在 `ctx1` 完成初始握手并拿到 resumable session
  - 把 session 放入 `ctx2` 的 resumption cache
  - `ctx1` 上 first-use resumed early-data 成功 accept
  - `ctx2` 上 second resumed early-data 必须被 reject
  - reject 后握手仍成功、session 仍 reused、early-data bytes 不可读

**Step 4: Run RED**

```bash
mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data
```

**Expected:**
- FAIL，优先失败在新的 file-backed provider symbols / unit 尚不存在。

### Task 2: GREEN - Add the minimal file-backed provider

**Files:**
- Create: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a narrow file-backed provider class**
- 在新单元里新增：
  - `TFreePascalFileReplayProviderEntry`
  - `TFreePascalFileEarlyDataReplayProvider`
- provider contract 继续只实现：
  - `TryAcquireReplayKey(const AKey: string; AExpiresAt, ANow: TDateTime): Boolean`

**Step 2: Implement minimal local persistence**
- provider 每次 acquire：
  - 进程内进入全局临界区
  - 从文件加载 entry 列表；文件不存在视为空 store
  - prune 已过期 entry
  - 如发现同 key 且未过期 => `False`
  - 否则追加新 entry
  - 用 temp file 写回并 replace 原文件
- 存储格式保持最小：
  - binary version
  - entry count
  - repeated `(key length + key bytes + expiresAt)`

**Step 3: Fail closed on storage errors**
- 若读写文件失败、版本不匹配、数据损坏：
  - provider 返回 `False`
  - 不接受 early data
- 这样不会让 anti-replay 在存储异常时静默退化

**Step 4: Keep scope tight**
- 不修改：
  - `IFreePascalEarlyDataReplayProvider`
  - `TFreePascalProviderBackedEarlyDataReplayLedger`
  - context / builder public surface
- 只在测试里直接实例化 file-backed provider，证明 seam 已足够

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify adjacent truth stays locked

**Files:**
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Keep capability wording stable**
- 本批不改：
  - `ZeroRTTSupport = sslSupportExperimental`
  - `EarlyDataSupport = sslSupportExperimental`
  - `KnownIssues = 0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

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
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_file_provider_proto_20260412
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-file-provider-prototype.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md
```

**Expected:**
- focused gate => PASS
- compile gate => PASS
- diff hygiene => exit `0`

## Definition Of Done

- 本地 file-backed anti-replay provider prototype 已存在
- replay truth 可跨 provider / ledger / context 重建持续存在
- 存储异常不会把 anti-replay 静默降级成 accept
- default in-memory path 与 current capability wording 均未退化
- 本轮 plan / findings / progress 都有 fresh RED / GREEN / verification evidence
