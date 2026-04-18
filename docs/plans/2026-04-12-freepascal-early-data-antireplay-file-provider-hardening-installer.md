# FreePascal Early-Data Anti-Replay File Provider Hardening And Installer Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 FreePascal TLS 1.3 0-RTT / early-data 的 file-backed anti-replay provider prototype 补齐失败语义 hardening，并增加 backend-private 安装 helper，让真实 context 装配路径无需手工拼 provider + ledger，同时保持 public API、builder 和 capability wording 不变。

**Architecture:** 继续复用现有 `IFreePascalEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`IFreePascalEarlyDataReplayLedgerAccess` 与 resumed early-data accept path。实现保持收敛在内部：一方面用 focused tests 锁住损坏/版本漂移 store 的 fail-closed 语义；另一方面在 file-provider 单元内补一个最小 installer/helper，直接把 file-backed provider-backed ledger 装到 context active-ledger seam，上层不需要手工拼接对象图。

**Tech Stack:** FreePascal (ObjFPC), `TFileStream`, `TRTLCriticalSection`, TLS 1.3 early-data runtime contracts, backend-private optional interfaces, focused completeness gate, file-based working memory.

---

## Summary

- 已完成的前置批次已经提供了：
  - replaceable replay-ledger seam
  - provider-backed ledger prototype
  - file-backed provider prototype
- 当前最高 ROI 的下一步不是继续扩 public surface，而是把 prototype 再收紧两层：
  1. 用 fresh RED 锁住 file-store corruption / version drift / prune 语义
  2. 增加 backend-private 安装 helper，避免真实接线还要在调用点手工 new provider + ledger
- 本批明确不做：
  - public API / builder 扩面
  - distributed / multi-process strong consistency
  - `KnownIssues` wording 升级
  - 文档对外宣称 persistent anti-replay 已默认 shipped

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，覆盖 fail-closed store semantics 与 helper-based install contract。
3. 最小 GREEN：在 file-provider 单元增加 hardening + backend-private installer/helper。
4. 跑 focused regressions、focused gate、compile gate、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: RED - Lock failure semantics and helper-based installation

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add raw store-file fixtures**
- 在测试里增加最小 helper：
  - 直接写 binary store file
  - 支持指定 version / count / key / expiresAt
  - 可构造 truncated / invalid-version / invalid-count fixture

**Step 2: Add direct fail-closed contracts**
- 新增 focused contracts：
  - invalid version store => first acquire returns `False`
  - truncated store => first acquire returns `False`
  - oversize count / key length store => first acquire returns `False`
  - expired persisted entry should be pruned, then fresh acquire for the same key succeeds after provider rebuild

**Step 3: Add helper-based runtime install contract**
- 新增 focused runtime contract：
  - `ctx1` / `ctx2` 通过一个新的 backend-private helper 安装指向同一路径的 file-backed replay ledger
  - `ctx1` 的 first-use resumed early-data 仍 accepted
  - `ctx2` 的 second resumed early-data 仍 rejected
  - reject 后 handshake 继续成功、session reused、early-data bytes 不可读

**Step 4: Run RED**

```bash
mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data
```

**Expected:**
- FAIL，优先暴露新的 helper / installer symbol 尚不存在，或新 contract 在当前 prototype 上不成立。

### Task 2: GREEN - Harden the file provider and add the installer/helper

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Keep store corruption fail-closed**
- 在 file-provider 单元里继续收紧：
  - invalid version => reject
  - invalid count / key length => reject
  - truncated read => reject
  - optional trailing garbage => reject
- 不允许在存储异常时 silent accept

**Step 2: Keep prune semantics stable**
- acquire 时继续：
  - load
  - prune expired persisted entries
  - reject replay on live entries
  - append fresh entry
  - write back with temp file replace

**Step 3: Add a backend-private installer/helper**
- 在 file-provider 单元里新增最小 helper：
  - create provider-backed ledger from `AFileName`
  - install it into `IFreePascalEarlyDataReplayLedgerAccess`
- helper 要求：
  - 不新增 public interface
  - 不改 builder
  - 复用 context 现有 `SetEarlyDataReplayLedger(...)`
  - 让 managed ledger capacity / enabled sync 继续由 context 负责

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
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_file_provider_hardening_installer_20260412
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-file-provider-hardening-installer.md src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md
```

**Expected:**
- focused gate => PASS
- compile gate => PASS
- diff hygiene => exit `0`

## Definition Of Done

- file-backed provider 对损坏 / 版本漂移 /截断 store 保持 fail-closed
- persisted expired entries 在 provider rebuild 后仍会被 prune，不污染 fresh acquire
- 已有 seam 上存在最小 backend-private installer/helper，可直接给 context 装 file-backed replay ledger
- helper-based runtime contract 证明 cross-context replay rejection 继续成立
- default shipped path 与 `KnownIssues` wording 都未退化
- 本轮 plan / findings / progress 都有 fresh RED / GREEN / verification evidence
