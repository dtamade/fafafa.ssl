# FreePascal Early-Data Anti-Replay File Installer Lifecycle Seam Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 为 FreePascal TLS 1.3 0-RTT / early-data 的 file-backed anti-replay path 增加一个更明确的 backend-private context installer seam，并用 fresh RED 锁住 install / reinstall / reset / session-cache-sync 生命周期，而不扩 public API、builder 或 capability wording。

**Architecture:** 继续保持现有 `IFreePascalEarlyDataReplayLedgerAccess`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`TFreePascalFileEarlyDataReplayProvider` 与 resumed early-data accept path 不变。这个批次只把“free helper 装配”收口成“context 自己暴露 backend-private from-file installer seam”：在 `src/fafafa.ssl.freepascal.context.material.pas` 新增一个仅供 FreePascal backend 内部使用的 installer interface，由 `TFreePascalContext` 实现；现有 `InstallFileBackedReplayLedger(...)` helper 退化成薄封装，优先调用新的 context seam。这样后续如果要接 builder/config，只需要在稳定 seam 上再薄接一层，不需要重做 ledger/provider 接线。

**Tech Stack:** FreePascal (ObjFPC), pure Pascal TLS 1.3 session/resumption/early-data units, backend-private optional interfaces, file-backed replay provider prototype, focused completeness gate, file-based working memory.

---

## Summary

- 当前 highest-ROI 下一步不是 public builder opt-in，而是先把 internal installer lifecycle 锁稳：
  - install 到指定 store file
  - reinstall 到新 store file
  - reset 回默认 in-memory ledger
  - session-cache enabled / size 变更继续同步到 helper-installed managed ledger
- 现有 free helper 已经证明装配路径可行，但它的 lifecycle contract 仍然隐式存在于测试和 `SetEarlyDataReplayLedger(...)` 行为里；这批把它变成显式 backend-private seam，减少后续 builder/config 接入的返工。
- scope 继续收敛：
  - 不改 public `ISSLContext` / `ISSLEarlyDataContext`
  - 不改 builder / import-export surface
  - 不改 `KnownIssues` wording
  - 不追 distributed / multi-process 强一致

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，锁住 context installer seam 和 lifecycle contracts。
3. 最小 GREEN：只改 `src/fafafa.ssl.freepascal.context.material.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`。
4. 跑 focused regressions、focused gate、compile gate、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: RED - Lock installer lifecycle contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.context.material.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`

**Step 1: Add backend-private installer interface coverage**
- 在测试 `uses` 中引入 `fafafa.ssl.freepascal.context.material`。
- 为 server context 增加 fresh contract：
  - 必须支持新的 backend-private installer interface
  - 继续支持 `IFreePascalEarlyDataReplayLedgerAccess`
  - 继续支持 `ISSLEarlyDataContext`

**Step 2: Add reinstall / reset / file-isolation contract**
- 新增 focused direct-ledger test，覆盖：
  - install 到 `file_a` 后，valid session 第一次 acquire 成功
  - reinstall 到 `file_b` 后，同一个 session 再次 acquire 仍成功（证明不同 store file 隔离）
  - `ResetEarlyDataReplayLedger` 后，同一个 session 在默认内存 ledger 上再次 acquire 仍成功（证明 reset 回默认实现）
  - 再 reinstall 回 `file_a` 后，同一个 session acquire 失败（证明原 file-backed replay truth 仍保留）

**Step 3: Add session-cache sync contract**
- 新增 focused direct-ledger test，覆盖：
  - context 在 `SetSessionCacheMode(False)` 后安装 file-backed ledger，first acquire 必须失败
  - 重新 `SetSessionCacheMode(True)` 后，同一 valid session acquire 必须成功
  - `SetSessionCacheSize(0)` 后，fresh valid session acquire 必须失败
  - `SetSessionCacheSize(8)` 后，fresh valid session acquire 必须成功

**Step 4: Repoint runtime cross-context install test**
- 把现有 runtime helper-based cross-context replay test 改为走新的 backend-private installer seam：
  - `ctx1` / `ctx2` 安装同一 replay store file
  - 第一次 resumed early-data 仍 accepted
  - 第二次跨 context resumed early-data 仍 rejected
  - reject 后 handshake 继续成功、session reused、early bytes 不可读

**Step 5: Run RED**

```bash
mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data
```

**Expected:**
- FAIL，优先暴露新的 backend-private installer interface / method 尚不存在，或 lifecycle contract 当前未显式成立。

### Task 2: GREEN - Add the explicit backend-private installer seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Add a backend-private installer interface**
- 在 `src/fafafa.ssl.freepascal.context.material.pas`：
  - 新增只供 FreePascal backend 内部使用的 interface
  - contract 至少表达：
    - `InstallFileBackedReplayLedger(const AFileName: string): Boolean`
- 不扩 public `ISSLContext`

**Step 2: Implement the seam on `TFreePascalContext`**
- 在 `src/fafafa.ssl.freepascal.context.pas`：
  - 让 `TFreePascalContext` 实现新的 backend-private installer interface
  - method 内部：
    - validate file name
    - create `TFreePascalFileEarlyDataReplayProvider`
    - create `TFreePascalProviderBackedEarlyDataReplayLedger`
    - 调用现有 `SetEarlyDataReplayLedger(...)`
  - 保持 managed ledger enabled / capacity sync 继续通过 context 现有逻辑生效

**Step 3: Thin the free helper into a wrapper**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas`：
  - 让现有 `InstallFileBackedReplayLedger(...)` helper 优先委托新的 backend-private installer seam
  - 继续保持 helper 可用，避免扩大改动面

**Step 4: Keep scope tight**
- 不修改：
  - public builder surface
  - config import/export fields
  - `KnownIssues` wording
  - default in-memory shipped path

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS
  - 输出 `✅ FreePascal TLS 1.3 early-data checks passed`

### Task 3: Verify adjacent truth stays locked

**Files:**
- Reference: `tests/test_freepascal_backend_basic.pas`
- Reference: `tests/test_capability_cache.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Keep capability wording stable**
- 重新确认：
  - `ZeroRTTSupport = sslSupportExperimental`
  - `EarlyDataSupport = sslSupportExperimental`
  - `KnownIssues = 0-RTT / early data is experimental and currently uses an in-memory single-process anti-replay ledger.`

**Step 2: Run focused adjacent regressions**

```bash
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache
```

**Expected:**
- PASS

### Task 4: Focused gate, compile gate, and diff hygiene

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run completeness gate**

```bash
bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_antireplay_file_installer_lifecycle_seam_20260412
```

**Expected:**
- PASS
- 末尾包含 `[PASS] freepascal tls13 completeness gate finished`

**Step 2: Run compile gate**

```bash
python3 scripts/compile_all_modules.py
```

**Expected:**
- PASS

**Step 3: Run path-limited diff hygiene**

```bash
git diff --check -- docs/plans/2026-04-12-freepascal-early-data-antireplay-file-installer-lifecycle-seam.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.earlydatareplay.fileprovider.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md
```

**Expected:**
- PASS

## Done Criteria

- `TFreePascalContext` 通过一个明确的 backend-private seam 暴露 file-backed replay ledger installer
- install / reinstall / reset / file-isolation lifecycle 都有 fresh tests 锁住
- helper-installed file-backed ledger 对 `SetSessionCacheMode(...)` / `SetSessionCacheSize(...)` 的同步语义不退化
- cross-context resumed early-data replay rejection 继续成立
- public API、builder、config import/export、capability wording 全部保持不变
- 本轮 plan / findings / progress 都记录 fresh RED / GREEN / verification evidence
