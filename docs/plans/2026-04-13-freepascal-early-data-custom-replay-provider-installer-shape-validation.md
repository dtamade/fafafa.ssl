# FreePascal Early-Data Custom Replay Provider Installer Shape Validation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不改变 public API、builder/factory/config surface 与 capability wording 的前提下，为 FreePascal TLS 1.3 early-data anti-replay 增加 backend-private custom replay-provider installer seam，并用 callback/provider shape 验证现有边界足够稳定。

**Architecture:** 继续复用现有 `IFreePascalEarlyDataReplayProvider`、`TFreePascalProviderBackedEarlyDataReplayLedger`、`IFreePascalEarlyDataReplayLedgerAccess` 与 resumed early-data accept path，不新增 public abstraction。最小修法是在 backend-private context material seam 上新增“安装任意 replay provider”的 installer 接口，再在 `fafafa.ssl.freepascal.earlydatareplay` 提供薄 helper / callback helper；现有 file-backed installer 改为这个通用 seam 的特化调用。这样下一步若要验证更重的持久化/provider 形态，不需要再重做 context wiring。

**Tech Stack:** FreePascal (ObjFPC), internal replay provider seam, callback-backed provider helper, TLS 1.3 early-data focused runtime tests, completeness gate, file-based working memory.

---

## Summary

- 当前已稳定收口的真值：
  - replaceable replay-ledger seam
  - provider-backed ledger prototype
  - file-backed provider prototype + lifecycle seam
  - builder/config/factory parity
  - orphan `.tmp` recovery + Unix/Linux cross-process advisory lock hardening
- 当前最高 ROI 的下一步不是重开 public surface，也不是立刻做 distributed persistence，而是：
  - 证明当前 seam 不只适用于 file-backed provider
  - 把“安装任意 replay provider”提升为 backend-private context seam
  - 用 callback/provider shape 锁住 lifecycle、cross-context replay 与 fail-closed 语义
- 本批明确不做：
  - capability wording 升级
  - distributed / cross-host replay coordination
  - builder / factory / config 新字段
  - 文档外扩

## Delivery Order

1. 写本轮 plan 与 working-memory 入口。
2. 先在 `tests/test_freepascal_tls13_early_data.pas` 加 RED，锁住 custom provider installer / helper 的 lifecycle、cache-sync、cross-context replay 合同。
3. 最小 GREEN：只改 `src/fafafa.ssl.freepascal.context.material.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.earlydatareplay.pas`。
4. 跑 focused regression、focused gate、compile gate、diff hygiene。
5. 回填 `task_plan.md` / `findings.md` / `progress.md`。

### Task 1: RED - Lock custom provider installer contracts

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.session.pas`
- Reference: `src/fafafa.ssl.freepascal.context.material.pas`
- Reference: `src/fafafa.ssl.freepascal.context.pas`
- Reference: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add backend-private installer interface coverage**
- 为 FreePascal server context 增加 fresh contract：
  - 必须继续支持 `IFreePascalEarlyDataReplayLedgerAccess`
  - 必须新增支持 generic custom provider installer seam
  - 现有 file-backed installer seam 仍继续存在

**Step 2: Add custom provider lifecycle contract**
- 新增 focused direct-ledger / context contract，覆盖：
  - install provider A 后，fresh valid session 第一次 acquire 成功
  - reinstall provider B 后，同一 session 再次 acquire 仍成功
  - reset 后，同一 session 在默认内存 ledger 上再次 acquire 成功
  - reinstall 回 provider A 后，同一 session acquire 失败

**Step 3: Add session-cache sync contract**
- 新增 focused contract，覆盖：
  - session cache disabled 时，installed provider-backed ledger 不得 acquire
  - re-enable 后，same session 可 acquire
  - cache size = 0 时，fresh session 不得 acquire
  - 恢复 capacity 后，fresh session 再次 acquire 成功

**Step 4: Add cross-context runtime contract**
- 新增 focused runtime contract，覆盖：
  - `ctx1` / `ctx2` 分别安装两个 custom callback providers，但共享同一底层 store object
  - `ctx1` 上 first resumed early-data 继续 accept
  - `ctx2` 上 second resumed early-data 必须 reject
  - reject 后握手仍成功、session 仍 reused、early bytes 不可读

**Step 5: Add helper wrapper contract**
- 如果新增 free helper，则锁住：
  - helper 能把 callback/provider-backed ledger 安装进 context
  - helper 对 `nil` context / `nil` provider / missing seam 保持 fail closed

**Step 6: Run RED**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - FAIL，优先失败在“当前 context 还不支持安装任意 replay provider / callback helper”

### Task 2: GREEN - Add generic custom replay-provider installer seam

**Files:**
- Modify: `src/fafafa.ssl.freepascal.context.material.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`

**Step 1: Add backend-private generic installer interface**
- 在 `src/fafafa.ssl.freepascal.context.material.pas`：
  - 新增 generic custom provider installer seam
  - contract 至少表达：
    - `InstallReplayProviderBackedLedger(AProvider: IFreePascalEarlyDataReplayProvider): Boolean`
- 不改 public `ISSLContext`

**Step 2: Implement the seam on `TFreePascalContext`**
- 在 `src/fafafa.ssl.freepascal.context.pas`：
  - 让 `TFreePascalContext` 实现新的 generic installer interface
  - method 内部：
    - reject `nil` provider
    - create `TFreePascalProviderBackedEarlyDataReplayLedger`
    - 复用现有 `SetEarlyDataReplayLedger(...)`
  - 现有 `InstallFileBackedReplayLedger(...)` 改为：
    - create file-backed provider
    - 委托新的 generic installer seam

**Step 3: Add thin helper(s) in `earlydatareplay`**
- 在 `src/fafafa.ssl.freepascal.earlydatareplay.pas`：
  - 新增 generic free helper，负责把 provider-backed ledger 装进 context
  - 如范围允许，再补 callback helper，直接从 callback 创建 provider 并安装
  - helper 对 nil / missing seam 保持 fail closed

**Step 4: Keep adjacent truth stable**
- 不修改：
  - resumed early-data accept 条件
  - file format / file-backed provider behavior
  - builder / factory / config surface
  - capability wording

**Step 5: Run GREEN**
- Re-run Task 1 command
- Expected:
  - PASS

### Task 3: Verify adjacent truth stays locked

**Files:**
- Reference: `tests/test_freepascal_tls13_early_data.pas`
- Reference: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Run focused verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_custom_replay_provider_installer_20260413`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-13-freepascal-early-data-custom-replay-provider-installer-shape-validation.md src/fafafa.ssl.freepascal.context.material.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.earlydatareplay.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`

### Definition Of Done

- FreePascal context 支持 backend-private generic custom replay-provider installer seam
- callback/provider shape 可通过该 seam 安装到 active replay ledger
- lifecycle / session-cache sync / cross-context replay 合同都有 fresh coverage
- existing file-backed installer 继续可用
- focused test、focused gate、compile gate、diff hygiene 都有 fresh evidence
- public API、builder/factory/config surface 与 capability wording 都不变
