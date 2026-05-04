# FreePascal Early-Data Default Durable Shipped Path Implementation Plan

**Goal:** 在不扩 public API 的前提下，把 FreePascal server-side `0-RTT / early data` 默认 shipped path 从单进程内存 replay ledger 收口到一个默认的本地持久化 replay-store 路径，并保住现有 `SetSessionCacheMode` / `SetSessionCacheSize` 的 default-ledger clear 语义。

**Architecture:** 继续复用已经稳定的 directory-backed replay-store durability 路线，不改 builder / `TSSLConfig` / `TSSLFactory` 的 public opt-in shape。默认 server context 改为装配一个 backend-private 的 managed persistent replay provider：底层真实存储使用 `TFreePascalDirectoryEarlyDataReplayStore`，provider 自己承担 clear / capacity 生命周期，使默认路径既能跨 context / 跨进程保留 replay truth，又能在 session-cache disable / zero-capacity 时清空默认 replay state。测试侧通过 backend-private default-path override seam 把默认目录重定向到 `tmp/...`，避免污染宿主状态目录。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalDirectoryEarlyDataReplayStore`, managed replay provider seam, focused runtime restart tests, capability truth update, file-based working memory.

---

## Summary

1. 先补 RED，锁住“无显式 replay-store 配置时，默认 server context 也具备跨 context / 跨进程 durable replay truth”的新真相。
2. 最小 GREEN 只改 FreePascal 默认 replay-ledger 装配与 capability wording，不重开 explicit file/directory opt-in 行为线。
3. `experimental` capability level 先保持不变；这批只消掉“默认 shipped path 仍是 in-memory single-process ledger”这个剩余 caveat。

## Task 1: RED - Lock Default Durable Replay Truth

**Files:**
- Modify: `tests/test_freepascal_tls13_early_data.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Add backend-private default-path override helper**
- 测试需要能把默认 replay-store 目录重定向到 `tmp/...`
- child self-exec runtime replay probe 也要能复用同一 override

**Step 2: Add same-process default durable contracts**
- 新增 focused contract，覆盖：
  - 默认 server context 不做 explicit replay-store config
  - 第一个 context 首次 acquire 成功
  - 第二个 context 对同一 session replay reject
  - 默认 replay-store canonical directory 被 materialize

**Step 3: Add restart durability contract**
- 新增 focused runtime contract，覆盖：
  - 默认 server context 首次 resumed early-data accept
  - child process 在同一默认 replay-store 路径上 reject replay
  - fresh resumed session 仍可 accept

**Step 4: Tighten capability wording assertions**
- capability test 明确不再接受：
  - `in-memory single-process anti-replay ledger`
- capability test 继续要求：
  - `0-RTT` / `ANTI-REPLAY` 仍在 `KnownIssues`
  - support level 继续是 `sslSupportExperimental`

## Task 2: GREEN - Switch Default Ledger To A Managed Persistent Path

**Files:**
- Modify: `src/fafafa.ssl.freepascal.earlydatareplay.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Add default replay-store path resolver**
- 新增 backend-private helper：
  - production path 解析默认本地持久化目录
  - tests 可用 override seam 重定向到 `tmp/...`

**Step 2: Add a managed persistent default provider**
- 默认 provider 继续走 store-backed replay truth
- 但要自己承担：
  - clear
  - capacity truncate
  - disable / zero-capacity clear parity

**Step 3: Repoint default server ledger**
- `TFreePascalContext.Create(...)` 的默认 server replay-ledger 改走新的 durable default path
- client path 不扩大行为承诺

**Step 4: Update capability truth**
- `KnownIssues` 改成：
  - 仍是 experimental
  - 默认 replay protection 已是 local persistent path
  - path 不可写/不可用时 fail closed

## Task 3: Verify And Close Out

**Files:**
- Modify: `docs/ROADMAP.md`
- Modify: `docs/BACKEND_CAPABILITY_MATRIX.md`
- Modify: `README.md`
- Modify: `docs/reference/API_REFERENCE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused verification**
- Run:
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `mkdir -p tmp/capability_cache_units && fpc -B -Fu./src -Fu./tests -FUtmp/capability_cache_units -FEtmp/capability_cache_units -otmp/capability_cache_units/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_units/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_default_durable_shipped_path_20260504`
  - `python3 scripts/compile_all_modules.py`

**Step 2: Run diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-05-04-freepascal-early-data-default-durable-shipped-path.md src/fafafa.ssl.freepascal.earlydatareplay.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.lib.pas tests/test_freepascal_tls13_early_data.pas tests/test_capability_cache.pas docs/ROADMAP.md docs/BACKEND_CAPABILITY_MATRIX.md README.md docs/reference/API_REFERENCE.md task_plan.md findings.md progress.md`

### Definition Of Done

- 默认 FreePascal server context 在无 explicit replay-store config 时也使用 durable replay truth
- 默认路径跨 context / 跨进程 replay reject 有 focused evidence
- `SetSessionCacheMode(False/True)` 与 `SetSessionCacheSize(0/restore)` 对默认 ledger 的 clear parity 继续成立
- capability wording 不再宣称默认路径仍是单进程内存 ledger
- focused tests、focused gate、compile gate、diff hygiene 都有 fresh evidence
