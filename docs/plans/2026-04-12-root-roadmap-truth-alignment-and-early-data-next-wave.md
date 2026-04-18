# Root Roadmap Truth Alignment And Early-Data Next Wave Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 收口 root-level roadmap 的陈旧表述，让仓库顶层入口重新指向当前真实状态，并把下一条最高 ROI 的 FreePascal early-data 工作收敛成明确的下一波队列。
**Architecture:** 本批不重开 OCSP / CT / validation 或 early-data runtime 实现。实现只做三件事：先用现有 root docs、CI workflow 和 FreePascal capability truth 重新基线；再把 `docs/ROADMAP.md` 改成当前可信的状态页；最后把本轮结论回写到 `task_plan.md`、`findings.md`、`progress.md`，避免后续继续按旧 roadmap 误排优先级。
**Tech Stack:** Markdown docs, GitHub Actions workflow inventory, FreePascal capability truth, file-based working memory.

---

## Summary

- `docs/ROADMAP.md` 仍保留了三条已经过时的“下一步主线”：
  - “把文档入口、架构/API 文档和默认 gate 收敛到同一套真相源”
  - “把 FreePascal TLS 1.3 focused tests 提升到更显眼的 CI 层”
  - “再推进 `OCSP stapling / Certificate Transparency / validation hardening`”
- 当前 fresh evidence 已经表明：
  - root docs / docs index / GitHub Actions docs 已基本收口到当前入口
  - `.github/workflows/ci.yml` 已包含 `minimal-gate-linux` 与 `freepascal-tls13-completeness`
  - FreePascal capability `KnownIssues` 已只剩 experimental 0-RTT / single-process anti-replay ledger
- 因此本批的最高 ROI 不是重开已完成行为线，而是：
  - 修正 root roadmap
  - 显式冻结已 closeout 的 OCSP / CT / validation 主线
  - 把 early-data 的真实下一步写成单独、可防扩散的 future queue：replaceable / persistent anti-replay coordination seam

## Delivery Order

1. 复核 root docs、CI workflow、FreePascal capability truth，确认 stale roadmap 断点。
2. 改写 `docs/ROADMAP.md`，把“已完成到这里 / 当前队列 / 下一条实现线”写成当前真相。
3. 回写 `task_plan.md`、`findings.md`、`progress.md`。
4. 跑本轮轻量验证，确认新 roadmap 不再重复陈旧 backlog。

### Task 1: Reconfirm Current Truth Sources

**Files:**
- Read: `docs/ROADMAP.md`
- Read: `README.md`
- Read: `docs/README.md`
- Read: `docs/DOCUMENTATION_INDEX.md`
- Read: `.github/README.md`
- Read: `.github/GITHUB_ACTIONS_GUIDE.md`
- Read: `.github/workflows/ci.yml`
- Read: `src/fafafa.ssl.freepascal.lib.pas`
- Read: `tests/test_freepascal_backend_basic.pas`
- Read: `tests/test_capability_cache.pas`

**Step 1: Capture stale roadmap claims**
- 记录 roadmap 中仍把下面几条写成 future queue：
  - docs / gate truth alignment
  - focused gate CI promotion
  - OCSP / CT / validation hardening

**Step 2: Capture current evidence**
- 用以下事实作为当前 truth source：
  - `.github/workflows/ci.yml` 已自动跑 focused gate
  - `.github/README.md` / `.github/GITHUB_ACTIONS_GUIDE.md` 已写明 focused gate 在 CI 内
  - `src/fafafa.ssl.freepascal.lib.pas` 与 capability tests 已把剩余边界收窄到 experimental 0-RTT / in-memory single-process anti-replay ledger

### Task 2: Rewrite The Root Roadmap

**Files:**
- Modify: `docs/ROADMAP.md`

**Step 1: Refresh the status block**
- 更新为 2026-04-12 口径：
  - 当前默认 build / minimal gate / focused gate
  - focused gate 已在 `ci.yml` 中自动执行
  - FreePascal 剩余 capability caveat 仅剩 experimental 0-RTT / in-memory single-process anti-replay ledger

**Step 2: Replace stale “next steps” with real queue**
- `已完成到这里` 至少覆盖：
  - TLS 1.3 modern cipher-suite parity
  - client/server session resumption / PSK
  - early-data public transport / policy / anti-replay hardening / config parity / ergonomics
  - OCSP / CT / validation closeout
  - FreePascal server-side OCSP stapling public closeout
- `当前这一批` 写成：
  - root roadmap truth alignment
  - freeze already-closed OCSP / CT / validation lines unless fresh RED
  - convert early-data remaining gap into a bounded next-wave queue
- `下一条最值得开的实现线` 写成：
  - replaceable / persistent anti-replay coordination seam
  - 不夸大成 distributed-ready，也不提前提升 capability level

### Task 3: Write Back Working Memory

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Add a new 2026-04-12 batch entry**
- `task_plan.md`：记录这批只做 roadmap truth alignment + next-wave queue clarification
- `findings.md`：记录为什么本轮不应重开 OCSP / CT / validation / focused CI promotion
- `progress.md`：记录本轮读取的 truth sources、编辑范围和验证命令

**Step 2: Freeze the next queue explicitly**
- 在三件套里都明确：
  - 已 closeout 线只有 fresh failing evidence 才允许重开
  - 当前最可信下一步是 early-data anti-replay replaceable/persistent seam

### Task 4: Verification

**Commands:**
```bash
rg -n '提升到更显眼的 CI 层|再推进 `OCSP stapling / Certificate Transparency / validation hardening`' docs/ROADMAP.md
```

```bash
rg -n "freepascal-tls13-completeness|single-process anti-replay ledger|replaceable / persistent anti-replay" docs/ROADMAP.md docs/plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md task_plan.md findings.md progress.md
```

```bash
rg -n "[[:blank:]]+$" docs/ROADMAP.md docs/plans/2026-04-12-root-roadmap-truth-alignment-and-early-data-next-wave.md
```

```bash
sed -n '1,40p' task_plan.md findings.md progress.md | rg -n "[[:blank:]]+$"
```

**Expected:**
- 第一个命令无命中（exit 1）
- 第二个命令命中新 truth / next-wave queue
- 第三个命令无命中（exit 1）
- 第四个命令无命中（exit 1）

## Definition Of Done

- `docs/ROADMAP.md` 不再把 focused gate CI promotion、OCSP / CT / validation hardening 写成当前 future queue
- roadmap 明确写出当前 focused gate 已进入 `ci.yml`
- roadmap 和 working-memory 都把 next queue 收敛到 early-data anti-replay replaceable/persistent seam
- 本轮证据明确说明：未出现 fresh RED，因此不重开已 closeout 的 runtime 行为线
