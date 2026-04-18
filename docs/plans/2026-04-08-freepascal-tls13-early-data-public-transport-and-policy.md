# FreePascal TLS 1.3 Early-Data Public Transport And Policy Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Status:** Implemented on 2026-04-08. This file records the executed batch plan; the focused gate promotion described below has already landed.

**Goal:** 在不新增公开 API 的前提下，把 pure Pascal backend 的 TLS 1.3 `0-RTT / early data` 从“只有 protocol primitives”推进到真实可验证的 transport / policy contract：builder/context 可声明 early-data 策略，resumption ticket 驱动的 client early-data transport 能在 offline scripted 流里工作，server accept/reject policy 可观察，同时 focused gate 把这条 family 纳入默认完整度表面。

**Architecture:** 继续沿用纯 Pascal TLS 1.3 resumption/PSK 路线，不扩新的 `ISSLConnection` 方法，也不在本批引入 anti-replay 系统。实现收口在现有接口面上：
- `ISSLContextBuilder` / `ISSLEarlyDataContext` 继续作为 early-data policy truth source
- resumable session 里的 `max_early_data_size` 作为 0-RTT eligibility gate
- `TFreePascalConnection` 负责 stream-backed handshake、early application-data staging、post-handshake `NewSessionTicket` drain、以及 accept-path 上的 early-data buffer 保留
- focused completeness gate 必须把 `tests/test_freepascal_tls13_early_data.pas` 纳入，而不是继续停留在局部回归

**Tech Stack:** FreePascal (ObjFPC), pure Pascal TLS 1.3 handshake/appschedule/keyschedule units, offline scripted stream fixtures, shell contract tests, file-based working memory.

---

## Summary

- 2026-03-27 那批已经落地了 early-data protocol primitives：
  - `ClientHello early_data`
  - `NewSessionTicket.max_early_data_size`
  - `EndOfEarlyData`
  - session metadata persistence
- 这一批不是重复补 primitives，而是把真正剩余的“transport / policy”缺口收口到最小可交付面：
  - builder/context 必须能公开 early-data client/server policy
  - resumed client 必须能在 offline scripted path 上发送 early application-data
  - server accept/reject policy 必须决定 early-data 是否被应用层读取
  - stream-backed `Connect` 必须能立即消费 post-handshake `NewSessionTicket`
  - server accept path 不能在握手收尾时丢失已接收的 early-data fragment
- focused gate promotion in this batch:
  - `tests/test_freepascal_tls13_early_data.pas` 已进入 `run_freepascal_tls13_completeness_gate.sh`
  - completeness contract 已从 6 个 test groups 升到 7 个

## Delivery Order

1. 把这批 plan 与 working-memory 入口写清楚。
2. 先用 focused RED 固化 builder/policy、keyschedule、transport/accept/reject 缺口。
3. 再做最小连接层修复，不扩公共接口。
4. 把 `test_freepascal_tls13_early_data.pas` 提升进 focused completeness gate。
5. 跑 fresh verification，并回填 ledgers。

### Task 1: Add Focused RED Contracts

**Files:**
- Modify: `tests/config/test_context_builder_early_data_contract.pas`
- Modify: `tests/test_tls13_keyschedule.pas`
- Add / Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Builder / policy contract**
- 在 `tests/config/test_context_builder_early_data_contract.pas`：
  - 断言 FreePascal context 暴露 `ISSLEarlyDataContext`
  - 断言 client early-data 默认关闭
  - 断言 server early-data policy 默认是 `sslEarlyDataServerReject`
  - 断言 fluent builder 能观察到 `WithClientEarlyData(...)` 与 `WithServerEarlyDataPolicy(...)`

**Step 2: Key schedule contract**
- 在 `tests/test_tls13_keyschedule.pas`：
  - 断言 early-data secret / binder / handshake/application secret 推导保持 TLS 1.3 contract
  - 为后续 scripted transport fixture 提供稳定 transcript / secret truth source

**Step 3: End-to-end early-data contract**
- 在 `tests/test_freepascal_tls13_early_data.pas`：
  - 用 scripted stream 模拟 initial handshake + ticket issuance + resumed handshake
  - 断言 accept path 上，server policy 为 accept 时能读到 early-data
  - 断言 reject 时 early-data 不会暴露为应用层读取
  - 断言 immediate post-handshake `NewSessionTicket` 能在 client `Connect` 后被消费并形成 resumable session

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/context_builder_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_early_data_contract -FEtmp/context_builder_early_data_contract -otmp/context_builder_early_data_contract/test_context_builder_early_data_contract tests/config/test_context_builder_early_data_contract.pas && ./tmp/context_builder_early_data_contract/test_context_builder_early_data_contract`
  - `mkdir -p tmp/tls13_keyschedule && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_keyschedule -FEtmp/tls13_keyschedule -otmp/tls13_keyschedule/test_tls13_keyschedule tests/test_tls13_keyschedule.pas && ./tmp/tls13_keyschedule/test_tls13_keyschedule`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - RED 暴露 transport/path gap，集中在：
    - client `Connect` 没有及时 drain stream-backed post-handshake ticket
    - server accept path 丢失 accepted early-data
    - clean EOF / no-record path 与 scripted stream fixture 语义不一致

### Task 2: Implement Minimal Connection-Side Transport Fixes

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Drain immediate post-handshake ticket on stream-backed connect**
- 增加 stream-buffer 探测 helper
- 在 client handshake 完成后 opportunistically drain 已经到达的 post-handshake records
- 允许 `RecvApplicationDataFragment(..., AAllowNoRecord=True)` 在处理完一个 post-handshake handshake fragment 后停下，而不是继续把 scripted EOF 当成错误

**Step 2: Preserve server-side accepted early data**
- 在 `DoAccept` 中使用局部 early-data staging buffer
- 在 handshake/application secret setup 完成后，把 staged early-data 拷回 `FApplicationReadBuffer`

**Step 3: Align clean EOF semantics**
- 在 `DoRead` 中，如果 stream 已 clean EOF 且没有剩余 application data，返回 `0`
- 不再把该情形报告成 `-1`

**Step 4: Run GREEN**
- Re-run:
  - `mkdir -p tmp/context_builder_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_early_data_contract -FEtmp/context_builder_early_data_contract -otmp/context_builder_early_data_contract/test_context_builder_early_data_contract tests/config/test_context_builder_early_data_contract.pas && ./tmp/context_builder_early_data_contract/test_context_builder_early_data_contract`
  - `mkdir -p tmp/tls13_keyschedule && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_keyschedule -FEtmp/tls13_keyschedule -otmp/tls13_keyschedule/test_tls13_keyschedule tests/test_tls13_keyschedule.pas && ./tmp/tls13_keyschedule/test_tls13_keyschedule`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - PASS

### Task 3: Promote Early-Data Coverage Into The Focused Completeness Gate

**Files:**
- Modify: `scripts/run_freepascal_tls13_completeness_gate.sh`
- Modify: `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`

**Step 1: Update gate inventory**
- 把 `test_freepascal_tls13_early_data` / `tests/test_freepascal_tls13_early_data.pas` 加入 gate 列表
- 保持 focused gate 仍然只覆盖 pure Pascal TLS 1.3 completeness 主线，不扩到其它 backend family

**Step 2: Update shell contract**
- dry-run output 必须提到 `tests/test_freepascal_tls13_early_data.pas`
- fake `fpc` invocation count 从 `6` 提升到 `7`
- summary report 至少要能落下新的 PASS row

**Step 3: Verify gate**
- Run:
  - `bash tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_transport_20260408`
- Expected:
  - PASS

### Task 4: Write Back Working Memory And Diff Hygiene

**Files:**
- Modify: `docs/plans/2026-04-08-freepascal-tls13-early-data-public-transport-and-policy.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record findings**
- 记录这批真正收口的 root causes：
  - stream-backed client `Connect` 需要 opportunistic post-handshake drain
  - server accept path 之前会丢失 accepted early-data buffer
  - clean EOF 语义必须是 `0`，不是 `-1`
  - gate 之前仍缺 `tests/test_freepascal_tls13_early_data.pas`

**Step 2: Fresh verification and diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-freepascal-tls13-early-data-public-transport-and-policy.md src/fafafa.ssl.freepascal.connection.pas tests/config/test_context_builder_early_data_contract.pas tests/test_tls13_keyschedule.pas tests/test_freepascal_tls13_early_data.pas scripts/run_freepascal_tls13_completeness_gate.sh tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- builder / keyschedule / early-data transport focused contracts are green
- completeness gate includes `tests/test_freepascal_tls13_early_data.pas`
- shell contract and real gate both pass
- working-memory files record commands and outcomes
- remaining backlog is clearly narrowed to:
  - anti-replay policy
  - broader public API ergonomics if desired later
  - more complete server-side early-data policy hardening beyond the current bounded accept/reject contract
