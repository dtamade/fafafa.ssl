# FreePascal TLS 1.3 Server Early-Data Policy And Max Size Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩 `ISSLContext` / `ISSLConnection` 核心接口的前提下，把 pure Pascal backend 的服务端 early-data surface 从 binary `reject/accept` 推进到更真实的公开可配置 contract：支持 `reject / issue-only / accept` 三态策略，并让 `max_early_data_size` 变成 context/builder 可配置值，而不是连接层硬编码。

**Architecture:** 沿用现有 optional early-data surfaces。`ISSLEarlyDataContext` 继续承担上下文级配置真值，`TSSLContextBuilder` 负责 fluent/config import-export/clone-merge 接线，`TFreePascalContext` 保存 policy/max-size，`TFreePascalConnection` 只负责：
- `NewSessionTicket` 按策略发行 `max_early_data_size`
- resumed server accept path 仅在 `sslEarlyDataServerAccept` 时真正接受 early data
- `sslEarlyDataServerIssueOnly` 只发可带 early-data limit 的 ticket，不接受 resumed early data

**Tech Stack:** FreePascal (ObjFPC), pure Pascal TLS 1.3 resumption/early-data units, offline scripted early-data tests, builder config round-trip tests, file-based working memory.

---

## Summary

- 当前树上已经具备：
  - early-data protocol primitives
  - public early-data context/connection optional interfaces
  - bounded process-local anti-replay ledger
- 当前剩余缺口已经从“是否支持 0-RTT”收缩到“公开 policy/max-size surface 过于粗糙”：
  - 服务端 policy 只有 `reject/accept`
  - `NewSessionTicket.max_early_data_size` 仍在连接层硬编码为 `8`
  - builder/config round-trip 还不能表达服务端 max size
- 本批只扩 optional/public early-data surface，不碰核心接口：
  - `TSSLEarlyDataServerPolicy` 增加 `sslEarlyDataServerIssueOnly`
  - `ISSLEarlyDataContext` / builder 增加 `ServerMaxEarlyDataSize`
  - `Reject` 发 `0`
  - `IssueOnly` / `Accept` 发配置值
  - 只有 `Accept` + existing anti-replay success 才接受 resumed early data

## Delivery Order

1. 写 plan 与 working-memory 入口。
2. 先在 builder/config 和 end-to-end early-data tests 上加 RED。
3. 最小实现只改 `src/fafafa.ssl.base.pas`、`src/fafafa.ssl.context.builder.pas`、`src/fafafa.ssl.freepascal.context.pas`、`src/fafafa.ssl.freepascal.connection.pas`。
4. 跑 focused regressions 与 focused completeness gate。
5. 回填 findings / progress / task plan。

### Task 1: Add RED Contracts For Public Policy/Max-Size Surface

**Files:**
- Modify: `tests/config/test_context_builder_early_data_contract.pas`
- Modify: `tests/config/test_config_import_export.pas`
- Modify: `tests/config/test_config_snapshot_clone.pas`
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Extend builder/context contract**
- 在 `tests/config/test_context_builder_early_data_contract.pas`：
  - 断言 `ServerMaxEarlyDataSize` 默认是 `0`
  - 断言 `WithServerEarlyDataPolicy(sslEarlyDataServerIssueOnly)` 可观察
  - 断言 `WithServerMaxEarlyDataSize(...)` 可观察

**Step 2: Add config round-trip coverage**
- 在 import/export 与 clone/merge tests 中覆盖：
  - `server_early_data_policy`
  - `server_max_early_data_size`
  - JSON / INI round-trip
  - clone / reset / merge / override surface

**Step 3: Add end-to-end early-data policy coverage**
- 在 `tests/test_freepascal_tls13_early_data.pas`：
  - `Reject` 发出的 resumable session `max_early_data_size = 0`
  - `IssueOnly + size>0` 发可恢复 ticket，但 resumed early data 仍被拒绝
  - `Accept + custom size` 暴露配置值，而不是硬编码 `8`
  - payload 超过 configured limit 时，`SetEarlyData(...)` 返回 error

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/context_builder_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_early_data_contract -FEtmp/context_builder_early_data_contract -otmp/context_builder_early_data_contract/test_context_builder_early_data_contract tests/config/test_context_builder_early_data_contract.pas && ./tmp/context_builder_early_data_contract/test_context_builder_early_data_contract`
  - `mkdir -p tmp/config_import_export && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_import_export -FEtmp/config_import_export -otmp/config_import_export/test_config_import_export tests/config/test_config_import_export.pas && ./tmp/config_import_export/test_config_import_export`
  - `mkdir -p tmp/config_snapshot_clone && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/config_snapshot_clone -FEtmp/config_snapshot_clone -otmp/config_snapshot_clone/test_config_snapshot_clone tests/config/test_config_snapshot_clone.pas && ./tmp/config_snapshot_clone/test_config_snapshot_clone`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - RED 聚焦暴露：
    - optional early-data context 缺少 server max-size getter/setter
    - builder/config round-trip 未接 `server_max_early_data_size`
    - service-side issuance 仍写死 `8`
    - `issue-only` 尚不存在

### Task 2: Implement Minimal Public Surface And Backend Wiring

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.context.builder.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Extend optional public early-data types**
- 在 `src/fafafa.ssl.base.pas`：
  - `TSSLEarlyDataServerPolicy` 增加 `sslEarlyDataServerIssueOnly`
  - `ISSLEarlyDataContext` 增加：
    - `SetServerMaxEarlyDataSize(ASize: Cardinal)`
    - `GetServerMaxEarlyDataSize: Cardinal`

**Step 2: Extend builder/config plumbing**
- 在 `src/fafafa.ssl.context.builder.pas`：
  - 增加 `WithServerMaxEarlyDataSize(ASize: Cardinal)`
  - 增加 backing field/default `0`
  - BuildClient/BuildServer 接线到 `ISSLEarlyDataContext`
  - JSON / INI import/export、clone、reset、merge、override 接线 `server_max_early_data_size`

**Step 3: Extend FreePascal context storage**
- 在 `src/fafafa.ssl.freepascal.context.pas`：
  - 保存 `FServerMaxEarlyDataSize`
  - 默认 `0`
  - 暴露 setter/getter

**Step 4: Rewire ticket issuance / accept path**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 发行 ticket 时：
    - `Reject` => issue `0`
    - `IssueOnly` / `Accept` => issue configured positive size
  - 接受 resumed early data 时：
    - 仅 `Accept` + anti-replay acquire success 才接受
    - `IssueOnly` 明确保持 `sslEarlyDataRejected`
  - client `SetEarlyData(...)` limit 校验继续跟随 session `max_early_data_size`

**Step 5: Run GREEN**
- Re-run Task 1 commands
- Expected:
  - PASS

### Task 3: Run Focused Regressions And Gate

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Focused regressions**
- Run:
  - `mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption`
  - `mkdir -p tmp/freepascal_server_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_session_resumption -FEtmp/freepascal_server_session_resumption -otmp/freepascal_server_session_resumption/test_freepascal_server_session_resumption tests/test_freepascal_server_session_resumption.pas && ./tmp/freepascal_server_session_resumption/test_freepascal_server_session_resumption`
  - `mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/freepascal_backend_basic/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic/test_freepascal_backend_basic`
  - `mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/capability_cache/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache/test_capability_cache`
  - `bash scripts/run_freepascal_tls13_completeness_gate.sh --fast-local --run-id early_data_policy_maxsize_20260408`
- Expected:
  - PASS

**Step 2: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-freepascal-tls13-server-early-data-policy-and-maxsize.md src/fafafa.ssl.base.pas src/fafafa.ssl.context.builder.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas tests/config/test_context_builder_early_data_contract.pas tests/config/test_config_import_export.pas tests/config/test_config_snapshot_clone.pas tests/test_freepascal_tls13_early_data.pas tests/test_freepascal_client_session_resumption.pas tests/test_freepascal_server_session_resumption.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- public optional early-data policy surface supports `reject / issue-only / accept`
- context/builder/config round-trip exposes `server_max_early_data_size`
- FreePascal ticket issuance no longer hardcodes `8`
- `IssueOnly` can issue resumable early-data-capable tickets without accepting resumed early data
- focused regressions and completeness gate are green
- working-memory files record RED/GREEN evidence and closeout notes
