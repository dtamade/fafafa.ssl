# Connector Client Early-Data Convenience Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不重开 early-data backend 真值与核心接口的前提下，为 `TSSLConnector` 增加客户端 early-data convenience API，把当前手工 `SetSession` + `SetEarlyData` + `Connect` 的编排收口成可读的 fluent facade。

**Architecture:** 保持 `ISSLEarlyDataContext` / `ISSLEarlyDataConnection` 继续作为唯一行为真值，不扩 `ISSLConnection.Write(...)` 语义，也不自动修改 context。`TSSLConnector` 只额外保存 queued client early-data 负载，并在既有 client connect flow 中，按 `session -> server name -> queue early data -> connect` 的顺序调用已有 contract。FreePascal end-to-end tests 继续作为 Linux-safe 真合同；另加一个 mock-based connector contract 固定 facade 顺序与错误传播。

**Tech Stack:** FreePascal (ObjFPC), connector facade record methods, mock Pascal contract tests, FreePascal TLS 1.3 early-data scripted tests, file-based working memory.

---

## Summary

- 当前 early-data 的核心链路已经存在：
  - `ISSLEarlyDataContext`
  - `ISSLEarlyDataConnection.SetEarlyData(...)`
  - `TSSLContextBuilder.WithClientEarlyData(...)`
  - resumed session + `Connect`
- 但 connector facade 仍缺一段真实 usability glue：
  - 用户若走 `TSSLConnector`，仍要退回到底层 `ISSLConnection`
  - 仍要手工写 `SetSession(...)`
  - 仍要手工 cast `ISSLEarlyDataConnection`
  - 仍要自己保证 `SetEarlyData(...)` 发生在 `Connect` 之前
- 本批只做 connector client convenience：
  - 增加 `TSSLConnector.WithEarlyData(const AData: TBytes)`
  - `TryConnect*` 在握手前自动 queue 这段 early data
  - unsupported / disabled / zero-limit 等失败保持透传底层现有错误
  - 不自动启用 context-level client early-data
  - 不改 `TSSLAcceptor`、builder 或 backend 行为

## Delivery Order

1. 写 plan 与 working-memory 入口，锁定范围为 connector client early-data convenience。
2. 先在 connector facade contract 上加 RED，证明当前缺少 `WithEarlyData(...)` 与 pre-connect queue glue。
3. 最小实现只改 `src/fafafa.ssl.tls.pas`。
4. 跑 focused tests、`python3 scripts/compile_all_modules.py`、diff hygiene。
5. 回填 findings / progress / task plan。

### Task 1: Add RED Contracts For Connector Early-Data Convenience

**Files:**
- Add: `tests/test_tls_connector_early_data_contract.pas`
- Modify: `tests/test_freepascal_tls13_early_data.pas`

**Step 1: Write the failing connector facade contract**
- 在 `tests/test_tls_connector_early_data_contract.pas`：
  - 定义 mock `ISSLContext`
  - 定义 mock client connection
  - 一条实现 `ISSLEarlyDataConnection`
  - 一条不实现 `ISSLEarlyDataConnection`
  - 增加 observable call log，至少记录：
    - `session`
    - `servername`
    - `earlydata`
    - `connect`
  - 覆盖：
    - `TSSLConnector.WithEarlyData(...)` 存在且可链式调用
    - connector 对 supported connection 的调用顺序固定为：
      - `session -> servername -> earlydata -> connect`
    - `WithEarlyData([])` 不触发 queue
    - connection 不支持 `ISSLEarlyDataConnection` 时：
      - `TryConnectStream(...)` 返回 `Err`
      - `ConnectStream(...)` 抛连接异常

**Step 2: Extend the FreePascal truth test**
- 在 `tests/test_freepascal_tls13_early_data.pas`：
  - 新增 connector-based accepted path
  - 新增 connector-based rejected path
  - 要求使用：
    - `TSSLConnector.FromContext(...)`
    - `.WithSession(...)`
    - `.WithEarlyData(BytesOf(...))`
    - `.ConnectStream(...)`
  - 保留现有 direct `ISSLConnection` tests，不做替换

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/tls_connector_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls_connector_early_data_contract -FEtmp/tls_connector_early_data_contract -otmp/tls_connector_early_data_contract/test_tls_connector_early_data_contract tests/test_tls_connector_early_data_contract.pas && ./tmp/tls_connector_early_data_contract/test_tls_connector_early_data_contract`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
- Expected:
  - RED 聚焦暴露：
    - `TSSLConnector` 缺少 `WithEarlyData(...)`
    - connector path 不会在 `Connect` 前 queue early data

### Task 2: Implement Minimal Connector Early-Data Glue

**Files:**
- Modify: `src/fafafa.ssl.tls.pas`

**Step 1: Add fluent API**
- 在 `TSSLConnector`：
  - 增加 `WithEarlyData(const AData: TBytes): TSSLConnector`
  - 增加私有字段保存 queued early-data payload
  - `WithEarlyData([])` 视为 clear / no-op，不触发 queue

**Step 2: Add pre-connect queue helper**
- 增加私有 helper，仅在 client connector path 使用：
  - 若 payload 为空，直接 `Ok`
  - 若 connection 不支持 `ISSLEarlyDataConnection`，返回 `sslErrUnsupported`
  - 否则调用 `SetEarlyData(...)` 并原样返回 `TSSLOperationResult`

**Step 3: Rewire client connect flow**
- 在 `TryConnectSocket(...)` / `TryConnectStream(...)`：
  - 保持既有 `ApplyClientOptions(...)` 顺序不变：
    - timeout / blocking
    - session
    - server name
  - 在 `Connect` 前调用 early-data queue helper
  - queue 失败时做 best-effort `Close` 并返回原始错误
  - handshake / verification 错误语义保持不变

**Step 4: Run GREEN**
- Re-run Task 1 commands
- Expected:
  - PASS

### Task 3: Verify Broadly And Close Out

**Files:**
- Modify: `docs/INTEGRATION_GUIDE.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Update connector-facing docs**
- 在 `docs/INTEGRATION_GUIDE.md` 的 connector/stream 接入段：
  - 增加 resumed client early-data 示例
  - 明确：
    - context 仍需先开启 client early-data
    - connector 只负责 queue payload，不会偷偷改 context

**Step 2: Focused regressions + core compile**
- Run:
  - `mkdir -p tmp/tls_connector_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls_connector_early_data_contract -FEtmp/tls_connector_early_data_contract -otmp/tls_connector_early_data_contract/test_tls_connector_early_data_contract tests/test_tls_connector_early_data_contract.pas && ./tmp/tls_connector_early_data_contract/test_tls_connector_early_data_contract`
  - `mkdir -p tmp/tls_connector_hostname_override_precedence && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls_connector_hostname_override_precedence -FEtmp/tls_connector_hostname_override_precedence -otmp/tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence tests/test_tls_connector_hostname_override_precedence.pas && ./tmp/tls_connector_hostname_override_precedence/test_tls_connector_hostname_override_precedence`
  - `mkdir -p tmp/early_data_public_api_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/early_data_public_api_contract -FEtmp/early_data_public_api_contract -otmp/early_data_public_api_contract/test_early_data_public_api_contract tests/test_early_data_public_api_contract.pas && ./tmp/early_data_public_api_contract/test_early_data_public_api_contract`
  - `mkdir -p tmp/context_builder_early_data_contract && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/context_builder_early_data_contract -FEtmp/context_builder_early_data_contract -otmp/context_builder_early_data_contract/test_context_builder_early_data_contract tests/config/test_context_builder_early_data_contract.pas && ./tmp/context_builder_early_data_contract/test_context_builder_early_data_contract`
  - `mkdir -p tmp/freepascal_tls13_early_data && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_tls13_early_data -FEtmp/freepascal_tls13_early_data -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data tests/test_freepascal_tls13_early_data.pas && ./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data`
  - `python3 scripts/compile_all_modules.py`
- Expected:
  - PASS

**Step 3: Diff hygiene**
- Run:
  - `git diff --check -- docs/plans/2026-04-08-connector-client-early-data-convenience.md docs/INTEGRATION_GUIDE.md src/fafafa.ssl.tls.pas tests/test_tls_connector_early_data_contract.pas tests/test_freepascal_tls13_early_data.pas task_plan.md findings.md progress.md`
- Expected:
  - exit `0`

### Definition Of Done

- `TSSLConnector` 提供 `WithEarlyData(...)` fluent facade
- client connector path 能在 `Connect` 前自动 queue configured early data
- unsupported / disabled / zero-limit 等失败继续透传现有 early-data 错误
- connector precedence tests 与 FreePascal end-to-end early-data tests 通过
- working-memory files 记录 RED/GREEN evidence 和 closeout notes
