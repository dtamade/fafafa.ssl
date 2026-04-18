# FreePascal TLS 1.3 0-RTT / Early Data Primitives Implementation Plan

**Goal:** 先为 pure Pascal backend 落地 TLS 1.3 0-RTT 的协议基础 primitives，而不是在同一批里同时引入新的 public API 与完整 early application-data 通路。

**Architecture:** 当前 client/server resumption/PSK 已闭环，但 0-RTT 仍完全空白。最小可收口的下一批不直接扩展 `ISSLConnection.Write(...)` 语义，而是先把后续 early-data path 必需的协议基元落地：`NewSessionTicket.max_early_data_size`、`ClientHello early_data`、`EndOfEarlyData`，以及 session 对 early-data ticket metadata 的保留。这样下一批如果继续推进真正的 early app-data path，就能建立在稳定的 ticket / parser / builder 合同之上。

**Tech Stack:** FreePascal / Pascal, pure TLS 1.3 primitives, offline resumption tests, TDD, file-based working memory.

---

## Scope

### In Scope
- `TLS_EXTENSION_EARLY_DATA` 与 `TLS_HANDSHAKE_TYPE_END_OF_EARLY_DATA` 常量
- `NewSessionTicket.max_early_data_size` 的 build/parse
- `ClientHello early_data` extension 的 build/parse（仅限 PSK path）
- `EndOfEarlyData` handshake 的 build/parse
- `TFreePascalSession` 保留 `max_early_data_size`
- focused gate 覆盖新的 0-RTT primitives test

### Out Of Scope
- 新 public API（例如“在 `Connect` 前写入 early data”）
- 真正的 early application-data 记录发送与回放策略
- anti-replay 策略
- server-side early-data accept/reject 状态机
- readiness/compatibility 的过度上调

## Task 1: Add Focused RED Tests

**Files:**
- Modify: `tests/test_tls13_posthandshake.pas`
- Modify: `tests/test_tls13_resumption.pas`
- Modify: `tests/test_freepascal_client_session_resumption.pas`

**Step 1: Add post-handshake RED**

- 在 `tests/test_tls13_posthandshake.pas`：
  - 断言 parser 能识别 `NewSessionTicket` 里的 `max_early_data_size` extension。
  - 断言 `BuildTLS13EndOfEarlyDataHandshake(...)` / `TryParseTLS13EndOfEarlyData(...)` 合同成立。
  - 断言错误类型或非法长度会被拒绝。

**Step 2: Add ClientHello RED**

- 在 `tests/test_tls13_resumption.pas`：
  - 断言 PSK ClientHello 在显式请求 early-data 时包含 `early_data` extension。
  - 断言 parser 会把 `HasEarlyData=True` 暴露给上层。
  - 断言 `pre_shared_key` 仍然必须保持最后一个 extension。

**Step 3: Add session metadata RED**

- 在 `tests/test_freepascal_client_session_resumption.pas`：
  - 让离线服务端在 `NewSessionTicket` 里带 `max_early_data_size`。
  - 断言客户端收到的 resumable session 能暴露该值。

**Step 4: Run RED**

```bash
mkdir -p tmp/tls13_posthandshake_early_data_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_posthandshake_early_data_red -FEtmp/tls13_posthandshake_early_data_red -otmp/tls13_posthandshake_early_data_red/test_tls13_posthandshake tests/test_tls13_posthandshake.pas && ./tmp/tls13_posthandshake_early_data_red/test_tls13_posthandshake
mkdir -p tmp/tls13_resumption_early_data_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_early_data_red -FEtmp/tls13_resumption_early_data_red -otmp/tls13_resumption_early_data_red/test_tls13_resumption tests/test_tls13_resumption.pas && ./tmp/tls13_resumption_early_data_red/test_tls13_resumption
mkdir -p tmp/freepascal_client_session_resumption_early_data_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption_early_data_red -FEtmp/freepascal_client_session_resumption_early_data_red -otmp/tls13_client_session_resumption_early_data_red/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/tls13_client_session_resumption_early_data_red/test_freepascal_client_session_resumption
```

Expected:
- RED 直接指向缺少的 early-data constants / parser fields / session metadata getter，而不是无关错误。

## Task 2: Implement Minimal Primitives

**Files:**
- Modify: `src/fafafa.ssl.tls13.wire.pas`
- Modify: `src/fafafa.ssl.tls13.posthandshake.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.parser.pas`
- Modify: `src/fafafa.ssl.freepascal.session.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

## Task 3: Promote Into Focused Gate

**Files:**
- Modify: `scripts/run_freepascal_tls13_completeness_gate.sh`
- Modify: `tests/scripts/test_freepascal_tls13_completeness_gate_contract.sh`

## Task 4: Verify GREEN And Write Back Working Memory

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
