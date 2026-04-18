# FreePascal TLS 1.3 Server Session Resumption PSK Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal backend 的服务端具备完整的 TLS 1.3 `issue ticket -> cache session -> accept resumed PSK handshake` 闭环。

**Architecture:** 当前 client-side resumption/PSK 已闭环，但 server path 仍停在 full-handshake accept skeleton。这个 family 只做 server-side session resumption/PSK，不扩到 `0-RTT`。实现策略是：在 FreePascal context 内加入 bounded in-memory ticket cache；在 full handshake 成功后由服务端发出 `NewSessionTicket` 并落缓存；在下次 `ClientHello` 带 `pre_shared_key` 时重建 binder transcript、校验 binder、命中缓存后走 `PSK + ECDHE` 握手，并在 `ServerHello` 回显 `pre_shared_key(selected_identity=0)`。

**Tech Stack:** FreePascal / Pascal, pure TLS 1.3 primitives, offline scripted client stream, TDD, file-based working memory.

---

## Scope

### In Scope
- 服务端 `NewSessionTicket` builder / sender
- FreePascal context 内部 session ticket cache
- 服务端解析并接受单 identity `pre_shared_key`
- binder transcript 重建与 binder 校验
- `ServerHello.pre_shared_key(selected_identity=0)` 输出
- `ISSLConnection.IsSessionReused` 在 server-side resumed accept 上对齐

### Out Of Scope
- `0-RTT / Early Data`
- 多 identity 选择策略
- 跨进程 / 持久化 session cache
- enterprise-grade validation / OCSP / CT

## Task 1: Add Focused RED Tests

**Files:**
- Modify: `tests/test_tls13_resumption.pas`
- Add: `tests/test_freepascal_server_session_resumption.pas`
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Add primitive RED coverage**

- 在 `tests/test_tls13_resumption.pas`：
  - 增加 “从完整 PSK ClientHello 重建 binder transcript” 的 contract。
  - 增加 `ServerHello` builder/parser 对 `pre_shared_key(selected_identity=0)` 的 contract，避免继续在测试里手写局部 helper。

**Step 2: Add offline server-accept RED coverage**

- 新建 `tests/test_freepascal_server_session_resumption.pas`：
  - 实现 scripted offline client stream，驱动真实 `TFreePascalConnection.Accept`。
  - 第一次 full handshake：
    - 断言 `Accept = True`
    - 断言服务端发出 post-handshake `NewSessionTicket`
    - 断言客户端脚本能从 ticket 恢复 resumable session
    - 断言服务端连接 `IsSessionReused = False`
  - 第二次 resumed handshake（复用同一个 server context）：
    - 断言客户端 `ClientHello` 带 `pre_shared_key`
    - 断言服务端 `Accept = True`
    - 断言服务端 `ServerHello` 带 `pre_shared_key(selected_identity=0)`
    - 断言服务端连接 `IsSessionReused = True`

**Step 3: Tighten capability wording RED**

- 在 `tests/test_freepascal_backend_basic.pas` / `tests/test_capability_cache.pas`：
  - 不再允许 `KnownIssues` 声明 `server-side resumption/PSK` 仍未实现。
  - 继续要求保留 `0-RTT`、validation hardening 等剩余缺口说明。

**Step 4: Run RED**

Run:

```bash
mkdir -p tmp/tls13_resumption_server_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_server_red -FEtmp/tls13_resumption_server_red -otmp/tls13_resumption_server_red/test_tls13_resumption tests/test_tls13_resumption.pas && ./tmp/tls13_resumption_server_red/test_tls13_resumption
mkdir -p tmp/freepascal_server_session_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_session_resumption_red -FEtmp/freepascal_server_session_resumption_red -otmp/tls13_server_session_resumption_red tests/test_freepascal_server_session_resumption.pas && ./tmp/tls13_server_session_resumption_red
mkdir -p tmp/freepascal_backend_basic_server_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic_server_resumption_red -FEtmp/freepascal_backend_basic_server_resumption_red -otmp/freepascal_backend_basic_server_resumption_red/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic_server_resumption_red/test_freepascal_backend_basic
mkdir -p tmp/capability_cache_server_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_server_resumption_red -FEtmp/capability_cache_server_resumption_red -otmp/capability_cache_server_resumption_red/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_server_resumption_red/test_capability_cache
```

Expected:
- primitive RED 指向 binder transcript / ServerHello PSK builder 缺口
- offline server RED 指向：
  - 没有 server ticket issuance
  - 没有 context ticket cache
  - server accept 没有 PSK binder verification / resumed path

## Task 2: Add Server-Side Session Cache And PSK Primitives

**Files:**
- Modify: `src/fafafa.ssl.freepascal.session.pas`
- Modify: `src/fafafa.ssl.freepascal.context.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.parser.pas`
- Modify: `src/fafafa.ssl.tls13.serverhello.pas`
- Modify: `src/fafafa.ssl.tls13.posthandshake.pas`
- Modify: `src/fafafa.ssl.tls13.keyschedule.pas`

**Step 1: Add internal cache/session interfaces**

- 在 `src/fafafa.ssl.freepascal.session.pas`：
  - 定义仅供 FreePascal backend 内部使用的 server-side resumption cache interface。
  - 复用 `TFreePascalSession` 作为 server cache entry，不新增第二套 session model。

**Step 2: Implement bounded in-memory cache on context**

- 在 `src/fafafa.ssl.freepascal.context.pas`：
  - 增加 ticket-keyed cache storage。
  - 受 `SetSessionCacheMode` / `SetSessionTimeout` / `SetSessionCacheSize` / `ssoEnableSessionTickets` 约束。
  - 提供：
    - store
    - lookup
    - prune expired
    - bounded eviction

**Step 3: Add binder-transcript / ServerHello helpers**

- 在 `src/fafafa.ssl.tls13.clienthello.parser.pas`：
  - 增加从完整 PSK ClientHello 重建 binder transcript 的 helper，供服务端校验 binder。
- 在 `src/fafafa.ssl.tls13.serverhello.pas`：
  - 增加带 `pre_shared_key(selected_identity)` 的 `ServerHello` builder。
- 在 `src/fafafa.ssl.tls13.posthandshake.pas`：
  - 增加 `NewSessionTicket` builder，避免连接层重复拼字节。
- 如有必要，在 `src/fafafa.ssl.tls13.keyschedule.pas` 增加显式 binder 校验 helper，但不要复制已有 hash 分支逻辑。

## Task 3: Wire Server-Side Resumption Into FreePascal Accept Path

**Files:**
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Accept PSK when client offers resumable identity**

- 在 `DoAccept`：
  - full parse `ClientHello.pre_shared_key`
  - 从 context cache lookup ticket
  - 只支持 first identity / selected_identity=`0`
  - 校验 binder transcript
  - 校验 cipher hash path兼容
  - 命中后走 `TryDeriveTLS13HandshakeSecretsWithPSK(...)`
  - `ServerHello` 改为带 `pre_shared_key(selected_identity=0)`
  - 设置 `FSessionReused := True`

**Step 2: Issue and cache session tickets after successful handshake**

- 在 full handshake 成功并得到 application secrets 后：
  - 生成 ticket nonce / ticket / age_add
  - 派生 resumption PSK
  - 构造 `TFreePascalSession`
  - 写入 context cache
  - 发送加密的 `NewSessionTicket`
  - 将 `FCurrentSession` 对齐到当前 issued session

**Step 3: Align capability wording**

- 在 `src/fafafa.ssl.freepascal.lib.pas`：
  - `KnownIssues` 改成不再列出 `server-side resumption/PSK`
  - 保留 `0-RTT`、`OCSP stapling`、`Certificate Transparency`、validation hardening
  - 视实现完整度适度上调 `CompatibilityLevel`

## Task 4: Verify GREEN And Adjacent Regressions

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused tests**

Run:

```bash
mkdir -p tmp/tls13_resumption_server && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_server -FEtmp/tls13_resumption_server -otmp/tls13_resumption_server/test_tls13_resumption tests/test_tls13_resumption.pas && ./tmp/tls13_resumption_server/test_tls13_resumption
mkdir -p tmp/freepascal_server_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_session_resumption -FEtmp/freepascal_server_session_resumption -otmp/tls13_server_session_resumption tests/test_freepascal_server_session_resumption.pas && ./tmp/tls13_server_session_resumption
mkdir -p tmp/freepascal_backend_basic_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic_server_resumption -FEtmp/freepascal_backend_basic_server_resumption -otmp/freepascal_backend_basic_server_resumption/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic_server_resumption/test_freepascal_backend_basic
mkdir -p tmp/capability_cache_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_server_resumption -FEtmp/capability_cache_server_resumption -otmp/capability_cache_server_resumption/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_server_resumption/test_capability_cache
```

Expected:
- 全部 PASS

**Step 2: Run adjacent regressions**

Run:

```bash
mkdir -p tmp/freepascal_server_accept_skeleton_adjacent_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_server_accept_skeleton_adjacent_resumption -FEtmp/freepascal_server_accept_skeleton_adjacent_resumption -otmp/freepascal_server_accept_skeleton_adjacent_resumption/test_freepascal_server_accept_skeleton tests/test_freepascal_server_accept_skeleton.pas && ./tmp/freepascal_server_accept_skeleton_adjacent_resumption/test_freepascal_server_accept_skeleton
mkdir -p tmp/freepascal_client_session_resumption_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption_adjacent -FEtmp/freepascal_client_session_resumption_adjacent -otmp/freepascal_client_session_resumption_adjacent/test_freepascal_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/freepascal_client_session_resumption_adjacent/test_freepascal_client_session_resumption
mkdir -p tmp/tls13_posthandshake_adjacent_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_posthandshake_adjacent_server_resumption -FEtmp/tls13_posthandshake_adjacent_server_resumption -otmp/tls13_posthandshake_adjacent_server_resumption/test_tls13_posthandshake tests/test_tls13_posthandshake.pas && ./tmp/tls13_posthandshake_adjacent_server_resumption/test_tls13_posthandshake
mkdir -p tmp/tls13_clienthello_parser_adjacent_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_clienthello_parser_adjacent_server_resumption -FEtmp/tls13_clienthello_parser_adjacent_server_resumption -otmp/tls13_clienthello_parser_adjacent_server_resumption/test_tls13_clienthello_parser tests/test_tls13_clienthello_parser.pas && ./tmp/tls13_clienthello_parser_adjacent_server_resumption/test_tls13_clienthello_parser
mkdir -p tmp/tls13_finished_adjacent_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_finished_adjacent_server_resumption -FEtmp/tls13_finished_adjacent_server_resumption -otmp/tls13_finished_adjacent_server_resumption/test_tls13_finished tests/test_tls13_finished.pas && ./tmp/tls13_finished_adjacent_server_resumption/test_tls13_finished
mkdir -p tmp/tls13_keyschedule_adjacent_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_keyschedule_adjacent_server_resumption -FEtmp/tls13_keyschedule_adjacent_server_resumption -otmp/tls13_keyschedule_adjacent_server_resumption/test_tls13_keyschedule tests/test_tls13_keyschedule.pas && ./tmp/tls13_keyschedule_adjacent_server_resumption/test_tls13_keyschedule
mkdir -p tmp/tls13_appschedule_adjacent_server_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_appschedule_adjacent_server_resumption -FEtmp/tls13_appschedule_adjacent_server_resumption -otmp/tls13_appschedule_adjacent_server_resumption/test_tls13_appschedule tests/test_tls13_appschedule.pas && ./tmp/tls13_appschedule_adjacent_server_resumption/test_tls13_appschedule
python3 scripts/compile_all_modules.py
git diff --check -- docs/plans/2026-03-26-freepascal-tls13-server-session-resumption-psk.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.freepascal.context.pas src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.tls13.clienthello.parser.pas src/fafafa.ssl.tls13.serverhello.pas src/fafafa.ssl.tls13.posthandshake.pas src/fafafa.ssl.tls13.keyschedule.pas src/fafafa.ssl.freepascal.lib.pas tests/test_tls13_resumption.pas tests/test_freepascal_server_session_resumption.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md
```

Expected:
- accept skeleton 继续保持原有 fail-fast contract
- client-side resumption 不回退
- compile gate 继续通过

## Task 5: Write Back Working Memory

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Record roadmap status**

- `task_plan.md`：
  - 标记 `server-side session resumption / PSK` 已完成
  - 下一条关键 family 改为 `0-RTT / Early Data`
- `findings.md`：
  - 明确 pure Pascal backend 现在已具备：
    - TLS 1.3 modern suite parity
    - client-side resumption / PSK
    - server-side resumption / PSK
  - 仍未达到 rustls 水平的缺口聚焦到：
    - `0-RTT`
    - `OCSP stapling`
    - `Certificate Transparency`
    - 更完整 validation
- `progress.md`：
  - 记录 RED/GREEN、ticket cache、binder 校验、server-issued ticket 证据
