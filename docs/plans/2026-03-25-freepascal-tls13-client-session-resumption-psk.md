# FreePascal TLS 1.3 Client Session Resumption PSK Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 pure Pascal backend 的 TLS 1.3 `NewSessionTicket -> ISSLSession -> SetSession -> resumed handshake` 客户端闭环真正打通。

**Architecture:** 这批只做 client-side session resumption/PSK，不把 scope 扩到 server-side 完整 ticket issuance/acceptance。实现路径是：把当前只会“解析并计数”的 `NewSessionTicket` 升级成可序列化、可恢复的 `ISSLSession`；在下次连接时让 ClientHello 真正携带 `pre_shared_key` + binder；在 ServerHello 里识别 `pre_shared_key` 选中状态，并把连接层 `IsSessionReused` 与实际握手结果对齐。测试以纯离线自驱动流完成，不依赖外网。

**Tech Stack:** FreePascal / Pascal, pure TLS 1.3 primitives, custom offline duplex test stream, TDD, file-based working memory.

---

### Task 1: Write Focused RED Tests

**Files:**
- Add: `tests/test_tls13_resumption.pas`
- Add: `tests/test_freepascal_client_session_resumption.pas`
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Add primitive RED coverage**

- `tests/test_tls13_resumption.pas`
  - 断言能够从 `master_secret + transcript + ticket_nonce` 派生 resumption PSK。
  - 断言能够从 PSK + partial ClientHello 派生 binder。
  - 断言带 PSK 的 ClientHello 会把 `pre_shared_key` 放在最后一个 extension。
  - 断言 ServerHello parser 能识别 `pre_shared_key(selected_identity=0)`。

**Step 2: Add offline connection RED coverage**

- `tests/test_freepascal_client_session_resumption.pas`
  - 用自定义 `TStream` 模拟服务端 first handshake：
    - 接收客户端 ClientHello
    - 发送最小 TLS 1.3 `ServerHello + EncryptedExtensions + Finished`
    - 接收并校验客户端 Finished
    - 发送 post-handshake `NewSessionTicket` 与一段应用数据
  - 断言首次连接在读取到 post-handshake 数据后：
    - `GetSession <> nil`
    - `GetSession.IsResumable = True`
    - `IsSessionReused = False`
  - 再用同一个 `ISSLSession` 驱动第二次离线连接：
    - 断言客户端 ClientHello 确实携带 `pre_shared_key`
    - 断言 resumed `ServerHello` 被接受
    - 断言 `Connect = True`
    - 断言 `IsSessionReused = True`

**Step 3: Tighten capability wording RED**

- `tests/test_freepascal_backend_basic.pas`
  - 断言 `KnownIssues` 不再笼统宣称 `PSK/resumption remain in progress`
  - 改为要求文案明确限定为 `server-side` / `0-RTT` / 其它剩余缺口
- `tests/test_capability_cache.pas`
  - 同样锁定新的 `KnownIssues` 运行时文案

**Step 4: Run tests to verify they fail**

Run:

```bash
mkdir -p tmp/tls13_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption_red -FEtmp/tls13_resumption_red -otmp/tls13_resumption_red/test_tls13_resumption tests/test_tls13_resumption.pas && ./tmp/tls13_resumption_red/test_tls13_resumption
mkdir -p tmp/freepascal_client_session_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption_red -FEtmp/freepascal_client_session_resumption_red -otmp/tls13_client_session_resumption_red tests/test_freepascal_client_session_resumption.pas && ./tmp/tls13_client_session_resumption_red
mkdir -p tmp/freepascal_backend_basic_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic_resumption_red -FEtmp/freepascal_backend_basic_resumption_red -otmp/freepascal_backend_basic_resumption_red/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic_resumption_red/test_freepascal_backend_basic
mkdir -p tmp/capability_cache_resumption_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_resumption_red -FEtmp/capability_cache_resumption_red -otmp/capability_cache_resumption_red/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_resumption_red/test_capability_cache
```

Expected:
- primitive tests 明确指向缺少 resumption secret / binder / ServerHello pre_shared_key 解析
- offline connection test 明确指向 `GetSession/SetSession/IsSessionReused` 空实现
- capability wording tests 明确指向 stale `KnownIssues`

### Task 2: Implement Minimal Production Changes

**Files:**
- Add: `src/fafafa.ssl.freepascal.session.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.parser.pas`
- Modify: `src/fafafa.ssl.tls13.parser.pas`
- Modify: `src/fafafa.ssl.tls13.keyschedule.pas`
- Modify: `src/fafafa.ssl.tls13.appschedule.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Add resumable session object**

- 在 `src/fafafa.ssl.freepascal.session.pas`：
  - 新增 `TFreePascalSession`
  - 实现 `ISSLSession`
  - 保存：
    - protocol / cipher suite
    - ticket lifetime / age_add / nonce / ticket
    - resumption PSK
    - creation time / timeout
  - 支持 `Clone` 与 `Serialize/Deserialize`

**Step 2: Add TLS 1.3 resumption primitives**

- 在 `src/fafafa.ssl.tls13.keyschedule.pas`：
  - 增加带 `PSK` 的 handshake secret 派生 helper
- 在 `src/fafafa.ssl.tls13.appschedule.pas`：
  - 增加 `resumption_master_secret` / resumption PSK 派生 helper
- 复用现有 suite-aware SHA256 / SHA384 路径，不引入新的 hash 分支

**Step 3: Add PSK-aware ClientHello / ServerHello parsing**

- 在 `src/fafafa.ssl.tls13.clienthello.pas`：
  - 增加单 identity 的 `pre_shared_key` extension builder
  - 保证 `pre_shared_key` 始终是最后一个 extension
  - 提供 partial ClientHello 输出，供 binder transcript hash 使用
- 在 `src/fafafa.ssl.tls13.clienthello.parser.pas`：
  - 解析首个 PSK identity / binder
- 在 `src/fafafa.ssl.tls13.parser.pas`：
  - 解析 `ServerHello` 的 `pre_shared_key(selected_identity)`

**Step 4: Wire client-side resumption into FreePascal connection**

- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 新增当前 session / configured session / resumed state 字段
  - `ProcessPostHandshakeFragment(...)` 收到 `NewSessionTicket` 后构建 `TFreePascalSession`
  - `DoGetSession` 返回当前可恢复 session
  - `DoSetSession` 接受并保存 resumable session
  - `ProbeServerHello` 在有 configured session 时：
    - 计算 binder
    - 发送带 `pre_shared_key` 的 ClientHello
    - 按 PSK + ECDHE 派生 handshake secrets
    - 根据 `ServerHello.pre_shared_key` 结果设置 `IsSessionReused`

**Step 5: Align capability wording**

- 在 `src/fafafa.ssl.freepascal.lib.pas`：
  - `KnownIssues` 更新为：
    - client-side PSK/session resumption 已闭环
    - server-side resumption / 0-RTT / 更完整 validation 仍在进行中
  - `CompatibilityLevel` 适度上调

### Task 3: Verify Green And Adjacent Regressions

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused tests**

Run:

```bash
mkdir -p tmp/tls13_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_resumption -FEtmp/tls13_resumption -otmp/tls13_resumption/test_tls13_resumption tests/test_tls13_resumption.pas && ./tmp/tls13_resumption/test_tls13_resumption
mkdir -p tmp/freepascal_client_session_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_client_session_resumption -FEtmp/freepascal_client_session_resumption -otmp/tls13_client_session_resumption tests/test_freepascal_client_session_resumption.pas && ./tmp/tls13_client_session_resumption
mkdir -p tmp/freepascal_backend_basic_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic_resumption -FEtmp/freepascal_backend_basic_resumption -otmp/freepascal_backend_basic_resumption/test_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/freepascal_backend_basic_resumption/test_freepascal_backend_basic
mkdir -p tmp/capability_cache_resumption && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_resumption -FEtmp/capability_cache_resumption -otmp/capability_cache_resumption/test_capability_cache tests/test_capability_cache.pas && ./tmp/capability_cache_resumption/test_capability_cache
```

Expected:
- 全部 PASS

**Step 2: Run adjacent regressions**

Run:

```bash
mkdir -p tmp/tls13_posthandshake_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_posthandshake_adjacent -FEtmp/tls13_posthandshake_adjacent -otmp/tls13_posthandshake_adjacent/test_tls13_posthandshake tests/test_tls13_posthandshake.pas && ./tmp/tls13_posthandshake_adjacent/test_tls13_posthandshake
mkdir -p tmp/tls13_clienthello_parser_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_clienthello_parser_adjacent -FEtmp/tls13_clienthello_parser_adjacent -otmp/tls13_clienthello_parser_adjacent/test_tls13_clienthello_parser tests/test_tls13_clienthello_parser.pas && ./tmp/tls13_clienthello_parser_adjacent/test_tls13_clienthello_parser
mkdir -p tmp/tls13_finished_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_finished_adjacent -FEtmp/tls13_finished_adjacent -otmp/tls13_finished_adjacent/test_tls13_finished tests/test_tls13_finished.pas && ./tmp/tls13_finished_adjacent/test_tls13_finished
python3 scripts/compile_all_modules.py
git diff --check -- docs/plans/2026-03-25-freepascal-tls13-client-session-resumption-psk.md src/fafafa.ssl.freepascal.session.pas src/fafafa.ssl.tls13.clienthello.pas src/fafafa.ssl.tls13.clienthello.parser.pas src/fafafa.ssl.tls13.parser.pas src/fafafa.ssl.tls13.keyschedule.pas src/fafafa.ssl.tls13.appschedule.pas src/fafafa.ssl.freepascal.connection.pas src/fafafa.ssl.freepascal.lib.pas tests/test_tls13_resumption.pas tests/test_freepascal_client_session_resumption.pas tests/test_freepascal_backend_basic.pas tests/test_capability_cache.pas task_plan.md findings.md progress.md
```

Expected:
- ticket parser / clienthello parser / Finished / compile gate 继续绿
- diff hygiene 通过

**Step 3: Write back working memory**

- `task_plan.md` 记录 Phase 3 当前 batch = `client-side session resumption / PSK`
- `findings.md` 记录真实结论：
  - pure Pascal client-side resumption 已闭环
  - server-side PSK/0-RTT/validation parity 仍未完成
- `progress.md` 记录本批 RED/GREEN 与离线 resumed-connection 证据
