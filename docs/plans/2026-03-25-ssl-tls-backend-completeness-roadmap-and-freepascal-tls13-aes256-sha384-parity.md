# SSL/TLS Backend Completeness Roadmap And FreePascal TLS 1.3 AES256/SHA384 Parity Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把仓库主线从“旧 family 收口”切换到“SSL/TLS 接口与后端完整度推进”，并完成第一批 pure Pascal backend 向 rustls 级别能力收敛的实现：打通 `TLS_AES_256_GCM_SHA384`。

**Architecture:** 当前公共接口面已经覆盖 library/context/connection/session/capabilities，但后端实现完成度不一致。第一批不追求一次性补齐所有缺口，而是先完成 pure Pascal TLS 1.3 cipher-suite parity 的一条闭环路径：配置面、advertise 面、select 面、Finished 面、capabilities 面统一对齐，再以此为模板推进 session resumption/PSK、0-RTT、OCSP/CT 等后续 family。

**Tech Stack:** FreePascal / Pascal, `fpc`, pure TLS 1.3 primitives, TDD, file-based working memory.

---

## Roadmap

### Phase 0: Definition Of Done

本主线的完成标准不是“接口都声明过”，而是下面四层都收敛：

1. 公共接口 inventory 完整，且能映射到每个后端的真实实现状态。
2. 每个后端都有 capability matrix，明确区分：
   - implemented
   - partial / experimental
   - unsupported by design
   - missing / drifted
3. pure Pascal backend 至少达到 rustls 风格的现代 TLS 1.3 基线：
   - 完整 TLS 1.3 cipher-suite support
   - 稳定的 SNI / ALPN
   - 可用的 session resumption
   - 明确的 post-handshake / KeyUpdate 行为
   - 能力声明与实际实现一致
4. 所有结论都有 focused regression 和相邻回归证据，而不是文档猜测。

### Phase 1: Interface And Backend Completeness Audit

**目标**
- 建立 `ISSLLibrary` / `ISSLContext` / `ISSLConnection` / `ISSLClientConnection` 到五个后端的完整度矩阵。

**关键文件**
- `src/fafafa.ssl.base.pas`
- `src/fafafa.ssl.factory.pas`
- `src/fafafa.ssl.openssl.backed.pas`
- `src/fafafa.ssl.winssl.lib.pas`
- `src/fafafa.ssl.mbedtls.lib.pas`
- `src/fafafa.ssl.wolfssl.lib.pas`
- `src/fafafa.ssl.freepascal.lib.pas`
- `src/fafafa.ssl.freepascal.context.pas`
- `src/fafafa.ssl.freepascal.connection.pas`

**完成条件**
- working memory 顶部不再把“closeout ledger”误当成“产品完成度”。
- 形成 backend completeness matrix 和 pure Pascal backend backlog。

### Phase 2: Pure Pascal TLS 1.3 Cipher-Suite Parity

**目标**
- 让 pure Pascal backend 真正打通 `TLS_AES_256_GCM_SHA384`。

**范围**
- ClientHello advertize
- server cipher intersection
- suite-aware Finished key / verify_data
- capability / KnownIssues / `IsCipherSupported`

**不在本批范围**
- PSK / resumption
- 0-RTT / early data
- OCSP stapling / CT
- post-handshake auth

### Phase 3: Session Resumption And PSK

**目标**
- 把 current parsed `NewSessionTicket` 状态提升为可实际复用的 session resumption contract。

### Phase 4: Advanced TLS 1.3 Features

**目标**
- 0-RTT / early data / post-handshake auth / key update hardening。

### Phase 5: Validation And PKI Parity

**目标**
- OCSP / CT / revocation / certificate-path hardening。

---

## Task 1: Write The Failing Tests For TLS 1.3 AES256/SHA384 Parity

**Files:**
- Modify: `tests/test_tls13_finished.pas`
- Modify: `tests/test_tls13_clienthello_parser.pas`
- Modify: `tests/test_tls13_foundation.pas`
- Modify: `tests/test_freepascal_backend_basic.pas`
- Modify: `tests/test_capability_cache.pas`

**Step 1: Add focused failing coverage**

- `tests/test_tls13_finished.pas`
  - 增加 SHA384 suite 的 Finished key / verify_data / verify round-trip。
- `tests/test_tls13_clienthello_parser.pas`
  - 断言 generated ClientHello 同时 advertize:
    - `TLS_AES_256_GCM_SHA384`
    - `TLS_CHACHA20_POLY1305_SHA256`
    - `TLS_AES_128_GCM_SHA256`
- `tests/test_tls13_foundation.pas`
  - 更新 ClientHello cipher suite encoding 断言，从单套件改成三套件顺序断言。
- `tests/test_freepascal_backend_basic.pas`
  - 断言 `Lib.IsCipherSupported('TLS_AES_256_GCM_SHA384') = True`
  - 断言 `SupportedCiphers` 包含 `sslCipherAES256GCM`
- `tests/test_capability_cache.pas`
  - 断言 FreePascal `KnownIssues` 不再宣称 SHA384 Finished pending
  - 断言 capability / runtime advertisement 已对齐

**Step 2: Run tests to verify they fail**

Run:

```bash
mkdir -p tmp/tls13_finished_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_finished_red -FEtmp/tls13_finished_red -otmp/tls13_finished_red/test_tls13_finished tests/test_tls13_finished.pas && ./tmp/tls13_finished_red/test_tls13_finished
mkdir -p tmp/tls13_clienthello_parser_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_clienthello_parser_red -FEtmp/tls13_clienthello_parser_red -otmp/tls13_clienthello_parser_red/test_tls13_clienthello_parser tests/test_tls13_clienthello_parser.pas && ./tmp/tls13_clienthello_parser_red/test_tls13_clienthello_parser
mkdir -p tmp/tls13_foundation_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation_red -FEtmp/tls13_foundation_red -otmp/tls13_foundation_red/test_tls13_foundation tests/test_tls13_foundation.pas && ./tmp/tls13_foundation_red/test_tls13_foundation
mkdir -p tmp/freepascal_backend_basic_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic_red -FEtmp/freepascal_backend_basic_red -otmp/tmp_freepascal_backend_basic_red tests/test_freepascal_backend_basic.pas && ./tmp/tmp_freepascal_backend_basic_red
mkdir -p tmp/capability_cache_red && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache_red -FEtmp/capability_cache_red -otmp/tls13_capability_cache_red tests/test_capability_cache.pas && ./tmp/tls13_capability_cache_red
```

Expected:
- 至少有一组失败明确指向：
  - SHA384 Finished helper 缺口
  - ClientHello advertize 缺口
  - FreePascal capability/runtime drift

## Task 2: Implement Minimal Production Changes

**Files:**
- Modify: `src/fafafa.ssl.tls13.finished.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`
- Modify: `src/fafafa.ssl.freepascal.lib.pas`

**Step 1: Add suite-aware Finished helpers**

- 在 `src/fafafa.ssl.tls13.finished.pas`：
  - 保留 SHA256 helper
  - 增加 SHA384 helper
  - 增加 suite-aware helper，按 cipher suite 选择 SHA256 / SHA384

**Step 2: Wire suite-aware Finished into FreePascal connection**

- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - server Finished 校验改为按 `ACipherSuite` / selected suite 走 suite-aware transcript hash 和 verify_data
  - client Finished 发送改为按 suite-aware helper
  - server-side Finished 生成与 client Finished 校验同样改成 suite-aware

**Step 3: Expand ClientHello cipher advertisement**

- 在 `src/fafafa.ssl.tls13.clienthello.pas`：
  - generated ClientHello advertize 三个 TLS 1.3 suites
  - 顺序与默认 cipher preference 保持一致：`AES256 -> CHACHA20 -> AES128`

**Step 4: Expand server-side suite intersection**

- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - server 在 ClientHello offered suites 中允许选中 `TLS_AES_256_GCM_SHA384`

**Step 5: Align capabilities**

- 在 `src/fafafa.ssl.freepascal.lib.pas`：
  - `IsCipherSupported('TLS_AES_256_GCM_SHA384') = True`
  - `SupportedCiphers` 包含 `sslCipherAES256GCM`
  - `KnownIssues` 删除 SHA384 Finished pending，保留 PSK/resumption in progress
  - 兼容度分数按当前实现适度提升

## Task 3: Verify Green And Adjacent Regressions

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Re-run focused tests**

Run:

```bash
mkdir -p tmp/tls13_finished && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_finished -FEtmp/tls13_finished -otmp/tls13_finished/test_tls13_finished tests/test_tls13_finished.pas && ./tmp/tls13_finished/test_tls13_finished
mkdir -p tmp/tls13_clienthello_parser && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_clienthello_parser -FEtmp/tls13_clienthello_parser -otmp/tls13_clienthello_parser/test_tls13_clienthello_parser tests/test_tls13_clienthello_parser.pas && ./tmp/tls13_clienthello_parser/test_tls13_clienthello_parser
mkdir -p tmp/tls13_foundation && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_foundation -FEtmp/tls13_foundation -otmp/tls13_foundation/test_tls13_foundation tests/test_tls13_foundation.pas && ./tmp/tls13_foundation/test_tls13_foundation
mkdir -p tmp/freepascal_backend_basic && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/freepascal_backend_basic -FEtmp/freepascal_backend_basic -otmp/tmp_freepascal_backend_basic tests/test_freepascal_backend_basic.pas && ./tmp/tmp_freepascal_backend_basic
mkdir -p tmp/capability_cache && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/capability_cache -FEtmp/capability_cache -otmp/tls13_capability_cache tests/test_capability_cache.pas && ./tmp/tls13_capability_cache
```

Expected:
- 全部 PASS

**Step 2: Run adjacent regressions**

Run:

```bash
mkdir -p tmp/tls13_keyschedule_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_keyschedule_adjacent -FEtmp/tls13_keyschedule_adjacent -otmp/tls13_keyschedule_adjacent/test_tls13_keyschedule tests/test_tls13_keyschedule.pas && ./tmp/tls13_keyschedule_adjacent/test_tls13_keyschedule
mkdir -p tmp/tls13_appschedule_adjacent && fpc -B -Fu./src -Fu./tests -Fu./tests/framework -FUtmp/tls13_appschedule_adjacent -FEtmp/tls13_appschedule_adjacent -otmp/tls13_appschedule_adjacent/test_tls13_appschedule tests/test_tls13_appschedule.pas && ./tmp/tls13_appschedule_adjacent/test_tls13_appschedule
python3 scripts/compile_all_modules.py
```

Expected:
- 相邻 TLS 1.3 primitives 继续绿
- compile gate 继续通过

**Step 3: Write back working memory**

- `task_plan.md` 记录新的主线已从 closeout ledger 切换到 completeness roadmap
- `findings.md` 记录当前真实结论：pure Pascal backend 已完成 SHA384 cipher-suite parity，但 PSK/resumption 等仍未完成
- `progress.md` 记录本批 RED/GREEN 证据
