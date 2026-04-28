# OpenSSL SSL Runtime Wave 3 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 OpenSSL capability matrix 中剩余高价值的 `SessionTickets` / `PostHandshakeAuth` 漂移收紧到 runtime helper readiness，并补齐当前 host 已导出的 post-handshake-auth helper surface。

**Architecture:** 延续前两轮的 contract-hardening 路线。本轮不再扩大范围，只收两类已经对外暴露、但仍有“能力声明和 runtime surface 不一致”的字段：`SupportsSessionTickets` / `SessionTicketsSupport`，以及 `PostHandshakeAuthSupport`。其中 session tickets 只做 capability 对齐；post-handshake auth 先补 load contract，再让 capability matrix 与真实 helper readiness 对齐。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Scan Summary (2026-04-06)
- wave2 已经完成：
  - `early-data / 0-RTT` capability drift 已收口
  - `keylog / record-padding` load contract 已收口
  - `async / QUIC` load contract 已收口
- 当前剩余的 capability drift 集中在 `src/fafafa.ssl.openssl.backed.pas:GetCapabilities`：
  - `SupportsSessionTickets := True`
  - `SessionTicketsSupport := sslSupportStable`
  - `PostHandshakeAuthSupport := sslSupportStable`（仅按 TLS 1.3 版本号判断）
- 当前源码证据：
  - `TOpenSSLLibrary.IsFeatureSupported(sslFeatSessionTickets)` 已经是 runtime-aware：
    - `Assigned(SSL_CTX_set_tlsext_ticket_key_cb) or Assigned(SSL_set_session_ticket_ext_cb)`
  - 但 `GetCapabilities` 还没有与这条 runtime probe 对齐
  - `src/fafafa.ssl.openssl.api.ssl.pas` 目前根本没有声明/绑定：
    - `SSL_CTX_set_post_handshake_auth`
    - `SSL_set_post_handshake_auth`
    - `SSL_verify_client_post_handshake`
- 当前 host `libssl.so.3` probe 结果：
  - session tickets:
    - `SSL_CTX_set_tlsext_ticket_key_cb`: not exported
    - `SSL_set_session_ticket_ext_cb`: exported
  - post-handshake auth:
    - `SSL_CTX_set_post_handshake_auth`: exported
    - `SSL_set_post_handshake_auth`: exported
    - `SSL_verify_client_post_handshake`: exported

## Delivery Order
1. `SessionTickets` capability drift contract + alignment
2. `PostHandshakeAuth` load contract + capability alignment
3. regression chain + working-memory closeout

---

### Task 1: SessionTickets Capability Drift Alignment

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Write the failing capability-drift contract**
- In `tests/openssl/test_openssl_features.pas`:
  - add a new runtime-drift test for session tickets
  - load/initialize OpenSSL
  - probe `SSL_set_session_ticket_ext_cb`
  - if the host exports it, temporarily set `SSL_set_session_ticket_ext_cb := nil`
  - call `GetCapabilities`
  - assert:
    - `SupportsSessionTickets = False`
    - `SessionTicketsSupport <> sslSupportStable`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL because capability matrix still hard-codes `SupportsSessionTickets := True` and `SessionTicketsSupport := sslSupportStable`

**Step 3: Write minimal implementation**
- In `src/fafafa.ssl.openssl.backed.pas`:
  - add a local `OpenSSLSessionTicketSurfaceReady` helper mirroring the current session-ticket runtime probe semantics
  - set:
    - `SupportsSessionTickets := OpenSSLSessionTicketSurfaceReady`
    - `SessionTicketsSupport := sslSupportStable` only when the helper surface is ready, otherwise `sslSupportNone`

**Step 4: Run GREEN**
- Re-run the command from Step 2
- Expected:
  - PASS

---

### Task 2: PostHandshakeAuth Load Contract And Capability Alignment

**Files:**
- Create: `tests/test_openssl_ssl_post_handshake_contract.pas`
- Modify: `tests/openssl/test_openssl_features.pas`
- Modify: `src/fafafa.ssl.openssl.api.ssl.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Write the failing load contract**
- In `tests/test_openssl_ssl_post_handshake_contract.pas`:
  - load `libssl`
  - call `LoadOpenSSLSSL`
  - assert exported helpers are bound when exported:
    - `SSL_CTX_set_post_handshake_auth`
    - `SSL_set_post_handshake_auth`
    - `SSL_verify_client_post_handshake`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_ssl_post_handshake_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_post_handshake_contract -FEtmp/openssl_ssl_post_handshake_contract -otmp/openssl_ssl_post_handshake_contract/test_openssl_ssl_post_handshake_contract tests/test_openssl_ssl_post_handshake_contract.pas && ./tmp/openssl_ssl_post_handshake_contract/test_openssl_ssl_post_handshake_contract`
- Expected:
  - FAIL because the helper family is currently undeclared/unbound in `LoadOpenSSLSSL`

**Step 3: Add the failing capability-drift contract**
- In `tests/openssl/test_openssl_features.pas`:
  - add a runtime-drift check for post-handshake auth
  - probe `SSL_verify_client_post_handshake`
  - if the host exports it, temporarily set `SSL_verify_client_post_handshake := nil`
  - call `GetCapabilities`
  - assert:
    - `PostHandshakeAuthSupport <> sslSupportStable`

**Step 4: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL because capability matrix still claims stable support from TLS 1.3 version alone

**Step 5: Write minimal implementation**
- In `src/fafafa.ssl.openssl.api.ssl.pas`:
  - declare the post-handshake-auth function pointer types and globals
  - clear them in `ClearSSLFunctions`
  - bind them in `LoadOpenSSLSSL`
- In `src/fafafa.ssl.openssl.backed.pas`:
  - add a local `OpenSSLPostHandshakeAuthSurfaceReady` helper
  - only publish `PostHandshakeAuthSupport := sslSupportStable` when:
    - TLS 1.3 is supported
    - the post-handshake-auth helper surface is actually assigned at runtime

**Step 6: Run GREEN**
- Re-run the commands from Step 2 and Step 4
- Expected:
  - both focused contracts PASS

---

### Task 3: Regression Chain And Ledger

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `docs/plans/2026-04-06-openssl-ssl-runtime-wave3.md`

**Step 1: Run focused regression chain**
- Run:
  - `mkdir -p tmp/openssl_ssl_post_handshake_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_post_handshake_contract -FEtmp/openssl_ssl_post_handshake_contract -otmp/openssl_ssl_post_handshake_contract/test_openssl_ssl_post_handshake_contract tests/test_openssl_ssl_post_handshake_contract.pas && ./tmp/openssl_ssl_post_handshake_contract/test_openssl_ssl_post_handshake_contract`
  - `mkdir -p tmp/openssl_ssl_load_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_load_contract -FEtmp/openssl_ssl_load_contract -otmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract tests/test_openssl_ssl_load_contract.pas && ./tmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract`
  - `mkdir -p tmp/openssl_ssl_unload_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_unload_contract -FEtmp/openssl_ssl_unload_contract -otmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract tests/test_openssl_ssl_unload_contract.pas && ./tmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract`
  - `mkdir -p tmp/openssl_ssl_early_data_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_early_data_contract -FEtmp/openssl_ssl_early_data_contract -otmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract tests/test_openssl_ssl_early_data_contract.pas && ./tmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract`
  - `mkdir -p tmp/openssl_ssl_padding_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_padding_contract -FEtmp/openssl_ssl_padding_contract -otmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract tests/test_openssl_ssl_padding_contract.pas && ./tmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract`
  - `mkdir -p tmp/openssl_ssl_async_quic_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_async_quic_contract -FEtmp/openssl_ssl_async_quic_contract -otmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract tests/test_openssl_ssl_async_quic_contract.pas && ./tmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`

**Step 2: Run repo baseline**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.api.ssl.pas src/fafafa.ssl.openssl.backed.pas tests/test_openssl_ssl_post_handshake_contract.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-06-openssl-ssl-runtime-wave3.md`

**Step 3: Update ledgers**
- In `task_plan.md`:
  - mark wave3 tasks complete
- In `findings.md`:
  - record the new runtime-aware session-ticket capability semantics
  - record the post-handshake-auth helper family and capability alignment
- In `progress.md`:
  - record RED/GREEN evidence and repo baseline outputs

**Expected Outcome:**
- `SupportsSessionTickets` / `SessionTicketsSupport` 不再与 `IsFeatureSupported(sslFeatSessionTickets)` 分叉
- `PostHandshakeAuthSupport` 不再只按 TLS 1.3 版本号宣称稳定，而是和真实 helper surface 对齐
- `LoadOpenSSLSSL` 补齐当前 host 已导出的 post-handshake-auth helper family
- focused contracts 与 repo baseline 继续保持绿
