# OpenSSL Capability Wave 7 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩大设计面的前提下，把 OpenSSL capability matrix 中仍然硬编码为 `True` 的 `SupportsDTLS` 收紧到真实 DTLS runtime protocol probe，并先补强这条 probe 本身。

**Architecture:** 延续 wave2-wave6 的 contract-hardening 路线，保持 strict alignment。本轮不引入新的 capability family，也不扩展到 key-format / callback / platform / perf 布尔字段；只处理 OpenSSL DTLS protocol truth source 与 `SupportsDTLS` 之间的分叉。所有 RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`，实现继续只落在 `src/fafafa.ssl.openssl.backed.pas`。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Scan Summary (2026-04-07)
- 当前 `src/fafafa.ssl.openssl.backed.pas:GetCapabilities` 仍有一条明显 capability/runtime drift：
  - `SupportsDTLS := True`
- 这条公开能力当前没有跟随 runtime protocol truth：
  - 其它 C-library backend 已经采用：
    - `SupportsDTLS = IsProtocolSupported(sslProtocolDTLS10) or IsProtocolSupported(sslProtocolDTLS12)`
- 但 OpenSSL 当前 `TOpenSSLLibrary.IsProtocolSupported(...)` 对 DTLS 仍然过宽：
  - `sslProtocolDTLS10`, `sslProtocolDTLS12`
  - `=> Assigned(DTLS_method) or (FVersionNumber >= $10000000)`
- 当前源码已具备补强 probe 所需的全部运行时表面：
  - method constructors:
    - `DTLS_method`
    - `DTLS_client_method`
    - `DTLS_server_method`
  - context lifecycle:
    - `SSL_CTX_new`
    - `SSL_CTX_free`
  - proto-version setters:
    - `SSL_CTX_set_min_proto_version`
    - `SSL_CTX_set_max_proto_version`
  - DTLS constants:
    - `DTLS1_VERSION`
    - `DTLS1_2_VERSION`
- 本轮明确不纳入：
  - `DTLSSupport` support-level 字段扩展（当前 capability matrix 没有该字段）
  - key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段

## Delivery Order
1. 持久化计划与台账入口
2. 为 DTLS protocol/capability drift 写 RED contract
3. 补强 DTLS runtime probe，并对齐 `SupportsDTLS`
4. 跑 focused regressions、repo baseline，并回填 ledgers

---

### Task 1: DTLS Policy-Aware RED Contract

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Add the failing contract**
- 在 `tests/openssl/test_openssl_features.pas` 新增：
  - `StubRejectDTLSSetMinProtoPolicy`
  - `StubRejectDTLSSetMaxProtoPolicy`
  - `TestDTLSCapabilityMatrixPolicyAwareContract`
- 约束：
  - baseline 要求：
    - `SupportsDTLS = (IsProtocolSupported(sslProtocolDTLS10) or IsProtocolSupported(sslProtocolDTLS12))`
  - drift 要求：若 proto-version setter 当前可用，则临时把 DTLS 1.0 / 1.2 policy 全部设为 reject，并断言：
    - `IsProtocolSupported(sslProtocolDTLS10) = False`
    - `IsProtocolSupported(sslProtocolDTLS12) = False`
    - `SupportsDTLS = False`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL because current DTLS probe ignores runtime proto-version setter policy and capability matrix still hardcodes `True`

---

### Task 2: Minimal DTLS Probe Hardening And Capability Alignment

**Files:**
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Reuse the existing runtime probe path for DTLS**
- In `ProtocolToOpenSSLVersion(...)`:
  - add DTLS mappings:
    - `sslProtocolDTLS10 => DTLS1_VERSION`
    - `sslProtocolDTLS12 => DTLS1_2_VERSION`
- In `RuntimeProbeProtocolSupport(...)`:
  - keep the existing TLS path intact
  - add DTLS method selection using:
    - `DTLS_method`
    - fallback `DTLS_client_method`
    - fallback `DTLS_server_method`
  - reuse the same `SSL_CTX_new` + min/max proto setter probe flow
- In `IsProtocolSupported(...)`:
  - make `sslProtocolDTLS10` / `sslProtocolDTLS12` reuse `RuntimeProbeProtocolSupport(...)`

**Step 2: Rewire the public capability**
- In `GetCapabilities`:
  - set `SupportsDTLS := IsProtocolSupported(sslProtocolDTLS10) or IsProtocolSupported(sslProtocolDTLS12)`

**Step 3: Run GREEN**
- Re-run the command from Task 1 Step 2
- Expected:
  - PASS

---

### Task 3: Regression Chain And Ledger Closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Reference: `docs/plans/2026-04-07-openssl-capability-wave7.md`

**Step 1: Run focused regressions**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
  - `mkdir -p tmp/openssl_ssl_post_handshake_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_post_handshake_contract -FEtmp/openssl_ssl_post_handshake_contract -otmp/openssl_ssl_post_handshake_contract/test_openssl_ssl_post_handshake_contract tests/test_openssl_ssl_post_handshake_contract.pas && ./tmp/openssl_ssl_post_handshake_contract/test_openssl_ssl_post_handshake_contract`
  - `mkdir -p tmp/openssl_ssl_load_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_load_contract -FEtmp/openssl_ssl_load_contract -otmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract tests/test_openssl_ssl_load_contract.pas && ./tmp/openssl_ssl_load_contract/test_openssl_ssl_load_contract`
  - `mkdir -p tmp/openssl_ssl_unload_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_unload_contract -FEtmp/openssl_ssl_unload_contract -otmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract tests/test_openssl_ssl_unload_contract.pas && ./tmp/openssl_ssl_unload_contract/test_openssl_ssl_unload_contract`
  - `mkdir -p tmp/openssl_ssl_early_data_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_early_data_contract -FEtmp/openssl_ssl_early_data_contract -otmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract tests/test_openssl_ssl_early_data_contract.pas && ./tmp/openssl_ssl_early_data_contract/test_openssl_ssl_early_data_contract`
  - `mkdir -p tmp/openssl_ssl_padding_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_padding_contract -FEtmp/openssl_ssl_padding_contract -otmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract tests/test_openssl_ssl_padding_contract.pas && ./tmp/openssl_ssl_padding_contract/test_openssl_ssl_padding_contract`
  - `mkdir -p tmp/openssl_ssl_async_quic_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_ssl_async_quic_contract -FEtmp/openssl_ssl_async_quic_contract -otmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract tests/test_openssl_ssl_async_quic_contract.pas && ./tmp/openssl_ssl_async_quic_contract/test_openssl_ssl_async_quic_contract`

**Step 2: Run repo baseline**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.backed.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-07-openssl-capability-wave7.md`

**Step 3: Update ledgers**
- Mark wave7 complete in `task_plan.md`
- Record the DTLS runtime-aware capability semantics in `findings.md`
- Record RED/GREEN evidence and baseline outputs in `progress.md`
