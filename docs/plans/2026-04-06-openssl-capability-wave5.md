# OpenSSL Capability Wave 5 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩大设计面的前提下，继续把 OpenSSL capability matrix 中仍然静态发布的布尔字段收紧到现成 runtime truth source。本轮只处理 `SupportsChaChaPoly` 和 `SupportsPKCS12`。

**Architecture:** 延续 wave2-wave4 的 contract-hardening 路线，保持 strict alignment。本轮不新增 helper family，也不为缺失字段补新的 module-ready 设计；只复用现有 cipher parser 与 PKCS#12 API surface。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Scan Summary (2026-04-06)
- `src/fafafa.ssl.openssl.backed.pas:GetCapabilities` 当前仍有两条适合 strict-alignment 收口的静态字段：
  - `SupportsChaChaPoly := (FVersionNumber >= $1010000F)`
  - `SupportsPKCS12 := True`
- 这两项已经各自有可复用的 runtime truth source：
  - ChaCha20-Poly1305:
    - 现有 `TOpenSSLLibrary.IsCipherSupported(...)` 使用真实 OpenSSL parser
    - 对 `TLS_CHACHA20_POLY1305_SHA256` 的真值取决于 `TLS_method` / `SSL_CTX_new` / `SSL_CTX_set_ciphersuites`
  - PKCS#12:
    - `src/fafafa.ssl.openssl.api.pkcs12.pas` 已加载并发布核心 API surface
    - 本轮 readiness 以以下 4 个指针为准：
      - `PKCS12_create`
      - `PKCS12_parse`
      - `d2i_PKCS12_bio`
      - `i2d_PKCS12_bio`
- 当前不纳入本轮的字段：
  - `SupportsDTLS`
  - key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段

## Delivery Order
1. 持久化计划与台账入口
2. 为 `SupportsChaChaPoly` / `SupportsPKCS12` 写 RED contracts
3. 对齐 `GetCapabilities`
4. 跑 focused regressions、repo baseline，并回填 ledgers

---

### Task 1: RED Contracts

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Add the failing contracts**
- 在 `tests/openssl/test_openssl_features.pas` 新增：
  - ChaChaPoly capability contract
    - 基线要求：`GetCapabilities.SupportsChaChaPoly` 必须等于 `IsCipherSupported('TLS_CHACHA20_POLY1305_SHA256')`
    - drift 要求：若 `SSL_CTX_set_ciphersuites` 当前可用，则临时置 `nil` 后 `SupportsChaChaPoly = False`
  - PKCS12 capability contract
    - 基线要求：`GetCapabilities.SupportsPKCS12` 必须等于 PKCS12 core API surface readiness
    - drift 要求：若 `PKCS12_parse` 当前可用，则临时置 `nil` 后 `SupportsPKCS12 = False`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL because capability matrix still publishes ChaCha by version and PKCS12 as unconditional `True`

---

### Task 2: Minimal Capability Alignment

**Files:**
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Add local readiness helpers**
- Add:
  - `OpenSSLChaChaPolySurfaceReady`
  - `OpenSSLPKCS12SurfaceReady`

**Step 2: Rewire GetCapabilities**
- Set:
  - `SupportsChaChaPoly := OpenSSLChaChaPolySurfaceReady`
  - `SupportsPKCS12 := OpenSSLPKCS12SurfaceReady`
- `OpenSSLChaChaPolySurfaceReady` must follow the same runtime semantics as `IsCipherSupported('TLS_CHACHA20_POLY1305_SHA256')`
- `OpenSSLPKCS12SurfaceReady` must require:
  - `PKCS12_create`
  - `PKCS12_parse`
  - `d2i_PKCS12_bio`
  - `i2d_PKCS12_bio`

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
- Reference: `docs/plans/2026-04-06-openssl-capability-wave5.md`

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
  - `git diff --check -- src/fafafa.ssl.openssl.backed.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-06-openssl-capability-wave5.md`

**Step 3: Update ledgers**
- Mark wave5 complete in `task_plan.md`
- Record the new runtime-aware semantics in `findings.md`
- Record RED/GREEN evidence and baseline outputs in `progress.md`
