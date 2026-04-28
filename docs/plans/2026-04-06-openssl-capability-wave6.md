# OpenSSL Capability Wave 6 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩大设计面的前提下，把 OpenSSL capability matrix 中仍按版本号静态发布的 TLS 1.3 主干声明收紧到现成 runtime protocol probe。

**Architecture:** 延续 wave2-wave5 的 contract-hardening 路线，保持 strict alignment。本轮不新增 helper family，也不重新定义 protocol semantics；只复用现有 `IsProtocolSupported(sslProtocolTLS13)` / `RuntimeProbeProtocolSupport(...)` 作为真值源。所有 RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`，实现只落在 `src/fafafa.ssl.openssl.backed.pas`。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Scan Summary (2026-04-06)
- 当前 `src/fafafa.ssl.openssl.backed.pas:GetCapabilities` 仍有一条高价值 protocol/capability drift：
  - `SupportsTLS13 := (FVersionNumber >= $1010100F)`
- 这条静态发布还会直接影响几项派生公开能力：
  - `MaxTLSVersion`
  - `ZeroRTTSupport`
  - `EarlyDataSupport`
  - `PostHandshakeAuthSupport`
- 当前源码已经有更严格的 runtime truth source：
  - `TOpenSSLLibrary.IsProtocolSupported(sslProtocolTLS13)`
  - 其内部复用 `RuntimeProbeProtocolSupport(...)`
  - 真值取决于 `TLS_method` / `SSL_CTX_new` / `SSL_CTX_set_min_proto_version` / `SSL_CTX_set_max_proto_version`
- `tests/openssl/test_openssl_features.pas` 已有 protocol policy-aware harness：
  - `StubSetMinProtoPolicy`
  - `StubSetMaxProtoPolicy`
  - 但当前只验证 `IsProtocolSupported(...)`，还没有锁住 capability matrix 跟随 TLS 1.3 policy drift
- 当前不纳入本轮：
  - `SupportsDTLS`
  - key-format / callback / TPM / FIPS / secure-memory / hardware-acceleration 类布尔字段

## Delivery Order
1. 持久化计划与台账入口
2. 为 TLS 1.3 capability drift 写 RED contract
3. 对齐 `GetCapabilities`
4. 跑 focused regressions、repo baseline，并回填 ledgers

---

### Task 1: TLS 1.3 Capability Drift RED Contract

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Add the failing contract**
- 在 `tests/openssl/test_openssl_features.pas` 新增：
  - `TestTLS13CapabilityMatrixPolicyAwareContract`
- 约束：
  - 基线要求：`GetCapabilities.SupportsTLS13` 必须等于 `IsProtocolSupported(sslProtocolTLS13)`
  - drift 要求：若 proto-version setter 当前可用，则临时把 TLS 1.3 policy 设为 reject，并断言：
    - `SupportsTLS13 = False`
    - `MaxTLSVersion = sslProtocolTLS12`
    - `ZeroRTTSupport <> sslSupportStable`
    - `EarlyDataSupport <> sslSupportStable`
    - `PostHandshakeAuthSupport <> sslSupportStable`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL because capability matrix still claims TLS 1.3 from version floor alone

---

### Task 2: Minimal TLS 1.3 Capability Alignment

**Files:**
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Reuse the existing runtime probe**
- In `GetCapabilities`:
  - compute one local `LTLS13Ready := IsProtocolSupported(sslProtocolTLS13)`

**Step 2: Rewire dependent capability fields**
- Set:
  - `SupportsTLS13 := LTLS13Ready`
  - `MaxTLSVersion := sslProtocolTLS13` only when `LTLS13Ready`, otherwise `sslProtocolTLS12`
  - `ZeroRTTSupport` / `EarlyDataSupport` require `LTLS13Ready`
  - `PostHandshakeAuthSupport` require `LTLS13Ready`

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
- Reference: `docs/plans/2026-04-06-openssl-capability-wave6.md`

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
  - `git diff --check -- src/fafafa.ssl.openssl.backed.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-06-openssl-capability-wave6.md`

**Step 3: Update ledgers**
- Mark wave6 complete in `task_plan.md`
- Record the new runtime-aware TLS 1.3 capability semantics in `findings.md`
- Record RED/GREEN evidence and baseline outputs in `progress.md`
