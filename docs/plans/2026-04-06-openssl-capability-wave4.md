# OpenSSL Capability Wave 4 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 把 OpenSSL capability matrix 中剩余仍按常量或版本号发布的 SSL 能力声明，收紧到与当前 runtime probe 一致的 helper/module readiness 语义。

**Architecture:** 延续 wave2/wave3 的 contract-hardening 路线。本轮不再新增 helper family，也不扩大 `osmSSL` / `osmCT` required 语义，只处理公开 capability matrix 与现有 `TOpenSSLLibrary.IsFeatureSupported(...)` 分叉的字段。所有 RED/GREEN 都集中在 `tests/openssl/test_openssl_features.pas`，实现只落在 `src/fafafa.ssl.openssl.backed.pas`。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Scan Summary (2026-04-06)
- 当前 `src/fafafa.ssl.openssl.backed.pas:GetCapabilities` 仍存在几组 capability/runtime drift：
  - `SupportsALPN := True`
  - `SupportsSNI := True`
  - `SupportsOCSPStapling := True`
  - `SNISupport := sslSupportStable`
  - `ALPNSupport := sslSupportStable`
  - `OCSPStaplingSupport := sslSupportStable`
  - `SessionCacheSupport := sslSupportStable`
  - `RenegotiationSupport := sslSupportStable/Deprecated`（仅按版本号）
  - `SupportsCertificateTransparency := (FVersionNumber >= $1010000F)`
  - `CertTransparencySupport := stable/experimental/none`（仅按版本号）
- 当前 `TOpenSSLLibrary.IsFeatureSupported(...)` 已有更严格的 runtime probe 真值源：
  - `sslFeatSNI` => `Assigned(SSL_set_tlsext_host_name) or Assigned(SSL_CTX_set_tlsext_servername_callback)`
  - `sslFeatALPN` => `Assigned(SSL_CTX_set_alpn_protos) and Assigned(SSL_get0_alpn_selected)`
  - `sslFeatSessionCache` => `Assigned(SSL_CTX_set_session_cache_mode) and Assigned(SSL_CTX_get_session_cache_mode)`
  - `sslFeatRenegotiation` => `Assigned(SSL_renegotiate)`
  - `sslFeatOCSPStapling` => `Assigned(SSL_CTX_set_tlsext_status_type) and Assigned(SSL_CTX_set_tlsext_status_cb)`
  - `sslFeatCertificateTransparency` => `(FVersionNumber >= $1010000F) and TOpenSSLLoader.IsModuleLoaded(osmCT)`
- 当前 host 上 certificate-transparency 的正确边界应由 `osmCT` loaded state 控制，而不是仅按版本号宣称可用。
- `RenegotiationSupport` 的目标语义固定为：
  - helper ready + OpenSSL `< 3.0` => `sslSupportStable`
  - helper ready + OpenSSL `>= 3.0` => `sslSupportDeprecated`
  - helper missing => `sslSupportNone`

## Delivery Order
1. 持久化计划与台账入口
2. 为 capability drift 增加 focused RED 合约
3. 对齐 `GetCapabilities` 到 runtime readiness
4. 跑 focused regressions、repo baseline，并回填 ledgers

---

### Task 1: Capability Drift RED Contracts

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Add the failing contracts**
- 在 `tests/openssl/test_openssl_features.pas` 中新增 capability runtime-drift checks：
  - `SupportsSNI` / `SNISupport`
  - `SupportsALPN` / `ALPNSupport`
  - `SupportsOCSPStapling` / `OCSPStaplingSupport`
  - `SessionCacheSupport`
  - `RenegotiationSupport`
  - `SupportsCertificateTransparency` / `CertTransparencySupport`
- 约束：
  - SNI: 临时清空 `SSL_set_tlsext_host_name` 与 `SSL_CTX_set_tlsext_servername_callback`
  - ALPN: 临时清空 `SSL_CTX_set_alpn_protos`
  - OCSP stapling: 临时清空 `SSL_CTX_set_tlsext_status_cb`
  - Session cache: 临时清空 `SSL_CTX_get_session_cache_mode`
  - Renegotiation: 临时清空 `SSL_renegotiate`
  - CT: 用 `TOpenSSLLoader.SetModuleLoaded(osmCT, False)` 暂时关闭模块 loaded state，并在 finally 中恢复

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL，因为当前 capability matrix 仍把这些字段按常量或版本号发布为 supported/stable

---

### Task 2: Minimal Capability Alignment

**Files:**
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Add local readiness helpers**
- 在 `src/fafafa.ssl.openssl.backed.pas` 增加本地 readiness helpers，语义与当前 `IsFeatureSupported(...)` 对齐：
  - `OpenSSLSNISurfaceReady`
  - `OpenSSLALPNSurfaceReady`
  - `OpenSSLSessionCacheSurfaceReady`
  - `OpenSSLRenegotiationSurfaceReady`
  - `OpenSSLOCSPStaplingSurfaceReady`
  - `OpenSSLCertificateTransparencySurfaceReady`

**Step 2: Rewire GetCapabilities**
- 用上述 helpers 重写以下字段：
  - `SupportsSNI`
  - `SupportsALPN`
  - `SupportsOCSPStapling`
  - `SupportsCertificateTransparency`
  - `SNISupport`
  - `ALPNSupport`
  - `OCSPStaplingSupport`
  - `SessionCacheSupport`
  - `RenegotiationSupport`
  - `CertTransparencySupport`
- 支持级别规则：
  - helper ready => `sslSupportStable`，除非该能力被版本语义标记为 deprecated/experimental
  - CT: helper ready + OpenSSL `>= 3.0` => `sslSupportStable`; helper ready + `>= 1.1.0` and `< 3.0` => `sslSupportExperimental`; helper missing => `sslSupportNone`
  - Renegotiation: helper missing => `sslSupportNone`

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
- Reference: `docs/plans/2026-04-06-openssl-capability-wave4.md`

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
- Release-strategy note: Rust commands default to `--release`, but this batch不涉及 Rust 构建。
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.backed.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-06-openssl-capability-wave4.md`

**Step 3: Update ledgers**
- 在 `task_plan.md` 标记 wave4 任务完成
- 在 `findings.md` 记录 capability matrix 已与 runtime probe 对齐的具体字段与边界
- 在 `progress.md` 记录 RED/GREEN 证据、focused regressions 与 baseline 结果

**Expected Outcome:**
- OpenSSL capability matrix 不再对 SNI / ALPN / OCSP / session cache / renegotiation / CT 继续发布与 runtime surface 分叉的 supported/stable 声明
- `RenegotiationSupport` 和 `CertTransparencySupport` 既保留版本语义，也要求真实 runtime readiness
- focused contracts 与 repo baseline 继续保持绿
