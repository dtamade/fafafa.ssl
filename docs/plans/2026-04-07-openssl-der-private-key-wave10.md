# OpenSSL DER Private-Key Wave 10 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不新增公开 API 的前提下，把 OpenSSL `ISSLContext.LoadPrivateKey(file/stream)` 的 DER 私钥支持从 wave9 继续推进到真实覆盖 EC SEC1 DER 私钥，并同步把 `SupportsDERPrivateKey` 收紧到包含“仅剩 EC SEC1 DER surface 仍可用”的 runtime truth。

**Architecture:** 延续 wave9 的 shared-buffer parser 与 capability-hardening 路线。实现继续集中在 `src/fafafa.ssl.openssl.context.pas` 与 `src/fafafa.ssl.openssl.backed.pas`，只补 OpenSSL 所需的最小 EC/EVP 动态绑定，不新增 `LoadPrivateKeyDER` 之类的新接口，不扩到 WinSSL，也不为 Ed25519 引入 raw DER 组装路径。解析顺序固定为：
- PEM
- encrypted DER PKCS#8
- DER PKCS#8
- DER PKCS#1 RSA
- DER SEC1 EC

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3` / `libcrypto.so.3`, OpenSSL PEM/PKCS/PKCS12/RSA/EC/EVP/BIO APIs, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Summary
- wave9 之后仍剩一条真实行为缺口：
  - EC 私钥 fixture 能稳定导出 PKCS#8 DER，但裸 SEC1 DER 仍不能被 `LoadPrivateKey(file/stream)` 消费
- capability 还剩一条 truth gap：
  - 当 PKCS#8 / RSA / PEM 读取 surface 都被 runtime-drift 清空，只剩 EC SEC1 DER surface 可用时，`SupportsDERPrivateKey` 仍必须保持 `True`
- 本轮默认范围：
  - 支持 EC SEC1 DER 私钥加载
  - 保持 EC / Ed25519 PKCS#8 DER 路径继续走现有 parser
  - 不新增公开 API
  - 不扩到 WinSSL
  - 不为 Ed25519 增加 raw DER / non-PKCS#8 私钥路径

## Delivery Order
1. 落 wave10 计划与台账入口。
2. 先用 focused RED contract 固化 EC SEC1 行为缺口与 capability fallback gap。
3. 再补最小 EC/EVP 绑定与 SEC1 parser/capability 接线。
4. 跑 focused regressions、repo baseline、diff hygiene，并回填 ledgers。

### Task 1: EC SEC1 RED Contracts

**Files:**
- Modify: `tests/test_openssl_context_der_private_key_contract.pas`
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Extend focused context contracts**
- 基于运行时导出的 EC fixture，补充：
  - EC DER PKCS#8 file/stream 成功路径
  - EC DER SEC1 file/stream 成功路径
  - encrypted EC DER PKCS#8 password 成功/失败路径
  - Ed25519 DER PKCS#8 与 encrypted DER PKCS#8 成功/失败路径
- helper-guard contract 必须继续覆盖 controlled failure，不允许 `EAccessViolation`

**Step 2: Extend capability RED contracts**
- 在 `tests/openssl/test_openssl_features.pas` 增加：
  - `ExpectedOpenSSLDERSEC1ECPrivateKeyReady`
  - “仅剩 EC SEC1 DER surface 仍可用时，`SupportsDERPrivateKey = True`” 的 fallback contract
  - no-DER-surface drift contract 继续要求 `SupportsDERPrivateKey = False`

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/openssl_context_der_private_key_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_der_private_key_contract -FEtmp/openssl_context_der_private_key_contract -otmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract tests/test_openssl_context_der_private_key_contract.pas && ./tmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL，集中暴露：
    - `LoadPrivateKey(file/stream)` 对 EC DER SEC1 仍未接线
    - `SupportsDERPrivateKey` 在 only-SEC1 fallback 下仍为错误真值

### Task 2: Minimal EC SEC1 Wiring

**Files:**
- Modify: `src/fafafa.ssl.openssl.api.ec.pas`
- Modify: `src/fafafa.ssl.openssl.api.evp.pas`
- Modify: `src/fafafa.ssl.openssl.context.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Add the minimum OpenSSL binding surface**
- 在 `src/fafafa.ssl.openssl.api.ec.pas` 增加 `d2i_ECPrivateKey`
- 在 `src/fafafa.ssl.openssl.api.evp.pas` 增加 `EVP_PKEY_set1_EC_KEY`
- 两者都保持 optional binding，不提高 required surface

**Step 2: Extend the shared parser with SEC1 EC**
- 在 `src/fafafa.ssl.openssl.context.pas` 增加 `TryLoadPrivateKeyFromDERSEC1EC`
- 解析顺序固定为：
  - PEM
  - encrypted DER PKCS#8
  - DER PKCS#8
  - DER PKCS#1 RSA
  - DER SEC1 EC
- 保持：
  - `LoadPrivateKeyPEM` 仍然是 PEM-only
  - Ed25519 继续只走已有 PKCS#8 path

**Step 3: Rewire capability truth source**
- 在 `src/fafafa.ssl.openssl.backed.pas` 增加 `OpenSSLDERSEC1ECPrivateKeySurfaceReady`
- `OpenSSLDERPrivateKeySurfaceReady` 与 `GetCapabilities` 里的 `SupportsDERPrivateKey` 必须纳入 SEC1 EC readiness
- `SupportsPKCS8PrivateKey` / `SupportsPasswordProtectedKeys` 仍不因为 EC SEC1 fallback 被错误拉高

**Step 4: Run GREEN**
- Re-run:
  - `mkdir -p tmp/openssl_context_der_private_key_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_der_private_key_contract -FEtmp/openssl_context_der_private_key_contract -otmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract tests/test_openssl_context_der_private_key_contract.pas && ./tmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - PASS

### Task 3: Focused Regressions And Ledger Closeout

**Files:**
- Modify: `docs/plans/2026-04-07-openssl-der-private-key-wave10.md`
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`

**Step 1: Run focused regressions**
- Run:
  - `mkdir -p tmp/openssl_context_der_private_key_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_der_private_key_contract -FEtmp/openssl_context_der_private_key_contract -otmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract tests/test_openssl_context_der_private_key_contract.pas && ./tmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
  - `mkdir -p tmp/helper_utilities && fpc -B -Fu./src -Fu./tests -FUtmp/helper_utilities -FEtmp/helper_utilities -otmp/helper_utilities/test_helper_utilities tests/test_helper_utilities.pas && ./tmp/helper_utilities/test_helper_utilities`
  - `mkdir -p tmp/openssl_context_bio_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_bio_contract -FEtmp/openssl_context_bio_contract -otmp/openssl_context_bio_contract/test_openssl_context_bio_contract tests/test_openssl_context_bio_contract.pas && ./tmp/openssl_context_bio_contract/test_openssl_context_bio_contract`
  - `mkdir -p tmp/pem_key_read_symbol_contract && fpc -B -Fu./src -Fu./tests -FUtmp/pem_key_read_symbol_contract -FEtmp/pem_key_read_symbol_contract -otmp/pem_key_read_symbol_contract/test_pem_key_read_symbol_contract tests/test_pem_key_read_symbol_contract.pas && ./tmp/pem_key_read_symbol_contract/test_pem_key_read_symbol_contract`
  - `mkdir -p tmp/pem_encrypted_privatekey_cipher_symbol_contract && fpc -B -Fu./src -Fu./tests -FUtmp/pem_encrypted_privatekey_cipher_symbol_contract -FEtmp/pem_encrypted_privatekey_cipher_symbol_contract -otmp/pem_encrypted_privatekey_cipher_symbol_contract/test_pem_encrypted_privatekey_cipher_symbol_contract tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas && ./tmp/pem_encrypted_privatekey_cipher_symbol_contract/test_pem_encrypted_privatekey_cipher_symbol_contract`
  - `mkdir -p tmp/cert_utils_generate_selfsigned_ec_keygen_family_contract && fpc -B -Fu./src -Fu./tests -FUtmp/cert_utils_generate_selfsigned_ec_keygen_family_contract -FEtmp/cert_utils_generate_selfsigned_ec_keygen_family_contract -otmp/cert_utils_generate_selfsigned_ec_keygen_family_contract/test_cert_utils_generate_selfsigned_ec_keygen_family_contract tests/test_cert_utils_generate_selfsigned_ec_keygen_family_contract.pas && ./tmp/cert_utils_generate_selfsigned_ec_keygen_family_contract/test_cert_utils_generate_selfsigned_ec_keygen_family_contract`
  - `mkdir -p tmp/cert_utils_ed25519_contract && fpc -B -Fu./src -Fu./tests -FUtmp/cert_utils_ed25519_contract -FEtmp/cert_utils_ed25519_contract -otmp/cert_utils_ed25519_contract/test_cert_utils_ed25519_contract tests/test_cert_utils_ed25519_contract.pas && ./tmp/cert_utils_ed25519_contract/test_cert_utils_ed25519_contract`

**Step 2: Run repo baseline**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.api.ec.pas src/fafafa.ssl.openssl.api.evp.pas src/fafafa.ssl.openssl.context.pas src/fafafa.ssl.openssl.backed.pas tests/test_openssl_context_der_private_key_contract.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-07-openssl-der-private-key-wave10.md`
- Expected:
  - 全部 exit `0`

**Step 3: Update ledgers**
- 在 `task_plan.md` 标记 wave10 完成，并把下一队列明确到：
  - 是否需要 raw Ed25519 / non-PKCS#8 DER 私钥支持
  - 是否需要公开的 DER-only helper/API
- 在 `findings.md` 记录：
  - OpenSSL context 现已真实接入 DER SEC1 EC 私钥
  - `SupportsDERPrivateKey` 已包含 only-EC-SEC1 fallback truth
  - `LoadPrivateKeyPEM` 仍然是 PEM-only
- 在 `progress.md` 记录：
  - RED/GREEN
  - fresh focused regressions
  - repo baseline
  - diff hygiene
