# OpenSSL DER Private-Key Wave 9 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不新增公开 API 的前提下，把 OpenSSL `ISSLContext.LoadPrivateKey(file/stream)` 从 PEM-only 推进到真实支持 DER PKCS#8、加密 DER PKCS#8 与 DER PKCS#1 RSA 私钥加载，并同步 capability truth source。

**Architecture:** 延续 wave2-wave8 的 contract-hardening 路线，继续先写 RED contract 再做最小实现。实现主战场放在 `src/fafafa.ssl.openssl.context.pas` 的私钥加载控制流，`src/fafafa.ssl.openssl.backed.pas` 只负责 capability 对齐；不新增 `LoadPrivateKeyDER` 之类的新接口，不扩到 EC/Ed25519 DER 私钥。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3` / `libcrypto.so.3`, OpenSSL PEM/PKCS/PKCS12/RSA/EVP/BIO APIs, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Summary
- 当前 OpenSSL context 的私钥加载仍然只接了 PEM surface：
  - 文件无密码：`SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)`
  - 文件有密码 / stream / PEM string：`PEM_read_bio_PrivateKey`
- 仓库里已有 DER 相关 API 绑定，但尚未接入 `TOpenSSLContext.LoadPrivateKey(...)`：
  - 未加密 PKCS#8：`d2i_PKCS8_PRIV_KEY_INFO` + `EVP_PKCS82PKEY`
  - 加密 PKCS#8：`d2i_X509_SIG` + `PKCS8_decrypt` + `EVP_PKCS82PKEY`
  - PKCS#1 RSA：`d2i_RSAPrivateKey` + `EVP_PKEY_new` + `EVP_PKEY_set1_RSA`
- 本轮默认范围：
  - 支持未加密 DER PKCS#8
  - 支持加密 DER PKCS#8
  - 支持 DER PKCS#1 RSA
  - 不新增公开 API
  - 不承诺裸 EC / Ed25519 DER 私钥

## Delivery Order
1. 落计划与台账入口。
2. 先给 DER 私钥 context 行为与 capability 写 RED contracts。
3. 再把 OpenSSL context/control-flow 与 capability truth source 接到真实 DER surface。
4. 跑 focused regressions、repo baseline、diff hygiene，并回填 ledgers。

### Task 1: DER Private-Key RED Contracts

**Files:**
- Create: `tests/test_openssl_context_der_private_key_contract.pas`
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Add focused context contracts**
- 新建 focused contract 程序，直接复用 `tests/certificate/test_certs/signer_key.pem`，运行时派生：
  - DER PKCS#8
  - DER PKCS#1 RSA
  - 加密 DER PKCS#8
- contract 必须覆盖：
  - `LoadPrivateKey(file)` 成功加载 DER PKCS#8
  - `LoadPrivateKey(file)` 成功加载 DER PKCS#1 RSA
  - `LoadPrivateKey(stream)` 成功加载 DER PKCS#8
  - `LoadPrivateKey(stream)` 成功加载 DER PKCS#1 RSA
  - `LoadPrivateKey(file/stream, password)` 成功加载加密 DER PKCS#8
  - 密码缺失或错误时抛出受控 `ESSLKeyException`
  - DER 相关 helper 被清空时抛出受控 `ESSLException`，不能出现 `EAccessViolation`

**Step 2: Add capability RED contracts**
- 在 `tests/openssl/test_openssl_features.pas` 增加 wave9 的 DER/key-format contracts：
  - baseline：DER surface ready 时 `SupportsDERPrivateKey = True`
  - DER PKCS#8 ready 但 PEM read surface 不可用时，`SupportsPKCS8PrivateKey = True`
  - 只有加密 DER PKCS#8 surface ready 时，`SupportsPasswordProtectedKeys = True`
  - DER helper surface 全部不可用时，`SupportsDERPrivateKey = False`

**Step 3: Run RED**
- Run:
  - `mkdir -p tmp/openssl_context_der_private_key_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_der_private_key_contract -FEtmp/openssl_context_der_private_key_contract -otmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract tests/test_openssl_context_der_private_key_contract.pas && ./tmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL，因为当前 context/control-flow 还没有真实 DER 私钥接线，capability 也还未发布 DER 真值。

### Task 2: Minimal DER Private-Key Wiring

**Files:**
- Modify: `src/fafafa.ssl.openssl.context.pas`
- Modify: `src/fafafa.ssl.openssl.backed.pas`
- Modify: `src/fafafa.ssl.openssl.api.pkcs12.pas`

**Step 1: Add shared DER private-key parser flow**
- 在 `src/fafafa.ssl.openssl.context.pas` 增加本地私有 helper，统一处理内存中的私钥 blob：
  - PEM first
  - encrypted DER PKCS#8 second
  - unencrypted DER PKCS#8 third
  - DER PKCS#1 RSA last
- helper 必须统一返回 `PEVP_PKEY`，并统一做资源清理。
- 文件路径逻辑：
  - 保留当前无密码 PEM 快路径
  - 快路径失败后回退到“读取文件字节 + shared parser”
  - 有密码时直接走“读取文件字节 + shared parser”
- stream 路径直接走 shared parser。

**Step 2: Fix loader semantics needed by runtime drift**
- 在 `src/fafafa.ssl.openssl.api.pkcs12.pas` 把 PKCS12 loader 补成与其它模块一致的状态管理：
  - 允许实现区判断“模块尚未加载时再 lazy-load”
  - 不能让 runtime-drift tests 中人为置空的符号被无条件重新绑定

**Step 3: Rewire capability truth source**
- 在 `src/fafafa.ssl.openssl.backed.pas` 增加 DER/private-key readiness helpers，并把字段改成：
  - `SupportsDERPrivateKey` 跟随真实 DER surface
  - `SupportsPKCS8PrivateKey` = PEM PKCS#8 or DER PKCS#8
  - `SupportsPasswordProtectedKeys` = PEM encrypted-key path or encrypted DER PKCS#8 path
- 其余 capability 字段保持不动。

**Step 4: Run GREEN**
- Re-run:
  - `mkdir -p tmp/openssl_context_der_private_key_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_der_private_key_contract -FEtmp/openssl_context_der_private_key_contract -otmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract tests/test_openssl_context_der_private_key_contract.pas && ./tmp/openssl_context_der_private_key_contract/test_openssl_context_der_private_key_contract`
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - PASS

### Task 3: Focused Regressions And Ledger Closeout

**Files:**
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

**Step 2: Run repo baseline**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.context.pas src/fafafa.ssl.openssl.backed.pas src/fafafa.ssl.openssl.api.pkcs12.pas tests/test_openssl_context_der_private_key_contract.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-07-openssl-der-private-key-wave9.md`
- Expected:
  - 全部 exit `0`

**Step 3: Update ledgers**
- 在 `task_plan.md` 标记 wave9 完成，并把下一队列明确到“若需要再扩到 EC/Ed25519 DER 或统一 DER-only public API”。
- 在 `findings.md` 记录：
  - OpenSSL context 现已真实接入 DER PKCS#8 / encrypted DER PKCS#8 / DER PKCS#1 RSA
  - `LoadPrivateKeyPEM` 仍然是 PEM-only
  - PKCS/PKCS12 loader lazy-load 语义已收紧，避免 runtime-drift 场景被偷偷重绑
- 在 `progress.md` 记录 RED/GREEN、focused regressions、baseline gate 和 diff hygiene 结果。
