# OpenSSL Capability Wave 8 Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 在不扩设计面的前提下，把 OpenSSL capability matrix 里的 key-format 相关布尔值收口到当前真实可达的私钥加载表面，消除 `SupportsPEMPrivateKey` / `SupportsDERPrivateKey` / `SupportsPKCS8PrivateKey` / `SupportsPasswordProtectedKeys` 的硬编码漂移。

**Architecture:** 继续沿用 wave2-wave7 的 contract-hardening 路线，只修 capability truth source，不新增 DER 解析功能，不改 `ISSLContext` 公共接口。所有 RED/GREEN 继续集中在 `tests/openssl/test_openssl_features.pas`，实现只落在 `src/fafafa.ssl.openssl.backed.pas`。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL `libssl.so.3` / `libcrypto.so.3`, Pascal program-style contract tests, existing repo compile/minimal CI gates.

---

## Summary
- 当前 OpenSSL 后端公开能力里，4 个 key-format 相关布尔值仍是硬编码 `True`。
- 已确认当前真实加载表面：
  - 文件无密码路径走 `SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)`。
  - 文件有密码、流、PEM 字符串路径走 `PEM_read_bio_PrivateKey`。
  - `Initialize` 默认不会加载 `osmPEM`，所以 capability 不能只看“当前指针是否已预加载”。
- 已确认当前不存在真实 DER 私钥加载路径：
  - 没有 `SSL_FILETYPE_ASN1` 私钥上下文接入。
  - 没有 `d2i_*PrivateKey*` / `d2i_PKCS8_PRIV_KEY_INFO` / `EVP_PKCS82PKEY` 被接入 `ISSLContext.LoadPrivateKey`。
- 已通过非仓库探针确认：
  - `tests/certificate/test_certs/signer_key.pem` 是 `BEGIN PRIVATE KEY` 的 PKCS#8 PEM。
  - 即使将 `PEM_read_bio_PrivateKey := nil`，`Ctx.LoadPrivateKey('tests/certificate/test_certs/signer_key.pem')` 仍可通过文件快路径成功加载。
- 本轮公共接口变化：
  - 无新增/删除 API。
  - 仅改变 `GetCapabilities` 返回值语义，使其与当前可达运行时表面对齐。

## Defaults And Assumptions
- 本轮采用“至少一条当前公开加载路径可用即可声明支持”的 capability 语义，而不是“所有 overload 都可用才声明支持”。
- `SupportsPasswordProtectedKeys` 采用更窄语义：必须存在实际密码私钥读取路径，不能拿导出/写出 helper 反推。
- `SupportsPKCS8PrivateKey` 在本轮明确表示“当前可加载 PKCS#8 私钥”，以 PEM 载体为准；不外推到 DER PKCS#8。
- `SupportsDERPrivateKey` 本轮直接收口为 `False`，直到仓库里存在真实的 OpenSSL DER 私钥上下文加载路径。
- 本轮不改 `src/fafafa.ssl.openssl.context.pas` 的加载控制流，不补 DER fallback，不补 PKCS API 接线。

## Delivery Order
1. 落计划与台账入口。
2. 先给 key-format capability 写 RED contract。
3. 再把 OpenSSL capability truth source 收口到真实运行时表面。
4. 跑 focused regressions、repo baseline，并回填 ledgers。

### Task 1: Key-Format Capability RED Contract

**Files:**
- Modify: `tests/openssl/test_openssl_features.pas`

**Step 1: Add the failing contracts**
- 在测试文件新增 3 组 fresh-instance contract，全部使用 `TOpenSSLLibrary.Create`，避免 capability cache 混入。
- 新增一个本地 helper，用于计算期望值，语义固定为：
  - `ExpectedPEMOrPKCS8 := Assigned(SSL_CTX_use_PrivateKey_file) or (Assigned(PEM_read_bio_PrivateKey) and Assigned(BIO_free) and (Assigned(BIO_new_file) or Assigned(BIO_new_mem_buf)))`
  - `ExpectedPasswordProtected := Assigned(PEM_read_bio_PrivateKey) and Assigned(BIO_free) and (Assigned(BIO_new_file) or Assigned(BIO_new_mem_buf))`
- 具体 contract：
  - baseline contract：
    - `SupportsPEMPrivateKey = ExpectedPEMOrPKCS8`
    - `SupportsPKCS8PrivateKey = ExpectedPEMOrPKCS8`
    - `SupportsPasswordProtectedKeys = ExpectedPasswordProtected`
    - `SupportsDERPrivateKey = False`
  - drift contract 1：
    - 暂时把 `PEM_read_bio_PrivateKey := nil`
    - 断言 `SupportsPasswordProtectedKeys = False`
    - 断言 `SupportsPEMPrivateKey = Assigned(SSL_CTX_use_PrivateKey_file)`
    - 断言 `SupportsPKCS8PrivateKey = Assigned(SSL_CTX_use_PrivateKey_file)`
  - drift contract 2：
    - 暂时把 `PEM_read_bio_PrivateKey := nil`
    - 暂时把 `SSL_CTX_use_PrivateKey_file := nil`
    - 断言 `SupportsPEMPrivateKey = False`
    - 断言 `SupportsPKCS8PrivateKey = False`
    - 断言 `SupportsPasswordProtectedKeys = False`
    - 断言 `SupportsDERPrivateKey = False`

**Step 2: Run RED**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - FAIL，因为当前 `src/fafafa.ssl.openssl.backed.pas` 仍把 4 个字段硬编码为 `True`。

### Task 2: Minimal Capability Truth-Source Alignment

**Files:**
- Modify: `src/fafafa.ssl.openssl.backed.pas`

**Step 1: Add small private-key surface helpers**
- 在实现区新增 3 个 helper，不改公开接口：
  - `OpenSSLPrivateKeyReadSurfaceReady`
  - `OpenSSLPrivateKeyFileSurfaceReady`
  - `OpenSSLPasswordProtectedKeySurfaceReady`
- helper 语义固定为：
  - `OpenSSLPrivateKeyFileSurfaceReady`：
    - 只检查 `Assigned(SSL_CTX_use_PrivateKey_file)`
  - `OpenSSLPrivateKeyReadSurfaceReady`：
    - 若 `PEM_read_bio_PrivateKey` 尚未绑定且 `osmPEM` 未加载，则调用 `LoadOpenSSLPEM(GetCryptoLibHandle)`
    - 最终要求：
      - `Assigned(PEM_read_bio_PrivateKey)`
      - `Assigned(BIO_free)`
      - `Assigned(BIO_new_file) or Assigned(BIO_new_mem_buf)`
  - `OpenSSLPasswordProtectedKeySurfaceReady`：
    - 直接复用 `OpenSSLPrivateKeyReadSurfaceReady`

**Step 2: Rewire the capability fields**
- 在 `GetCapabilities` 中把 4 个字段改成：
  - `SupportsPEMPrivateKey := OpenSSLPrivateKeyFileSurfaceReady or OpenSSLPrivateKeyReadSurfaceReady`
  - `SupportsPKCS8PrivateKey := OpenSSLPrivateKeyFileSurfaceReady or OpenSSLPrivateKeyReadSurfaceReady`
  - `SupportsPasswordProtectedKeys := OpenSSLPasswordProtectedKeySurfaceReady`
  - `SupportsDERPrivateKey := False`
- 其余 capability 字段保持不动。
- 不引入 `LoadOpenSSLPKCS(...)`、不接 `d2i_PKCS8_PRIV_KEY_INFO`、不新增 DER loader。
- 不改现有 capability cache 行为；仍由 fresh-instance tests 覆盖 runtime drift。

**Step 3: Run GREEN**
- Re-run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
- Expected:
  - PASS

### Task 3: Focused Regressions And Ledger Closeout

**Files:**
- Modify: `task_plan.md`
- Modify: `findings.md`
- Modify: `progress.md`
- Create: `docs/plans/2026-04-07-openssl-capability-wave8.md`

**Step 1: Run focused regressions**
- Run:
  - `mkdir -p tmp/openssl_features && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_features -FEtmp/openssl_features -otmp/openssl_features/test_openssl_features tests/openssl/test_openssl_features.pas && ./tmp/openssl_features/test_openssl_features`
  - `mkdir -p tmp/helper_utilities && fpc -B -Fu./src -Fu./tests -FUtmp/helper_utilities -FEtmp/helper_utilities -otmp/helper_utilities/test_helper_utilities tests/test_helper_utilities.pas && ./tmp/helper_utilities/test_helper_utilities`
  - `mkdir -p tmp/openssl_context_bio_contract && fpc -B -Fu./src -Fu./tests -FUtmp/openssl_context_bio_contract -FEtmp/openssl_context_bio_contract -otmp/openssl_context_bio_contract/test_openssl_context_bio_contract tests/test_openssl_context_bio_contract.pas && ./tmp/openssl_context_bio_contract/test_openssl_context_bio_contract`
  - `mkdir -p tmp/pem_key_read_symbol_contract && fpc -B -Fu./src -Fu./tests -FUtmp/pem_key_read_symbol_contract -FEtmp/pem_key_read_symbol_contract -otmp/pem_key_read_symbol_contract/test_pem_key_read_symbol_contract tests/test_pem_key_read_symbol_contract.pas && ./tmp/pem_key_read_symbol_contract/test_pem_key_read_symbol_contract`
  - `mkdir -p tmp/pem_encrypted_privatekey_cipher_symbol_contract && fpc -B -Fu./src -Fu./tests -FUtmp/pem_encrypted_privatekey_cipher_symbol_contract -FEtmp/pem_encrypted_privatekey_cipher_symbol_contract -otmp/pem_encrypted_privatekey_cipher_symbol_contract/test_pem_encrypted_privatekey_cipher_symbol_contract tests/test_pem_encrypted_privatekey_cipher_symbol_contract.pas && ./tmp/pem_encrypted_privatekey_cipher_symbol_contract/test_pem_encrypted_privatekey_cipher_symbol_contract`

**Step 2: Run repo baseline**
- Run:
  - `python3 scripts/compile_all_modules.py`
  - `bash scripts/run_minimal_ci_gate.sh --fast-local`
  - `git diff --check -- src/fafafa.ssl.openssl.backed.pas tests/openssl/test_openssl_features.pas task_plan.md findings.md progress.md docs/plans/2026-04-07-openssl-capability-wave8.md`
- Expected:
  - 全部 exit `0`

**Step 3: Update ledgers**
- 在 `task_plan.md` 记录 wave8 进入完成态，并把下一批待办明确切到“若需要再补 DER/PKCS API 真正接线”。
- 在 `findings.md` 记录 3 条已锁定事实：
  - OpenSSL `Initialize` 默认不加载 PEM module
  - `SSL_CTX_use_PrivateKey_file(..., SSL_FILETYPE_PEM)` 当前可直接加载 PKCS#8 PEM 文件
  - 当前不存在 OpenSSL DER 私钥上下文加载路径
- 在 `progress.md` 记录 RED/GREEN、focused regressions、baseline gate 的命令与结果。

## Acceptance Scenarios
- `GetCapabilities` 不再把 4 个 key-format 字段硬编码为 `True`。
- `PEM_read_bio_PrivateKey` 丢失时：
  - `SupportsPasswordProtectedKeys` 必须下沉为 `False`
  - `SupportsPEMPrivateKey` / `SupportsPKCS8PrivateKey` 只能由 `SSL_CTX_use_PrivateKey_file` 决定
- `PEM_read_bio_PrivateKey` 与 `SSL_CTX_use_PrivateKey_file` 都丢失时：
  - `SupportsPEMPrivateKey = False`
  - `SupportsPKCS8PrivateKey = False`
  - `SupportsPasswordProtectedKeys = False`
  - `SupportsDERPrivateKey = False`
- `tests/certificate/test_certs/signer_key.pem` 代表的 PKCS#8 PEM 现有加载能力保持不回退。
