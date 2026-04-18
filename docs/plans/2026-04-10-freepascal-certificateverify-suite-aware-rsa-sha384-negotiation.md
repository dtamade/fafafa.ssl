# FreePascal CertificateVerify Suite-Aware RSA SHA384 Negotiation Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client/server 在默认协商路径里真正 advertize 并选中 RSA `*_SHA384` `CertificateVerify` schemes，使 `TLS_AES_256_GCM_SHA384` 不再依赖 forced test hook 才能走到 RSA SHA384。

**Architecture:** 这批继续保持窄边界，只补两件事：`ClientHello.signature_algorithms` 默认广告 RSA SHA384 schemes；`servercertverify` 选择器按已协商 cipher suite 做 hash-family-aware 选择。保持旧 selector 兼容入口，不扩到 `secp384r1` / Ed25519 / 更大 state machine。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.tls13.clienthello`, `fafafa.ssl.tls13.servercertverify`, `fafafa.ssl.freepascal.connection`, TLS 1.3 ClientHello parser tests, scripted runtime handshake tests, file-based working memory.

---

## Task 1: RED - Prove default negotiation still does not reach RSA SHA384

**Files:**
- Modify: `tests/test_tls13_clienthello_parser.pas`
- Modify: `tests/test_tls13_servercertverify.pas`
- Modify: `tests/test_freepascal_client_certificateverify_runtime.pas`

**Step 1: Add ClientHello advertisement RED**
- 在 `tests/test_tls13_clienthello_parser.pas` 断言 generated `ClientHello` 默认 advertize：
  - `rsa_pss_rsae_sha384`
  - `rsa_pkcs1_sha384`
  - `rsa_pss_pss_sha384`
- 当前预期：parser 能看到 SHA256 schemes，但看不到上述 SHA384 schemes。

**Step 2: Add suite-aware selector RED**
- 在 `tests/test_tls13_servercertverify.pas` 增加 suite-aware selector focused case：
  - 当 client 同时 advertize RSA SHA256/RSA SHA384 schemes，且 cipher suite 是 `TLS_AES_256_GCM_SHA384` 时，应优先选 `rsa_pss_rsae_sha384`
  - 当 cipher suite 是 `TLS_CHACHA20_POLY1305_SHA256` 时，应继续优先选 `rsa_pss_rsae_sha256`
- 当前预期：现有 selector 不看 suite，只会继续优先 SHA256。

**Step 3: Add runtime RED**
- 在 `tests/test_freepascal_client_certificateverify_runtime.pas`：
  - 新增一个不使用 forced scheme 的 AES256/SHA384 handshake case
  - 让 scripted server 断言默认真实选择到 `rsa_pss_rsae_sha384`
- 当前预期：若只走默认路径，会仍然选到 SHA256 scheme。

**Commands (RED):**
```bash
mkdir -p tmp/tls13_clienthello_signature_algorithms_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_clienthello_signature_algorithms_red \
  -FEtmp/tls13_clienthello_signature_algorithms_red \
  -otmp/tls13_clienthello_signature_algorithms_red/test_tls13_clienthello_parser \
  tests/test_tls13_clienthello_parser.pas && \
./tmp/tls13_clienthello_signature_algorithms_red/test_tls13_clienthello_parser
```

```bash
mkdir -p tmp/tls13_servercertverify_suite_selector_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_servercertverify_suite_selector_red \
  -FEtmp/tls13_servercertverify_suite_selector_red \
  -otmp/tls13_servercertverify_suite_selector_red/test_tls13_servercertverify \
  tests/test_tls13_servercertverify.pas && \
./tmp/tls13_servercertverify_suite_selector_red/test_tls13_servercertverify
```

```bash
mkdir -p tmp/freepascal_client_certificateverify_suite_selector_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_suite_selector_red \
  -FEtmp/freepascal_client_certificateverify_suite_selector_red \
  -otmp/freepascal_client_certificateverify_suite_selector_red/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_suite_selector_red/test_freepascal_client_certificateverify_runtime
```

## Task 2: GREEN - Add the smallest real-negotiation support

**Files:**
- Modify: `src/fafafa.ssl.tls13.clienthello.pas`
- Modify: `src/fafafa.ssl.tls13.servercertverify.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Expand default ClientHello signature_algorithms advertisement**
- 在 `src/fafafa.ssl.tls13.clienthello.pas`：
  - 默认 advertize RSA SHA384 schemes
  - 保持现有 RSA/ECDSA SHA256 schemes 仍在列表中
  - 不新增 `ecdsa_secp384r1_sha384`

**Step 2: Add suite-aware CertificateVerify selector**
- 在 `src/fafafa.ssl.tls13.servercertverify.pas`：
  - 增加一个按 cipher suite 选择 hash-family 的最小 selector 入口
  - 对 RSA key：
    - SHA384 suite 优先 `rsa_pss_rsae_sha384` / `rsa_pkcs1_sha384` / `rsa_pss_pss_sha384`
    - SHA256 suite 继续优先 `rsa_pss_rsae_sha256` / `rsa_pkcs1_sha256` / `rsa_pss_pss_sha256`
  - ECDSA path 仍保持 `ecdsa_secp256r1_sha256`
  - 保留旧 selector 作为兼容 fallback

**Step 3: Wire real server path to the suite-aware selector**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - server outgoing `CertificateVerify` path 改用 suite-aware selector
- 在 scripted runtime tests：
  - 同样改走 suite-aware selector，让默认 AES256/SHA384 path 真正协商到 RSA SHA384

## Task 3: Verification and closeout

**Commands:**
```bash
mkdir -p tmp/tls13_clienthello_signature_algorithms_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_clienthello_signature_algorithms_green \
  -FEtmp/tls13_clienthello_signature_algorithms_green \
  -otmp/tls13_clienthello_signature_algorithms_green/test_tls13_clienthello_parser \
  tests/test_tls13_clienthello_parser.pas && \
./tmp/tls13_clienthello_signature_algorithms_green/test_tls13_clienthello_parser
```

```bash
mkdir -p tmp/tls13_servercertverify_suite_selector_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_servercertverify_suite_selector_green \
  -FEtmp/tls13_servercertverify_suite_selector_green \
  -otmp/tls13_servercertverify_suite_selector_green/test_tls13_servercertverify \
  tests/test_tls13_servercertverify.pas && \
./tmp/tls13_servercertverify_suite_selector_green/test_tls13_servercertverify
```

```bash
mkdir -p tmp/freepascal_client_certificateverify_suite_selector_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_suite_selector_green \
  -FEtmp/freepascal_client_certificateverify_suite_selector_green \
  -otmp/freepascal_client_certificateverify_suite_selector_green/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_suite_selector_green/test_freepascal_client_certificateverify_runtime
```

```bash
mkdir -p tmp/freepascal_client_peer_certificate_surface_suite_selector_regression && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_peer_certificate_surface_suite_selector_regression \
  -FEtmp/freepascal_client_peer_certificate_surface_suite_selector_regression \
  -otmp/freepascal_client_peer_certificate_surface_suite_selector_regression/test_freepascal_client_peer_certificate_surface \
  tests/test_freepascal_client_peer_certificate_surface.pas && \
./tmp/freepascal_client_peer_certificate_surface_suite_selector_regression/test_freepascal_client_peer_certificate_surface
```

```bash
mkdir -p tmp/freepascal_client_chain_trust_suite_selector_regression && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_chain_trust_suite_selector_regression \
  -FEtmp/freepascal_client_chain_trust_suite_selector_regression \
  -otmp/freepascal_client_chain_trust_suite_selector_regression/test_freepascal_client_chain_trust_runtime \
  tests/test_freepascal_client_chain_trust_runtime.pas && \
./tmp/freepascal_client_chain_trust_suite_selector_regression/test_freepascal_client_chain_trust_runtime
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-10-freepascal-certificateverify-suite-aware-rsa-sha384-negotiation.md src/fafafa.ssl.tls13.clienthello.pas src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_clienthello_parser.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md
```

---

## Execution Result

- 待执行。

## Final Verification

- 待执行。
