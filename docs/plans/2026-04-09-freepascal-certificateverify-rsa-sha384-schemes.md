# FreePascal CertificateVerify RSA SHA384 Schemes Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 补齐 pure Pascal TLS 1.3 `CertificateVerify` 对 RSA `*_SHA384` 签名方案的支持，让 `TLS_AES_256_GCM_SHA384` 路径不再只停留在 transcript parity，而能完成真实的 RSA SHA384 选型、签名与验签。

**Architecture:** 这批继续保持 validation hardening 的窄边界：只扩 `rsa_pss_rsae_sha384` / `rsa_pss_pss_sha384` / `rsa_pkcs1_sha384` 三个 RSA 方案，不碰 `secp384r1`、Ed25519、CT/OCSP、或更大的 TLS 1.3 state machine。实现路径是先在 unit/runtime 层写 RED，证明当前树上仍把 RSA SHA384 方案当 unsupported；然后在 `wire` 常量、`servercertverify` 的 RSA signer/verify/helper 以及 `freepascal.connection` 的 server/client `CertificateVerify` call path 做最小扩展。

**Tech Stack:** FreePascal (ObjFPC), `fafafa.ssl.tls13.wire`, `fafafa.ssl.tls13.servercertverify`, `fafafa.ssl.freepascal.connection`, `fafafa.ssl.crypto.hash`, scripted TLS 1.3 `TStream` runtime tests, file-based working memory.

---

## Task 1: RED - Prove RSA SHA384 CertificateVerify is still unsupported

**Files:**
- Modify: `tests/test_tls13_servercertverify.pas`
- Modify: `tests/test_freepascal_client_certificateverify_runtime.pas`

**Step 1: Add selector/signer RED in unit tests**
- 在 `tests/test_tls13_servercertverify.pas` 增加 RSA SHA384 focused cases：
  - `TrySelectTLS13ServerCertificateVerifySchemeForKeyType(...)` 在 client 只提供 `rsa_pss_rsae_sha384` / `rsa_pkcs1_sha384` / `rsa_pss_pss_sha384` 时应成功选中
  - `TryBuildTLS13CertificateVerifySignature(...)` 对 RSA SHA384 方案应能产出非空签名
  - `TryVerifyTLS13CertificateVerifySignature(...)` 对同一输入/签名应验签成功
- 当前预期：
  - 选择器会报 “No supported ... for RSA key”
  - signer / verifier 会报 unsupported scheme

**Step 2: Add runtime RED**
- 在 `tests/test_freepascal_client_certificateverify_runtime.pas`：
  - 扩展 scripted server mode，使其能显式发送 `TLS13_SIG_RSA_PSS_RSAE_SHA384`
  - 新增 `TLS_AES_256_GCM_SHA384` + RSA SHA384 valid `CertificateVerify` 成功用例
- 当前预期：
  - client runtime 会因为 unsupported `CertificateVerify` scheme fail-closed

**Commands (RED):**
```bash
mkdir -p tmp/tls13_servercertverify_sha384_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_servercertverify_sha384_red \
  -FEtmp/tls13_servercertverify_sha384_red \
  -otmp/tls13_servercertverify_sha384_red/test_tls13_servercertverify \
  tests/test_tls13_servercertverify.pas && \
./tmp/tls13_servercertverify_sha384_red/test_tls13_servercertverify
```

```bash
mkdir -p tmp/freepascal_client_certificateverify_sha384_red && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_sha384_red \
  -FEtmp/freepascal_client_certificateverify_sha384_red \
  -otmp/freepascal_client_certificateverify_sha384_red/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_sha384_red/test_freepascal_client_certificateverify_runtime
```

## Task 2: GREEN - Add the smallest RSA SHA384 scheme support

**Files:**
- Modify: `src/fafafa.ssl.tls13.wire.pas`
- Modify: `src/fafafa.ssl.tls13.servercertverify.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Publish the missing TLS 1.3 signature-scheme constants**
- 在 `src/fafafa.ssl.tls13.wire.pas` 增加：
  - `TLS13_SIG_RSA_PKCS1_SHA384`
  - `TLS13_SIG_RSA_PSS_RSAE_SHA384`
  - `TLS13_SIG_RSA_PSS_PSS_SHA384`
- 同步补 `TLS13SignatureSchemeToString(...)`

**Step 2: Extend RSA helper stack from SHA256 to SHA384**
- 在 `src/fafafa.ssl.tls13.servercertverify.pas`：
  - 增加 `SHA384_DIGESTINFO_PREFIX`
  - 增加 `MGF1_SHA384(...)`
  - 增加 RSA-PSS SHA384 encode/verify helper
  - 增加 RSA PKCS#1 v1.5 SHA384 encode/verify helper
  - 保持 helper 形状和现有 SHA256 路径一致，不重构出更大抽象

**Step 3: Extend supported scheme tables and call sites**
- 在 `src/fafafa.ssl.tls13.servercertverify.pas`：
  - `IsSupportedTLS13CertificateVerifyScheme(...)` 纳入三种 RSA SHA384 方案
  - `TrySelectTLS13ServerCertificateVerifyScheme(...)` 与 `TrySelectTLS13ServerCertificateVerifySchemeForKeyType(...)` 为 RSA key 在 SHA384-only client offer 下给出正确选择
  - `TryBuildTLS13CertificateVerifySignature(...)` 对 RSA SHA384 方案正确签名
  - `TryVerifyTLS13CertificateVerifySignature(...)` 对 RSA SHA384 方案正确验签
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 不改 larger state machine，只让现有 server/client `CertificateVerify` path 自动吃到新的 SHA384 RSA 方案支持

## Task 3: Verification and closeout

**Commands:**
```bash
mkdir -p tmp/tls13_servercertverify_sha384_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/tls13_servercertverify_sha384_green \
  -FEtmp/tls13_servercertverify_sha384_green \
  -otmp/tls13_servercertverify_sha384_green/test_tls13_servercertverify \
  tests/test_tls13_servercertverify.pas && \
./tmp/tls13_servercertverify_sha384_green/test_tls13_servercertverify
```

```bash
mkdir -p tmp/freepascal_client_certificateverify_sha384_green && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_sha384_green \
  -FEtmp/freepascal_client_certificateverify_sha384_green \
  -otmp/freepascal_client_certificateverify_sha384_green/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_sha384_green/test_freepascal_client_certificateverify_runtime
```

```bash
mkdir -p tmp/freepascal_client_peer_certificate_surface_sha384_regression && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_peer_certificate_surface_sha384_regression \
  -FEtmp/freepascal_client_peer_certificate_surface_sha384_regression \
  -otmp/freepascal_client_peer_certificate_surface_sha384_regression/test_freepascal_client_peer_certificate_surface \
  tests/test_freepascal_client_peer_certificate_surface.pas && \
./tmp/freepascal_client_peer_certificate_surface_sha384_regression/test_freepascal_client_peer_certificate_surface
```

```bash
mkdir -p tmp/freepascal_client_chain_trust_sha384_regression && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_chain_trust_sha384_regression \
  -FEtmp/freepascal_client_chain_trust_sha384_regression \
  -otmp/freepascal_client_chain_trust_sha384_regression/test_freepascal_client_chain_trust_runtime \
  tests/test_freepascal_client_chain_trust_runtime.pas && \
./tmp/freepascal_client_chain_trust_sha384_regression/test_freepascal_client_chain_trust_runtime
```

```bash
python3 scripts/compile_all_modules.py
```

```bash
git diff --check -- docs/plans/2026-04-09-freepascal-certificateverify-rsa-sha384-schemes.md src/fafafa.ssl.tls13.wire.pas src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md
```

---

## Execution Result

- Task 1 RED 已确认：
  - `tests/test_tls13_servercertverify.pas` 新增的 RSA SHA384 selector/signer 断言先失败在 `keytype-rsa-sha384-only should succeed`
  - `tests/test_freepascal_client_certificateverify_runtime.pas` 在 forced `0x0805` path 上失败于 `Unsupported signature scheme for pure FreePascal signer: 0x0805`
- Task 2 GREEN 采用最小实现：
  - `src/fafafa.ssl.tls13.wire.pas` 增加 `rsa_pkcs1_sha384` / `rsa_pss_rsae_sha384` / `rsa_pss_pss_sha384` 常量与字符串映射
  - `src/fafafa.ssl.tls13.servercertverify.pas` 增加 SHA384 `DigestInfo`、`MGF1`、RSA-PSS / PKCS#1 v1.5 SHA384 encode/verify helper，并扩展 supported-scheme table、selector、signer、verifier
  - `src/fafafa.ssl.freepascal.connection.pas` 最终无需改动；现有 server/client `CertificateVerify` path 自动吃到新的 scheme 支持

## Final Verification

- `tests/test_tls13_servercertverify.pas` => PASS
- `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
- `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
- `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
- `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
- `git diff --check -- docs/plans/2026-04-09-freepascal-certificateverify-rsa-sha384-schemes.md src/fafafa.ssl.tls13.wire.pas src/fafafa.ssl.tls13.servercertverify.pas src/fafafa.ssl.freepascal.connection.pas tests/test_tls13_servercertverify.pas tests/test_freepascal_client_certificateverify_runtime.pas task_plan.md findings.md progress.md` => PASS
