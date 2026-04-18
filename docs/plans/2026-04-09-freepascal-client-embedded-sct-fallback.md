# FreePascal Client Embedded SCT Fallback Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在 `sslVerifyPeer` client path 上，当服务端没有通过 TLS `signed_certificate_timestamp` 扩展返回 SCT list 时，能够从 leaf X.509 证书的 embedded SCT 扩展（OID `1.3.6.1.4.1.11129.2.4.2`）做 fallback surface，并对 malformed embedded SCT fail-closed。

**Architecture:** 继续沿上一批 CT surface 的窄边界推进，不引入 CT policy / cryptographic verification，也不扩证书生成 API。测试侧使用静态 CA-signed fixture 提供 valid / malformed embedded SCT 证书；生产侧优先保留 TLS SCT extension 的优先级，仅当 TLS SCT 缺失时，才在 `TFreePascalConnection.TryCachePeerCertificatesFromHandshake(...)` 基于 leaf DER/X.509 扩展值做 fallback，并复用同一套 SCT list 边界解析。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `TTLS13ServerCertificateInfo`, `TX509Certificate`, scripted TLS 1.3 handshake tests, OpenSSL CLI 仅用于离线生成测试 fixture，file-based working memory.

---

## Task 1: RED - Reproduce missing embedded SCT fallback and malformed embedded SCT acceptance

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`
- Create: `tests/certificate/test_certs/ct_embedded_sct_leaf_cert.pem`
- Create: `tests/certificate/test_certs/ct_embedded_sct_leaf_key.pem`
- Create: `tests/certificate/test_certs/ct_embedded_sct_malformed_leaf_cert.pem`

**Step 1: Add embedded SCT fixture readers**
- 在测试文件新增静态 fixture 装载 helper：
  - valid embedded SCT leaf cert + key
  - malformed embedded SCT leaf cert + key（可与 valid key 共用）
  - chain 继续拼接 `tests/certificate/test_certs/ca_cert.pem`
- 保持 TLS `CertificateEntry.extensions` 为空，确保场景只覆盖 embedded fallback，而不是 TLS source。

**Step 2: Add failing embedded SCT surface contract**
- 新增 valid embedded SCT 场景，断言：
  - `Connect = True`
  - `ObservedSCTRequest = True`
  - `GetCertificateTransparencyEnabled = True`
  - raw SCT list bytes 与 fixture 中 embedded value 一致
  - count 与构造值一致
  - status 明确提到 `embedded` / `x509`

**Step 3: Add failing malformed embedded SCT contract**
- 新增 malformed embedded SCT 场景，断言：
  - `Connect = False`
  - verify/error string 提到 `signed_certificate_timestamp` / `sct`
  - 失败发生在 embedded fallback 路径，而不是 TLS extension parser

**Command (RED):**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
  tests/test_freepascal_client_ct_sct_surface.pas && \
./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface
```

**Expected RED:**
- embedded SCT valid 场景先失败，证明当前实现只 surface TLS SCT extension，不会 fallback 到 X.509 embedded SCT
- malformed embedded SCT 场景先失败，证明当前实现还不会对 embedded SCT list 做 fail-closed

---

## Task 2: GREEN - Minimal embedded SCT fallback in connection cache

**Files:**
- Modify: `src/fafafa.ssl.tls13.servercertificate.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Expose a generic SCT list parser helper**
- 在 `src/fafafa.ssl.tls13.servercertificate.pas` interface 暴露一个通用 helper：
  - 输入 raw `SignedCertificateTimestampList` bytes
  - 输出 parsed count / error
- 保持它仍只做 list/vector 边界检查，不做签名验证。

**Step 2: Add leaf embedded SCT fallback**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 新增 leaf embedded SCT 读取 helper，基于 leaf DER 创建 `TX509Certificate`
  - 仅在：
    - `ACertificateTransparencyRequested = True`
    - TLS SCT extension 未提供 list
  - 时读取 OID `1.3.6.1.4.1.11129.2.4.2`
- 若 embedded value 存在：
  - 用通用 SCT parser 校验
  - 成功则缓存 raw bytes / count
  - status 写成 `Received from embedded X.509 extension (%d SCTs)` 或等价文案

**Step 3: Preserve source precedence and failure semantics**
- TLS SCT extension 优先级高于 embedded fallback：
  - 若 TLS SCT list 已存在，不读取 embedded 扩展
- malformed embedded SCT：
  - 视为 certificate parse / validation failure
  - fail-closed 并返回带 `signed_certificate_timestamp` / `sct` 语义的错误
- 无 TLS / 无 embedded：
  - 保持上一批行为：不失败，surface `No SCT List`

---

## Task 3: Verification

**Commands:**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
  tests/test_freepascal_client_ct_sct_surface.pas && \
./tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface

mkdir -p tmp/test_freepascal_client_peer_certificate_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_peer_certificate_surface \
  -FEtmp/test_freepascal_client_peer_certificate_surface \
  -otmp/test_freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface \
  tests/test_freepascal_client_peer_certificate_surface.pas && \
./tmp/test_freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface

mkdir -p tmp/test_freepascal_client_certificateverify_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_certificateverify_runtime \
  -FEtmp/test_freepascal_client_certificateverify_runtime \
  -otmp/test_freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/test_freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime

mkdir -p tmp/test_freepascal_client_chain_trust_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_chain_trust_runtime \
  -FEtmp/test_freepascal_client_chain_trust_runtime \
  -otmp/test_freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime \
  tests/test_freepascal_client_chain_trust_runtime.pas && \
./tmp/test_freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime

mkdir -p tmp/test_freepascal_client_ocsp_stapling_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_ocsp_stapling_runtime \
  -FEtmp/test_freepascal_client_ocsp_stapling_runtime \
  -otmp/test_freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime \
  tests/test_freepascal_client_ocsp_stapling_runtime.pas && \
./tmp/test_freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime

mkdir -p tmp/test_freepascal_client_session_resumption && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_client_session_resumption \
  -FEtmp/test_freepascal_client_session_resumption \
  -otmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
./tmp/test_freepascal_client_session_resumption/test_freepascal_client_session_resumption

mkdir -p tmp/test_freepascal_tls13_early_data && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/test_freepascal_tls13_early_data \
  -FEtmp/test_freepascal_tls13_early_data \
  -otmp/test_freepascal_tls13_early_data/test_freepascal_tls13_early_data \
  tests/test_freepascal_tls13_early_data.pas && \
./tmp/test_freepascal_tls13_early_data/test_freepascal_tls13_early_data

python3 scripts/compile_all_modules.py

git diff --check -- \
  docs/plans/2026-04-09-freepascal-client-embedded-sct-fallback.md \
  src/fafafa.ssl.tls13.servercertificate.pas \
  src/fafafa.ssl.freepascal.connection.pas \
  tests/test_freepascal_client_ct_sct_surface.pas \
  tests/certificate/test_certs/ct_embedded_sct_leaf_cert.pem \
  tests/certificate/test_certs/ct_embedded_sct_leaf_key.pem \
  tests/certificate/test_certs/ct_embedded_sct_malformed_leaf_cert.pem \
  task_plan.md findings.md progress.md
```

**Expected:**
- embedded SCT valid / malformed contract => PASS
- adjacent peer-cert / CertificateVerify / chain-trust / OCSP / resumption / early-data regressions => PASS
- `python3 scripts/compile_all_modules.py` => PASS
- targeted `git diff --check` => PASS
