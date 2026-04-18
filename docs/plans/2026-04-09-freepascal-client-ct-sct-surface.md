# FreePascal Client CT SCT Surface Plan

**Goal:** 让 pure Pascal TLS 1.3 client 在 `sslVerifyPeer` 的 client path 上，能够主动请求并 surface 服务端通过 TLS `signed_certificate_timestamp` 证书扩展返回的 SCT list，同时对 malformed SCT list fail-closed；这批不宣称 CT policy / cryptographic verification 已完成。

**Architecture:** 这批继续保持 validation hardening 的窄边界，只补 `ClientHello` request、TLS 1.3 `CertificateEntry` SCT list framing parser，以及 connection-level surface。实现上复用 OCSP stapling 那批已经建立的模式：在 `fafafa.ssl.tls13.clienthello` 增加可选扩展 builder，在 `fafafa.ssl.tls13.servercertificate` 对 leaf `CertificateEntry.extensions` 做 bounded parse，在 `TBaseSSLConnection` 增加可选 `ISSLCertificateTransparency` surface，并在 `TFreePascalConnection` 缓存 raw SCT list / count / status。**明确不做** OpenSSL-only `TSCTValidator` 接线，不做 CT policy fail-closed，不改 FreePascal backend capability wording，也不扩到 X.509 embedded SCT extension / OCSP SCT source。

**Tech Stack:** FreePascal (ObjFPC), `TBaseSSLConnection`, `TFreePascalConnection`, TLS 1.3 ClientHello / Certificate parsers, offline scripted `TStream` handshake tests, file-based working memory.

---

## Task 1: RED - Reproduce missing CT request/surface and malformed SCT acceptance

**Files:**
- Add: `tests/test_freepascal_client_ct_sct_surface.pas`
- Reference: `src/fafafa.ssl.tls13.clienthello.pas`
- Reference: `src/fafafa.ssl.tls13.servercertificate.pas`
- Reference: `src/fafafa.ssl.freepascal.connection.pas`
- Reference: `src/fafafa.ssl.base.pas`

**Step 1: Add a scripted full-handshake CT surface contract**
- 新建 `tests/test_freepascal_client_ct_sct_surface.pas`
- 复用 `test_freepascal_client_ocsp_stapling_runtime.pas` 的 scripted TLS 1.3 server stream 模式
- server script 发送：
  - `ServerHello`
  - `EncryptedExtensions`
  - `Certificate`
  - `CertificateVerify`
  - `Finished`
- 其中 `Certificate` 分 3 个场景：
  - leaf 无 SCT extension
  - leaf 带一个结构合法的 `signed_certificate_timestamp` extension
  - leaf 带一个长度不合法的 malformed SCT extension

**Step 2: Assert the bounded runtime contract**
- optional/no-SCT 场景：
  - `Connect = True`
  - `ClientHello` 已包含空 `signed_certificate_timestamp` extension
  - `Supports(LConn, ISSLCertificateTransparency, ...) = True`
  - `GetCertificateTransparencyEnabled = False`
  - raw SCT list 为空
  - SCT count = 0
  - status 提示无 SCT
- valid TLS SCT 场景：
  - `Connect = True`
  - surface 返回原始 SCT list bytes
  - SCT count 与构造值一致
  - status 提示来源于 TLS extension
- malformed TLS SCT 场景：
  - `Connect = False`
  - verify/error string 包含 `signed_certificate_timestamp` / `sct`

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
- 至少一个场景失败，证明当前 FreePascal client 还不会请求 / surface SCT，且 malformed SCT extension 仍不会在 TLS `Certificate` parser 上被识别

---

## Task 2: GREEN - Minimal Client Request / TLS Parser / Surface Plumbing

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Modify: `src/fafafa.ssl.tls13.wire.pas`
- Modify: `src/fafafa.ssl.tls13.clienthello.pas`
- Modify: `src/fafafa.ssl.tls13.servercertificate.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a bounded optional CT surface on the connection base**
- 在 `src/fafafa.ssl.base.pas`：
  - 新增可选接口 `ISSLCertificateTransparency`
  - 只暴露：
    - `GetCertificateTransparencyEnabled`
    - `GetSignedCertificateTimestampList`
    - `GetSignedCertificateTimestampCount`
    - `GetCertificateTransparencyStatus`
- 在 `src/fafafa.ssl.connection.base.pas`：
  - 让 `TBaseSSLConnection` 实现该接口
  - 提供 default stub（false / empty / 0 / `Not Supported`）

**Step 2: Add ClientHello request plumbing**
- 在 `src/fafafa.ssl.tls13.wire.pas` 增加：
  - `TLS_EXTENSION_SIGNED_CERTIFICATE_TIMESTAMP = $0012`
- 在 `src/fafafa.ssl.tls13.clienthello.pas`：
  - 增加空 `signed_certificate_timestamp` extension builder
  - 给 `BuildTLS13ClientHelloHandshake*` / `Record*` 增加可选布尔参数
  - 保持 `pre_shared_key` 仍为最后一个 extension
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - `ProbeServerHello(...)` 只在 `sslVerifyPeer` client path 打开该 request

**Step 3: Parse and cache TLS SCT list**
- 在 `src/fafafa.ssl.tls13.servercertificate.pas`：
  - 增加 bounded `SignedCertificateTimestampList` framing parser
  - 只校验 list/vector 边界与 item 长度，不做签名验证
  - 在 leaf `CertificateEntry.extensions` 里识别 `signed_certificate_timestamp`
  - malformed SCT list => parser 失败
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 增加 connection-level CT state：raw list bytes / count / status
  - 在 `TryCachePeerCertificatesFromHandshake(...)` 缓存 parser 结果
  - 覆盖新的 CT surface getters
  - connect/start/close/failure path 清空 state，避免连接间泄漏

**Out of Scope / Guardrails**
- 不把 OpenSSL `TSCTValidator` 接到 FreePascal client
- 不做 CT policy / fail-closed requirement 开关
- 不改 `sslFeatCertificateTransparency` / `CertTransparencySupport`
- 不扩到 leaf X.509 embedded SCT extension / OCSP SCT source

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

mkdir -p tmp/freepascal_client_peer_certificate_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_peer_certificate_surface \
  -FEtmp/freepascal_client_peer_certificate_surface \
  -otmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface \
  tests/test_freepascal_client_peer_certificate_surface.pas && \
./tmp/freepascal_client_peer_certificate_surface/test_freepascal_client_peer_certificate_surface

mkdir -p tmp/freepascal_client_certificateverify_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_certificateverify_runtime \
  -FEtmp/freepascal_client_certificateverify_runtime \
  -otmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime \
  tests/test_freepascal_client_certificateverify_runtime.pas && \
./tmp/freepascal_client_certificateverify_runtime/test_freepascal_client_certificateverify_runtime

mkdir -p tmp/freepascal_client_chain_trust_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_chain_trust_runtime \
  -FEtmp/freepascal_client_chain_trust_runtime \
  -otmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime \
  tests/test_freepascal_client_chain_trust_runtime.pas && \
./tmp/freepascal_client_chain_trust_runtime/test_freepascal_client_chain_trust_runtime

mkdir -p tmp/freepascal_client_ocsp_stapling_runtime && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ocsp_stapling_runtime \
  -FEtmp/freepascal_client_ocsp_stapling_runtime \
  -otmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime \
  tests/test_freepascal_client_ocsp_stapling_runtime.pas && \
./tmp/freepascal_client_ocsp_stapling_runtime/test_freepascal_client_ocsp_stapling_runtime

mkdir -p tmp/freepascal_client_session_resumption && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_session_resumption \
  -FEtmp/freepascal_client_session_resumption \
  -otmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption \
  tests/test_freepascal_client_session_resumption.pas && \
./tmp/freepascal_client_session_resumption/test_freepascal_client_session_resumption

mkdir -p tmp/freepascal_tls13_early_data && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_tls13_early_data \
  -FEtmp/freepascal_tls13_early_data \
  -otmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data \
  tests/test_freepascal_tls13_early_data.pas && \
./tmp/freepascal_tls13_early_data/test_freepascal_tls13_early_data

python3 scripts/compile_all_modules.py

git diff --check -- \
  docs/plans/2026-04-09-freepascal-client-ct-sct-surface.md \
  src/fafafa.ssl.base.pas \
  src/fafafa.ssl.connection.base.pas \
  src/fafafa.ssl.tls13.wire.pas \
  src/fafafa.ssl.tls13.clienthello.pas \
  src/fafafa.ssl.tls13.servercertificate.pas \
  src/fafafa.ssl.freepascal.connection.pas \
  tests/test_freepascal_client_ct_sct_surface.pas \
  task_plan.md findings.md progress.md
```

**Expected:**
- new CT/SCT runtime contract => PASS
- adjacent FreePascal certificate / OCSP / resumption / early-data regressions => PASS
- `python3 scripts/compile_all_modules.py` => PASS
- targeted `git diff --check` => PASS
