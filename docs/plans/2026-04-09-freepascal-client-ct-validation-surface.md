# FreePascal Client CT Validation Surface Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 让 pure Pascal TLS 1.3 client 在已有 SCT surface 之上，额外暴露 bounded 的 CT cryptographic validation / policy status；只做结果 surface，不把 validation 失败升级成握手 fail-closed。

**Architecture:** 继续沿当前 FreePascal client CT 路径的窄边界推进。连接层复用已经缓存的 raw `SignedCertificateTimestampList` 与 peer leaf/issuer 证书 DER，在存在 OpenSSL CT validator 时，把 raw SCT list 解码成 `PSCT_LIST` 并通过 `TSCTValidator.ValidateSCTList(...)` + `CheckPolicy(...)` 生成 validation result/policy status；若 OpenSSL CT 依赖不可用，则只 surface `Validation unavailable`，不改变握手成败。

**Tech Stack:** FreePascal (ObjFPC), `TFreePascalConnection`, `ISSLCertificateTransparency`, 新增 CT validation optional interface, `fafafa.ssl.ct.sct`, OpenSSL CT/STACK bindings, scripted TLS 1.3 handshake tests, file-based working memory.

---

## Task 1: RED - Prove the connection still lacks CT validation surface

**Files:**
- Modify: `tests/test_freepascal_client_ct_sct_surface.pas`

**Step 1: Add a local CT validation optional interface contract**
- 在测试里新增本批目标接口，例如：
  - `HasCertificateTransparencyValidationResult`
  - `IsCertificateTransparencyPolicySatisfied`
  - `GetCertificateTransparencyValidationStatus`
- 继续保留上一批的 `ISSLCertificateTransparency` raw surface 断言不变。

**Step 2: Extend the no-SCT scenario with “not attempted” assertions**
- 在 `TestClientHelloRequestsSCTAndEmptySurfaceWhenMissing` 中新增断言：
  - connection 支持新的 validation optional interface
  - 缺少 SCT list 时 `Has...Result = False`
  - validation status 提到 `Not Attempted` / `No SCT`

**Step 3: Add a TLS-SCT validation contract**
- 新增 TLS SCT list 场景断言：
  - 若 OpenSSL CT 模块可加载，则 connection 应给出 validation result
  - dummy SCT list 不应满足默认 CT policy
  - validation status 应提到 `policy` / `unknown` / `invalid` / `unverified`
- 若环境没有 OpenSSL CT 模块，测试显式跳过该 validation 子场景，不把整个 CT surface 程序判红。

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
- 当前实现还不支持新的 CT validation interface，或没有返回 validation result / status。

---

## Task 2: GREEN - Add bounded CT validation surface on the connection

**Files:**
- Modify: `src/fafafa.ssl.base.pas`
- Modify: `src/fafafa.ssl.connection.base.pas`
- Modify: `src/fafafa.ssl.freepascal.connection.pas`

**Step 1: Add a new optional validation interface**
- 在 `src/fafafa.ssl.base.pas` 新增 `ISSLCertificateTransparencyValidation`：
  - `HasCertificateTransparencyValidationResult`
  - `IsCertificateTransparencyPolicySatisfied`
  - `GetCertificateTransparencyValidationStatus`
- 保持原有 `ISSLCertificateTransparency` raw surface 不变。

**Step 2: Add base stubs**
- 在 `src/fafafa.ssl.connection.base.pas`：
  - 让 `TBaseSSLConnection` 实现新的 validation optional interface
  - 默认返回 `False / False / 'Not Supported'`

**Step 3: Bridge FreePascal SCT cache to OpenSSL CT validator**
- 在 `src/fafafa.ssl.freepascal.connection.pas`：
  - 新增 connection-level validation state
  - `ClearCertificateTransparencyState` 一并清空 validation state
  - 新增一个 bounded helper：
    - 仅当 raw SCT list 已缓存时触发
    - lazy 检查 / 初始化 OpenSSL + CT/STACK bindings
    - 将 peer leaf/issuer DER materialize 成 OpenSSL `PX509`
    - 用 `o2i_SCT_LIST(...)` 解码 raw SCT list
    - 用 `TSCTValidator.ValidateSCTList(...)` 生成结果
    - 用 `TSCTValidator.CheckPolicy(...)` 生成默认 policy status
  - validation helper 失败时只 surface `Validation unavailable: ...`，不影响握手成功
  - 覆盖新的 validation getters

**Step 4: Keep the boundary explicit**
- 不新增 context policy 开关
- 不改变现有 `GetCertificateTransparencyStatus` 的 source/status 语义
- 不把 validation failure 变成 handshake failure
- 不扩到 OCSP-delivered SCT source

---

## Task 3: Verification

**Commands:**
```bash
mkdir -p tmp/freepascal_client_ct_sct_surface && \
fpc -B -Fu./src -Fu./tests -Fu./tests/framework \
  -FUtmp/freepascal_client_ct_sct_surface \
  -FEtmp/freepascal_client_ct_sct_surface \
  -otmp/tmp/freepascal_client_ct_sct_surface/test_freepascal_client_ct_sct_surface \
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
  docs/plans/2026-04-09-freepascal-client-ct-validation-surface.md \
  src/fafafa.ssl.base.pas \
  src/fafafa.ssl.connection.base.pas \
  src/fafafa.ssl.freepascal.connection.pas \
  tests/test_freepascal_client_ct_sct_surface.pas \
  task_plan.md findings.md progress.md
```

**Expected:**
- new CT validation surface contract => PASS
- adjacent FreePascal peer-cert / CertificateVerify / chain-trust / OCSP / resumption / early-data regressions => PASS
- `python3 scripts/compile_all_modules.py` => PASS
- targeted `git diff --check` => PASS

---

## Execution Notes

- RED observed as expected:
  - `FAIL: Connection should support ISSLCertificateTransparencyValidation`
- Final implementation kept the planned product boundary:
  - exposed validation result / policy status only
  - did not add context policy knobs
  - did not change handshake allow/deny behavior
  - did not extend to OCSP-delivered SCT source
- Runtime evidence forced one implementation change versus the original plan:
  - the initial `o2i_SCT_LIST(...) + TSCTValidator.ValidateSCTList(...)` path produced `Access violation`
  - after narrowing the input framing, the same list-based path still produced `Failed to decode SignedCertificateTimestampList with OpenSSL`
  - the final bridge therefore switched to per-SCT decode/validate:
    - split cached raw list into serialized SCT items
    - decode each item with `o2i_SCT`
    - validate each item with `SCT_validate`
    - aggregate status with `SCT_get_validation_status`
    - compute default-policy satisfied truth locally using the same default-options semantics
- Final verification results:
  - `tests/test_freepascal_client_ct_sct_surface.pas` => PASS
  - `tests/test_freepascal_client_peer_certificate_surface.pas` => PASS
  - `tests/test_freepascal_client_certificateverify_runtime.pas` => PASS
  - `tests/test_freepascal_client_chain_trust_runtime.pas` => PASS
  - `tests/test_freepascal_client_ocsp_stapling_runtime.pas` => PASS
  - `tests/test_freepascal_client_session_resumption.pas` => PASS
  - `tests/test_freepascal_tls13_early_data.pas` => PASS
  - `python3 scripts/compile_all_modules.py` => PASS (`182/182`)
  - targeted `git diff --check` => PASS
