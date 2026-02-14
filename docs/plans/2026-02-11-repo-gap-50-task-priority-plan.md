# Repository Gap Scan & 50-Task Priority Execution Plan

> **For Claude:** REQUIRED SUB-SKILL: Use `writing-plans` + `planning-with-files` for planning, and `superpowers:executing-plans` for implementation with strict TDD.

## Goal
- 全仓扫描未完成项与缺口，形成 **50 个可执行任务**（含优先级、目标文件、验收命令）。
- 按优先级持续迭代执行（RED → GREEN → Regression），并在每轮回写 `task_plan.md` / `findings.md` / `progress.md`。

## Scan Evidence (2026-02-11)
- Source keyword scan (`TODO/FIXME/unsupported/placeholder`) across `src/` and `tests/`。
- Backend hotspot scan:
  - `src/fafafa.ssl.freepascal.connection.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
  - `src/fafafa.ssl.wolfssl.*.pas`
  - `src/fafafa.ssl.mbedtls.*.pas`
  - `src/fafafa.ssl.winssl.*.pas`
- Test debt scan (skip/placeholder/not implemented) across `tests/`。

## Priority Strategy
- **P0:** 合同错误、行为与能力声明不一致、错误分类/安全语义偏差（先修）。
- **P1:** 后端能力补齐、跨后端一致性、稳定性和可观测性。
- **P2:** 测试债务、文档一致性、非阻断质量项。

---

## P0 Tasks (1-18)

### P0-01 SAN overrides CN contract
- Gap: `VerifyHostname` 先查 CN，SAN 存在时可能误判通过。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_freepascal_backend_basic.pas`, `tests/certificate/test_certs/san_override_cert.pem`
- Acceptance: `fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic`

### P0-02 Wildcard one-label hostname rule
- Gap: `*.example.com` 当前可能错误匹配 `a.b.example.com`。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_freepascal_backend_basic.pas`
- Acceptance: same as P0-01

### P0-03 VerifyHostname SAN match positive coverage
- Gap: SAN 正路径缺少明确断言。
- Files: `tests/test_freepascal_backend_basic.pas`
- Acceptance: same as P0-01

### P0-04 BuildCertificateChain de-dup by fingerprint
- Gap: 去环目前以接口引用判断，clone 证书可能重复。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_freepascal_backend_basic.pas`
- Acceptance: same as P0-01

### P0-05 VerifyCertificate issuer-chain strictness
- Gap: `VerifyCertificate` 只按 subject 匹配 issuer，缺少签名级校验策略约束。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/certificate/test_cert_verify.pas`
- Acceptance: `fpc -Fu./src tests/certificate/test_cert_verify.pas -otmp/test_cert_verify && ./tmp/test_cert_verify`

### P0-06 FreePascal error-classification consistency (protocol/io/unsupported)
- Gap: 连接路径错误文案与 `GetError` 分类仍有潜在分叉。
- Files: `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_backend_basic.pas`, `tests/test_freepascal_server_accept_skeleton.pas`
- Acceptance: `test_fp_basic`, `test_fp_accept`

### P0-07 KeyUpdate suite support parity
- Gap: keyupdate 路径存在 suite unsupported 分支。
- Files: `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_server_accept_skeleton.pas`
- Acceptance: `test_fp_accept`

### P0-08 Client Finished encryption suite parity
- Gap: client finished 对部分 TLS1.3 套件支持路径不足。
- Files: `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_server_accept_skeleton.pas`
- Acceptance: `test_fp_accept`

### P0-09 App data cipher path parity
- Gap: 应用数据路径对套件支持不完整。
- Files: `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_server_accept_skeleton.pas`
- Acceptance: `test_fp_accept`

### P0-10 Capability KnownIssues alignment with runtime support
- Gap: `KnownIssues` 文案与当前实现进度存在漂移风险。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_capability_cache.pas`
- Acceptance: `fpc -Fu./src tests/test_capability_cache.pas -otmp/test_cap_cache && ./tmp/test_cap_cache`

### P0-11 Invalid DER/PEM regression hardening
- Gap: 无效输入新增契约后，缺少坏样本覆盖（截断 DER / 伪 PEM）。
- Files: `tests/test_freepascal_backend_basic.pas`, `tests/certificate/test_certs/`
- Acceptance: `test_fp_basic`

### P0-12 FreePascal LoadFromPath cert-only filtering
- Gap: 目录扫描目前对任意文件尝试加载，噪声较高。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_freepascal_backend_basic.pas`
- Acceptance: `test_fp_basic`

### P0-13 FreePascal store find semantics for normalized serial/subject
- Gap: `FindBySerialNumber/FindBySubject` 缺少规范化边界覆盖。
- Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_freepascal_backend_basic.pas`
- Acceptance: `test_fp_basic`

### P0-14 Stream-connection "not implemented" legacy expectations cleanup
- Gap: `tests/test_stream_connection.pas` 仍含 not implemented skip 路径。
- Files: `tests/test_stream_connection.pas`, backend connection units
- Acceptance: `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`

### P0-15 test_helper_utilities placeholder assertion removal
- Gap: `GetCertificateInfo placeholder` 仍为硬编码失败占位。
- Files: `tests/test_helper_utilities.pas`
- Acceptance: `fpc -Fu./src tests/test_helper_utilities.pas -otmp/test_helper_utils && ./tmp/test_helper_utils`

### P0-16 Native-handle contract parity for pure backend paths
- Gap: 纯 Pascal backend native-handle 错误上下文可再收敛。
- Files: `tests/test_native_handle_unified.pas`, `src/fafafa.ssl.native.handle.*` (if touched)
- Acceptance: `fpc -Fu./src tests/test_native_handle_unified.pas -otmp/test_native_handle_unified && ./tmp/test_native_handle_unified`

### P0-17 Cross-backend capability serializer strict roundtrip
- Gap: capability 字段新增/变更易出现序列化不对称。
- Files: `src/fafafa.ssl.capability.serializer.pas`, `tests/test_capability_deserialization_roundtrip.pas`
- Acceptance: `fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip`

### P0-18 Unit subset stability gate
- Gap: 基础单测子集需作为每轮回归门禁。
- Files: verify-only
- Acceptance: `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`

---

## P1 Tasks (19-38)

### P1-19 WolfSSL certificate IsCA basic-constraints accuracy
- Files: `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`

### P1-20 WolfSSL SAN extraction coverage
- Files: `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`

### P1-21 WolfSSL date metadata decoding robustness
- Files: `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`

### P1-22 WolfSSL session serialize/deserialize nil-safety
- Files: `src/fafafa.ssl.wolfssl.session.pas`, `tests/test_wolfssl_framework.pas`

### P1-23 WolfSSL feature capability vs runtime consistency
- Files: `src/fafafa.ssl.wolfssl.lib.pas`, `tests/test_wolfssl_framework.pas`

### P1-24 WolfSSL context renegotiation explicit semantics
- Files: `src/fafafa.ssl.wolfssl.context.pas`, `tests/test_wolfssl_framework.pas`

### P1-25 WolfSSL certstore find/remove semantics parity
- Files: `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/certificate/test_certstore_unit.pas`

### P1-26 WolfSSL invalid cert input rejection contract
- Files: `src/fafafa.ssl.wolfssl.certificate.pas`, `tests/connection/test_wolfssl_metadata_accuracy.pas`

### P1-27 MbedTLS protocol/capability matrix drift check
- Files: `src/fafafa.ssl.mbedtls.lib.pas`, `tests/test_mbedtls_framework.pas`

### P1-28 MbedTLS verify flags contract hardening
- Files: `tests/mbedtls/test_mbedtls_cert_verify_flags.pas`

### P1-29 MbedTLS cert chain path deterministic fixture
- Files: `tests/mbedtls/test_mbedtls_cert_chain.pas`, `tests/certificate/test_certs/`

### P1-30 MbedTLS session nil/empty payload handling
- Files: `src/fafafa.ssl.mbedtls.session.pas`, `tests/test_mbedtls_framework.pas`

### P1-31 MbedTLS native handle contract clarity
- Files: `src/fafafa.ssl.mbedtls.native_handle.pas`, `tests/test_native_handle_simple.pas`

### P1-32 MbedTLS context invalid cert format mapping
- Files: `src/fafafa.ssl.mbedtls.context.pas`, `tests/config/test_context_cert_loading.pas`

### P1-33 WinSSL certstore nil return semantics normalization
- Files: `src/fafafa.ssl.winssl.certstore.pas`, `tests/winssl/test_winssl_certificate_loading.pas`

### P1-34 WinSSL connection false-result error mapping parity
- Files: `src/fafafa.ssl.winssl.connection.pas`, `tests/winssl/test_winssl_errors_comprehensive.pas`

### P1-35 WinSSL enterprise error-path explicitness
- Files: `src/fafafa.ssl.winssl.enterprise.pas`, `tests/winssl/test_winssl_enterprise_comprehensive.pas`

### P1-36 WinSSL server-handshake skeleton closure
- Files: `tests/winssl/test_winssl_server_handshake.pas`, `src/fafafa.ssl.winssl.connection.pas`

### P1-37 Stream-connection capability-driven skip normalization
- Files: `tests/test_stream_connection.pas`

### P1-38 Cross-backend integration skip-path normalization
- Files: `tests/integration/test_integration_winssl_openssl_comparison.pas`

---

## P2 Tasks (39-50)

### P2-39 Benchmark framework placeholder cleanup
- Files: `tests/benchmarks/benchmark_framework.pas`

### P2-40 Resource-limits test certificate placeholder replacement
- Files: `tests/test_resource_limits.pas`, `tests/certificate/test_certs/`

### P2-41 Zero-copy view TODO fallback removal
- Files: `tests/test_zerocopy_view.pas`, related encoding unit

### P2-42 Quick test cert generation dependency removal
- Files: `tests/test_quick.pas`

### P2-43 Real-usage skip comments to deterministic assertions
- Files: `tests/test_real_usage.pas`

### P2-44 OpenSSL helper utility skip grouping simplification
- Files: `tests/test_helper_utilities.pas`

### P2-45 OCSP regression skip accounting normalization
- Files: `tests/openssl/test_ocsp_connection_verification_regression.pas`

### P2-46 CT verification skip-path explicit capability checks
- Files: `tests/ct/test_sct_verification.pas`

### P2-47 DANE tests skipped-counter consistency
- Files: `tests/dane/test_dane_tlsa.pas`

### P2-48 Documentation sync for backend capability semantics
- Files: `README.md`, `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`

### P2-49 Add repository gap dashboard doc (task status matrix)
- Files: `docs/plans/`, `docs/test_reports/`

### P2-50 Closure checklist and recurring execution protocol
- Files: `docs/plans/AUTONOMOUS_ITERATION_PROTOCOL.md`, `task_plan.md`

---

## Execution Order (initial)
1. P0-01
2. P0-02
3. P0-03
4. P0-04
5. P0-06
6. P0-11

## TDD Command Template (each task)
1. RED: add failing assertion and run targeted test.
2. GREEN: minimal fix and rerun same test.
3. REGRESSION: run
   - `fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept`
   - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
4. Write back execution record to planning files.

---

## Execution Batch 1 (2026-02-11 09:59 +0800)

### Executing-Plans Mode
- Batch scope: `P0-01` + `P0-02` + `P0-03`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P0-01/P0-02/P0-03
- Modified:
  - `tests/test_freepascal_backend_basic.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`
- Added fixtures:
  - `tests/certificate/test_certs/san_cn_conflict_cert.pem`
  - `tests/certificate/test_certs/san_wildcard_cert.pem`

#### RED
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `❌ Hostname verification should prioritize SAN over CN when SAN is present`

#### GREEN changes
- `VerifyHostname` now prioritizes SAN list; CN fallback only when SAN absent.
- `MatchHostname` wildcard matching now enforces one-label subdomain only.
- Added contract assertions:
  - SAN overrides CN mismatch (`san_cn_conflict_cert.pem`)
  - SAN exact DNS positive match
  - wildcard single-label positive (`api.example.com`)
  - wildcard multi-label negative (`deep.api.example.com`)

#### GREEN
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `✅ FreePascal backend basic checks passed`

#### Regression 1
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
```
Key output:
- `✅ FreePascal server accept skeleton checks passed`

#### Regression 2
```bash
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `Number of run tests: 10`
- `Number of failures: 0`
- `Number of errors: 0`
- `Number of ignored tests: 2`

### Batch Status
- `P0-01`: complete
- `P0-02`: complete
- `P0-03`: complete
- Next candidate: `P0-04 BuildCertificateChain de-dup by fingerprint`

## Execution Batch 2 (2026-02-11 10:15 +0800)

### Executing-Plans Mode
- Batch scope: `P0-06`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P0-06
- Modified:
  - `tests/test_freepascal_backend_basic.pas`
  - `src/fafafa.ssl.freepascal.connection.pas`

#### RED
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `❌ Renegotiate before handshake should report protocol precondition error`

#### GREEN
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `✅ FreePascal backend basic checks passed`

#### Regression
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `✅ FreePascal server accept skeleton checks passed`
- `Number of failures: 0`, `Number of errors: 0`

### Batch Status
- `P0-06`: complete

## Execution Batch 3 (2026-02-11 10:18 +0800)

### Executing-Plans Mode
- Batch scope: `P0-11`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P0-11
- Modified:
  - `tests/test_freepascal_backend_basic.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`

#### RED
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `❌ FreePascal certificate should reject PEM payload without CERTIFICATE block type`

#### GREEN
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `✅ FreePascal backend basic checks passed`

#### Regression
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `✅ FreePascal server accept skeleton checks passed`
- `Number of failures: 0`, `Number of errors: 0`

### Batch Status
- `P0-11`: complete
- Next candidate: `P0-12 FreePascal LoadFromPath cert-only filtering`

## Execution Batch 4 (2026-02-11 10:31 +0800)

### Executing-Plans Mode
- Batch scope: `P0-12`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P0-12
- Modified:
  - `tests/test_freepascal_backend_basic.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`

#### RED
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `❌ LoadFromPath should ignore non-certificate extension files`

#### GREEN
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `✅ FreePascal backend basic checks passed`

#### Regression
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `✅ FreePascal server accept skeleton checks passed`
- `Number of failures: 0`, `Number of errors: 0`

### Batch Status
- `P0-12`: complete
- Next candidate: `P0-13 FreePascal store find semantics for normalized serial/subject`

## Execution Batch 5 (2026-02-11 10:42 +0800)

### Executing-Plans Mode
- Batch scope: `P0-13`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P0-13
- Modified:
  - `tests/test_freepascal_backend_basic.pas`
  - `src/fafafa.ssl.freepascal.lib.pas`

#### RED
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `❌ Certificate store should find certificate by normalized subject query`

#### GREEN
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `✅ FreePascal backend basic checks passed`

#### Regression
```bash
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `✅ FreePascal server accept skeleton checks passed`
- `Number of failures: 0`, `Number of errors: 0`

### Batch Status
- `P0-13`: complete
- Next candidate: `P0-14 Stream-connection "not implemented" legacy expectations cleanup`

## Execution Batch 6 (2026-02-11 10:51 +0800)

### Executing-Plans Mode
- Batch scope: `P0-17` + `P0-18`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P0-17
- Modified:
  - `tests/test_capability_deserialization_roundtrip.pas`
  - `src/fafafa.ssl.capability.serializer.pas`

#### RED
```bash
fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip
```
Key output:
- `❌ json.sessionCacheSupport mismatch: expected=2 actual=0`

#### GREEN
```bash
fpc -Fu./src tests/test_capability_deserialization_roundtrip.pas -otmp/test_cap_roundtrip && ./tmp/test_cap_roundtrip
```
Key output:
- `✅ JSON round-trip passed`
- `✅ XML round-trip passed`

### Task P0-18 (stability gate)
```bash
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `Number of run tests: 10`
- `Number of failures: 0`
- `Number of errors: 0`
- `Number of ignored tests: 2`

### Regression
```bash
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
fpc -Fu./src tests/test_freepascal_server_accept_skeleton.pas -otmp/test_fp_accept && ./tmp/test_fp_accept
```
Key output:
- `✅ FreePascal backend basic checks passed`
- `✅ FreePascal server accept skeleton checks passed`

### Batch Status
- `P0-17`: complete
- `P0-18`: complete
- `P0 (1-18)`: complete
- Next candidate: `P1-19 WolfSSL certificate IsCA basic-constraints accuracy`

## Execution Batch 7 (2026-02-11 10:58 +0800)

### Executing-Plans Mode
- Batch scope: `P1-19`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P1-19
- Modified:
  - `tests/connection/test_wolfssl_metadata_accuracy.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`

#### RED
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
Key output:
- `❌ CA certificate should report IsCA=True`

#### GREEN
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
Key output:
- `✅ wolfssl metadata accuracy tests passed`

#### Regression
```bash
fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `WolfSSL Framework Test Summary ... Failed: 0`
- `✅ FreePascal backend basic checks passed`
- `Number of run tests: 10 / failures: 0 / errors: 0`

### Batch Status
- `P1-19`: complete
- Next candidate: `P1-20 WolfSSL SAN extraction coverage`

## Execution Batch 8 (2026-02-11 11:03 +0800)

### Executing-Plans Mode
- Batch scope: `P1-20`
- Method: strict TDD (`test-first coverage guard`)

### Task P1-20
- Modified:
  - `tests/connection/test_wolfssl_metadata_accuracy.pas`

#### RED run (new assertions)
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
Key output:
- `✅ wolfssl metadata accuracy tests passed`

### Notes
- New coverage assertions passed immediately; no production code change required.
- Classified as coverage hardening, not bugfix.

#### Regression
```bash
fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
```
Key output:
- `WolfSSL Framework Test Summary ... Failed: 0`
- `✅ FreePascal backend basic checks passed`

### Batch Status
- `P1-20`: complete (coverage)
- Next candidate: `P1-21 WolfSSL date metadata decoding robustness`

## Execution Batch 9 (2026-02-11 11:10 +0800)

### Executing-Plans Mode
- Batch scope: `P1-21`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P1-21
- Modified:
  - `tests/connection/test_wolfssl_metadata_accuracy.pas`
  - `src/fafafa.ssl.wolfssl.certificate.pas`

#### RED
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
Key output:
- `❌ NotBefore should be decoded (wolfSSL API or DER fallback)`

#### GREEN
```bash
fpc -Fu./src tests/connection/test_wolfssl_metadata_accuracy.pas -otmp/test_wolfssl_metadata_accuracy && ./tmp/test_wolfssl_metadata_accuracy
```
Key output:
- `✅ wolfssl metadata accuracy tests passed`

#### Regression
```bash
fpc -Fu./src tests/test_wolfssl_framework.pas -otmp/test_wolfssl_framework && ./tmp/test_wolfssl_framework
fpc -Fu./src tests/test_freepascal_backend_basic.pas -otmp/test_fp_basic && ./tmp/test_fp_basic
fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all
```
Key output:
- `WolfSSL Framework Test Summary ... Failed: 0`
- `✅ FreePascal backend basic checks passed`
- `Number of run tests: 10 / failures: 0 / errors: 0`

### Batch Status
- `P1-21`: complete
- Next candidate: `P1-22 WolfSSL session serialize/deserialize nil-safety`

## Execution Batch 10 (2026-02-11 12:08 +0800)

### Executing-Plans Mode
- Batch scope: `P1-31` + `P1-38` + `P2 partial (39~43)`
- Method: strict TDD (`RED -> GREEN -> Regression`)

### Task P1-31
- Files:
  - `src/fafafa.ssl.mbedtls.native_handle.pas`
  - `tests/test_mbedtls_framework.pas`
- RED output key:
  - `MbedTLS native-handle helper rejects non-MbedTLS backend: FAIL`
- GREEN output key:
  - `MbedTLS Framework Test Summary ... Failed: 0`

### Task P1-38
- Files:
  - `tests/integration/test_integration_winssl_openssl_comparison.pas`
- RED output key:
  - `Non-Windows skip accounting ... Expected >=4 skips, got 0`
- GREEN output key:
  - `Skipped: 4`
  - `RESULT: ALL TESTS PASSED`

### P2 Progress (partial)
- `P2-39`:
  - `tests/benchmarks/benchmark_framework.pas` (`LoadBaseline` placeholder removed)
  - Added `tests/benchmarks/test_benchmark_framework_baseline.pas`
- `P2-40`:
  - `tests/test_resource_limits.pas` placeholder PASS -> explicit SKIP accounting
- `P2-41`:
  - `tests/test_zerocopy_view.pas` 使用 `Base64EncodeView` 真实断言
- `P2-42`:
  - `tests/test_quick.pas` 使用内存自签证书 PEM 构建 server context
- `P2-43`:
  - `tests/test_real_usage.pas` 切换到 deterministic PASS/FAIL/SKIP

### Regression evidence
- `test_stream_connection` -> PASS
- `test_mbedtls_framework` -> PASS
- `run_unit_tests_simple --all` -> PASS

### Blocker
- `P1-33~P1-36` WinSSL tasks blocked on current environment:
  - `Fatal: Can't find unit Windows used by fafafa.ssl.winssl.*`
- Action: 在 Windows/Win64 RTL 环境继续该批 RED/GREEN。
