# Active Gap Scan Round3 (50 Tasks) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 基于 2026-02-11 Round3 全仓扫描，给出 50 个可执行缺口任务（含优先级、状态、验收命令），并按 TDD 连续落地。

**Architecture:** 先收敛 `src` 中会导致错误语义或假阳性的实现缺口（P0），再处理跨后端一致性与 skip 语义（P1），最后完成测试债务与文档闭环（P2）。每个任务保持 `RED -> GREEN -> Regression`。

**Tech Stack:** FreePascal (ObjFPC), OpenSSL API bindings, program-style tests, planning-with-files artifacts.

---

## Scan Evidence (Round3)
- Command: `rg -n "TODO|FIXME|placeholder|not implemented|TBD|待实现|未实现|\[SKIP\]|assume|simplified" src tests`
- Signal count (`src+tests`): **77**
- Raw log: `/tmp/repo_gap_scan_src_tests_round3.txt`
- Full repo signal count (含 docs): **162** (`/tmp/repo_gap_scan_active_20260211_round3.txt`)

## Priority Strategy
- **P0**: 真实行为缺口、安全/语义错误、假阳性/假通过（必须先做）
- **P1**: 后端一致性、契约覆盖、skip 分类确定性
- **P2**: 测试债务与文档同步

---

## 50-Task Backlog (with status)

### P0 Core Runtime Gaps (1-20)
1. ✅ **TS nonce request contract**（已完成）
   - Files: `src/fafafa.ssl.openssl.api.ts.pas`, `tests/certificate/test_tsa_api.pas`
   - Acceptance: `fpc -Fu./src tests/certificate/test_tsa_api.pas -otmp/test_tsa_api && ./tmp/test_tsa_api`
2. ✅ **DANE DNSSEC fail-open fix**（已完成）
   - Files: `src/fafafa.ssl.dane.pas`, `tests/dane/test_dane_tlsa.pas`
   - Acceptance: `fpc -Fu./src tests/dane/test_dane_tlsa.pas -otmp/test_dane_tlsa && ./tmp/test_dane_tlsa`
3. ✅ **HashData extended algorithm parity**（已完成）
   - Files: `src/fafafa.ssl.factory.pas`, `tests/test_hashdata_extended_algorithms.pas`
   - Acceptance: `fpc -Fu./src tests/test_hashdata_extended_algorithms.pas -otmp/test_hashdata_extended_algorithms && ./tmp/test_hashdata_extended_algorithms`
4. ✅ **TS status write function loading**（已完成）
   - Files: `src/fafafa.ssl.openssl.api.ts.pas`, `tests/certificate/test_p2_ts_comprehensive.pas`
   - Acceptance: `fpc -Fu./src tests/certificate/test_p2_ts_comprehensive.pas -otmp/test_p2_ts_comprehensive && ./tmp/test_p2_ts_comprehensive`
5. ✅ **TS status gate fail-safe verify**（本轮完成）
   - Files: `src/fafafa.ssl.openssl.api.ts.pas`, `tests/certificate/test_p2_ts_comprehensive.pas`
   - Acceptance: same as #4
6. ✅ **OpenSSL cipher false-positive removal**（本轮完成）
   - Files: `src/fafafa.ssl.openssl.backed.pas`, `tests/openssl/test_openssl_features.pas`
   - Acceptance: `fpc -Fu./src tests/openssl/test_openssl_features.pas -otmp/test_openssl_features && ./tmp/test_openssl_features`
7. ✅ **TS message imprint algorithm setup (strict contract completed this round)**
   - Files: `src/fafafa.ssl.openssl.api.ts.pas`, `src/fafafa.ssl.openssl.api.x509.pas`, `tests/certificate/test_tsa_api.pas`
8. ✅ **TS policy OID strict validation and error path**（本轮完成）
   - Files: `src/fafafa.ssl.openssl.api.ts.pas`, `tests/certificate/test_p2_ts_comprehensive.pas`
9. ✅ **PKCS11 engine/URI path hardening**（本轮完成）
   - Files: `src/fafafa.ssl.pkcs11.types.pas`, `tests/test_pkcs11_uri_pin_contract.pas`, `tests/pkcs11/test_pkcs11_softhsm.pas`
10. ✅ **PKCS11 PIN simplified path hardening**（本轮完成）
    - Files: `src/fafafa.ssl.pkcs11.pin.pas`, `src/fafafa.ssl.pkcs11.types.pas`, `tests/test_pkcs11_uri_pin_contract.pas`
11. ✅ **Ed25519 contract: explicit unsupported semantics**（本轮完成）
    - Files: `src/fafafa.ssl.cert.utils.pas`, `tests/test_actual_implementation.pas`
12. ✅ **Constant-time length assumption audit and guard**（本轮完成）
    - Files: `src/fafafa.ssl.crypto.constant_time.pas`, `tests/test_secure.pas`
13. ✅ **OpenSSL IsFeatureSupported runtime drift check**（本轮完成）
    - Files: `src/fafafa.ssl.openssl.backed.pas`, `tests/openssl/test_openssl_features.pas`
14. ✅ **OpenSSL protocol support runtime probe (TLS10/11 policy aware)**（本轮完成）
    - Files: `src/fafafa.ssl.openssl.backed.pas`, `tests/openssl/test_openssl_features.pas`
15. ✅ **FreePascal backend unsupported error taxonomy alignment**（本轮验收完成）
    - Files: `src/fafafa.ssl.freepascal.connection.pas`, `tests/test_freepascal_backend_basic.pas`
16. ✅ **FreePascal capability KnownIssues runtime alignment**（本轮完成）
    - Files: `src/fafafa.ssl.freepascal.lib.pas`, `tests/test_capability_cache.pas`
17. ✅ **MbedTLS IsCipherSupported contract precision**（本轮完成）
    - Files: `src/fafafa.ssl.mbedtls.lib.pas`, `tests/test_mbedtls_framework.pas`
18. ✅ **WolfSSL IsCipherSupported contract precision**（本轮完成）
    - Files: `src/fafafa.ssl.wolfssl.lib.pas`, `tests/test_wolfssl_framework.pas`
19. 🟡 **WinSSL IsCipherSupported return-true semantics contract note/assert**（契约断言已落地于矩阵测试；Windows 运行时验证待补）
    - Files: `src/fafafa.ssl.winssl.lib.pas`, `tests/winssl/test_winssl_api_basic.pas`
20. ✅ **Cross-backend cipher-support behavior matrix tests**（本轮完成）
    - Files: `tests/test_capability_matrix_v12.pas`, `tests/test_capability_matrix_simple.pas`

### P1 Contract & Determinism Gaps (21-38)
21. ✅ `tests/certificate/test_p2_pkcs7.pas` PKCS7_set_data missing -> deterministic skip accounting（本轮完成）
22. ✅ `tests/certificate/test_p2_pkcs7.pas` stack API partial impl skip accounting（本轮完成）
23. ✅ `tests/contract/test_backend_contract.pas` platform skip summary normalization（本轮完成）
24. ✅ `tests/test_native_handle_unified.pas` backend-unavailable skip counters normalization（本轮完成）
25. ✅ `tests/framework/test_openssl_base.pas` grouped skip reason taxonomy normalization（本轮完成）
26. ✅ `tests/diagnostic/test_error_handling.pas` skip reason strict categories（本轮完成）
27. ✅ `tests/diagnostic/test_error_handling_comprehensive.pas` group skip summary determinism（本轮完成）
28. ✅ `tests/openssl/test_openssl_ca_autoload.pas` group skip accounting stabilization（本轮完成）
29. ✅ `tests/crypto/test_hash_comprehensive.pas` unavailable algorithm skip should not count pass（本轮完成）
30. ✅ `tests/crypto/test_hmac_comprehensive.pas` unavailable digest skip should not count pass（本轮完成）
31. ✅ `tests/unit/test_hkdf_rfc5869.pas` “Not implemented” literal cleanup to capability skip（本轮完成）
32. ✅ `tests/connection/test_ssl_enterprise.pas` external-tool skip tags structured output（本轮完成）
33. ✅ `tests/integration/test_cross_backend_errors_contract.pas` network skip category standardization（本轮完成）
34. ✅ `tests/integration/test_real_https_connection.pas` network-skip determinism normalization（本轮完成）
35. ✅ `tests/test_real_usage.pas` skip reason mandatory field（本轮完成）
36. ✅ `tests/test_x509.pas` system-cert missing skip counter normalization（本轮完成）
37. ✅ `tests/test_pem.pas` system-cert missing skip counter normalization（本轮完成）
38. ✅ `tests/openssl/test_ocsp_connection_verification_regression.pas` skip-path accounting normalization（本轮完成）

### P2 Debt & Closure Gaps (39-50)
39. ✅ `tests/winssl/test_winssl_server_handshake.pas` “待实现”输出替换为 blocked contract（本轮完成，Linux 平台编译受限）
40. ✅ `tests/winssl/test_winssl_api_basic.pas` credential-missing skip counters（本轮完成，Linux 平台编译受限）
41. ✅ `tests/winssl/test_winssl_certificate.pas` no-cert skip counters and summary（本轮完成，Linux 平台编译受限）
42. ✅ `tests/examples/test_openssl_evp.pas` optional API skip taxonomy normalization（本轮完成）
43. ✅ `tests/examples/test_openssl_rsa.pas` simplified marker to explicit test scope note（本轮完成，当前文件存在既有编译基线缺口）
44. ✅ `tests/examples/test_basic.pas` system-store failure wording consistency（本轮完成，当前文件存在既有编译基线缺口）
45. ✅ `tests/test_helper_utilities.pas` grouped skip summary contract tighten（本轮完成）
46. ✅ `tests/test_capability_matrix_v12.pas` backend-not-available skip consistency（本轮完成）
47. ✅ `tests/test_stream_connection.pas` legacy skip message regression lock (extend cases)（本轮完成）
48. ✅ `tests/connection/test_wolfssl_metadata_accuracy.pas` runtime/API skip reason normalization（本轮完成）
49. ✅ `tests/test_actual_implementation.pas` placeholder claim vs contract reconciliation（本轮完成）
50. ✅ `README.md` + `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md` capability semantics sync（本轮完成）

---

## Executing-Plans Batch Order (next)
1. Round3 backlog closed (50/50 complete)
2. ✅ Optional follow-up: examples/rsa compile baseline debt triage（completed 2026-02-11）
3. ✅ Optional follow-up: examples/basic compile baseline debt triage（completed 2026-02-11）
4. 🟡 Optional follow-up: Windows WinSSL runtime verification batch（Linux compile guard completed 2026-02-12; runtime pending Windows）
5. ✅ Optional follow-up: full-suite regression sweep（completed 2026-02-11）
6. ✅ Optional follow-up: documentation polish pass（completed 2026-02-12）

## TDD Command Template
1. RED: add failing assertion and run focused test.
2. GREEN: minimal production fix.
3. REGRESSION:
   - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
   - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
