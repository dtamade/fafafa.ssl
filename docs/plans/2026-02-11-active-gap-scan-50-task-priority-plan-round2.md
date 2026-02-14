# Active Gap Scan Round2 (50 Tasks) Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** 全仓扫描活跃未完成项与缺口，形成 50 个可执行任务并按优先级推进；本轮先执行可在 Linux 环境落地的测试契约收敛任务。

**Architecture:** 以 `src/ + tests/ + docs(非archive)` 为主范围，优先修复“假通过/无计数SKIP/摘要不确定”问题，再推进后端能力与文档一致性；WinSSL 相关任务保留为 Windows 批次。

**Tech Stack:** FreePascal (ObjFPC), program-style tests, markdown planning artifacts.

---

## Scan Evidence (Round2)
- Command: `rg -n "TODO|FIXME|placeholder|not implemented|TBD|待实现|未实现|\[SKIP\]|skipping group|skipping test" src tests docs --glob '!docs/archive/**' ...`
- Signal count: `130`
- Raw log: `/tmp/repo_gap_scan_active_20260211_round2.txt`

## Priority Strategy
- **P0**: 测试结果口径确定性（skip accounting、summary 防除零、placeholder/待实现显式化）。
- **P1**: 后端能力与契约一致性（OpenSSL/WolfSSL/MbedTLS/WinSSL/FreePascal）。
- **P2**: 文档与报告去漂移（TODO/TBD 同步、状态矩阵闭环）。

---

## P0 Tasks (1-18)
1. `tests/openssl/test_openssl_ca_autoload.pas` skip accounting deterministic
2. `tests/diagnostic/test_error_handling.pas` skip accounting deterministic
3. `tests/diagnostic/test_error_handling_comprehensive.pas` group skip accounting deterministic
4. `tests/contract/test_backend_contract.pas` platform skip计数与总结
5. `tests/test_native_handle_unified.pas` backend-unavailable skip计数统一
6. `tests/integration/test_cross_backend_errors_contract.pas` skip不再计作PASS
7. `tests/certificate/test_p2_pkcs7.pas` needs-PKCS7_set_data skip contract explicit
8. `tests/certificate/test_p2_pkcs7.pas` stack API partial-impl skip contract explicit
9. `tests/winssl/test_winssl_server_handshake.pas` “待实现”转显式 blocked contract
10. `tests/winssl/test_winssl_api_basic.pas` credential-missing skip计数
11. `tests/winssl/test_winssl_certificate.pas` no-cert skip计数
12. `tests/framework/test_openssl_base.pas` skip reason分类统一
13. `tests/test_x509.pas` system cert missing skip计数
14. `tests/crypto/test_hash_comprehensive.pas` algorithm unavailable skip计数
15. `tests/crypto/test_hkdf_rfc5869.pas` SHA-1 unavailable skip计数
16. `tests/crypto/test_hmac_comprehensive.pas` digest unavailable skip计数
17. `tests/examples/test_openssl_bn.pas` optional API skip计数
18. `tests/examples/test_openssl_evp.pas` optional API skip计数

## P1 Tasks (19-38)
19. `src/fafafa.ssl.openssl.api.ts.pas` nonce TODO路径契约测试化
20. `src/fafafa.ssl.openssl.connection.pas` invalid cert-verify cache hit路径回归
21. `src/fafafa.ssl.factory.pas` hash capability matrix vs实现一致性
22. `src/fafafa.ssl.cert.utils.pas` Ed25519未实现语义能力声明
23. `src/fafafa.ssl.dane.pas` invalid TLSA entries skip/diagnostic contract
24. `src/fafafa.ssl.freepascal.connection.pas` unsupported KeyUpdate suite coverage
25. `src/fafafa.ssl.freepascal.connection.pas` unsupported encrypted-handshake suite coverage
26. `src/fafafa.ssl.freepascal.connection.pas` unsupported client-finished suite coverage
27. `src/fafafa.ssl.freepascal.connection.pas` unsupported app-data suite coverage
28. `src/fafafa.ssl.freepascal.connection.pas` MarkUnsupported error-class contract
29. `tests/pkcs11/test_pkcs11_softhsm.pas` deterministic skip accounting
30. `tests/connection/test_ssl_enterprise.pas` external-tool skip分类明确
31. `tests/connection/test_wolfssl_metadata_accuracy.pas` runtime/API skip reasons normalization
32. `tests/openssl/test_openssl_features.pas` skip reasons normalization
33. `tests/diagnostic/test_error_handling_comprehensive.pas` perf-group timing nondeterminism guard
34. `tests/diagnostic/test_error_handling.pas` error-queue separator contract hardening
35. `tests/openssl/test_openssl_ca_autoload.pas` real network dependency gated by env var
36. `tests/test_capability_matrix_v12.pas` backend unavailable skip统计
37. `tests/test_capability_serialization.pas` backend unavailable skip统计
38. `tests/test_real_usage.pas` remaining optional checks contract normalization

## P2 Tasks (39-50)
39. `docs/guides/WINSSL_QUICKSTART.md` 待实现段落与当前状态同步
40. `docs/reference/API_INVENTORY.md` 缺失方法清单与代码现状同步
41. `docs/testing/TEST_RESULTS.md` “未实现函数”原因更新
42. `docs/test_reports/P2_PKCS12_TEST_REPORT.md` TODO项状态同步
43. `docs/test_reports/WAVE_C_B113_RELEASE_SIGNOFF_RECORD_2026-02-08.md` TBD字段收口
44. `docs/test_reports/WAVE_B_CROSS_PLATFORM_GATE_MANIFEST_2026-02-08.md` TODO字段收口
45. `docs/test_reports/REPO_GAP_TASK_STATUS_MATRIX_2026-02-11.md` round2状态更新
46. `docs/plans/AUTONOMOUS_ITERATION_PROTOCOL.md` round2执行协议附录
47. `task_plan.md` round2执行回写
48. `findings.md` round2发现回写
49. `progress.md` round2命令证据回写
50. 新增 round2 iteration plan 与执行记录闭环

---

## Immediate Execution Batch (default first 3 tasks)
1. P0-01 `test_openssl_ca_autoload` skip accounting
2. P0-02 `test_error_handling` skip accounting
3. P0-03 `test_error_handling_comprehensive` group skip accounting

## TDD Template (each task)
1. RED: inject failing assertion/compile reference and run target command
2. GREEN: minimal implementation and rerun target
3. REGRESSION:
   - `fpc -Fu./src tests/test_stream_connection.pas -otmp/test_stream_conn && ./tmp/test_stream_conn`
   - `fpc -Fu./src -Fu./tests/framework -Fu./tests/unit tests/unit/run_unit_tests_simple.lpr -otmp/run_unit_tests_simple && ./tmp/run_unit_tests_simple --format=plain --all`
4. Writeback: `task_plan.md` / `findings.md` / `progress.md`
