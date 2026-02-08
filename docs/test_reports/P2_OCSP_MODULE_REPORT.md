# P2 OCSP 模块测试报告

**模块**：OCSP  
**范围**：OpenSSL OCSP API 绑定、辅助函数、离线失败场景  
**阶段**：Q1 2026 P2 验证补齐

---

## 1. 验证目标

- 确认 OCSP 关键入口在 OpenSSL 3.x 下可加载并可调用。
- 确认关键失败路径（malformed、truncated、时间窗口无效、无签名响应）稳定失败。
- 提供模块级可复现命令与证据，支撑路线图验收。

---

## 2. 代码范围

核心实现位于 `src/fafafa.ssl.openssl.api.ocsp.pas`：

- 模块加载/卸载：`LoadOpenSSLOCSP` / `UnloadOpenSSLOCSP`
- 工作流辅助函数：
  - `CreateOCSPRequest`
  - `SendOCSPRequest`
  - `VerifyOCSPResponse`
  - `CheckCertificateStatus`

关键实现位置：
- `src/fafafa.ssl.openssl.api.ocsp.pas:486`
- `src/fafafa.ssl.openssl.api.ocsp.pas:513`
- `src/fafafa.ssl.openssl.api.ocsp.pas:685`
- `src/fafafa.ssl.openssl.api.ocsp.pas:719`
- `src/fafafa.ssl.openssl.api.ocsp.pas:983`

---

## 3. 测试覆盖与失败场景

主测试：`tests/certificate/test_p2_ocsp_comprehensive.pas`

重点覆盖：
- API 绑定加载（请求/响应/验证/序列化）
- OpenSSL 1.x only 函数在 3.x 下按分支跳过
- 离线失败场景：
  - malformed 响应 fixture
  - truncated 请求解析失败
  - `OCSP_check_validity` 时间窗口失败
  - 空/无签名响应验证失败

关键用例位置：
- `tests/certificate/test_p2_ocsp_comprehensive.pas:264`
- `tests/certificate/test_p2_ocsp_comprehensive.pas:308`
- `tests/certificate/test_p2_ocsp_comprehensive.pas:334`
- `tests/certificate/test_p2_ocsp_comprehensive.pas:392`

离线夹具：
- `tests/fixtures/p2/ocsp/ocsp_response_malformed_v1.der`

---

## 4. 当前结果摘要

基于模块综合结果：
- `test-reports/test_p2_ocsp_comprehensive_result.txt`
- 总测试数：55
- 通过：55
- 失败：0
- 通过率：100%

本轮（2026-02-07）回归：
- OCSP 模块回归：`test-reports/test_report_20260207_024318.txt`（2/2，100%）
- CT/OCSP/TS 联合回归：`test-reports/test_report_20260207_025410.txt`（6/6，100%）
- P2 聚焦回归：`test-reports/test_report_20260207_025418.txt`（15/15，100%）

---

## 5. 复现命令

```bash
bash scripts/run_all_module_tests.sh --modules OCSP --verbose
```

可选（可执行存在时）：

```bash
./bin/test_p2_ocsp_comprehensive
```

---

## 6. 已知限制与说明

- `SendOCSPRequest` 涉及网络请求（HTTP/HTTPS），线上验证受网络与 responder 状态影响。
- 为保证稳定性，P2 关键失败场景优先使用离线 deterministic 用例。
- OpenSSL 3.x 场景下部分 1.x only 符号按测试分支跳过，不作为失败判据。

---

## 7. 关联文档

- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/plans/Q1_2026_P2_VALIDATION_PLAN.md`
