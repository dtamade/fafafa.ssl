# P2 TS 模块测试报告

**模块**：TS（Time-Stamp Protocol）  
**范围**：OpenSSL TS API 绑定、辅助函数、离线失败场景  
**阶段**：Q1 2026 P2 验证补齐

---

## 1. 验证目标

- 确认 TS 关键入口在 OpenSSL 3.x 下可加载并可调用。
- 确认关键失败路径（malformed、truncated、rejection/无状态、空响应签名失败）稳定可复现。
- 提供模块级可复现命令与证据。

---

## 2. 代码范围

核心实现：`src/fafafa.ssl.openssl.api.ts.pas`

- 模块加载：`LoadTSFunctions`
- 请求构造：`CreateTimestampRequest`
- 响应验证：`VerifyTimestampResponse`
- 时间提取：`GetTimestampTime`

关键实现位置：
- `src/fafafa.ssl.openssl.api.ts.pas:454`
- `src/fafafa.ssl.openssl.api.ts.pas:555`
- `src/fafafa.ssl.openssl.api.ts.pas:651`
- `src/fafafa.ssl.openssl.api.ts.pas:703`

---

## 3. 测试覆盖与失败场景

主测试：`tests/certificate/test_p2_ts_comprehensive.pas`

重点覆盖：
- API 绑定加载（请求/响应/验证/序列化/状态）
- OpenSSL 1.x only 函数在 3.x 下按分支跳过
- 离线失败场景：
  - malformed 响应 fixture
  - truncated 响应解析失败
  - rejection 或无状态信息验证失败
  - 空响应签名验证失败

关键用例位置：
- `tests/certificate/test_p2_ts_comprehensive.pas:238`
- `tests/certificate/test_p2_ts_comprehensive.pas:297`
- `tests/certificate/test_p2_ts_comprehensive.pas:335`
- `tests/certificate/test_p2_ts_comprehensive.pas:410`

离线夹具：
- `tests/fixtures/p2/ts/ts_response_malformed_v1.der`

---

## 4. 当前结果摘要

基于模块综合结果：
- `docs/archive/reports/test-p2-history/test_p2_ts_comprehensive_result.txt`
- 总测试数：58
- 通过：58
- 失败：0
- 通过率：100%

本轮（2026-02-07）回归：
- TS 模块回归：`docs/archive/reports/test-report-history/test_report_20260207_024905.txt`（2/2，100%）
- CT/OCSP/TS 联合回归：`docs/archive/reports/test-report-history/test_report_20260207_025410.txt`（6/6，100%）
- P2 聚焦回归：`docs/archive/reports/test-report-history/test_report_20260207_025418.txt`（15/15，100%）

---

## 5. 复现命令

```bash
bash scripts/run_all_module_tests.sh --modules TS --verbose
```

可选（可执行存在时）：

```bash
./bin/test_p2_ts_comprehensive
```

---

## 6. 已知限制与说明

- `VerifyTimestampResponse` 在状态值提取上采用当前实现的简化路径，优先保证 fail-closed 与稳定验证。
- OpenSSL 3.x 场景下，部分 1.x only TS 符号按测试分支跳过，不作为失败判据。
- `LoadTSFunctions` 已适配 OpenSSL 3.x 的 BIO 符号命名（`d2i_TS_REQ_bio` / `d2i_TS_RESP_bio`）。

---

## 7. 关联文档

- `docs/guides/TS_USAGE_GUIDE.md`
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
- `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- `docs/plans/Q1_2026_P2_VALIDATION_PLAN.md`
