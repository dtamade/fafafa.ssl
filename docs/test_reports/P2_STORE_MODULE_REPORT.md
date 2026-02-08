# P2 Store 模块测试报告

**模块**：Store  
**范围**：OpenSSL Store API（`OSSL_STORE_*`）与离线失败场景  
**阶段**：Q1 2026 P2 验证补齐

---

## 1. 验证目标

- 验证 Store 模块在 OpenSSL 3.x 下可加载、可调用、可离线复现。
- 验证关键失败路径（无效 payload、缺失文件）行为稳定且可重复。
- 为跨平台差异文档提供可追溯的测试证据。

---

## 2. 环境基线

- OS：Debian 13 / x86_64
- FPC：3.3.1
- OpenSSL：3.5.4

---

## 3. 主要测试入口

- 模块综合测试：`tests/crypto/test_p2_store_comprehensive.pas`
- 离线失败夹具：`tests/fixtures/p2/store/store_invalid_cert_payload_v1.txt`
- 缺失文件失败用例：`tests/crypto/test_p2_store_comprehensive.pas:307`

---

## 4. 当前结果摘要

基于 `test-reports/test_p2_store_comprehensive_result.txt`：

- 总测试数：56
- 通过：56
- 失败：0
- 通过率：100%

本轮（2026-02-07）聚焦回归：
- Store 模块：`test-reports/test_report_20260207_022221.txt`（2/2，100%）
- P2 聚焦回归：`test-reports/test_report_20260207_024912.txt`（15/15，100%）

已覆盖的关键失败路径：
- 无效 payload（`file:` URI 指向非证书文本）
- 缺失文件（`file:` URI 指向不存在路径）

---

## 5. 回归命令（可复现）

```bash
bash scripts/run_all_module_tests.sh --modules Store --verbose
```

可选直接运行（已编译可执行存在时）：

```bash
./bin/test_p2_store_comprehensive
```

---

## 6. 关联文档

- `docs/reference/STORE_CROSS_PLATFORM_DIFFERENCES.md`
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
- `docs/plans/Q1_2026_P2_VALIDATION_PLAN.md`
