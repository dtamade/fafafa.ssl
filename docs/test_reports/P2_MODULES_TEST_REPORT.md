# P2 模块测试报告（汇总）

**目的**：记录 P2 核心模块在不同环境下的验证结果（可复现命令 + 结果摘要）。  
**适用范围**：PKCS7 / PKCS12 / CMS / Store / OCSP / CT / TS

---

## 最新结果（Linux / OpenSSL 3.x）

**日期**：2026-02-07  
**系统**：Debian 13 / Linux x86_64  
**FPC**：3.3.1  
**OpenSSL**：3.5.4（libcrypto.so.3）

### 运行命令

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

### 结果摘要

- 总测试：15
- 通过：15
- 失败：0
- 通过率：100%

脚本生成的详细报告（本次运行）：
- `docs/archive/reports/test-report-history/test_report_20260207_025418.txt`

> 备注：`test-reports/` 下的运行日志是否纳入版本控制取决于团队策略；本文件只保存“结果摘要 + 可复现命令”。

---

## 模块级备注

- **PKCS12 / OpenSSL 3.x 兼容性**：参考 `docs/archive/PKCS12_OPENSSL3_COMPATIBILITY_REPORT.md`。
