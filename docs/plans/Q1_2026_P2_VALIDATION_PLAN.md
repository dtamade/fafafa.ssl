# Q1 2026 执行计划：P2 模块验证补齐（可离线复现）

**开始日期**：2026-02-06  
**目标**：把 P2 模块从“可用”推进到“生产级可验证”（离线夹具 + 失败场景覆盖 + 可复现实验步骤）。  
**范围**：PKCS7 / PKCS12 / CMS / Store / OCSP / CT / TS

---

## Phase 0 基线（已完成）

在 Linux（Debian 13 / FPC 3.3.1 / OpenSSL 3.5.4）验证：

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
python3 scripts/compile_all_modules.py
```

结果摘要见：
- `docs/test_reports/P2_MODULES_TEST_REPORT.md`

---

## 成功标准（验收口径）

每个模块都需要满足：

1. **可离线跑通**：测试不依赖外网或临时第三方服务。
2. **成功 + 失败路径**：至少覆盖 1 个成功路径 + 2 个高价值失败场景。
3. **版本差异可解释**：OpenSSL 1.1.1 vs 3.x 的差异（符号/弃用/行为）有记录，有回归策略。
4. **文档可复现**：给出命令、输入、预期输出；读者能复现。

---

## 工作拆分（建议顺序）

### Week 1：夹具与失败场景框架（通用）

- 固化夹具目录与约定（证书链、样本数据、日志/响应样本）。
- 为测试新增“失败场景辅助函数”（统一断言与错误输出格式）。
- 把“如何生成夹具”写成脚本或 README（避免手工步骤）。

交付物：
- 一份统一的夹具说明（目录、用途、生成方式）。
- 1–2 个模块新增失败场景测试（作为样板）。

### Week 2–3：PKCS7 / CMS（签名 + 验证 + 加解密）

目标：
- 用仓库内证书链跑通签名/验证/加解密工作流。
- 覆盖失败场景：证书链不可信、签名被篡改、密码/recipient 不匹配等。

交付物：
- 更新/新增离线夹具（必要时）。
- 工作流级测试 + 失败场景测试。
- 文档：如何在本地复现（命令 + 输入/输出）。

### Week 4：PKCS12（创建/解析/导出 + 算法矩阵）

目标：
- 验证创建 → 序列化 → 解析 → 导出/再解析的往返链路。
- 覆盖失败场景：密码错误、损坏的容器、缺失私钥等。
- 记录 OpenSSL 3.x 兼容性与 legacy provider 相关注意事项。

交付物：
- 测试：往返工作流 + 失败场景 +（可选）算法矩阵。
- 文档：`docs/archive/PKCS12_OPENSSL3_COMPATIBILITY_REPORT.md` 的结论与实践步骤补充到用户指南（如需要）。

### Week 5：OCSP / CT / TS / Store（证书服务模块）

目标：
- 建立可离线的验证方式（本地 responder / 固定响应样本 / 可脚本生成）。
- 覆盖失败场景：响应无效、时间不一致、签名不正确、查找失败等。

交付物：
- 离线夹具 + 失败场景测试。
- 文档：本地复现步骤（含如何生成或更新夹具）。

### Week 6：OpenSSL 版本矩阵与回归策略

目标：
- 定义“最低成本但可信”的版本矩阵（至少 1.1.1 + 3.x）。
- 对每个模块给出：哪些函数在 3.x 不可用、如何跳过或替代、为何不影响生产路径。

交付物：
- 一份差异清单（按模块）。
- CI/本地回归建议命令集。

---

## 已完成交付（更新至 2026-02-07）

- [x] 每个 P2 模块“最低可用 API 集”已明确并落盘：
  - `docs/reference/P2_MINIMUM_API_CAPABILITY_MATRIX.md`
- [x] 能力矩阵字段映射（支持/部分支持/无直接字段）已显式记录。
- [x] 无直接字段模块（PKCS7/CMS/TS）已定义统一判定规则：
  - 模块加载 + API 可用 + 测试通过。
- [x] 离线夹具目录骨架已建立（Week 1 首个交付）：
  - `tests/fixtures/p2/{pkcs7,cms,pkcs12,ocsp,ct,ts,store}`
  - `docs/testing/P2_OFFLINE_FIXTURE_GUIDE.md`
- [x] 全模块（7/7）首批离线失败夹具已落地：
  - `tests/fixtures/p2/pkcs7/pkcs7_malformed_v1.der`
  - `tests/fixtures/p2/cms/cms_malformed_v1.der`
  - `tests/fixtures/p2/pkcs12/pkcs12_malformed_v1.der`
  - `tests/fixtures/p2/ocsp/ocsp_response_malformed_v1.der`
  - `tests/fixtures/p2/ct/ct_log_list_invalid_v1.txt`
  - `tests/fixtures/p2/ts/ts_response_malformed_v1.der`
  - `tests/fixtures/p2/store/store_invalid_cert_payload_v1.txt`
- [x] 七个 comprehensive 测试均已接入离线失败夹具用例并通过：
  - `test_p2_pkcs7_comprehensive` / `test_p2_cms_comprehensive` / `test_p2_pkcs12_comprehensive`
  - `test_p2_ocsp_comprehensive` / `test_p2_ct_comprehensive` / `test_p2_ts_comprehensive`
  - `test_p2_store_comprehensive`
- [x] 7 模块聚焦回归全通过：
  - `bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT`
  - 结果：15/15 通过（`test-reports/test_report_20260207_025418.txt`）
- [x] OpenSSL 1.1.1 vs 3.x 差异清单与回归策略已落盘：
  - `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`
  - 包含符号差异、行为差异、P2 模块回归分层命令（Tier 1/2/3）
- [x] Store 模块文档交付（模块报告 + 使用指南）已落盘：
  - `docs/test_reports/P2_STORE_MODULE_REPORT.md`
  - `docs/guides/STORE_USAGE_GUIDE.md`
  - `docs/reference/STORE_CROSS_PLATFORM_DIFFERENCES.md`
- [x] OCSP 模块文档交付（模块报告 + 使用指南）已落盘：
  - `docs/test_reports/P2_OCSP_MODULE_REPORT.md`
  - `docs/guides/OCSP_USAGE_GUIDE.md`
- [x] TS 模块文档交付（模块报告 + 使用指南）已落盘：
  - `docs/test_reports/P2_TS_MODULE_REPORT.md`
  - `docs/guides/TS_USAGE_GUIDE.md`
- [x] 证书服务离线验证说明（CT/OCSP/TS）已落盘：
  - `docs/testing/P2_CERT_SERVICE_OFFLINE_VALIDATION_GUIDE.md`

---

## 模块任务清单（可并行）

### 全模块离线失败夹具基线（本轮已完成）
- [x] PKCS7：malformed DER 解析失败用例
- [x] CMS：malformed DER 解析失败用例
- [x] PKCS12：malformed DER 解析失败用例
- [x] OCSP：malformed 响应用例
- [x] CT：invalid log list 用例
- [x] TS：malformed 响应用例
- [x] Store：invalid file payload 用例

### PKCS7
- [x] 工作流测试：签名创建/验证（detached 签名 + 验签）
- [x] 工作流测试：加密/解密（recipient 证书）
- [x] 失败场景：篡改数据导致验证失败
- [x] 失败场景：证书链不可信/缺失 CA
- [x] 失败场景：空输入验签失败
- [x] 失败场景：接收者不匹配解密失败

### CMS
- [x] 工作流测试：CMS_sign/CMS_verify（detached）
- [x] 工作流测试：CMS_encrypt/CMS_decrypt（recipient 解密成功）
- [x] 失败场景：Signer/Recipient 不匹配（错误接收者解密失败）
- [x] 失败场景：篡改数据/空输入/缺失受信任 CA 验签失败

### PKCS12
- [x] 往返测试：create → export → parse → export
- [x] 失败场景：错误密码
- [x] 失败场景：损坏的容器数据（malformed fixture）
- [x] 失败场景：缺失私钥容器（parse 失败或私钥为空）
- [ ] 记录 legacy provider 相关策略（如需要）

### OCSP
- [x] 离线验证：固定响应样本或本地 responder
- [x] 失败场景：无效签名（无签名/无效响应验证失败）
- [x] 失败场景：时间窗口无效（future thisUpdate / expired nextUpdate）
- [x] 失败场景：malformed/截断 DER 解析失败
- [x] 模块级报告 + 使用指南（P2_OCSP_MODULE_REPORT + OCSP_USAGE_GUIDE）

### CT
- [x] 离线验证：固定 SCT/日志数据
- [x] 失败场景：时间/issuer 不匹配
- [x] 失败场景：invalid log list/缺失文件加载失败

### TS
- [x] 离线验证：固定 TSR 样本或本地 TSA
- [x] 失败场景：签名无效（空响应签名验证失败）
- [x] 失败场景：拒绝状态响应（status rejection 或无状态信息）验证失败
- [x] 失败场景：malformed/截断响应解析失败
- [x] 模块级报告 + 使用指南（P2_TS_MODULE_REPORT + TS_USAGE_GUIDE）

### Store
- [x] 失败场景：打开/查找失败的错误路径（invalid payload + missing file）
- [x] 跨平台差异记录（Linux/macOS/Windows 的证书存储路径/行为）
- [x] 模块级报告 + 使用指南（P2_STORE_MODULE_REPORT + STORE_USAGE_GUIDE）
