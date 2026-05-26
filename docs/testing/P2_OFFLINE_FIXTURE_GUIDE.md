# P2 离线测试夹具指南（Q1 2026）

**目标**：为 PKCS7 / CMS / PKCS12 / OCSP / CT / TS / Store 提供统一、可复现的离线测试数据约定。  
**适用范围**：`tests/certificate/test_p2_*.pas`、`tests/crypto/test_p2_store*.pas`。

---

## 现在可用的夹具

当前仓库里可直接复用的固定夹具在：

- `tests/certificate/test_certs/ca_cert.pem`
- `tests/certificate/test_certs/ca_key.pem`
- `tests/certificate/test_certs/signer_cert.pem`
- `tests/certificate/test_certs/signer_key.pem`
- `tests/certificate/test_certs/recipient_cert.pem`
- `tests/certificate/test_certs/recipient_key.pem`
- `tests/certificate/test_certs/test_data.txt`

这些文件已被 PKCS7/PKCS12 相关测试直接引用。

---

## 统一目录规范（新增）

本轮已建立统一目录骨架：

- `tests/fixtures/p2/pkcs7/`
- `tests/fixtures/p2/cms/`
- `tests/fixtures/p2/pkcs12/`
- `tests/fixtures/p2/ocsp/`
- `tests/fixtures/p2/ct/`
- `tests/fixtures/p2/ts/`
- `tests/fixtures/p2/store/`

建议后续新增样本都放到以上目录，不再散落到单个测试文件旁。

---

## 本轮新增失败夹具（7/7 基线）

- `tests/fixtures/p2/pkcs7/pkcs7_malformed_v1.der`
- `tests/fixtures/p2/cms/cms_malformed_v1.der`
- `tests/fixtures/p2/pkcs12/pkcs12_malformed_v1.der`
- `tests/fixtures/p2/ocsp/ocsp_response_malformed_v1.der`
- `tests/fixtures/p2/ct/ct_log_list_invalid_v1.txt`
- `tests/fixtures/p2/ts/ts_response_malformed_v1.der`
- `tests/fixtures/p2/store/store_invalid_cert_payload_v1.txt`

---

## 每个模块最少需要哪些样本

| 模块 | 最低离线样本（建议） | 当前状态 |
|---|---|---|
| PKCS7 | 签名输入、签名输出、篡改后的输入 | 已有失败样本：`pkcs7_malformed_v1.der`（成功链路复用 `test_certs`） |
| CMS | 签名/加密输入、签名体/密文样本 | 已有失败样本：`cms_malformed_v1.der` |
| PKCS12 | `.p12` 正常样本、错误密码样本、损坏样本 | 已有失败样本：`pkcs12_malformed_v1.der`（成功链路可由现有证书生成） |
| OCSP | 固定 OCSP 请求/响应（DER）与无效响应样本 | 已有失败样本：`ocsp_response_malformed_v1.der` |
| CT | 固定 SCT 列表样本、日志列表样本、无效 SCT 样本 | 已有失败样本：`ct_log_list_invalid_v1.txt` |
| TS | 固定 TSQ/TSR 样本、签名无效或时间无效样本 | 已有失败样本：`ts_response_malformed_v1.der` |
| Store | 文件存储 URI 样本、缺失对象样本、错误 URI 样本 | 已有失败样本：`store_invalid_cert_payload_v1.txt` |

---

## 已接入的失败夹具测试

- `tests/certificate/test_p2_pkcs7_comprehensive.pas`：`TestPKCS7_OfflineMalformedFixture`
- `tests/certificate/test_p2_cms_comprehensive.pas`：`TestCMS_OfflineMalformedFixture`
- `tests/certificate/test_p2_pkcs12_comprehensive.pas`：`TestPKCS12_OfflineMalformedFixture`
- `tests/certificate/test_p2_ocsp_comprehensive.pas`：OCSP malformed fixture 用例
- `tests/certificate/test_p2_ct_comprehensive.pas`：CT invalid fixture 用例
- `tests/certificate/test_p2_ts_comprehensive.pas`：TS malformed fixture 用例
- `tests/crypto/test_p2_store_comprehensive.pas`：`TestSTORE_OfflineInvalidFixture`

说明：当前已完成“每个模块至少 1 个离线失败夹具”的基线，后续继续补充高价值失败场景（密码错误、证书链不可信、时间窗口无效等）。

---

## 生成与更新建议

优先原则：

1. 先复用 `tests/certificate/test_certs/` 已有证书链。
2. 新样本只做增量，不改写历史样本。
3. 每个样本都要有一个对应的“失败场景”版本。

建议在后续迭代中新增：

- 一个 `scripts/fixtures/generate_p2_fixtures.sh`（统一生成入口）
- 一个 `tests/fixtures/p2/README.md`（已创建，后续持续补充样本来源、命令、预期哈希）

---

## 验证命令

生成/更新夹具后，运行：

```bash
# 推荐：fast-local（输出到 ./tmp，避免污染 git 工作区）
bash scripts/run_all_module_tests.sh --fast-local --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
python3 scripts/compile_all_modules.py
```

并在报告中确认没有新增失败：

```bash
ls -1 tmp/test-reports/test_report_*.txt | tail -n 3
```

若需要把证据落盘到 `test-reports/`（便于提交/归档），请显式指定输出目录：

```bash
bash scripts/run_all_module_tests.sh --reports-dir test-reports --bin-dir bin --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```
