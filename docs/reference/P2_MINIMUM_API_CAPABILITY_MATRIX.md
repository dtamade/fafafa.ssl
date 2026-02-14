# P2 模块最低可用 API 与能力矩阵字段（OpenSSL）

**状态**：Phase 1 已确认（2026-02-06）  
**范围**：PKCS7 / CMS / PKCS12 / OCSP / CT / TS / Store  
**目标**：明确每个模块“最低可用 API 集”与“能力矩阵字段映射”，并显式标注支持状态。

---

## 先看结论

- OpenSSL 3.5.4 基线下，P2 模块测试集已通过（见 `docs/test_reports/P2_MODULES_TEST_REPORT.md`）。
- `TSSLBackendCapabilities` 已能直接表达 **PKCS12 / CT**，并部分表达 **OCSP / Store**。
- **PKCS7 / CMS / TS** 当前没有一对一能力字段，使用“模块加载状态 + 测试结果”作为显式支持依据。

---

## 验收口径（本轮统一定义）

一个模块被判定为“支持”需要同时满足：

1. 最低可用 API 集存在并可调用。
2. 模块加载成功（`TOpenSSLLoader.IsModuleLoaded(...)` 或函数指针可用）。
3. 对应模块测试在当前基线上通过。
4. 若当前运行环境后端不可用，测试输出必须是结构化 SKIP（如 `[SKIP] [backend-not-available]` / `[SKIP] [dependency]` / `[SKIP] [capability]`），且不得计入 PASS。

5. 对 WinSSL 等平台特定测试，在非目标平台必须输出结构化阻塞契约（`[BLOCKED] [platform]` + `[SKIP] [platform]`），不得记为 FAIL。

---

## 模块清单（最低可用 API + 字段映射）

| 模块 | 最低可用 API 集（必须可用） | 能力矩阵字段映射 | 当前状态（OpenSSL 3.5.4） |
|---|---|---|---|
| PKCS7 | `LoadPKCS7Functions` + `SignData` + `VerifySignedData` + `EncryptData` + `DecryptData` | 无直接字段（用模块加载状态 `osmPKCS7` + 测试结果） | ✅ 支持 |
| CMS | `LoadOpenSSLCMS` + `CMSSignData` + `CMSVerifySignature` + `CMSEncryptData` + `CMSDecryptData` | 无直接字段（用模块加载状态 `osmCMS` + 测试结果） | ✅ 支持 |
| PKCS12 | `LoadPKCS12Module` + `PKCS12_create` + `PKCS12_parse` + `d2i_PKCS12_bio` + `i2d_PKCS12_bio` | `SupportsPKCS12`（直接字段） | ✅ 支持 |
| OCSP | `LoadOpenSSLOCSP` + `CheckCertificateStatus` + `CreateOCSPRequest` + `SendOCSPRequest` + `VerifyOCSPResponse` | `SupportsOCSPStapling` / `OCSPStaplingSupport`（仅装订能力，非完整 OCSP 客户端语义） | ✅ 支持（字段部分映射） |
| CT | `LoadCTFunctions` + `EnableCertificateTransparency` + `ValidateSCTList` + `LoadCTLogStore` + `X509_get_SCT_LIST` | `SupportsCertificateTransparency` / `CertTransparencySupport`（直接映射） | ✅ 支持 |
| TS | `LoadTSFunctions` + `CreateTimestampRequest` + `VerifyTimestampResponse` + `GetTimestampTime` | 无直接字段（用函数可用性 + 测试结果） | ✅ 支持 |
| Store | `LoadSTOREFunctions` + `LoadCertificateFromStore` + `LoadPrivateKeyFromStore` + `LoadCertificateChainFromStore` + `SearchCertificateByAlias` | `SupportsSystemCertStore` / `RequiresExternalLibrary`（部分映射） | ✅ 支持（字段部分映射） |

---

## 字段来源与判定依据

- 能力矩阵结构：`src/fafafa.ssl.base.pas` 中的 `TSSLBackendCapabilities`。
- OpenSSL 字段赋值：`src/fafafa.ssl.openssl.backed.pas` 的 `TOpenSSLLibrary.GetCapabilities`。
- 序列化输出：`src/fafafa.ssl.capability.serializer.pas`，示例文件：`capability_openssl.json`、`capability_openssl.xml`。
- P2 测试汇总：`docs/test_reports/P2_MODULES_TEST_REPORT.md`。

---

## 特别说明（避免误解）

### PKCS12 helper API 的编译开关

`CreatePKCS12` / `ParsePKCS12` / `LoadPKCS12FromFile` 等 helper 在
`src/fafafa.ssl.openssl.api.pkcs12.pas` 中受 `ENABLE_PKCS12_HELPERS` 条件编译控制。  
本清单将低层 API（`PKCS12_create`、`PKCS12_parse`、`d2i/i2d_PKCS12_bio`）作为最低门槛。

### OCSP 字段是“装订能力”不是“全 OCSP 客户端”

`SupportsOCSPStapling` 和 `OCSPStaplingSupport` 只直接表示 TLS OCSP stapling 能力。  
完整 OCSP 请求/响应处理能力仍需结合模块 API 可用性与测试结果确认。

### 无直接字段模块的显式策略

PKCS7 / CMS / TS 当前无一对一 capability 字段，采用以下显式规则：

- OpenSSL 模块加载成功；
- 最低可用 API 集可调用；
- 模块测试通过。

这三项都满足时，标记为“支持”。

---

## 可复现验证命令

运行：

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
python3 scripts/compile_all_modules.py
```

然后查看：

```bash
cat docs/test_reports/P2_MODULES_TEST_REPORT.md
```

