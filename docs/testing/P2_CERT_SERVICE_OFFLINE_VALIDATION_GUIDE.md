# P2 证书服务模块离线验证指南（CT / OCSP / TS）

**目标**：提供 CT / OCSP / TS 的 deterministic（可重复、离线优先）验证流程。  
**适用范围**：
- `tests/certificate/test_p2_ct_comprehensive.pas`
- `tests/certificate/test_p2_ocsp_comprehensive.pas`
- `tests/certificate/test_p2_ts_comprehensive.pas`

---

## 1. 设计原则

1. **离线优先**：默认使用仓库内固定夹具与本地生成对象，不依赖公网服务。
2. **失败路径可复现**：每个模块至少覆盖 malformed / truncated / 状态异常等高价值失败场景。
3. **在线路径仅补充**：在线 OCSP/TSA/CT 日志仅用于集成补充，不作为主验收依据。
4. **OpenSSL 版本差异显式化**：对 1.x only API 使用分支或跳过策略，不把“未加载可选 API”计为主流程失败。

---

## 2. 当前离线夹具清单

| 模块 | 夹具路径 | 用途 |
|---|---|---|
| OCSP | `tests/fixtures/p2/ocsp/ocsp_response_malformed_v1.der` | 覆盖 malformed 响应解析失败 |
| CT | `tests/fixtures/p2/ct/ct_log_list_invalid_v1.txt` | 覆盖无效 log list 加载失败 |
| TS | `tests/fixtures/p2/ts/ts_response_malformed_v1.der` | 覆盖 malformed 响应解析失败 |

说明：
- “缺失文件”类失败场景使用固定的不存在路径（例如 `*_missing_v1.*`）进行断言，不需要额外二进制夹具。
- 新增夹具时建议按 `*_v{n}` 版本化命名，禁止覆盖已有样本。

---

## 3. 失败场景覆盖矩阵（已落地）

### 3.1 OCSP（`test_p2_ocsp_comprehensive`）

- `TestOCSP_OfflineMalformedFixture`：malformed 响应解析返回 `nil`。
- `TestOCSP_TruncatedRequestFailure`：截断请求在解析/构造链路中失败。
- `TestOCSP_TimeValidityWindowFailure`：
  - `thisUpdate` 未来时间失败；
  - `nextUpdate` 过期失败。
- `TestOCSP_UnsignedResponseVerificationFailure`：无签名（或无效）响应 `VerifyOCSPResponse` 失败。

### 3.2 CT（`test_p2_ct_comprehensive`）

- `TestCT_OfflineInvalidLogListFixture`：无效日志列表加载失败。
- `TestCT_MissingLogListFileFailure`：缺失日志文件加载失败。
- `TestCT_TimeIssuerMismatchFailure`：时间与 issuer 条件不匹配时验证失败。

### 3.3 TS（`test_p2_ts_comprehensive`）

- `TestTS_OfflineMalformedResponseFixture`：malformed 响应解析失败。
- `TestTS_TruncatedResponseFailure`：截断响应解析失败。
- `TestTS_RejectionStatusFailure`：
  - 响应无状态信息时验证失败（稳定兜底）；
  - 若 `TS_STATUS_INFO_set_status` 可用，额外覆盖显式 rejection 状态。
- `TestTS_EmptyResponseSignatureFailure`：空响应签名验证失败。

---

## 4. 标准离线验证流程（推荐）

### Step A：仅验证证书服务模块（CT/OCSP/TS）

```bash
bash scripts/run_all_module_tests.sh --modules CT,OCSP,TS --verbose
```

验收标准：
- 无失败用例（`failed: 0`）。
- 输出中可看到三模块 comprehensive 套件均已执行。

### Step B：执行 P2 聚焦回归

```bash
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

验收标准：
- 总体 `failed: 0`。
- CT/OCSP/TS 变更未引入对其他 P2 模块的回归。

---

## 5. 在线路径与离线路径边界

以下能力存在网络依赖，默认不作为离线验收口径：

- OCSP 在线请求发送（`SendOCSPRequest`）需要上层提供 HTTP transport hooks（见 `fafafa.ssl.net.hooks`），线上验证仍受网络与 responder 状态影响。
- TS 完整端到端（向 TSA 发送请求并获取真实响应）需要可达 TSA 服务。
- CT 远端日志拉取（如在线日志源）需要上层提供 HTTP GET hooks（`DownloadCTLogList` 通过 hooks 拉取），并依赖外部网络与服务稳定性。

建议：
- 将在线路径归类为“集成验证（Integration）”；
- 将离线路径归类为“门禁验证（Gate）”，保证 CI/本地可重复。

---

## 6. OpenSSL 1.1.1 vs 3.x 兼容策略

- 3.x 下若部分 1.x only API 未加载，按测试分支策略跳过可选断言。
- 跳过范围仅限辅助 API，不影响以下主流程断言：
  - OCSP 响应解析与验证失败路径；
  - CT log list 加载/策略失败路径；
  - TS 响应解析/状态校验/签名失败路径。

相关差异说明见：
- `docs/reference/OPENSSL_1_1_1_VS_3X_DIFF_AND_REGRESSION.md`

---

## 7. 故障排查（离线验证）

1. **夹具路径错误**：优先检查 `tests/fixtures/p2/{ocsp,ct,ts}/`。
2. **模块未加载**：确认初始化已调用 `LoadOpenSSLOCSP` / `LoadCTFunctions` / `LoadTSFunctions`。
3. **OpenSSL 可选符号缺失**：按 3.x 分支策略确认是否应走兜底断言，而非直接判失败。
4. **结果不稳定**：优先改为固定输入 + 固定时间窗口，避免依赖系统时间与外部服务。

---

## 8. 关联文档

- `docs/testing/P2_OFFLINE_FIXTURE_GUIDE.md`
- `docs/test_reports/P2_MODULES_TEST_REPORT.md`
- `docs/test_reports/P2_OCSP_MODULE_REPORT.md`
- `docs/test_reports/P2_TS_MODULE_REPORT.md`
- `docs/guides/OCSP_USAGE_GUIDE.md`
- `docs/guides/TS_USAGE_GUIDE.md`
- `docs/guides/CT_IMPLEMENTATION_GUIDE.md`
