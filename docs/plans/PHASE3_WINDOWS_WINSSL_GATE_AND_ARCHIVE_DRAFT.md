# Phase 3 Windows WinSSL 门禁与归档映射草案（Draft）

**目标**：在 Windows 平台建立 “WinSSL 主路径 + OpenSSL 对照路径” 的分层门禁，并与 CI 产物归档策略对齐。  
**阶段**：Batch B14

---

## 1. 策略原则

1. **WinSSL 是 Windows 主路径**：发布门禁必须包含 Schannel（WinSSL）验证。
2. **OpenSSL 是兼容对照路径**：用于发现 API/行为差异，不替代 WinSSL 主路径。
3. **同一批次统一归档**：无论 WinSSL 或 OpenSSL，都归档到同一 `run_id` 下，便于对照。

---

## 2. Windows 分层门禁（Draft）

### L0：环境与依赖预检（阻断）

```powershell
fpc -iV
# vcpkg OpenSSL 安装与 OPENSSL_ROOT/PATH 注入
# Lazarus/lazbuild 可用性校验
```

### L1：编译门禁（阻断）

```powershell
lazbuild tests/test_core_comprehensive.lpi
lazbuild tests/test_p2_pkcs7_comprehensive.lpi
lazbuild tests/test_p2_pkcs12_comprehensive.lpi
lazbuild tests/test_p2_cms_comprehensive.lpi
lazbuild tests/test_p2_ocsp_comprehensive.lpi
lazbuild tests/test_p2_ct_comprehensive.lpi
lazbuild tests/test_p2_ts_comprehensive.lpi
lazbuild tests/test_p2_engine_comprehensive.lpi
lazbuild tests/test_winssl_comprehensive.lpi
```

### L2：功能回归（阻断）

```powershell
.\tests\bin\test_core_comprehensive.exe
.\tests\bin\test_p2_pkcs7_comprehensive.exe
.\tests\bin\test_p2_pkcs12_comprehensive.exe
.\tests\bin\test_p2_cms_comprehensive.exe
.\tests\bin\test_p2_ocsp_comprehensive.exe
.\tests\bin\test_p2_ct_comprehensive.exe
.\tests\bin\test_p2_ts_comprehensive.exe
.\tests\bin\test_p2_engine_comprehensive.exe
.\tests\bin\test_winssl_comprehensive.exe
```

### L3：对照验证（Nightly/Release）

```powershell
# WinSSL 主路径回归
powershell -ExecutionPolicy Bypass -File run_winssl_tests.ps1

# OpenSSL 对照回归
powershell -ExecutionPolicy Bypass -File run_openssl_tests.ps1
```

---

## 3. 双轨结果判定

- **WinSSL 轨（必须）**：`L1 + L2` 全通过，否则阻断。
- **OpenSSL 轨（对照）**：
  - PR：失败不阻断，但自动生成差异工单。
  - Release：失败阻断，需给出差异解释或临时豁免。

---

## 4. 与 B11 归档策略映射

| 输出内容 | 建议归档 class |
|---------|----------------|
| 编译/回归报告 | `core-reports` |
| 对照日志（WinSSL vs OpenSSL） | `debug-logs` |
| 发布候选对照结论文档 | `docs-evidence` |

建议命令（在 Windows runner 的 bash step 中执行）：

```bash
bash scripts/archive_ci_artifacts_draft.sh --profile release --run-id windows_${RUN_ID}
```

---

## 5. CI 接入建议（Draft）

- 复用 `test-all-platforms.yml` 的 Windows job：
  - 注意：该 workflow 当前以模板形式保留为 `.github/workflows/test-all-platforms.yml.disabled`，需要时可按需启用。
  - L0/L1/L2 按现有 lazbuild + exe 执行流程。
  - L3 在 nightly/release 触发时追加。
- 归档命名建议：
  - `Test-Results-Windows-FPC<version>`（原有）
  - `CI-Archive-Windows-<run_id>`（新增）

---

## 6. 验收口径（B14）

- 明确 WinSSL 与 OpenSSL 的分工与阻断规则。
- 明确 Windows L0-L3 对应命令。
- 明确与 B11 产物归档 class 的映射关系。

---

## 7. 后续任务

- B15：将 Linux/macOS/Windows 分层门禁整合为 CI workflow 草案。
- B16：统一门禁证据模板（平台一致字段 + 阈值 +结论）。
- B17：发布级归档保留策略与清理窗口草案。
