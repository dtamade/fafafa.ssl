# Phase 3 跨平台门禁分层策略草案（Draft）

**目标**：把 Linux/macOS/Windows 的验证流程拆分为可分级执行的门禁层（快速阻断 + 深度验证），并统一归档口径。  
**阶段**：Batch B12

---

## 1. 分层模型（Gate Layer）

定义四层门禁，按成本递增：

- `L0` 环境预检：工具链与关键依赖可用性。
- `L1` 快速阻断：编译 + 核心模块回归（面向 PR 必跑）。
- `L2` 扩展验证：平台特定兼容链路 + 基准入口 dry-run。
- `L3` 深度验证：矩阵/性能/对照验证（Nightly/Release 优先）。

阻断原则：

1. `L0/L1` 失败：阻断合并。
2. `L2` 失败：默认不阻断 PR，但必须进入当日修复队列。
3. `L3` 失败：不阻断普通 PR；阻断 release 分支或 release tag。

---

## 2. 平台分层命令（Draft）

### 2.1 Linux

`L0`（预检）

```bash
fpc -iV
openssl version
```

`L1`（快速阻断）

```bash
python3 scripts/compile_all_modules.py
bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT
```

`L2`（扩展验证）

```bash
bash scripts/run_minimal_ci_gate.sh --dry-run
bash scripts/run_linux_openssl_matrix_draft.sh --dry-run
```

`L3`（深度验证，夜间/发布）

```bash
bash scripts/run_linux_openssl_matrix_draft.sh --skip-phase2-dryrun --verbose
bash scripts/run_phase2_performance_baseline.sh --iterations 50 --tls-iterations 20 --with-tls
```

### 2.2 macOS

`L0`（预检）

```bash
fpc -iV
brew list openssl@3 >/dev/null
```

`L1`（快速阻断）

```bash
# 参考 .github/workflows/test-all-platforms.yml.disabled 的 compile steps
for test in tests/test_*.pas; do
  if [[ ! "$test" =~ winssl ]]; then
    fpc -Fusrc -FEtests/bin "$test" || exit 1
  fi
done
```

`L2`（扩展验证）

```bash
for test in tests/bin/test_*; do
  if [[ ! "$test" =~ winssl ]] && [[ -x "$test" ]]; then
    "$test" || true
  fi
done
bash scripts/run_phase2_performance_baseline.sh --dry-run --iterations 200 --tls-iterations 50
```

`L3`（深度验证，后续 B13 落细）

```bash
# 目标：补齐 openssl@3 路径探测 + rpath 校验 + 失败证据归档
# 由 B13 产出可执行命令草案
```

### 2.3 Windows

`L0`（预检）

```powershell
fpc -iV
# OpenSSL 路径由 vcpkg 安装后注入
# OPENSSL_ROOT / PATH 可见性校验
```

`L1`（快速阻断）

```powershell
# 参考 .github/workflows/test-all-platforms.yml.disabled 的 lazbuild 阶段
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

`L2`（扩展验证）

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

`L3`（深度验证，后续 B14 落细）

```powershell
# 目标：WinSSL 与 OpenSSL 对照验证 + 产物分层归档
# 由 B14 产出差异门禁与归档映射草案
```

---

## 3. 与 B11 归档策略映射

每层门禁输出统一归档到 `scripts/archive_ci_artifacts_draft.sh` 的 class：

- `L0/L1` 失败日志、编译与测试汇总 → `core-reports`
- `L2` 基准 dry-run 与兼容命令日志 → `core-reports` + `perf-baseline`
- `L3` 性能与对照验证日志 → `perf-baseline` + `docs-evidence`
- 调试细节（编译日志等） → `debug-logs`

建议归档命令（示例）：

```bash
bash scripts/archive_ci_artifacts_draft.sh --profile pr --run-id ${RUN_ID}
```

---

## 4. 触发策略建议（Draft）

| Trigger | 默认执行层 | 目的 |
|---------|------------|------|
| Pull Request | `L0 + L1`（Linux 必跑，macOS/Windows 可并行） | 快速阻断回归 |
| Nightly | `L0 + L1 + L2`（三平台） | 发现环境与兼容漂移 |
| Release Tag | `L0 + L1 + L2 + L3` | 发布前深度验证 |

---

## 5. 验收口径（B12）

- 明确 Linux/macOS/Windows 各层门禁职责与命令入口。
- 明确 `L0~L3` 的阻断策略（PR/Nightly/Release）。
- 明确与 B11 归档类别映射关系。

---

## 6. 后续任务

- B13：补齐 macOS OpenSSL 路径与 rpath 验证命令草案。
- B14：补齐 Windows WinSSL/OpenSSL 对照门禁与归档映射。
- B15：将分层策略转为 CI workflow 草案（可直接接入 GitHub Actions）。
