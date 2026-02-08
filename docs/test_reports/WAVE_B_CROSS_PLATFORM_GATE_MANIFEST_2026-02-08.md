# Wave B 跨平台门禁执行清单（2026-02-08）

## 目标

在保持 Linux 门禁稳定 PASS 的前提下，为 Wave B / B2 提供可直接执行的 macOS / Windows 门禁清单、统一产物命名和回填判定模板。

## 当前基线（已完成）

| 平台 | 报告 | 结果 |
|------|------|------|
| Linux | `test-reports/wave_b_ci_gate_summary_20260208_025426.md` | PASS |
| Linux examples | `test-reports/examples_compile_ci_gate.json` | `71/75`，`failed=0`，`pass_rate=100.0%` |
| macOS 探针（非实机） | `test-reports/wave_b_macos_gate_probe_20260208.json` | `status=error`（非 macOS 环境预期） |

---

## macOS 执行口径（Runner 实机）

### Step M1：OpenSSL 路径探测

```bash
bash scripts/detect_macos_openssl_enhanced.sh --json \
  > test-reports/wave_b_macos_gate_probe_${RUN_ID}.json
```

### Step M2：门禁脚本联调

```bash
bash scripts/run_macos_openssl_path_check_draft.sh --dry-run \
  > test-reports/wave_b_macos_path_check_${RUN_ID}.log 2>&1
```

### Step M3：核心门禁（建议与 Linux 同口径）

```bash
python3 scripts/compile_all_modules.py \
  > test-reports/wave_b_macos_compile_${RUN_ID}.log 2>&1

bash scripts/run_all_module_tests.sh --modules PKCS7,PKCS12,CMS,Store,OCSP,TS,CT \
  > test-reports/wave_b_macos_modules_${RUN_ID}.log 2>&1

bash scripts/verify_examples_compile.sh -f json \
  -o test-reports/examples_compile_gate_macos_${RUN_ID}.json \
  > test-reports/wave_b_macos_examples_${RUN_ID}.log 2>&1
```

---

## Windows 执行口径（Runner 实机）

### Step W1：WinSSL 主路径

```powershell
powershell -ExecutionPolicy Bypass -File run_winssl_tests.ps1 \
  *> test-reports/wave_b_windows_winssl_${RUN_ID}.log
```

### Step W2：OpenSSL 对照路径

```powershell
powershell -ExecutionPolicy Bypass -File run_openssl_tests.ps1 \
  *> test-reports/wave_b_windows_openssl_${RUN_ID}.log
```

### Step W3：模块完整性校验

```powershell
powershell -ExecutionPolicy Bypass -File scripts/validate_all_modules.ps1 \
  *> test-reports/wave_b_windows_modules_${RUN_ID}.log
```

---

## 统一判定模板（回填）

回填文件建议：`test-reports/wave_b_cross_platform_summary_${RUN_ID}.md`

| Check | Linux | macOS | Windows |
|------|-------|-------|---------|
| compile_all_modules | PASS | TODO | TODO |
| P2 modules (PKCS7/PKCS12/CMS/Store/OCSP/TS/CT) | PASS | TODO | TODO |
| examples compile gate | PASS (`failed=0`) | TODO | TODO |
| overall | PASS | TODO | TODO |

---

## B2 状态

- 当前状态：`in_progress`
- 完成条件：
  1. macOS 与 Windows 各至少 1 次实机执行记录；
  2. 统一摘要 `wave_b_cross_platform_summary_<run_id>.md` 回填完成；
  3. 三平台门禁判定字段可横向比较。

---

## B89 自动化补充（2026-02-08 03:49 +0800）

新增统一摘要生成脚本：

- `scripts/generate_wave_b_cross_platform_summary.sh`

### 示例命令

```bash
bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id 20260208_034029 \
  --linux-summary test-reports/wave_b_ci_gate_summary_20260208_034029.md \
  --macos-probe test-reports/wave_b_macos_gate_probe_20260208.json \
  --output test-reports/wave_b_cross_platform_summary_20260208_034029.md
```

### 本轮产物

- `test-reports/wave_b_cross_platform_summary_20260208_034029.md`
  - linux: `PASS`
  - macos: `PROBE_ONLY`
  - windows: `PENDING`

结论：B2 进入“可一键汇总 + 待实机回填”状态。

---

## B90 Runner 封装脚本（2026-02-08 03:55 +0800）

为 B2 实机执行新增统一入口：

- macOS: `scripts/run_wave_b_macos_gate.sh`
- Windows: `scripts/run_wave_b_windows_gate.ps1`

### macOS 封装示例

```bash
# 实机执行
bash scripts/run_wave_b_macos_gate.sh \
  --run-id <RUN_ID> \
  --openssl-root /opt/homebrew/opt/openssl@3

# 联调演练（非 macOS 可 dry-run）
bash scripts/run_wave_b_macos_gate.sh --dry-run --run-id rehearsal_001
```

### Windows 封装示例

```powershell
# 实机执行
powershell -ExecutionPolicy Bypass -File scripts/run_wave_b_windows_gate.ps1 -RunId <RUN_ID>

# 演练
powershell -ExecutionPolicy Bypass -File scripts/run_wave_b_windows_gate.ps1 -DryRun -RunId rehearsal_001
```

### 产物约定（封装脚本输出）

- macOS: `test-reports/wave_b_macos_gate_summary_<run_id>.md`
- Windows: `test-reports/wave_b_windows_gate_summary_<run_id>.md`

说明：上述平台摘要可直接作为 `generate_wave_b_cross_platform_summary.sh` 的输入证据。

---

## B90 收口校验 + B91 状态判定修复（2026-02-08 04:04 +0800）

### 本轮验证

- macOS runner 语法检查：
  - `bash -n scripts/run_wave_b_macos_gate.sh`（通过）
- macOS runner dry-run：
  - `bash scripts/run_wave_b_macos_gate.sh --dry-run --run-id 20260208_041500 --output-dir test-reports`
  - 产物：`test-reports/wave_b_macos_gate_summary_20260208_041500.md`
  - 关键字段：`mode: dry-run`，`overall: DRY_RUN`
- Windows runner 说明：
  - 当前 Linux 环境 `pwsh` 不可用，仅完成静态脚本校对，待 Windows runner 实机回填。

### B91 修复内容

- 修复脚本：`scripts/generate_wave_b_cross_platform_summary.sh`
  - 新增平台摘要 `overall` 字段解析（`PASS`/`FAIL`/`DRY_RUN`）。
  - 避免把 macOS dry-run 误标为 `READY`。
- 验证产物：
  - `test-reports/wave_b_cross_platform_summary_20260208_041500.md`
  - 平台状态更新为：`macos = DRY_RUN`，`windows = PENDING`。

### 下一步（B2 实机回填）

1. macOS runner 执行 live 门禁并提交 `wave_b_macos_gate_summary_<run_id>.md`。
2. Windows runner 执行 live 门禁并提交 `wave_b_windows_gate_summary_<run_id>.md`。
3. 复跑 `scripts/generate_wave_b_cross_platform_summary.sh` 生成最终三平台闭环摘要。

---

## B92 Runner 命令块（2026-02-08 04:08 +0800）

- 新增：`docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`
- 作用：提供可直接粘贴到 CI Job 的 macOS/Windows live 执行命令，以及三平台汇总命令。
- 价值：降低实机回填接入成本，缩短 B2 最后闭环路径。

---

## B93 闭环判定脚本（2026-02-08 04:18 +0800）

- 新增：`scripts/check_wave_b_b2_closure_readiness.sh`
- 产物：`test-reports/wave_b_b2_closure_readiness_<run_id>.md`
- 能力：
  - 解析三平台 summary 的 `overall` 字段；
  - 输出 B2 是否 `CLOSED` / `IN_PROGRESS`；
  - `--strict` 模式下未闭环返回非 0，便于 CI 门禁接入。

本轮样例：
- `test-reports/wave_b_b2_closure_readiness_20260208_041500.md`（`IN_PROGRESS`）

---

## B94 CI Job 模板化（2026-02-08 04:26 +0800）

- 新增模板：`.github/workflows/wave-b-b2-manual.yml.disabled`
- 特点：
  - 仅手动触发（`workflow_dispatch`），避免自动跑批；
  - 汇总阶段自动执行：
    - `scripts/generate_wave_b_cross_platform_summary.sh`
    - `scripts/check_wave_b_b2_closure_readiness.sh`
- 作用：把 B2 从“命令块执行”升级为“可重复 job 模板执行”。

---

## B95 证据一致性校验（2026-02-08 04:30 +0800）

- 新增：`scripts/check_wave_b_b2_evidence_consistency.sh`
- 功能：
  - 校验证据文件存在性（summary/json）；
  - 校验 markdown 证据 `run_id` 与目标 run_id 一致性；
  - 在 `--strict` 模式下将不一致状态转换为非 0 退出。

样例：
- `test-reports/wave_b_b2_evidence_consistency_20260208_041500.md`（`INCONSISTENT`）
  - 原因：Linux summary run_id 与目标 run_id 不一致。

---

## B96 流水线接线（2026-02-08 04:35 +0800）

- 已更新模板：`.github/workflows/wave-b-b2-manual.yml.disabled`
- summary job 现支持：
  - 先生成 cross summary + closure readiness + evidence consistency 三份报告；
  - 再在 strict 模式下执行 dry-run 门禁判定。
- 结果：B2 回填路径具备“报告产出 + 可选严格阻断”的完整闭环行为。

---

## B97 交接包生成器（2026-02-08 04:38 +0800）

- 新增：`scripts/prepare_wave_b_b2_handoff_bundle.sh`
- 能力：
  - 一次执行串联生成 cross summary / closure readiness / evidence consistency；
  - 产出 handoff index：`wave_b_b2_handoff_bundle_<run_id>.md`；
  - `--strict` 模式下执行最终阻断判定。

样例：
- `test-reports/wave_b_b2_handoff_bundle_20260208_041500.md`
  - `handoff_state=NEEDS_EVIDENCE_SYNC`
