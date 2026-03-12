# Wave B / B2 Runner 命令块（2026-02-08）

## 目标

提供可直接粘贴到 CI Job（或 runner 终端）的命令块，完成 B2 实机回填闭环：

1. macOS 产出 `wave_b_macos_gate_summary_<run_id>.md`
2. Windows 产出 `wave_b_windows_gate_summary_<run_id>.md`
3. 汇总产出 `wave_b_cross_platform_summary_<run_id>.md`

---

## 统一变量

```bash
# 在 Linux/macOS shell 中
RUN_ID="$(date +%Y%m%d_%H%M%S)"
LINUX_SUMMARY="docs/archive/reports/wave-b-history/wave_b_ci_gate_summary_20260208_034029.md"
LINUX_EXAMPLES="docs/archive/reports/examples-compile-history/examples_compile_ci_gate.json"
```

> 说明：`LINUX_SUMMARY` 可替换为最新一次 Linux gate 报告。

---

## A. macOS runner（live）

```bash
# 1) 进入仓库
cd /path/to/fafafa.ssl

# 2) 执行 Wave B macOS gate（live）
bash scripts/run_wave_b_macos_gate.sh \
  --run-id "$RUN_ID" \
  --output-dir test-reports \
  --openssl-root /opt/homebrew/opt/openssl@3

# 3) 产物检查
ls -lh "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md"
```

若 Intel Homebrew 路径不同，可改为：`--openssl-root /usr/local/opt/openssl@3`。

---

## B. Windows runner（live）

```powershell
# 1) 进入仓库
Set-Location C:\path\to\fafafa.ssl

# 2) 执行 Wave B Windows gate（live）
powershell -ExecutionPolicy Bypass -File scripts/run_wave_b_windows_gate.ps1 -RunId $env:RUN_ID

# 3) 产物检查
Get-Item "test-reports/wave_b_windows_gate_summary_$($env:RUN_ID).md"
```

建议在 Windows Job 环境先设置 `RUN_ID`：

```powershell
$env:RUN_ID = Get-Date -Format "yyyyMMdd_HHmmss"
```

---

## C. 汇总（三平台）

在任一可访问三平台产物的 Linux/macOS 环境执行：

```bash
cd /path/to/fafafa.ssl

bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --output "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md"

cat "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md"
```

---

## D. 验收检查

- `wave_b_macos_gate_summary_<run_id>.md` 存在且 `overall` 非 `DRY_RUN`
- `wave_b_windows_gate_summary_<run_id>.md` 存在且 `overall` 非 `DRY_RUN`
- `wave_b_cross_platform_summary_<run_id>.md` 中：
  - `linux` 为 `PASS`
  - `macos` / `windows` 状态可读（`PASS` 或 `FAIL`，不应为 `PENDING`）

---

## E. 演练命令（可选）

```bash
# macOS 链路演练（非 macOS 也可）
bash scripts/run_wave_b_macos_gate.sh --dry-run --run-id rehearsal_001 --output-dir test-reports

# 汇总演练（使用 dry-run 摘要）
bash scripts/generate_wave_b_cross_platform_summary.sh \
  --run-id rehearsal_001 \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_rehearsal_001.md" \
  --output "test-reports/wave_b_cross_platform_summary_rehearsal_001.md"
```

---

## F. B2 闭环判定（推荐）

```bash
cd /path/to/fafafa.ssl

bash scripts/check_wave_b_b2_closure_readiness.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --output "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md"

cat "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md"
```

若需要把“未闭环”作为流水线失败条件：

```bash
bash scripts/check_wave_b_b2_closure_readiness.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --strict
```

---

## G. GitHub Actions 模板（B94）

- 模板文件：`.github/workflows/wave-b-b2-manual.yml.disabled`
- 触发方式：`workflow_dispatch`（手动触发）
- 覆盖链路：
  1. Linux baseline（可选）
  2. macOS gate
  3. Windows gate
  4. cross-platform summary + closure readiness

启用方式（按需）：

```bash
cp .github/workflows/wave-b-b2-manual.yml.disabled .github/workflows/wave-b-b2-manual.yml
```

> 建议先在测试分支启用，再执行一次手动 workflow_dispatch 验证。

---

## H. 证据一致性校验（B95）

```bash
cd /path/to/fafafa.ssl

bash scripts/check_wave_b_b2_evidence_consistency.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --cross-summary "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md" \
  --closure-report "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md" \
  --output "test-reports/wave_b_b2_evidence_consistency_${RUN_ID}.md"
```

严格门禁模式：

```bash
bash scripts/check_wave_b_b2_evidence_consistency.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --cross-summary "test-reports/wave_b_cross_platform_summary_${RUN_ID}.md" \
  --closure-report "test-reports/wave_b_b2_closure_readiness_${RUN_ID}.md" \
  --strict
```

---

## I. B96 接入说明（workflow summary job）

在 `.github/workflows/wave-b-b2-manual.yml.disabled` 的 `summary` job 中，已按顺序串联：

1. `generate_wave_b_cross_platform_summary.sh`
2. `check_wave_b_b2_closure_readiness.sh`
3. `check_wave_b_b2_evidence_consistency.sh`

当 `strict_closure=true` 时，会额外触发 strict dry-run 判定（不重写报告，仅执行门禁退出码判断）。

---

## J. 一键交接包生成（B97）

```bash
cd /path/to/fafafa.ssl

bash scripts/prepare_wave_b_b2_handoff_bundle.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --output-dir test-reports
```

严格模式（闭环前会失败，作为阻断信号）：

```bash
bash scripts/prepare_wave_b_b2_handoff_bundle.sh \
  --run-id "$RUN_ID" \
  --linux-summary "$LINUX_SUMMARY" \
  --linux-examples "$LINUX_EXAMPLES" \
  --macos-summary "test-reports/wave_b_macos_gate_summary_${RUN_ID}.md" \
  --windows-summary "test-reports/wave_b_windows_gate_summary_${RUN_ID}.md" \
  --output-dir test-reports \
  --strict
```
