# Phase 3 CI 产物归档策略草案（Draft）

**目标**：将 CI 输出统一归档为“可追溯、可下载、可保留策略化”的结构，降低回归定位成本。  
**阶段**：Batch B11

---

## 1. 归档目标与边界

归档策略覆盖以下来源：

1. `test-reports/`：模块测试汇总、单测输出、编译日志。
2. `tests/benchmarks/results/`：性能基线汇总与明细日志。
3. `docs/test_reports/` 与 `docs/plans/`：对应批次的审阅文档证据。
4. （可选）测试可执行文件：用于一次性离线复盘，不作为默认产物。

---

## 2. 产物分类与保留策略（Draft）

| class | 典型内容 | 建议保留（PR） | 建议保留（Nightly） | 建议保留（Release） |
|------|----------|----------------|---------------------|---------------------|
| `core-reports` | `test_report_*.txt`、`*_result.txt`、`*_compile.log` | 30 天 | 14 天 | 90 天 |
| `perf-baseline` | `benchmark_summary_*.txt`、`*.log`、`*baseline*.json` | 14 天 | 30 天 | 90 天 |
| `docs-evidence` | `docs/test_reports/PHASE2_*.md`、`docs/plans/PHASE3_*.md` | 30 天 | 30 天 | 90 天 |
| `debug-logs` | 额外编译日志与调试日志 | 7 天 | 7 天 | 14 天 |
| `binaries`（可选） | `bin/test_*`、`tests/benchmarks/bin/*` | 7 天 | 7 天 | 14 天 |

说明：
- `PR` 偏向“快速回溯”；
- `Nightly` 偏向“趋势观察”；
- `Release` 偏向“审计留痕”。

---

## 3. 命名与目录规范（Draft）

统一输出根目录：`artifacts/ci/`。

- 单次归档目录：`artifacts/ci/<run_id>/`
- 压缩包：`artifacts/ci/<run_id>_<profile>_ci_artifacts.tar.gz`
- 清单文件：
  - `manifest.csv`：逐文件（class/retention/path）
  - `manifest.md`：摘要（计数/策略/批次信息）

`run_id` 建议使用：`yyyyMMdd_HHmmss`，保证可排序与可读性。

---

## 4. 脚本入口（已提供）

- `scripts/archive_ci_artifacts_draft.sh`

常用命令：

```bash
# 建议先跑 dry-run 查看归档计划
bash scripts/archive_ci_artifacts_draft.sh --dry-run

# PR 场景（默认 profile=pr）
bash scripts/archive_ci_artifacts_draft.sh --profile pr

# Release 场景 + 保留二进制
bash scripts/archive_ci_artifacts_draft.sh --profile release --include-binaries

# 指定 run_id 与输出目录
bash scripts/archive_ci_artifacts_draft.sh \
  --run-id 20260207_040000 \
  --output-root artifacts/ci
```

---

## 5. 建议接入点（GitHub Actions）

最小接入顺序：

1. 先执行门禁脚本（如 `run_minimal_ci_gate.sh`）。
2. 执行归档脚本（建议先 dry-run 联调，再实跑）。
3. 上传 `artifacts/ci/<run_id>/`（或压缩包）为 workflow artifact。

示例（片段）：

```yaml
- name: Build CI artifact bundle
  run: bash scripts/archive_ci_artifacts_draft.sh --profile pr

- name: Upload CI artifacts
  uses: actions/upload-artifact@v4
  with:
    name: ci-artifacts-${{ github.run_id }}
    path: artifacts/ci/
    retention-days: 30
```

---

## 6. 验收口径（B11）

- `scripts/archive_ci_artifacts_draft.sh --dry-run` 返回 0。
- 至少识别并输出 `core-reports`、`perf-baseline`、`docs-evidence` 三类产物计划。
- 实跑时可生成 `manifest.csv` 与 `manifest.md`。

---

## 7. 与后续任务关系

- B12：基于该归档策略，定义跨平台门禁分层（Linux/macOS/Windows）的产物最小集合。
- B13：补齐 macOS OpenSSL 路径验证命令，并对接到归档策略的 `core-reports` 类别。
