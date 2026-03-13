# Wave B / B2 收口 + Wave C 启动（2026-03-14）

## Goal
- **收口 B2**：在 GitHub Actions 里让 Linux/macOS/Windows 三平台 Wave B gate 全链路可运行、证据一致性脚本不误报，并形成可交接的闭环产物。
- **启动 Wave C**：在保持门禁稳定的前提下，开始 Phase 2 性能基线与“至少一项可证明优化”的执行准备。

## Non-Goals
- 不在本计划内实现网络通信库（HTTP/TCP/Socket 等）；`fafafa.ssl` 仅提供 TLS/SSL 能力与可注入 hooks。

## Why now
- 2026 路线图要求 Q2 建立可信的跨平台最小验证矩阵；B2 是 Wave B 的最后短板。
- B2 的证据链（run_id / summary / examples json）一旦漂移，会让 closure readiness 与 consistency 校验失去审查价值。

## Architecture / Approach
1) **统一 run_id**：所有平台 summary 与 examples report 统一使用同一 `RUN_ID`，避免文件名与内容漂移。
2) **平台脚本稳健性**：
   - macOS：不依赖 GNU timeout（回退 python 超时执行）
   - Windows：PowerShell common `-Verbose` 兼容；gate 子步骤优先 `pwsh`；日志 UTF-8
   - Windows modules：模块清单动态扫描 + 最小数量阈值，防止假阳性 PASS
3) **闭环证据产物**：
   - cross-platform summary
   - closure readiness
   - evidence consistency

## Files (touched in this wave)
- Workflows:
  - `.github/workflows/wave-b-b2-manual.yml`
  - `.github/workflows/wave-b-b2-manual.yml.disabled`
- Runner docs:
  - `docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`

## Step-by-step (CI)
1) 触发 `Wave B B2 Manual Gate (Template)` workflow_dispatch：
   - `run_id`: 留空或手工指定（推荐在回填/复盘时手工指定以便追踪）
   - `strict_closure`: `true`（要把“未闭环”当作失败信号时）
2) 预期 artifacts：
   - `wave-b-linux-<run_id>`
   - `wave-b-macos-<run_id>`
   - `wave-b-windows-<run_id>`
   - `wave-b-summary-<run_id>`
3) 预期 summary 产物（来自 `wave-b-summary-<run_id>`）：
   - `test-reports/wave_b_cross_platform_summary_<run_id>.md`
   - `test-reports/wave_b_b2_closure_readiness_<run_id>.md`
   - `test-reports/wave_b_b2_evidence_consistency_<run_id>.md`
4) 验收口径：
   - 三平台 `overall` 字段可横向比较
   - `wave_b_b2_evidence_consistency_<run_id>.md` 不出现 run_id mismatch / parse issue

## Step-by-step (Runner / Manual)
参考命令块：`docs/test_reports/WAVE_B_B2_RUNNER_COMMAND_BLOCKS_2026-02-08.md`。

## Step-by-step (Wave C kickoff)
1) 基线演练（低成本验证脚本可跑通）：
   - `bash scripts/run_minimal_ci_gate.sh --fast-local --skip-compile`
   - `bash scripts/run_phase2_performance_baseline.sh --dry-run`
2) 选 1 个优化点（从 roadmap 候选中挑“最容易证明”的）：
   - 证书链验证缓存命中（或）
   - 会话缓存策略（或）
   - 热点路径减少内存拷贝
3) 每个优化必须提供：
   - 优化前/后同机同参对比报告
   - 回归门禁命令三件套（compile + P2 modules + examples）

## Expected Outputs / Acceptance
- B2：CI summary job 产物齐全且一致性/闭环检查不误报
- Wave C：基线脚本可复现，且优化候选有可执行的对比与回归命令链路
