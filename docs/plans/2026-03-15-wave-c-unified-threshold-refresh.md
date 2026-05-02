# Wave C unified threshold refresh（2026-03-15）

## Goal
- 基于 2026-03-15 的统一 fast-local 入口，重新生成 B106 风格阈值观察表。
- 用新的 B101 full-gate 连续样本替代旧的 2026-02-08 旁路 probe 样本。

## Architecture / Approach
1. 固定入口：
   - `bash scripts/run_wave_c_b101_validation_playbook.sh --fast-local --strict --full-gate`
2. 采样：
   - 使用 3 次连续 run 的 `overall / hit_rate_percent / speedup_factor_x`
3. 产物：
   - 新 comparison/threshold 文档
   - 更新 readiness refresh 文档
   - 更新文档索引

## Evidence Runs
- `20260315_172046`
- `20260315_180632`
- `20260315_180735`

## Expected Outputs
- 新的阈值观察文档只引用 2026-03-15 之后的新入口证据
- readiness 文档明确：
  - 阈值已重新证明
  - 默认策略是否变化仍属人工决策
